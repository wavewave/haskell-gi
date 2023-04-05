{-# LANGUAGE ImplicitParams, RankNTypes, TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- t'GI.Gtk.Objects.Gesture.Gesture' is the base object for gesture recognition, although this
-- object is quite generalized to serve as a base for multi-touch gestures,
-- it is suitable to implement single-touch and pointer-based gestures (using
-- the special 'P.Nothing' t'GI.Gdk.Structs.EventSequence.EventSequence' value for these).
-- 
-- The number of touches that a t'GI.Gtk.Objects.Gesture.Gesture' need to be recognized is controlled
-- by the [Gesture:nPoints]("GI.Gtk.Objects.Gesture#g:attr:nPoints") property, if a gesture is keeping track of less
-- or more than that number of sequences, it won\'t check wether the gesture
-- is recognized.
-- 
-- As soon as the gesture has the expected number of touches, the gesture will
-- run the t'GI.Gtk.Objects.Gesture.Gesture'::@/check/@ signal regularly on input events until the gesture
-- is recognized, the criteria to consider a gesture as \"recognized\" is left to
-- t'GI.Gtk.Objects.Gesture.Gesture' subclasses.
-- 
-- A recognized gesture will then emit the following signals:
-- 
-- * [Gesture::begin]("GI.Gtk.Objects.Gesture#g:signal:begin") when the gesture is recognized.
-- * A number of [Gesture::update]("GI.Gtk.Objects.Gesture#g:signal:update"), whenever an input event is processed.
-- * [Gesture::end]("GI.Gtk.Objects.Gesture#g:signal:end") when the gesture is no longer recognized.
-- 
-- 
-- == Event propagation
-- 
-- In order to receive events, a gesture needs to either set a propagation phase
-- through 'GI.Gtk.Objects.EventController.eventControllerSetPropagationPhase', or feed those manually
-- through 'GI.Gtk.Objects.EventController.eventControllerHandleEvent'.
-- 
-- In the capture phase, events are propagated from the toplevel down to the
-- target widget, and gestures that are attached to containers above the widget
-- get a chance to interact with the event before it reaches the target.
-- 
-- After the capture phase, GTK+ emits the traditional [Widget::buttonPressEvent]("GI.Gtk.Objects.Widget#g:signal:buttonPressEvent"),
-- [Widget::buttonReleaseEvent]("GI.Gtk.Objects.Widget#g:signal:buttonReleaseEvent"), [Widget::touchEvent]("GI.Gtk.Objects.Widget#g:signal:touchEvent"), etc signals. Gestures
-- with the 'GI.Gtk.Enums.PropagationPhaseTarget' phase are fed events from the default [Widget::event]("GI.Gtk.Objects.Widget#g:signal:event")
-- handlers.
-- 
-- In the bubble phase, events are propagated up from the target widget to the
-- toplevel, and gestures that are attached to containers above the widget get
-- a chance to interact with events that have not been handled yet.
-- 
-- ## States of a sequence # {@/touch/@-sequence-states}
-- 
-- Whenever input interaction happens, a single event may trigger a cascade of
-- @/GtkGestures/@, both across the parents of the widget receiving the event and
-- in parallel within an individual widget. It is a responsibility of the
-- widgets using those gestures to set the state of touch sequences accordingly
-- in order to enable cooperation of gestures around the @/GdkEventSequences/@
-- triggering those.
-- 
-- Within a widget, gestures can be grouped through 'GI.Gtk.Objects.Gesture.gestureGroup',
-- grouped gestures synchronize the state of sequences, so calling
-- 'GI.Gtk.Objects.Gesture.gestureSetSequenceState' on one will effectively propagate
-- the state throughout the group.
-- 
-- By default, all sequences start out in the @/GTK_EVENT_SEQUENCE_NONE/@ state,
-- sequences in this state trigger the gesture event handler, but event
-- propagation will continue unstopped by gestures.
-- 
-- If a sequence enters into the @/GTK_EVENT_SEQUENCE_DENIED/@ state, the gesture
-- group will effectively ignore the sequence, letting events go unstopped
-- through the gesture, but the \"slot\" will still remain occupied while
-- the touch is active.
-- 
-- If a sequence enters in the @/GTK_EVENT_SEQUENCE_CLAIMED/@ state, the gesture
-- group will grab all interaction on the sequence, by:
-- 
-- * Setting the same sequence to @/GTK_EVENT_SEQUENCE_DENIED/@ on every other gesture
-- group within the widget, and every gesture on parent widgets in the propagation
-- chain.
-- * calling [Gesture::cancel]("GI.Gtk.Objects.Gesture#g:signal:cancel") on every gesture in widgets underneath in the
-- propagation chain.
-- * Stopping event propagation after the gesture group handles the event.
-- 
-- 
-- Note: if a sequence is set early to @/GTK_EVENT_SEQUENCE_CLAIMED/@ on
-- @/GDK_TOUCH_BEGIN/@\/@/GDK_BUTTON_PRESS/@ (so those events are captured before
-- reaching the event widget, this implies @/GTK_PHASE_CAPTURE/@), one similar
-- event will emulated if the sequence changes to @/GTK_EVENT_SEQUENCE_DENIED/@.
-- This way event coherence is preserved before event propagation is unstopped
-- again.
-- 
-- Sequence states can\'t be changed freely, see 'GI.Gtk.Objects.Gesture.gestureSetSequenceState'
-- to know about the possible lifetimes of a t'GI.Gdk.Structs.EventSequence.EventSequence'.
-- 
-- == Touchpad gestures
-- 
-- On the platforms that support it, t'GI.Gtk.Objects.Gesture.Gesture' will handle transparently
-- touchpad gesture events. The only precautions users of t'GI.Gtk.Objects.Gesture.Gesture' should do
-- to enable this support are:
-- 
-- * Enabling 'GI.Gdk.Flags.EventMaskTouchpadGestureMask' on their @/GdkWindows/@
-- * If the gesture has 'GI.Gtk.Enums.PropagationPhaseNone', ensuring events of type
-- 'GI.Gdk.Enums.EventTypeTouchpadSwipe' and 'GI.Gdk.Enums.EventTypeTouchpadPinch' are handled by the t'GI.Gtk.Objects.Gesture.Gesture'
-- 

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.Gesture
    ( 

-- * Exported types
    Gesture(..)                             ,
    IsGesture                               ,
    toGesture                               ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [group]("GI.Gtk.Objects.Gesture#g:method:group"), [handleEvent]("GI.Gtk.Objects.EventController#g:method:handleEvent"), [handlesSequence]("GI.Gtk.Objects.Gesture#g:method:handlesSequence"), [isActive]("GI.Gtk.Objects.Gesture#g:method:isActive"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [isGroupedWith]("GI.Gtk.Objects.Gesture#g:method:isGroupedWith"), [isRecognized]("GI.Gtk.Objects.Gesture#g:method:isRecognized"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [reset]("GI.Gtk.Objects.EventController#g:method:reset"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [ungroup]("GI.Gtk.Objects.Gesture#g:method:ungroup"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getBoundingBox]("GI.Gtk.Objects.Gesture#g:method:getBoundingBox"), [getBoundingBoxCenter]("GI.Gtk.Objects.Gesture#g:method:getBoundingBoxCenter"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getDevice]("GI.Gtk.Objects.Gesture#g:method:getDevice"), [getGroup]("GI.Gtk.Objects.Gesture#g:method:getGroup"), [getLastEvent]("GI.Gtk.Objects.Gesture#g:method:getLastEvent"), [getLastUpdatedSequence]("GI.Gtk.Objects.Gesture#g:method:getLastUpdatedSequence"), [getPoint]("GI.Gtk.Objects.Gesture#g:method:getPoint"), [getPropagationPhase]("GI.Gtk.Objects.EventController#g:method:getPropagationPhase"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getSequenceState]("GI.Gtk.Objects.Gesture#g:method:getSequenceState"), [getSequences]("GI.Gtk.Objects.Gesture#g:method:getSequences"), [getWidget]("GI.Gtk.Objects.EventController#g:method:getWidget"), [getWindow]("GI.Gtk.Objects.Gesture#g:method:getWindow").
-- 
-- ==== Setters
-- [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setPropagationPhase]("GI.Gtk.Objects.EventController#g:method:setPropagationPhase"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setSequenceState]("GI.Gtk.Objects.Gesture#g:method:setSequenceState"), [setState]("GI.Gtk.Objects.Gesture#g:method:setState"), [setWindow]("GI.Gtk.Objects.Gesture#g:method:setWindow").

#if defined(ENABLE_OVERLOADING)
    ResolveGestureMethod                    ,
#endif

-- ** getBoundingBox #method:getBoundingBox#

#if defined(ENABLE_OVERLOADING)
    GestureGetBoundingBoxMethodInfo         ,
#endif
    gestureGetBoundingBox                   ,


-- ** getBoundingBoxCenter #method:getBoundingBoxCenter#

#if defined(ENABLE_OVERLOADING)
    GestureGetBoundingBoxCenterMethodInfo   ,
#endif
    gestureGetBoundingBoxCenter             ,


-- ** getDevice #method:getDevice#

#if defined(ENABLE_OVERLOADING)
    GestureGetDeviceMethodInfo              ,
#endif
    gestureGetDevice                        ,


-- ** getGroup #method:getGroup#

#if defined(ENABLE_OVERLOADING)
    GestureGetGroupMethodInfo               ,
#endif
    gestureGetGroup                         ,


-- ** getLastEvent #method:getLastEvent#

#if defined(ENABLE_OVERLOADING)
    GestureGetLastEventMethodInfo           ,
#endif
    gestureGetLastEvent                     ,


-- ** getLastUpdatedSequence #method:getLastUpdatedSequence#

#if defined(ENABLE_OVERLOADING)
    GestureGetLastUpdatedSequenceMethodInfo ,
#endif
    gestureGetLastUpdatedSequence           ,


-- ** getPoint #method:getPoint#

#if defined(ENABLE_OVERLOADING)
    GestureGetPointMethodInfo               ,
#endif
    gestureGetPoint                         ,


-- ** getSequenceState #method:getSequenceState#

#if defined(ENABLE_OVERLOADING)
    GestureGetSequenceStateMethodInfo       ,
#endif
    gestureGetSequenceState                 ,


-- ** getSequences #method:getSequences#

#if defined(ENABLE_OVERLOADING)
    GestureGetSequencesMethodInfo           ,
#endif
    gestureGetSequences                     ,


-- ** getWindow #method:getWindow#

#if defined(ENABLE_OVERLOADING)
    GestureGetWindowMethodInfo              ,
#endif
    gestureGetWindow                        ,


-- ** group #method:group#

#if defined(ENABLE_OVERLOADING)
    GestureGroupMethodInfo                  ,
#endif
    gestureGroup                            ,


-- ** handlesSequence #method:handlesSequence#

#if defined(ENABLE_OVERLOADING)
    GestureHandlesSequenceMethodInfo        ,
#endif
    gestureHandlesSequence                  ,


-- ** isActive #method:isActive#

#if defined(ENABLE_OVERLOADING)
    GestureIsActiveMethodInfo               ,
#endif
    gestureIsActive                         ,


-- ** isGroupedWith #method:isGroupedWith#

#if defined(ENABLE_OVERLOADING)
    GestureIsGroupedWithMethodInfo          ,
#endif
    gestureIsGroupedWith                    ,


-- ** isRecognized #method:isRecognized#

#if defined(ENABLE_OVERLOADING)
    GestureIsRecognizedMethodInfo           ,
#endif
    gestureIsRecognized                     ,


-- ** setSequenceState #method:setSequenceState#

#if defined(ENABLE_OVERLOADING)
    GestureSetSequenceStateMethodInfo       ,
#endif
    gestureSetSequenceState                 ,


-- ** setState #method:setState#

#if defined(ENABLE_OVERLOADING)
    GestureSetStateMethodInfo               ,
#endif
    gestureSetState                         ,


-- ** setWindow #method:setWindow#

#if defined(ENABLE_OVERLOADING)
    GestureSetWindowMethodInfo              ,
#endif
    gestureSetWindow                        ,


-- ** ungroup #method:ungroup#

#if defined(ENABLE_OVERLOADING)
    GestureUngroupMethodInfo                ,
#endif
    gestureUngroup                          ,




 -- * Properties


-- ** nPoints #attr:nPoints#
-- | The number of touch points that trigger recognition on this gesture,
-- 
-- /Since: 3.14/

#if defined(ENABLE_OVERLOADING)
    GestureNPointsPropertyInfo              ,
#endif
    constructGestureNPoints                 ,
#if defined(ENABLE_OVERLOADING)
    gestureNPoints                          ,
#endif
    getGestureNPoints                       ,


-- ** window #attr:window#
-- | If non-'P.Nothing', the gesture will only listen for events that happen on
-- this t'GI.Gdk.Objects.Window.Window', or a child of it.
-- 
-- /Since: 3.14/

#if defined(ENABLE_OVERLOADING)
    GestureWindowPropertyInfo               ,
#endif
    clearGestureWindow                      ,
    constructGestureWindow                  ,
#if defined(ENABLE_OVERLOADING)
    gestureWindow                           ,
#endif
    getGestureWindow                        ,
    setGestureWindow                        ,




 -- * Signals


-- ** begin #signal:begin#

    GestureBeginCallback                    ,
#if defined(ENABLE_OVERLOADING)
    GestureBeginSignalInfo                  ,
#endif
    afterGestureBegin                       ,
    onGestureBegin                          ,


-- ** cancel #signal:cancel#

    GestureCancelCallback                   ,
#if defined(ENABLE_OVERLOADING)
    GestureCancelSignalInfo                 ,
#endif
    afterGestureCancel                      ,
    onGestureCancel                         ,


-- ** end #signal:end#

    GestureEndCallback                      ,
#if defined(ENABLE_OVERLOADING)
    GestureEndSignalInfo                    ,
#endif
    afterGestureEnd                         ,
    onGestureEnd                            ,


-- ** sequenceStateChanged #signal:sequenceStateChanged#

    GestureSequenceStateChangedCallback     ,
#if defined(ENABLE_OVERLOADING)
    GestureSequenceStateChangedSignalInfo   ,
#endif
    afterGestureSequenceStateChanged        ,
    onGestureSequenceStateChanged           ,


-- ** update #signal:update#

    GestureUpdateCallback                   ,
#if defined(ENABLE_OVERLOADING)
    GestureUpdateSignalInfo                 ,
#endif
    afterGestureUpdate                      ,
    onGestureUpdate                         ,




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
import qualified GI.Gdk.Objects.Window as Gdk.Window
import qualified GI.Gdk.Structs.EventSequence as Gdk.EventSequence
import qualified GI.Gdk.Structs.Rectangle as Gdk.Rectangle
import qualified GI.Gdk.Unions.Event as Gdk.Event
import {-# SOURCE #-} qualified GI.Gtk.Enums as Gtk.Enums
import {-# SOURCE #-} qualified GI.Gtk.Objects.EventController as Gtk.EventController

-- | Memory-managed wrapper type.
newtype Gesture = Gesture (SP.ManagedPtr Gesture)
    deriving (Eq)

instance SP.ManagedPtrNewtype Gesture where
    toManagedPtr (Gesture p) = p

foreign import ccall "gtk_gesture_get_type"
    c_gtk_gesture_get_type :: IO B.Types.GType

instance B.Types.TypedObject Gesture where
    glibType = c_gtk_gesture_get_type

instance B.Types.GObject Gesture

-- | Type class for types which can be safely cast to `Gesture`, for instance with `toGesture`.
class (SP.GObject o, O.IsDescendantOf Gesture o) => IsGesture o
instance (SP.GObject o, O.IsDescendantOf Gesture o) => IsGesture o

instance O.HasParentTypes Gesture
type instance O.ParentTypes Gesture = '[Gtk.EventController.EventController, GObject.Object.Object]

-- | Cast to `Gesture`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toGesture :: (MIO.MonadIO m, IsGesture o) => o -> m Gesture
toGesture = MIO.liftIO . B.ManagedPtr.unsafeCastTo Gesture

-- | Convert 'Gesture' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe Gesture) where
    gvalueGType_ = c_gtk_gesture_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr Gesture)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr Gesture)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject Gesture ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveGestureMethod (t :: Symbol) (o :: *) :: * where
    ResolveGestureMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveGestureMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveGestureMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveGestureMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveGestureMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveGestureMethod "group" o = GestureGroupMethodInfo
    ResolveGestureMethod "handleEvent" o = Gtk.EventController.EventControllerHandleEventMethodInfo
    ResolveGestureMethod "handlesSequence" o = GestureHandlesSequenceMethodInfo
    ResolveGestureMethod "isActive" o = GestureIsActiveMethodInfo
    ResolveGestureMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveGestureMethod "isGroupedWith" o = GestureIsGroupedWithMethodInfo
    ResolveGestureMethod "isRecognized" o = GestureIsRecognizedMethodInfo
    ResolveGestureMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveGestureMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveGestureMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveGestureMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveGestureMethod "reset" o = Gtk.EventController.EventControllerResetMethodInfo
    ResolveGestureMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveGestureMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveGestureMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveGestureMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveGestureMethod "ungroup" o = GestureUngroupMethodInfo
    ResolveGestureMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveGestureMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveGestureMethod "getBoundingBox" o = GestureGetBoundingBoxMethodInfo
    ResolveGestureMethod "getBoundingBoxCenter" o = GestureGetBoundingBoxCenterMethodInfo
    ResolveGestureMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveGestureMethod "getDevice" o = GestureGetDeviceMethodInfo
    ResolveGestureMethod "getGroup" o = GestureGetGroupMethodInfo
    ResolveGestureMethod "getLastEvent" o = GestureGetLastEventMethodInfo
    ResolveGestureMethod "getLastUpdatedSequence" o = GestureGetLastUpdatedSequenceMethodInfo
    ResolveGestureMethod "getPoint" o = GestureGetPointMethodInfo
    ResolveGestureMethod "getPropagationPhase" o = Gtk.EventController.EventControllerGetPropagationPhaseMethodInfo
    ResolveGestureMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveGestureMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveGestureMethod "getSequenceState" o = GestureGetSequenceStateMethodInfo
    ResolveGestureMethod "getSequences" o = GestureGetSequencesMethodInfo
    ResolveGestureMethod "getWidget" o = Gtk.EventController.EventControllerGetWidgetMethodInfo
    ResolveGestureMethod "getWindow" o = GestureGetWindowMethodInfo
    ResolveGestureMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveGestureMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveGestureMethod "setPropagationPhase" o = Gtk.EventController.EventControllerSetPropagationPhaseMethodInfo
    ResolveGestureMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveGestureMethod "setSequenceState" o = GestureSetSequenceStateMethodInfo
    ResolveGestureMethod "setState" o = GestureSetStateMethodInfo
    ResolveGestureMethod "setWindow" o = GestureSetWindowMethodInfo
    ResolveGestureMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveGestureMethod t Gesture, O.OverloadedMethod info Gesture p) => OL.IsLabel t (Gesture -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveGestureMethod t Gesture, O.OverloadedMethod info Gesture p, R.HasField t Gesture p) => R.HasField t Gesture p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveGestureMethod t Gesture, O.OverloadedMethodInfo info Gesture) => OL.IsLabel t (O.MethodProxy info Gesture) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- signal Gesture::begin
-- | This signal is emitted when the gesture is recognized. This means the
-- number of touch sequences matches [Gesture:nPoints]("GI.Gtk.Objects.Gesture#g:attr:nPoints"), and the t'GI.Gtk.Objects.Gesture.Gesture'::@/check/@
-- handler(s) returned @/TRUE/@.
-- 
-- Note: These conditions may also happen when an extra touch (eg. a third touch
-- on a 2-touches gesture) is lifted, in that situation /@sequence@/ won\'t pertain
-- to the current set of active touches, so don\'t rely on this being true.
-- 
-- /Since: 3.14/
type GestureBeginCallback =
    Maybe Gdk.EventSequence.EventSequence
    -- ^ /@sequence@/: the t'GI.Gdk.Structs.EventSequence.EventSequence' that made the gesture to be recognized
    -> IO ()

type C_GestureBeginCallback =
    Ptr Gesture ->                          -- object
    Ptr Gdk.EventSequence.EventSequence ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_GestureBeginCallback`.
foreign import ccall "wrapper"
    mk_GestureBeginCallback :: C_GestureBeginCallback -> IO (FunPtr C_GestureBeginCallback)

wrap_GestureBeginCallback :: 
    GObject a => (a -> GestureBeginCallback) ->
    C_GestureBeginCallback
wrap_GestureBeginCallback gi'cb gi'selfPtr sequence _ = do
    maybeSequence <-
        if sequence == nullPtr
        then return Nothing
        else do
            B.ManagedPtr.withTransient  sequence $ \sequence' -> do
                return $ Just sequence'
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  maybeSequence


-- | Connect a signal handler for the [begin](#signal:begin) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' gesture #begin callback
-- @
-- 
-- 
onGestureBegin :: (IsGesture a, MonadIO m) => a -> ((?self :: a) => GestureBeginCallback) -> m SignalHandlerId
onGestureBegin obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_GestureBeginCallback wrapped
    wrapped'' <- mk_GestureBeginCallback wrapped'
    connectSignalFunPtr obj "begin" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [begin](#signal:begin) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' gesture #begin callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterGestureBegin :: (IsGesture a, MonadIO m) => a -> ((?self :: a) => GestureBeginCallback) -> m SignalHandlerId
afterGestureBegin obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_GestureBeginCallback wrapped
    wrapped'' <- mk_GestureBeginCallback wrapped'
    connectSignalFunPtr obj "begin" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data GestureBeginSignalInfo
instance SignalInfo GestureBeginSignalInfo where
    type HaskellCallbackType GestureBeginSignalInfo = GestureBeginCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_GestureBeginCallback cb
        cb'' <- mk_GestureBeginCallback cb'
        connectSignalFunPtr obj "begin" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Gesture::begin"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Gesture.html#g:signal:begin"})

#endif

-- signal Gesture::cancel
-- | This signal is emitted whenever a sequence is cancelled. This usually
-- happens on active touches when 'GI.Gtk.Objects.EventController.eventControllerReset' is called
-- on /@gesture@/ (manually, due to grabs...), or the individual /@sequence@/
-- was claimed by parent widgets\' controllers (see 'GI.Gtk.Objects.Gesture.gestureSetSequenceState').
-- 
-- /@gesture@/ must forget everything about /@sequence@/ as a reaction to this signal.
-- 
-- /Since: 3.14/
type GestureCancelCallback =
    Maybe Gdk.EventSequence.EventSequence
    -- ^ /@sequence@/: the t'GI.Gdk.Structs.EventSequence.EventSequence' that was cancelled
    -> IO ()

type C_GestureCancelCallback =
    Ptr Gesture ->                          -- object
    Ptr Gdk.EventSequence.EventSequence ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_GestureCancelCallback`.
foreign import ccall "wrapper"
    mk_GestureCancelCallback :: C_GestureCancelCallback -> IO (FunPtr C_GestureCancelCallback)

wrap_GestureCancelCallback :: 
    GObject a => (a -> GestureCancelCallback) ->
    C_GestureCancelCallback
wrap_GestureCancelCallback gi'cb gi'selfPtr sequence _ = do
    maybeSequence <-
        if sequence == nullPtr
        then return Nothing
        else do
            B.ManagedPtr.withTransient  sequence $ \sequence' -> do
                return $ Just sequence'
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  maybeSequence


-- | Connect a signal handler for the [cancel](#signal:cancel) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' gesture #cancel callback
-- @
-- 
-- 
onGestureCancel :: (IsGesture a, MonadIO m) => a -> ((?self :: a) => GestureCancelCallback) -> m SignalHandlerId
onGestureCancel obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_GestureCancelCallback wrapped
    wrapped'' <- mk_GestureCancelCallback wrapped'
    connectSignalFunPtr obj "cancel" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [cancel](#signal:cancel) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' gesture #cancel callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterGestureCancel :: (IsGesture a, MonadIO m) => a -> ((?self :: a) => GestureCancelCallback) -> m SignalHandlerId
afterGestureCancel obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_GestureCancelCallback wrapped
    wrapped'' <- mk_GestureCancelCallback wrapped'
    connectSignalFunPtr obj "cancel" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data GestureCancelSignalInfo
instance SignalInfo GestureCancelSignalInfo where
    type HaskellCallbackType GestureCancelSignalInfo = GestureCancelCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_GestureCancelCallback cb
        cb'' <- mk_GestureCancelCallback cb'
        connectSignalFunPtr obj "cancel" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Gesture::cancel"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Gesture.html#g:signal:cancel"})

#endif

-- signal Gesture::end
-- | This signal is emitted when /@gesture@/ either stopped recognizing the event
-- sequences as something to be handled (the t'GI.Gtk.Objects.Gesture.Gesture'::@/check/@ handler returned
-- 'P.False'), or the number of touch sequences became higher or lower than
-- [Gesture:nPoints]("GI.Gtk.Objects.Gesture#g:attr:nPoints").
-- 
-- Note: /@sequence@/ might not pertain to the group of sequences that were
-- previously triggering recognition on /@gesture@/ (ie. a just pressed touch
-- sequence that exceeds [Gesture:nPoints]("GI.Gtk.Objects.Gesture#g:attr:nPoints")). This situation may be detected
-- by checking through 'GI.Gtk.Objects.Gesture.gestureHandlesSequence'.
-- 
-- /Since: 3.14/
type GestureEndCallback =
    Maybe Gdk.EventSequence.EventSequence
    -- ^ /@sequence@/: the t'GI.Gdk.Structs.EventSequence.EventSequence' that made gesture recognition to finish
    -> IO ()

type C_GestureEndCallback =
    Ptr Gesture ->                          -- object
    Ptr Gdk.EventSequence.EventSequence ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_GestureEndCallback`.
foreign import ccall "wrapper"
    mk_GestureEndCallback :: C_GestureEndCallback -> IO (FunPtr C_GestureEndCallback)

wrap_GestureEndCallback :: 
    GObject a => (a -> GestureEndCallback) ->
    C_GestureEndCallback
wrap_GestureEndCallback gi'cb gi'selfPtr sequence _ = do
    maybeSequence <-
        if sequence == nullPtr
        then return Nothing
        else do
            B.ManagedPtr.withTransient  sequence $ \sequence' -> do
                return $ Just sequence'
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  maybeSequence


-- | Connect a signal handler for the [end](#signal:end) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' gesture #end callback
-- @
-- 
-- 
onGestureEnd :: (IsGesture a, MonadIO m) => a -> ((?self :: a) => GestureEndCallback) -> m SignalHandlerId
onGestureEnd obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_GestureEndCallback wrapped
    wrapped'' <- mk_GestureEndCallback wrapped'
    connectSignalFunPtr obj "end" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [end](#signal:end) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' gesture #end callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterGestureEnd :: (IsGesture a, MonadIO m) => a -> ((?self :: a) => GestureEndCallback) -> m SignalHandlerId
afterGestureEnd obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_GestureEndCallback wrapped
    wrapped'' <- mk_GestureEndCallback wrapped'
    connectSignalFunPtr obj "end" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data GestureEndSignalInfo
instance SignalInfo GestureEndSignalInfo where
    type HaskellCallbackType GestureEndSignalInfo = GestureEndCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_GestureEndCallback cb
        cb'' <- mk_GestureEndCallback cb'
        connectSignalFunPtr obj "end" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Gesture::end"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Gesture.html#g:signal:end"})

#endif

-- signal Gesture::sequence-state-changed
-- | This signal is emitted whenever a sequence state changes. See
-- 'GI.Gtk.Objects.Gesture.gestureSetSequenceState' to know more about the expectable
-- sequence lifetimes.
-- 
-- /Since: 3.14/
type GestureSequenceStateChangedCallback =
    Maybe Gdk.EventSequence.EventSequence
    -- ^ /@sequence@/: the t'GI.Gdk.Structs.EventSequence.EventSequence' that was cancelled
    -> Gtk.Enums.EventSequenceState
    -- ^ /@state@/: the new sequence state
    -> IO ()

type C_GestureSequenceStateChangedCallback =
    Ptr Gesture ->                          -- object
    Ptr Gdk.EventSequence.EventSequence ->
    CUInt ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_GestureSequenceStateChangedCallback`.
foreign import ccall "wrapper"
    mk_GestureSequenceStateChangedCallback :: C_GestureSequenceStateChangedCallback -> IO (FunPtr C_GestureSequenceStateChangedCallback)

wrap_GestureSequenceStateChangedCallback :: 
    GObject a => (a -> GestureSequenceStateChangedCallback) ->
    C_GestureSequenceStateChangedCallback
wrap_GestureSequenceStateChangedCallback gi'cb gi'selfPtr sequence state _ = do
    maybeSequence <-
        if sequence == nullPtr
        then return Nothing
        else do
            B.ManagedPtr.withTransient  sequence $ \sequence' -> do
                return $ Just sequence'
    let state' = (toEnum . fromIntegral) state
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  maybeSequence state'


-- | Connect a signal handler for the [sequenceStateChanged](#signal:sequenceStateChanged) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' gesture #sequenceStateChanged callback
-- @
-- 
-- 
onGestureSequenceStateChanged :: (IsGesture a, MonadIO m) => a -> ((?self :: a) => GestureSequenceStateChangedCallback) -> m SignalHandlerId
onGestureSequenceStateChanged obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_GestureSequenceStateChangedCallback wrapped
    wrapped'' <- mk_GestureSequenceStateChangedCallback wrapped'
    connectSignalFunPtr obj "sequence-state-changed" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [sequenceStateChanged](#signal:sequenceStateChanged) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' gesture #sequenceStateChanged callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterGestureSequenceStateChanged :: (IsGesture a, MonadIO m) => a -> ((?self :: a) => GestureSequenceStateChangedCallback) -> m SignalHandlerId
afterGestureSequenceStateChanged obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_GestureSequenceStateChangedCallback wrapped
    wrapped'' <- mk_GestureSequenceStateChangedCallback wrapped'
    connectSignalFunPtr obj "sequence-state-changed" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data GestureSequenceStateChangedSignalInfo
instance SignalInfo GestureSequenceStateChangedSignalInfo where
    type HaskellCallbackType GestureSequenceStateChangedSignalInfo = GestureSequenceStateChangedCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_GestureSequenceStateChangedCallback cb
        cb'' <- mk_GestureSequenceStateChangedCallback cb'
        connectSignalFunPtr obj "sequence-state-changed" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Gesture::sequence-state-changed"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Gesture.html#g:signal:sequenceStateChanged"})

#endif

-- signal Gesture::update
-- | This signal is emitted whenever an event is handled while the gesture is
-- recognized. /@sequence@/ is guaranteed to pertain to the set of active touches.
-- 
-- /Since: 3.14/
type GestureUpdateCallback =
    Maybe Gdk.EventSequence.EventSequence
    -- ^ /@sequence@/: the t'GI.Gdk.Structs.EventSequence.EventSequence' that was updated
    -> IO ()

type C_GestureUpdateCallback =
    Ptr Gesture ->                          -- object
    Ptr Gdk.EventSequence.EventSequence ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_GestureUpdateCallback`.
foreign import ccall "wrapper"
    mk_GestureUpdateCallback :: C_GestureUpdateCallback -> IO (FunPtr C_GestureUpdateCallback)

wrap_GestureUpdateCallback :: 
    GObject a => (a -> GestureUpdateCallback) ->
    C_GestureUpdateCallback
wrap_GestureUpdateCallback gi'cb gi'selfPtr sequence _ = do
    maybeSequence <-
        if sequence == nullPtr
        then return Nothing
        else do
            B.ManagedPtr.withTransient  sequence $ \sequence' -> do
                return $ Just sequence'
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  maybeSequence


-- | Connect a signal handler for the [update](#signal:update) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' gesture #update callback
-- @
-- 
-- 
onGestureUpdate :: (IsGesture a, MonadIO m) => a -> ((?self :: a) => GestureUpdateCallback) -> m SignalHandlerId
onGestureUpdate obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_GestureUpdateCallback wrapped
    wrapped'' <- mk_GestureUpdateCallback wrapped'
    connectSignalFunPtr obj "update" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [update](#signal:update) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' gesture #update callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterGestureUpdate :: (IsGesture a, MonadIO m) => a -> ((?self :: a) => GestureUpdateCallback) -> m SignalHandlerId
afterGestureUpdate obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_GestureUpdateCallback wrapped
    wrapped'' <- mk_GestureUpdateCallback wrapped'
    connectSignalFunPtr obj "update" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data GestureUpdateSignalInfo
instance SignalInfo GestureUpdateSignalInfo where
    type HaskellCallbackType GestureUpdateSignalInfo = GestureUpdateCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_GestureUpdateCallback cb
        cb'' <- mk_GestureUpdateCallback cb'
        connectSignalFunPtr obj "update" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Gesture::update"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Gesture.html#g:signal:update"})

#endif

-- VVV Prop "n-points"
   -- Type: TBasicType TUInt
   -- Flags: [PropertyReadable,PropertyWritable,PropertyConstructOnly]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@n-points@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' gesture #nPoints
-- @
getGestureNPoints :: (MonadIO m, IsGesture o) => o -> m Word32
getGestureNPoints obj = MIO.liftIO $ B.Properties.getObjectPropertyUInt32 obj "n-points"

-- | Construct a `GValueConstruct` with valid value for the “@n-points@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructGestureNPoints :: (IsGesture o, MIO.MonadIO m) => Word32 -> m (GValueConstruct o)
constructGestureNPoints val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyUInt32 "n-points" val

#if defined(ENABLE_OVERLOADING)
data GestureNPointsPropertyInfo
instance AttrInfo GestureNPointsPropertyInfo where
    type AttrAllowedOps GestureNPointsPropertyInfo = '[ 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint GestureNPointsPropertyInfo = IsGesture
    type AttrSetTypeConstraint GestureNPointsPropertyInfo = (~) Word32
    type AttrTransferTypeConstraint GestureNPointsPropertyInfo = (~) Word32
    type AttrTransferType GestureNPointsPropertyInfo = Word32
    type AttrGetType GestureNPointsPropertyInfo = Word32
    type AttrLabel GestureNPointsPropertyInfo = "n-points"
    type AttrOrigin GestureNPointsPropertyInfo = Gesture
    attrGet = getGestureNPoints
    attrSet = undefined
    attrTransfer _ v = do
        return v
    attrConstruct = constructGestureNPoints
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Gesture.nPoints"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Gesture.html#g:attr:nPoints"
        })
#endif

-- VVV Prop "window"
   -- Type: TInterface (Name {namespace = "Gdk", name = "Window"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just True,Just True)

-- | Get the value of the “@window@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' gesture #window
-- @
getGestureWindow :: (MonadIO m, IsGesture o) => o -> m (Maybe Gdk.Window.Window)
getGestureWindow obj = MIO.liftIO $ B.Properties.getObjectPropertyObject obj "window" Gdk.Window.Window

-- | Set the value of the “@window@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' gesture [ #window 'Data.GI.Base.Attributes.:=' value ]
-- @
setGestureWindow :: (MonadIO m, IsGesture o, Gdk.Window.IsWindow a) => o -> a -> m ()
setGestureWindow obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyObject obj "window" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@window@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructGestureWindow :: (IsGesture o, MIO.MonadIO m, Gdk.Window.IsWindow a) => a -> m (GValueConstruct o)
constructGestureWindow val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyObject "window" (P.Just val)

-- | Set the value of the “@window@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #window
-- @
clearGestureWindow :: (MonadIO m, IsGesture o) => o -> m ()
clearGestureWindow obj = liftIO $ B.Properties.setObjectPropertyObject obj "window" (Nothing :: Maybe Gdk.Window.Window)

#if defined(ENABLE_OVERLOADING)
data GestureWindowPropertyInfo
instance AttrInfo GestureWindowPropertyInfo where
    type AttrAllowedOps GestureWindowPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint GestureWindowPropertyInfo = IsGesture
    type AttrSetTypeConstraint GestureWindowPropertyInfo = Gdk.Window.IsWindow
    type AttrTransferTypeConstraint GestureWindowPropertyInfo = Gdk.Window.IsWindow
    type AttrTransferType GestureWindowPropertyInfo = Gdk.Window.Window
    type AttrGetType GestureWindowPropertyInfo = (Maybe Gdk.Window.Window)
    type AttrLabel GestureWindowPropertyInfo = "window"
    type AttrOrigin GestureWindowPropertyInfo = Gesture
    attrGet = getGestureWindow
    attrSet = setGestureWindow
    attrTransfer _ v = do
        unsafeCastTo Gdk.Window.Window v
    attrConstruct = constructGestureWindow
    attrClear = clearGestureWindow
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Gesture.window"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Gesture.html#g:attr:window"
        })
#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList Gesture
type instance O.AttributeList Gesture = GestureAttributeList
type GestureAttributeList = ('[ '("nPoints", GestureNPointsPropertyInfo), '("propagationPhase", Gtk.EventController.EventControllerPropagationPhasePropertyInfo), '("widget", Gtk.EventController.EventControllerWidgetPropertyInfo), '("window", GestureWindowPropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
gestureNPoints :: AttrLabelProxy "nPoints"
gestureNPoints = AttrLabelProxy

gestureWindow :: AttrLabelProxy "window"
gestureWindow = AttrLabelProxy

#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList Gesture = GestureSignalList
type GestureSignalList = ('[ '("begin", GestureBeginSignalInfo), '("cancel", GestureCancelSignalInfo), '("end", GestureEndSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo), '("sequenceStateChanged", GestureSequenceStateChangedSignalInfo), '("update", GestureUpdateSignalInfo)] :: [(Symbol, *)])

#endif

-- method Gesture::get_bounding_box
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "gesture"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Gesture" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkGesture" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "rect"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "Rectangle" }
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "bounding box containing all active touches."
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

foreign import ccall "gtk_gesture_get_bounding_box" gtk_gesture_get_bounding_box :: 
    Ptr Gesture ->                          -- gesture : TInterface (Name {namespace = "Gtk", name = "Gesture"})
    Ptr Gdk.Rectangle.Rectangle ->          -- rect : TInterface (Name {namespace = "Gdk", name = "Rectangle"})
    IO CInt

-- | If there are touch sequences being currently handled by /@gesture@/,
-- this function returns 'P.True' and fills in /@rect@/ with the bounding
-- box containing all active touches. Otherwise, 'P.False' will be
-- returned.
-- 
-- Note: This function will yield unexpected results on touchpad
-- gestures. Since there is no correlation between physical and
-- pixel distances, these will look as if constrained in an
-- infinitely small area, /@rect@/ width and height will thus be 0
-- regardless of the number of touchpoints.
-- 
-- /Since: 3.14/
gestureGetBoundingBox ::
    (B.CallStack.HasCallStack, MonadIO m, IsGesture a) =>
    a
    -- ^ /@gesture@/: a t'GI.Gtk.Objects.Gesture.Gesture'
    -> m ((Bool, Gdk.Rectangle.Rectangle))
    -- ^ __Returns:__ 'P.True' if there are active touches, 'P.False' otherwise
gestureGetBoundingBox gesture = liftIO $ do
    gesture' <- unsafeManagedPtrCastPtr gesture
    rect <- SP.callocBoxedBytes 16 :: IO (Ptr Gdk.Rectangle.Rectangle)
    result <- gtk_gesture_get_bounding_box gesture' rect
    let result' = (/= 0) result
    rect' <- (wrapBoxed Gdk.Rectangle.Rectangle) rect
    touchManagedPtr gesture
    return (result', rect')

#if defined(ENABLE_OVERLOADING)
data GestureGetBoundingBoxMethodInfo
instance (signature ~ (m ((Bool, Gdk.Rectangle.Rectangle))), MonadIO m, IsGesture a) => O.OverloadedMethod GestureGetBoundingBoxMethodInfo a signature where
    overloadedMethod = gestureGetBoundingBox

instance O.OverloadedMethodInfo GestureGetBoundingBoxMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Gesture.gestureGetBoundingBox",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Gesture.html#v:gestureGetBoundingBox"
        })


#endif

-- method Gesture::get_bounding_box_center
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "gesture"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Gesture" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkGesture" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "x"
--           , argType = TBasicType TDouble
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "X coordinate for the bounding box center"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       , Arg
--           { argCName = "y"
--           , argType = TBasicType TDouble
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Y coordinate for the bounding box center"
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

foreign import ccall "gtk_gesture_get_bounding_box_center" gtk_gesture_get_bounding_box_center :: 
    Ptr Gesture ->                          -- gesture : TInterface (Name {namespace = "Gtk", name = "Gesture"})
    Ptr CDouble ->                          -- x : TBasicType TDouble
    Ptr CDouble ->                          -- y : TBasicType TDouble
    IO CInt

-- | If there are touch sequences being currently handled by /@gesture@/,
-- this function returns 'P.True' and fills in /@x@/ and /@y@/ with the center
-- of the bounding box containing all active touches. Otherwise, 'P.False'
-- will be returned.
-- 
-- /Since: 3.14/
gestureGetBoundingBoxCenter ::
    (B.CallStack.HasCallStack, MonadIO m, IsGesture a) =>
    a
    -- ^ /@gesture@/: a t'GI.Gtk.Objects.Gesture.Gesture'
    -> m ((Bool, Double, Double))
    -- ^ __Returns:__ 'P.False' if no active touches are present, 'P.True' otherwise
gestureGetBoundingBoxCenter gesture = liftIO $ do
    gesture' <- unsafeManagedPtrCastPtr gesture
    x <- allocMem :: IO (Ptr CDouble)
    y <- allocMem :: IO (Ptr CDouble)
    result <- gtk_gesture_get_bounding_box_center gesture' x y
    let result' = (/= 0) result
    x' <- peek x
    let x'' = realToFrac x'
    y' <- peek y
    let y'' = realToFrac y'
    touchManagedPtr gesture
    freeMem x
    freeMem y
    return (result', x'', y'')

#if defined(ENABLE_OVERLOADING)
data GestureGetBoundingBoxCenterMethodInfo
instance (signature ~ (m ((Bool, Double, Double))), MonadIO m, IsGesture a) => O.OverloadedMethod GestureGetBoundingBoxCenterMethodInfo a signature where
    overloadedMethod = gestureGetBoundingBoxCenter

instance O.OverloadedMethodInfo GestureGetBoundingBoxCenterMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Gesture.gestureGetBoundingBoxCenter",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Gesture.html#v:gestureGetBoundingBoxCenter"
        })


#endif

-- method Gesture::get_device
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "gesture"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Gesture" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkGesture" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gdk" , name = "Device" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_gesture_get_device" gtk_gesture_get_device :: 
    Ptr Gesture ->                          -- gesture : TInterface (Name {namespace = "Gtk", name = "Gesture"})
    IO (Ptr Gdk.Device.Device)

-- | Returns the master t'GI.Gdk.Objects.Device.Device' that is currently operating
-- on /@gesture@/, or 'P.Nothing' if the gesture is not being interacted.
-- 
-- /Since: 3.14/
gestureGetDevice ::
    (B.CallStack.HasCallStack, MonadIO m, IsGesture a) =>
    a
    -- ^ /@gesture@/: a t'GI.Gtk.Objects.Gesture.Gesture'
    -> m (Maybe Gdk.Device.Device)
    -- ^ __Returns:__ a t'GI.Gdk.Objects.Device.Device', or 'P.Nothing'
gestureGetDevice gesture = liftIO $ do
    gesture' <- unsafeManagedPtrCastPtr gesture
    result <- gtk_gesture_get_device gesture'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (newObject Gdk.Device.Device) result'
        return result''
    touchManagedPtr gesture
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data GestureGetDeviceMethodInfo
instance (signature ~ (m (Maybe Gdk.Device.Device)), MonadIO m, IsGesture a) => O.OverloadedMethod GestureGetDeviceMethodInfo a signature where
    overloadedMethod = gestureGetDevice

instance O.OverloadedMethodInfo GestureGetDeviceMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Gesture.gestureGetDevice",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Gesture.html#v:gestureGetDevice"
        })


#endif

-- method Gesture::get_group
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "gesture"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Gesture" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkGesture" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just
--               (TGList (TInterface Name { namespace = "Gtk" , name = "Gesture" }))
-- throws : False
-- Skip return : False

foreign import ccall "gtk_gesture_get_group" gtk_gesture_get_group :: 
    Ptr Gesture ->                          -- gesture : TInterface (Name {namespace = "Gtk", name = "Gesture"})
    IO (Ptr (GList (Ptr Gesture)))

-- | Returns all gestures in the group of /@gesture@/
-- 
-- /Since: 3.14/
gestureGetGroup ::
    (B.CallStack.HasCallStack, MonadIO m, IsGesture a) =>
    a
    -- ^ /@gesture@/: a t'GI.Gtk.Objects.Gesture.Gesture'
    -> m [Gesture]
    -- ^ __Returns:__ The list
    --   of @/GtkGestures/@, free with @/g_list_free()/@
gestureGetGroup gesture = liftIO $ do
    gesture' <- unsafeManagedPtrCastPtr gesture
    result <- gtk_gesture_get_group gesture'
    result' <- unpackGList result
    result'' <- mapM (newObject Gesture) result'
    g_list_free result
    touchManagedPtr gesture
    return result''

#if defined(ENABLE_OVERLOADING)
data GestureGetGroupMethodInfo
instance (signature ~ (m [Gesture]), MonadIO m, IsGesture a) => O.OverloadedMethod GestureGetGroupMethodInfo a signature where
    overloadedMethod = gestureGetGroup

instance O.OverloadedMethodInfo GestureGetGroupMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Gesture.gestureGetGroup",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Gesture.html#v:gestureGetGroup"
        })


#endif

-- method Gesture::get_last_event
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "gesture"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Gesture" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkGesture" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "sequence"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "EventSequence" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GdkEventSequence"
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
-- returnType: Just (TInterface Name { namespace = "Gdk" , name = "Event" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_gesture_get_last_event" gtk_gesture_get_last_event :: 
    Ptr Gesture ->                          -- gesture : TInterface (Name {namespace = "Gtk", name = "Gesture"})
    Ptr Gdk.EventSequence.EventSequence ->  -- sequence : TInterface (Name {namespace = "Gdk", name = "EventSequence"})
    IO (Ptr Gdk.Event.Event)

-- | Returns the last event that was processed for /@sequence@/.
-- 
-- Note that the returned pointer is only valid as long as the /@sequence@/
-- is still interpreted by the /@gesture@/. If in doubt, you should make
-- a copy of the event.
gestureGetLastEvent ::
    (B.CallStack.HasCallStack, MonadIO m, IsGesture a) =>
    a
    -- ^ /@gesture@/: a t'GI.Gtk.Objects.Gesture.Gesture'
    -> Maybe (Gdk.EventSequence.EventSequence)
    -- ^ /@sequence@/: a t'GI.Gdk.Structs.EventSequence.EventSequence'
    -> m (Maybe Gdk.Event.Event)
    -- ^ __Returns:__ The last event from /@sequence@/
gestureGetLastEvent gesture sequence = liftIO $ do
    gesture' <- unsafeManagedPtrCastPtr gesture
    maybeSequence <- case sequence of
        Nothing -> return nullPtr
        Just jSequence -> do
            jSequence' <- unsafeManagedPtrGetPtr jSequence
            return jSequence'
    result <- gtk_gesture_get_last_event gesture' maybeSequence
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (newBoxed Gdk.Event.Event) result'
        return result''
    touchManagedPtr gesture
    whenJust sequence touchManagedPtr
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data GestureGetLastEventMethodInfo
instance (signature ~ (Maybe (Gdk.EventSequence.EventSequence) -> m (Maybe Gdk.Event.Event)), MonadIO m, IsGesture a) => O.OverloadedMethod GestureGetLastEventMethodInfo a signature where
    overloadedMethod = gestureGetLastEvent

instance O.OverloadedMethodInfo GestureGetLastEventMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Gesture.gestureGetLastEvent",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Gesture.html#v:gestureGetLastEvent"
        })


#endif

-- method Gesture::get_last_updated_sequence
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "gesture"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Gesture" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkGesture" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just
--               (TInterface Name { namespace = "Gdk" , name = "EventSequence" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_gesture_get_last_updated_sequence" gtk_gesture_get_last_updated_sequence :: 
    Ptr Gesture ->                          -- gesture : TInterface (Name {namespace = "Gtk", name = "Gesture"})
    IO (Ptr Gdk.EventSequence.EventSequence)

-- | Returns the t'GI.Gdk.Structs.EventSequence.EventSequence' that was last updated on /@gesture@/.
-- 
-- /Since: 3.14/
gestureGetLastUpdatedSequence ::
    (B.CallStack.HasCallStack, MonadIO m, IsGesture a) =>
    a
    -- ^ /@gesture@/: a t'GI.Gtk.Objects.Gesture.Gesture'
    -> m (Maybe Gdk.EventSequence.EventSequence)
    -- ^ __Returns:__ The last updated sequence
gestureGetLastUpdatedSequence gesture = liftIO $ do
    gesture' <- unsafeManagedPtrCastPtr gesture
    result <- gtk_gesture_get_last_updated_sequence gesture'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (newBoxed Gdk.EventSequence.EventSequence) result'
        return result''
    touchManagedPtr gesture
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data GestureGetLastUpdatedSequenceMethodInfo
instance (signature ~ (m (Maybe Gdk.EventSequence.EventSequence)), MonadIO m, IsGesture a) => O.OverloadedMethod GestureGetLastUpdatedSequenceMethodInfo a signature where
    overloadedMethod = gestureGetLastUpdatedSequence

instance O.OverloadedMethodInfo GestureGetLastUpdatedSequenceMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Gesture.gestureGetLastUpdatedSequence",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Gesture.html#v:gestureGetLastUpdatedSequence"
        })


#endif

-- method Gesture::get_point
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "gesture"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Gesture" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkGesture" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "sequence"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "EventSequence" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "a #GdkEventSequence, or %NULL for pointer events"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "x"
--           , argType = TBasicType TDouble
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "return location for X axis of the sequence coordinates"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       , Arg
--           { argCName = "y"
--           , argType = TBasicType TDouble
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "return location for Y axis of the sequence coordinates"
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

foreign import ccall "gtk_gesture_get_point" gtk_gesture_get_point :: 
    Ptr Gesture ->                          -- gesture : TInterface (Name {namespace = "Gtk", name = "Gesture"})
    Ptr Gdk.EventSequence.EventSequence ->  -- sequence : TInterface (Name {namespace = "Gdk", name = "EventSequence"})
    Ptr CDouble ->                          -- x : TBasicType TDouble
    Ptr CDouble ->                          -- y : TBasicType TDouble
    IO CInt

-- | If /@sequence@/ is currently being interpreted by /@gesture@/, this
-- function returns 'P.True' and fills in /@x@/ and /@y@/ with the last coordinates
-- stored for that event sequence. The coordinates are always relative to the
-- widget allocation.
-- 
-- /Since: 3.14/
gestureGetPoint ::
    (B.CallStack.HasCallStack, MonadIO m, IsGesture a) =>
    a
    -- ^ /@gesture@/: a t'GI.Gtk.Objects.Gesture.Gesture'
    -> Maybe (Gdk.EventSequence.EventSequence)
    -- ^ /@sequence@/: a t'GI.Gdk.Structs.EventSequence.EventSequence', or 'P.Nothing' for pointer events
    -> m ((Bool, Double, Double))
    -- ^ __Returns:__ 'P.True' if /@sequence@/ is currently interpreted
gestureGetPoint gesture sequence = liftIO $ do
    gesture' <- unsafeManagedPtrCastPtr gesture
    maybeSequence <- case sequence of
        Nothing -> return nullPtr
        Just jSequence -> do
            jSequence' <- unsafeManagedPtrGetPtr jSequence
            return jSequence'
    x <- allocMem :: IO (Ptr CDouble)
    y <- allocMem :: IO (Ptr CDouble)
    result <- gtk_gesture_get_point gesture' maybeSequence x y
    let result' = (/= 0) result
    x' <- peek x
    let x'' = realToFrac x'
    y' <- peek y
    let y'' = realToFrac y'
    touchManagedPtr gesture
    whenJust sequence touchManagedPtr
    freeMem x
    freeMem y
    return (result', x'', y'')

#if defined(ENABLE_OVERLOADING)
data GestureGetPointMethodInfo
instance (signature ~ (Maybe (Gdk.EventSequence.EventSequence) -> m ((Bool, Double, Double))), MonadIO m, IsGesture a) => O.OverloadedMethod GestureGetPointMethodInfo a signature where
    overloadedMethod = gestureGetPoint

instance O.OverloadedMethodInfo GestureGetPointMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Gesture.gestureGetPoint",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Gesture.html#v:gestureGetPoint"
        })


#endif

-- method Gesture::get_sequence_state
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "gesture"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Gesture" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkGesture" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "sequence"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "EventSequence" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GdkEventSequence"
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
--                  Name { namespace = "Gtk" , name = "EventSequenceState" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_gesture_get_sequence_state" gtk_gesture_get_sequence_state :: 
    Ptr Gesture ->                          -- gesture : TInterface (Name {namespace = "Gtk", name = "Gesture"})
    Ptr Gdk.EventSequence.EventSequence ->  -- sequence : TInterface (Name {namespace = "Gdk", name = "EventSequence"})
    IO CUInt

-- | Returns the /@sequence@/ state, as seen by /@gesture@/.
-- 
-- /Since: 3.14/
gestureGetSequenceState ::
    (B.CallStack.HasCallStack, MonadIO m, IsGesture a) =>
    a
    -- ^ /@gesture@/: a t'GI.Gtk.Objects.Gesture.Gesture'
    -> Gdk.EventSequence.EventSequence
    -- ^ /@sequence@/: a t'GI.Gdk.Structs.EventSequence.EventSequence'
    -> m Gtk.Enums.EventSequenceState
    -- ^ __Returns:__ The sequence state in /@gesture@/
gestureGetSequenceState gesture sequence = liftIO $ do
    gesture' <- unsafeManagedPtrCastPtr gesture
    sequence' <- unsafeManagedPtrGetPtr sequence
    result <- gtk_gesture_get_sequence_state gesture' sequence'
    let result' = (toEnum . fromIntegral) result
    touchManagedPtr gesture
    touchManagedPtr sequence
    return result'

#if defined(ENABLE_OVERLOADING)
data GestureGetSequenceStateMethodInfo
instance (signature ~ (Gdk.EventSequence.EventSequence -> m Gtk.Enums.EventSequenceState), MonadIO m, IsGesture a) => O.OverloadedMethod GestureGetSequenceStateMethodInfo a signature where
    overloadedMethod = gestureGetSequenceState

instance O.OverloadedMethodInfo GestureGetSequenceStateMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Gesture.gestureGetSequenceState",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Gesture.html#v:gestureGetSequenceState"
        })


#endif

-- method Gesture::get_sequences
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "gesture"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Gesture" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkGesture" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just
--               (TGList
--                  (TInterface Name { namespace = "Gdk" , name = "EventSequence" }))
-- throws : False
-- Skip return : False

foreign import ccall "gtk_gesture_get_sequences" gtk_gesture_get_sequences :: 
    Ptr Gesture ->                          -- gesture : TInterface (Name {namespace = "Gtk", name = "Gesture"})
    IO (Ptr (GList (Ptr Gdk.EventSequence.EventSequence)))

-- | Returns the list of @/GdkEventSequences/@ currently being interpreted
-- by /@gesture@/.
-- 
-- /Since: 3.14/
gestureGetSequences ::
    (B.CallStack.HasCallStack, MonadIO m, IsGesture a) =>
    a
    -- ^ /@gesture@/: a t'GI.Gtk.Objects.Gesture.Gesture'
    -> m [Gdk.EventSequence.EventSequence]
    -- ^ __Returns:__ A list
    --          of @/GdkEventSequences/@, the list elements are owned by GTK+
    --          and must not be freed or modified, the list itself must be deleted
    --          through @/g_list_free()/@
gestureGetSequences gesture = liftIO $ do
    gesture' <- unsafeManagedPtrCastPtr gesture
    result <- gtk_gesture_get_sequences gesture'
    result' <- unpackGList result
    result'' <- mapM (newBoxed Gdk.EventSequence.EventSequence) result'
    g_list_free result
    touchManagedPtr gesture
    return result''

#if defined(ENABLE_OVERLOADING)
data GestureGetSequencesMethodInfo
instance (signature ~ (m [Gdk.EventSequence.EventSequence]), MonadIO m, IsGesture a) => O.OverloadedMethod GestureGetSequencesMethodInfo a signature where
    overloadedMethod = gestureGetSequences

instance O.OverloadedMethodInfo GestureGetSequencesMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Gesture.gestureGetSequences",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Gesture.html#v:gestureGetSequences"
        })


#endif

-- method Gesture::get_window
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "gesture"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Gesture" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkGesture" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gdk" , name = "Window" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_gesture_get_window" gtk_gesture_get_window :: 
    Ptr Gesture ->                          -- gesture : TInterface (Name {namespace = "Gtk", name = "Gesture"})
    IO (Ptr Gdk.Window.Window)

-- | Returns the user-defined window that receives the events
-- handled by /@gesture@/. See 'GI.Gtk.Objects.Gesture.gestureSetWindow' for more
-- information.
-- 
-- /Since: 3.14/
gestureGetWindow ::
    (B.CallStack.HasCallStack, MonadIO m, IsGesture a) =>
    a
    -- ^ /@gesture@/: a t'GI.Gtk.Objects.Gesture.Gesture'
    -> m (Maybe Gdk.Window.Window)
    -- ^ __Returns:__ the user defined window, or 'P.Nothing' if none
gestureGetWindow gesture = liftIO $ do
    gesture' <- unsafeManagedPtrCastPtr gesture
    result <- gtk_gesture_get_window gesture'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (newObject Gdk.Window.Window) result'
        return result''
    touchManagedPtr gesture
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data GestureGetWindowMethodInfo
instance (signature ~ (m (Maybe Gdk.Window.Window)), MonadIO m, IsGesture a) => O.OverloadedMethod GestureGetWindowMethodInfo a signature where
    overloadedMethod = gestureGetWindow

instance O.OverloadedMethodInfo GestureGetWindowMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Gesture.gestureGetWindow",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Gesture.html#v:gestureGetWindow"
        })


#endif

-- method Gesture::group
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "group_gesture"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Gesture" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "#GtkGesture to group @gesture with"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "gesture"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Gesture" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkGesture" , sinceVersion = Nothing }
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

foreign import ccall "gtk_gesture_group" gtk_gesture_group :: 
    Ptr Gesture ->                          -- group_gesture : TInterface (Name {namespace = "Gtk", name = "Gesture"})
    Ptr Gesture ->                          -- gesture : TInterface (Name {namespace = "Gtk", name = "Gesture"})
    IO ()

-- | Adds /@gesture@/ to the same group than /@groupGesture@/. Gestures
-- are by default isolated in their own groups.
-- 
-- When gestures are grouped, the state of @/GdkEventSequences/@
-- is kept in sync for all of those, so calling 'GI.Gtk.Objects.Gesture.gestureSetSequenceState',
-- on one will transfer the same value to the others.
-- 
-- Groups also perform an \"implicit grabbing\" of sequences, if a
-- t'GI.Gdk.Structs.EventSequence.EventSequence' state is set to @/GTK_EVENT_SEQUENCE_CLAIMED/@ on one group,
-- every other gesture group attached to the same t'GI.Gtk.Objects.Widget.Widget' will switch the
-- state for that sequence to @/GTK_EVENT_SEQUENCE_DENIED/@.
-- 
-- /Since: 3.14/
gestureGroup ::
    (B.CallStack.HasCallStack, MonadIO m, IsGesture a, IsGesture b) =>
    a
    -- ^ /@groupGesture@/: t'GI.Gtk.Objects.Gesture.Gesture' to group /@gesture@/ with
    -> b
    -- ^ /@gesture@/: a t'GI.Gtk.Objects.Gesture.Gesture'
    -> m ()
gestureGroup groupGesture gesture = liftIO $ do
    groupGesture' <- unsafeManagedPtrCastPtr groupGesture
    gesture' <- unsafeManagedPtrCastPtr gesture
    gtk_gesture_group groupGesture' gesture'
    touchManagedPtr groupGesture
    touchManagedPtr gesture
    return ()

#if defined(ENABLE_OVERLOADING)
data GestureGroupMethodInfo
instance (signature ~ (b -> m ()), MonadIO m, IsGesture a, IsGesture b) => O.OverloadedMethod GestureGroupMethodInfo a signature where
    overloadedMethod = gestureGroup

instance O.OverloadedMethodInfo GestureGroupMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Gesture.gestureGroup",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Gesture.html#v:gestureGroup"
        })


#endif

-- method Gesture::handles_sequence
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "gesture"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Gesture" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkGesture" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "sequence"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "EventSequence" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GdkEventSequence or %NULL"
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

foreign import ccall "gtk_gesture_handles_sequence" gtk_gesture_handles_sequence :: 
    Ptr Gesture ->                          -- gesture : TInterface (Name {namespace = "Gtk", name = "Gesture"})
    Ptr Gdk.EventSequence.EventSequence ->  -- sequence : TInterface (Name {namespace = "Gdk", name = "EventSequence"})
    IO CInt

-- | Returns 'P.True' if /@gesture@/ is currently handling events corresponding to
-- /@sequence@/.
-- 
-- /Since: 3.14/
gestureHandlesSequence ::
    (B.CallStack.HasCallStack, MonadIO m, IsGesture a) =>
    a
    -- ^ /@gesture@/: a t'GI.Gtk.Objects.Gesture.Gesture'
    -> Maybe (Gdk.EventSequence.EventSequence)
    -- ^ /@sequence@/: a t'GI.Gdk.Structs.EventSequence.EventSequence' or 'P.Nothing'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if /@gesture@/ is handling /@sequence@/, 'P.False' otherwise
gestureHandlesSequence gesture sequence = liftIO $ do
    gesture' <- unsafeManagedPtrCastPtr gesture
    maybeSequence <- case sequence of
        Nothing -> return nullPtr
        Just jSequence -> do
            jSequence' <- unsafeManagedPtrGetPtr jSequence
            return jSequence'
    result <- gtk_gesture_handles_sequence gesture' maybeSequence
    let result' = (/= 0) result
    touchManagedPtr gesture
    whenJust sequence touchManagedPtr
    return result'

#if defined(ENABLE_OVERLOADING)
data GestureHandlesSequenceMethodInfo
instance (signature ~ (Maybe (Gdk.EventSequence.EventSequence) -> m Bool), MonadIO m, IsGesture a) => O.OverloadedMethod GestureHandlesSequenceMethodInfo a signature where
    overloadedMethod = gestureHandlesSequence

instance O.OverloadedMethodInfo GestureHandlesSequenceMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Gesture.gestureHandlesSequence",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Gesture.html#v:gestureHandlesSequence"
        })


#endif

-- method Gesture::is_active
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "gesture"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Gesture" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkGesture" , sinceVersion = Nothing }
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

foreign import ccall "gtk_gesture_is_active" gtk_gesture_is_active :: 
    Ptr Gesture ->                          -- gesture : TInterface (Name {namespace = "Gtk", name = "Gesture"})
    IO CInt

-- | Returns 'P.True' if the gesture is currently active.
-- A gesture is active meanwhile there are touch sequences
-- interacting with it.
-- 
-- /Since: 3.14/
gestureIsActive ::
    (B.CallStack.HasCallStack, MonadIO m, IsGesture a) =>
    a
    -- ^ /@gesture@/: a t'GI.Gtk.Objects.Gesture.Gesture'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if gesture is active
gestureIsActive gesture = liftIO $ do
    gesture' <- unsafeManagedPtrCastPtr gesture
    result <- gtk_gesture_is_active gesture'
    let result' = (/= 0) result
    touchManagedPtr gesture
    return result'

#if defined(ENABLE_OVERLOADING)
data GestureIsActiveMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsGesture a) => O.OverloadedMethod GestureIsActiveMethodInfo a signature where
    overloadedMethod = gestureIsActive

instance O.OverloadedMethodInfo GestureIsActiveMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Gesture.gestureIsActive",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Gesture.html#v:gestureIsActive"
        })


#endif

-- method Gesture::is_grouped_with
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "gesture"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Gesture" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkGesture" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "other"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Gesture" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "another #GtkGesture"
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

foreign import ccall "gtk_gesture_is_grouped_with" gtk_gesture_is_grouped_with :: 
    Ptr Gesture ->                          -- gesture : TInterface (Name {namespace = "Gtk", name = "Gesture"})
    Ptr Gesture ->                          -- other : TInterface (Name {namespace = "Gtk", name = "Gesture"})
    IO CInt

-- | Returns 'P.True' if both gestures pertain to the same group.
-- 
-- /Since: 3.14/
gestureIsGroupedWith ::
    (B.CallStack.HasCallStack, MonadIO m, IsGesture a, IsGesture b) =>
    a
    -- ^ /@gesture@/: a t'GI.Gtk.Objects.Gesture.Gesture'
    -> b
    -- ^ /@other@/: another t'GI.Gtk.Objects.Gesture.Gesture'
    -> m Bool
    -- ^ __Returns:__ whether the gestures are grouped
gestureIsGroupedWith gesture other = liftIO $ do
    gesture' <- unsafeManagedPtrCastPtr gesture
    other' <- unsafeManagedPtrCastPtr other
    result <- gtk_gesture_is_grouped_with gesture' other'
    let result' = (/= 0) result
    touchManagedPtr gesture
    touchManagedPtr other
    return result'

#if defined(ENABLE_OVERLOADING)
data GestureIsGroupedWithMethodInfo
instance (signature ~ (b -> m Bool), MonadIO m, IsGesture a, IsGesture b) => O.OverloadedMethod GestureIsGroupedWithMethodInfo a signature where
    overloadedMethod = gestureIsGroupedWith

instance O.OverloadedMethodInfo GestureIsGroupedWithMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Gesture.gestureIsGroupedWith",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Gesture.html#v:gestureIsGroupedWith"
        })


#endif

-- method Gesture::is_recognized
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "gesture"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Gesture" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkGesture" , sinceVersion = Nothing }
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

foreign import ccall "gtk_gesture_is_recognized" gtk_gesture_is_recognized :: 
    Ptr Gesture ->                          -- gesture : TInterface (Name {namespace = "Gtk", name = "Gesture"})
    IO CInt

-- | Returns 'P.True' if the gesture is currently recognized.
-- A gesture is recognized if there are as many interacting
-- touch sequences as required by /@gesture@/, and t'GI.Gtk.Objects.Gesture.Gesture'::@/check/@
-- returned 'P.True' for the sequences being currently interpreted.
-- 
-- /Since: 3.14/
gestureIsRecognized ::
    (B.CallStack.HasCallStack, MonadIO m, IsGesture a) =>
    a
    -- ^ /@gesture@/: a t'GI.Gtk.Objects.Gesture.Gesture'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if gesture is recognized
gestureIsRecognized gesture = liftIO $ do
    gesture' <- unsafeManagedPtrCastPtr gesture
    result <- gtk_gesture_is_recognized gesture'
    let result' = (/= 0) result
    touchManagedPtr gesture
    return result'

#if defined(ENABLE_OVERLOADING)
data GestureIsRecognizedMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsGesture a) => O.OverloadedMethod GestureIsRecognizedMethodInfo a signature where
    overloadedMethod = gestureIsRecognized

instance O.OverloadedMethodInfo GestureIsRecognizedMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Gesture.gestureIsRecognized",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Gesture.html#v:gestureIsRecognized"
        })


#endif

-- method Gesture::set_sequence_state
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "gesture"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Gesture" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkGesture" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "sequence"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "EventSequence" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GdkEventSequence"
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
--               TInterface Name { namespace = "Gtk" , name = "EventSequenceState" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the sequence state" , sinceVersion = Nothing }
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

foreign import ccall "gtk_gesture_set_sequence_state" gtk_gesture_set_sequence_state :: 
    Ptr Gesture ->                          -- gesture : TInterface (Name {namespace = "Gtk", name = "Gesture"})
    Ptr Gdk.EventSequence.EventSequence ->  -- sequence : TInterface (Name {namespace = "Gdk", name = "EventSequence"})
    CUInt ->                                -- state : TInterface (Name {namespace = "Gtk", name = "EventSequenceState"})
    IO CInt

-- | Sets the state of /@sequence@/ in /@gesture@/. Sequences start
-- in state @/GTK_EVENT_SEQUENCE_NONE/@, and whenever they change
-- state, they can never go back to that state. Likewise,
-- sequences in state @/GTK_EVENT_SEQUENCE_DENIED/@ cannot turn
-- back to a not denied state. With these rules, the lifetime
-- of an event sequence is constrained to the next four:
-- 
-- * None
-- * None → Denied
-- * None → Claimed
-- * None → Claimed → Denied
-- 
-- Note: Due to event handling ordering, it may be unsafe to
-- set the state on another gesture within a [Gesture::begin]("GI.Gtk.Objects.Gesture#g:signal:begin")
-- signal handler, as the callback might be executed before
-- the other gesture knows about the sequence. A safe way to
-- perform this could be:
-- 
-- >
-- >static void
-- >first_gesture_begin_cb (GtkGesture       *first_gesture,
-- >                        GdkEventSequence *sequence,
-- >                        gpointer          user_data)
-- >{
-- >  gtk_gesture_set_sequence_state (first_gesture, sequence, GTK_EVENT_SEQUENCE_CLAIMED);
-- >  gtk_gesture_set_sequence_state (second_gesture, sequence, GTK_EVENT_SEQUENCE_DENIED);
-- >}
-- >
-- >static void
-- >second_gesture_begin_cb (GtkGesture       *second_gesture,
-- >                         GdkEventSequence *sequence,
-- >                         gpointer          user_data)
-- >{
-- >  if (gtk_gesture_get_sequence_state (first_gesture, sequence) == GTK_EVENT_SEQUENCE_CLAIMED)
-- >    gtk_gesture_set_sequence_state (second_gesture, sequence, GTK_EVENT_SEQUENCE_DENIED);
-- >}
-- 
-- 
-- If both gestures are in the same group, just set the state on
-- the gesture emitting the event, the sequence will be already
-- be initialized to the group\'s global state when the second
-- gesture processes the event.
-- 
-- /Since: 3.14/
gestureSetSequenceState ::
    (B.CallStack.HasCallStack, MonadIO m, IsGesture a) =>
    a
    -- ^ /@gesture@/: a t'GI.Gtk.Objects.Gesture.Gesture'
    -> Gdk.EventSequence.EventSequence
    -- ^ /@sequence@/: a t'GI.Gdk.Structs.EventSequence.EventSequence'
    -> Gtk.Enums.EventSequenceState
    -- ^ /@state@/: the sequence state
    -> m Bool
    -- ^ __Returns:__ 'P.True' if /@sequence@/ is handled by /@gesture@/,
    --          and the state is changed successfully
gestureSetSequenceState gesture sequence state = liftIO $ do
    gesture' <- unsafeManagedPtrCastPtr gesture
    sequence' <- unsafeManagedPtrGetPtr sequence
    let state' = (fromIntegral . fromEnum) state
    result <- gtk_gesture_set_sequence_state gesture' sequence' state'
    let result' = (/= 0) result
    touchManagedPtr gesture
    touchManagedPtr sequence
    return result'

#if defined(ENABLE_OVERLOADING)
data GestureSetSequenceStateMethodInfo
instance (signature ~ (Gdk.EventSequence.EventSequence -> Gtk.Enums.EventSequenceState -> m Bool), MonadIO m, IsGesture a) => O.OverloadedMethod GestureSetSequenceStateMethodInfo a signature where
    overloadedMethod = gestureSetSequenceState

instance O.OverloadedMethodInfo GestureSetSequenceStateMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Gesture.gestureSetSequenceState",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Gesture.html#v:gestureSetSequenceState"
        })


#endif

-- method Gesture::set_state
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "gesture"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Gesture" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkGesture" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "state"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "EventSequenceState" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the sequence state" , sinceVersion = Nothing }
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

foreign import ccall "gtk_gesture_set_state" gtk_gesture_set_state :: 
    Ptr Gesture ->                          -- gesture : TInterface (Name {namespace = "Gtk", name = "Gesture"})
    CUInt ->                                -- state : TInterface (Name {namespace = "Gtk", name = "EventSequenceState"})
    IO CInt

-- | Sets the state of all sequences that /@gesture@/ is currently
-- interacting with. See 'GI.Gtk.Objects.Gesture.gestureSetSequenceState'
-- for more details on sequence states.
-- 
-- /Since: 3.14/
gestureSetState ::
    (B.CallStack.HasCallStack, MonadIO m, IsGesture a) =>
    a
    -- ^ /@gesture@/: a t'GI.Gtk.Objects.Gesture.Gesture'
    -> Gtk.Enums.EventSequenceState
    -- ^ /@state@/: the sequence state
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the state of at least one sequence
    --     was changed successfully
gestureSetState gesture state = liftIO $ do
    gesture' <- unsafeManagedPtrCastPtr gesture
    let state' = (fromIntegral . fromEnum) state
    result <- gtk_gesture_set_state gesture' state'
    let result' = (/= 0) result
    touchManagedPtr gesture
    return result'

#if defined(ENABLE_OVERLOADING)
data GestureSetStateMethodInfo
instance (signature ~ (Gtk.Enums.EventSequenceState -> m Bool), MonadIO m, IsGesture a) => O.OverloadedMethod GestureSetStateMethodInfo a signature where
    overloadedMethod = gestureSetState

instance O.OverloadedMethodInfo GestureSetStateMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Gesture.gestureSetState",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Gesture.html#v:gestureSetState"
        })


#endif

-- method Gesture::set_window
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "gesture"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Gesture" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkGesture" , sinceVersion = Nothing }
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
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GdkWindow, or %NULL"
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

foreign import ccall "gtk_gesture_set_window" gtk_gesture_set_window :: 
    Ptr Gesture ->                          -- gesture : TInterface (Name {namespace = "Gtk", name = "Gesture"})
    Ptr Gdk.Window.Window ->                -- window : TInterface (Name {namespace = "Gdk", name = "Window"})
    IO ()

-- | Sets a specific window to receive events about, so /@gesture@/
-- will effectively handle only events targeting /@window@/, or
-- a child of it. /@window@/ must pertain to 'GI.Gtk.Objects.EventController.eventControllerGetWidget'.
-- 
-- /Since: 3.14/
gestureSetWindow ::
    (B.CallStack.HasCallStack, MonadIO m, IsGesture a, Gdk.Window.IsWindow b) =>
    a
    -- ^ /@gesture@/: a t'GI.Gtk.Objects.Gesture.Gesture'
    -> Maybe (b)
    -- ^ /@window@/: a t'GI.Gdk.Objects.Window.Window', or 'P.Nothing'
    -> m ()
gestureSetWindow gesture window = liftIO $ do
    gesture' <- unsafeManagedPtrCastPtr gesture
    maybeWindow <- case window of
        Nothing -> return nullPtr
        Just jWindow -> do
            jWindow' <- unsafeManagedPtrCastPtr jWindow
            return jWindow'
    gtk_gesture_set_window gesture' maybeWindow
    touchManagedPtr gesture
    whenJust window touchManagedPtr
    return ()

#if defined(ENABLE_OVERLOADING)
data GestureSetWindowMethodInfo
instance (signature ~ (Maybe (b) -> m ()), MonadIO m, IsGesture a, Gdk.Window.IsWindow b) => O.OverloadedMethod GestureSetWindowMethodInfo a signature where
    overloadedMethod = gestureSetWindow

instance O.OverloadedMethodInfo GestureSetWindowMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Gesture.gestureSetWindow",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Gesture.html#v:gestureSetWindow"
        })


#endif

-- method Gesture::ungroup
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "gesture"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Gesture" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkGesture" , sinceVersion = Nothing }
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

foreign import ccall "gtk_gesture_ungroup" gtk_gesture_ungroup :: 
    Ptr Gesture ->                          -- gesture : TInterface (Name {namespace = "Gtk", name = "Gesture"})
    IO ()

-- | Separates /@gesture@/ into an isolated group.
-- 
-- /Since: 3.14/
gestureUngroup ::
    (B.CallStack.HasCallStack, MonadIO m, IsGesture a) =>
    a
    -- ^ /@gesture@/: a t'GI.Gtk.Objects.Gesture.Gesture'
    -> m ()
gestureUngroup gesture = liftIO $ do
    gesture' <- unsafeManagedPtrCastPtr gesture
    gtk_gesture_ungroup gesture'
    touchManagedPtr gesture
    return ()

#if defined(ENABLE_OVERLOADING)
data GestureUngroupMethodInfo
instance (signature ~ (m ()), MonadIO m, IsGesture a) => O.OverloadedMethod GestureUngroupMethodInfo a signature where
    overloadedMethod = gestureUngroup

instance O.OverloadedMethodInfo GestureUngroupMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Gesture.gestureUngroup",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Gesture.html#v:gestureUngroup"
        })


#endif


