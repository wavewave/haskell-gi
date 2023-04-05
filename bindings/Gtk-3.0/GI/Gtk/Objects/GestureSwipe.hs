{-# LANGUAGE ImplicitParams, RankNTypes, TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- t'GI.Gtk.Objects.GestureSwipe.GestureSwipe' is a t'GI.Gtk.Objects.Gesture.Gesture' implementation able to recognize
-- swipes, after a press\/move\/...\/move\/release sequence happens, the
-- [GestureSwipe::swipe]("GI.Gtk.Objects.GestureSwipe#g:signal:swipe") signal will be emitted, providing the velocity
-- and directionality of the sequence at the time it was lifted.
-- 
-- If the velocity is desired in intermediate points,
-- 'GI.Gtk.Objects.GestureSwipe.gestureSwipeGetVelocity' can be called on eg. a
-- [Gesture::update]("GI.Gtk.Objects.Gesture#g:signal:update") handler.
-- 
-- All velocities are reported in pixels\/sec units.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.GestureSwipe
    ( 

-- * Exported types
    GestureSwipe(..)                        ,
    IsGestureSwipe                          ,
    toGestureSwipe                          ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [group]("GI.Gtk.Objects.Gesture#g:method:group"), [handleEvent]("GI.Gtk.Objects.EventController#g:method:handleEvent"), [handlesSequence]("GI.Gtk.Objects.Gesture#g:method:handlesSequence"), [isActive]("GI.Gtk.Objects.Gesture#g:method:isActive"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [isGroupedWith]("GI.Gtk.Objects.Gesture#g:method:isGroupedWith"), [isRecognized]("GI.Gtk.Objects.Gesture#g:method:isRecognized"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [reset]("GI.Gtk.Objects.EventController#g:method:reset"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [ungroup]("GI.Gtk.Objects.Gesture#g:method:ungroup"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getBoundingBox]("GI.Gtk.Objects.Gesture#g:method:getBoundingBox"), [getBoundingBoxCenter]("GI.Gtk.Objects.Gesture#g:method:getBoundingBoxCenter"), [getButton]("GI.Gtk.Objects.GestureSingle#g:method:getButton"), [getCurrentButton]("GI.Gtk.Objects.GestureSingle#g:method:getCurrentButton"), [getCurrentSequence]("GI.Gtk.Objects.GestureSingle#g:method:getCurrentSequence"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getDevice]("GI.Gtk.Objects.Gesture#g:method:getDevice"), [getExclusive]("GI.Gtk.Objects.GestureSingle#g:method:getExclusive"), [getGroup]("GI.Gtk.Objects.Gesture#g:method:getGroup"), [getLastEvent]("GI.Gtk.Objects.Gesture#g:method:getLastEvent"), [getLastUpdatedSequence]("GI.Gtk.Objects.Gesture#g:method:getLastUpdatedSequence"), [getPoint]("GI.Gtk.Objects.Gesture#g:method:getPoint"), [getPropagationPhase]("GI.Gtk.Objects.EventController#g:method:getPropagationPhase"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getSequenceState]("GI.Gtk.Objects.Gesture#g:method:getSequenceState"), [getSequences]("GI.Gtk.Objects.Gesture#g:method:getSequences"), [getTouchOnly]("GI.Gtk.Objects.GestureSingle#g:method:getTouchOnly"), [getVelocity]("GI.Gtk.Objects.GestureSwipe#g:method:getVelocity"), [getWidget]("GI.Gtk.Objects.EventController#g:method:getWidget"), [getWindow]("GI.Gtk.Objects.Gesture#g:method:getWindow").
-- 
-- ==== Setters
-- [setButton]("GI.Gtk.Objects.GestureSingle#g:method:setButton"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setExclusive]("GI.Gtk.Objects.GestureSingle#g:method:setExclusive"), [setPropagationPhase]("GI.Gtk.Objects.EventController#g:method:setPropagationPhase"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setSequenceState]("GI.Gtk.Objects.Gesture#g:method:setSequenceState"), [setState]("GI.Gtk.Objects.Gesture#g:method:setState"), [setTouchOnly]("GI.Gtk.Objects.GestureSingle#g:method:setTouchOnly"), [setWindow]("GI.Gtk.Objects.Gesture#g:method:setWindow").

#if defined(ENABLE_OVERLOADING)
    ResolveGestureSwipeMethod               ,
#endif

-- ** getVelocity #method:getVelocity#

#if defined(ENABLE_OVERLOADING)
    GestureSwipeGetVelocityMethodInfo       ,
#endif
    gestureSwipeGetVelocity                 ,


-- ** new #method:new#

    gestureSwipeNew                         ,




 -- * Signals


-- ** swipe #signal:swipe#

    GestureSwipeSwipeCallback               ,
#if defined(ENABLE_OVERLOADING)
    GestureSwipeSwipeSignalInfo             ,
#endif
    afterGestureSwipeSwipe                  ,
    onGestureSwipeSwipe                     ,




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
import {-# SOURCE #-} qualified GI.Gtk.Objects.Gesture as Gtk.Gesture
import {-# SOURCE #-} qualified GI.Gtk.Objects.GestureSingle as Gtk.GestureSingle
import {-# SOURCE #-} qualified GI.Gtk.Objects.Widget as Gtk.Widget

-- | Memory-managed wrapper type.
newtype GestureSwipe = GestureSwipe (SP.ManagedPtr GestureSwipe)
    deriving (Eq)

instance SP.ManagedPtrNewtype GestureSwipe where
    toManagedPtr (GestureSwipe p) = p

foreign import ccall "gtk_gesture_swipe_get_type"
    c_gtk_gesture_swipe_get_type :: IO B.Types.GType

instance B.Types.TypedObject GestureSwipe where
    glibType = c_gtk_gesture_swipe_get_type

instance B.Types.GObject GestureSwipe

-- | Type class for types which can be safely cast to `GestureSwipe`, for instance with `toGestureSwipe`.
class (SP.GObject o, O.IsDescendantOf GestureSwipe o) => IsGestureSwipe o
instance (SP.GObject o, O.IsDescendantOf GestureSwipe o) => IsGestureSwipe o

instance O.HasParentTypes GestureSwipe
type instance O.ParentTypes GestureSwipe = '[Gtk.GestureSingle.GestureSingle, Gtk.Gesture.Gesture, Gtk.EventController.EventController, GObject.Object.Object]

-- | Cast to `GestureSwipe`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toGestureSwipe :: (MIO.MonadIO m, IsGestureSwipe o) => o -> m GestureSwipe
toGestureSwipe = MIO.liftIO . B.ManagedPtr.unsafeCastTo GestureSwipe

-- | Convert 'GestureSwipe' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe GestureSwipe) where
    gvalueGType_ = c_gtk_gesture_swipe_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr GestureSwipe)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr GestureSwipe)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject GestureSwipe ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveGestureSwipeMethod (t :: Symbol) (o :: *) :: * where
    ResolveGestureSwipeMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveGestureSwipeMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveGestureSwipeMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveGestureSwipeMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveGestureSwipeMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveGestureSwipeMethod "group" o = Gtk.Gesture.GestureGroupMethodInfo
    ResolveGestureSwipeMethod "handleEvent" o = Gtk.EventController.EventControllerHandleEventMethodInfo
    ResolveGestureSwipeMethod "handlesSequence" o = Gtk.Gesture.GestureHandlesSequenceMethodInfo
    ResolveGestureSwipeMethod "isActive" o = Gtk.Gesture.GestureIsActiveMethodInfo
    ResolveGestureSwipeMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveGestureSwipeMethod "isGroupedWith" o = Gtk.Gesture.GestureIsGroupedWithMethodInfo
    ResolveGestureSwipeMethod "isRecognized" o = Gtk.Gesture.GestureIsRecognizedMethodInfo
    ResolveGestureSwipeMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveGestureSwipeMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveGestureSwipeMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveGestureSwipeMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveGestureSwipeMethod "reset" o = Gtk.EventController.EventControllerResetMethodInfo
    ResolveGestureSwipeMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveGestureSwipeMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveGestureSwipeMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveGestureSwipeMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveGestureSwipeMethod "ungroup" o = Gtk.Gesture.GestureUngroupMethodInfo
    ResolveGestureSwipeMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveGestureSwipeMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveGestureSwipeMethod "getBoundingBox" o = Gtk.Gesture.GestureGetBoundingBoxMethodInfo
    ResolveGestureSwipeMethod "getBoundingBoxCenter" o = Gtk.Gesture.GestureGetBoundingBoxCenterMethodInfo
    ResolveGestureSwipeMethod "getButton" o = Gtk.GestureSingle.GestureSingleGetButtonMethodInfo
    ResolveGestureSwipeMethod "getCurrentButton" o = Gtk.GestureSingle.GestureSingleGetCurrentButtonMethodInfo
    ResolveGestureSwipeMethod "getCurrentSequence" o = Gtk.GestureSingle.GestureSingleGetCurrentSequenceMethodInfo
    ResolveGestureSwipeMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveGestureSwipeMethod "getDevice" o = Gtk.Gesture.GestureGetDeviceMethodInfo
    ResolveGestureSwipeMethod "getExclusive" o = Gtk.GestureSingle.GestureSingleGetExclusiveMethodInfo
    ResolveGestureSwipeMethod "getGroup" o = Gtk.Gesture.GestureGetGroupMethodInfo
    ResolveGestureSwipeMethod "getLastEvent" o = Gtk.Gesture.GestureGetLastEventMethodInfo
    ResolveGestureSwipeMethod "getLastUpdatedSequence" o = Gtk.Gesture.GestureGetLastUpdatedSequenceMethodInfo
    ResolveGestureSwipeMethod "getPoint" o = Gtk.Gesture.GestureGetPointMethodInfo
    ResolveGestureSwipeMethod "getPropagationPhase" o = Gtk.EventController.EventControllerGetPropagationPhaseMethodInfo
    ResolveGestureSwipeMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveGestureSwipeMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveGestureSwipeMethod "getSequenceState" o = Gtk.Gesture.GestureGetSequenceStateMethodInfo
    ResolveGestureSwipeMethod "getSequences" o = Gtk.Gesture.GestureGetSequencesMethodInfo
    ResolveGestureSwipeMethod "getTouchOnly" o = Gtk.GestureSingle.GestureSingleGetTouchOnlyMethodInfo
    ResolveGestureSwipeMethod "getVelocity" o = GestureSwipeGetVelocityMethodInfo
    ResolveGestureSwipeMethod "getWidget" o = Gtk.EventController.EventControllerGetWidgetMethodInfo
    ResolveGestureSwipeMethod "getWindow" o = Gtk.Gesture.GestureGetWindowMethodInfo
    ResolveGestureSwipeMethod "setButton" o = Gtk.GestureSingle.GestureSingleSetButtonMethodInfo
    ResolveGestureSwipeMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveGestureSwipeMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveGestureSwipeMethod "setExclusive" o = Gtk.GestureSingle.GestureSingleSetExclusiveMethodInfo
    ResolveGestureSwipeMethod "setPropagationPhase" o = Gtk.EventController.EventControllerSetPropagationPhaseMethodInfo
    ResolveGestureSwipeMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveGestureSwipeMethod "setSequenceState" o = Gtk.Gesture.GestureSetSequenceStateMethodInfo
    ResolveGestureSwipeMethod "setState" o = Gtk.Gesture.GestureSetStateMethodInfo
    ResolveGestureSwipeMethod "setTouchOnly" o = Gtk.GestureSingle.GestureSingleSetTouchOnlyMethodInfo
    ResolveGestureSwipeMethod "setWindow" o = Gtk.Gesture.GestureSetWindowMethodInfo
    ResolveGestureSwipeMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveGestureSwipeMethod t GestureSwipe, O.OverloadedMethod info GestureSwipe p) => OL.IsLabel t (GestureSwipe -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveGestureSwipeMethod t GestureSwipe, O.OverloadedMethod info GestureSwipe p, R.HasField t GestureSwipe p) => R.HasField t GestureSwipe p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveGestureSwipeMethod t GestureSwipe, O.OverloadedMethodInfo info GestureSwipe) => OL.IsLabel t (O.MethodProxy info GestureSwipe) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- signal GestureSwipe::swipe
-- | This signal is emitted when the recognized gesture is finished, velocity
-- and direction are a product of previously recorded events.
-- 
-- /Since: 3.14/
type GestureSwipeSwipeCallback =
    Double
    -- ^ /@velocityX@/: velocity in the X axis, in pixels\/sec
    -> Double
    -- ^ /@velocityY@/: velocity in the Y axis, in pixels\/sec
    -> IO ()

type C_GestureSwipeSwipeCallback =
    Ptr GestureSwipe ->                     -- object
    CDouble ->
    CDouble ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_GestureSwipeSwipeCallback`.
foreign import ccall "wrapper"
    mk_GestureSwipeSwipeCallback :: C_GestureSwipeSwipeCallback -> IO (FunPtr C_GestureSwipeSwipeCallback)

wrap_GestureSwipeSwipeCallback :: 
    GObject a => (a -> GestureSwipeSwipeCallback) ->
    C_GestureSwipeSwipeCallback
wrap_GestureSwipeSwipeCallback gi'cb gi'selfPtr velocityX velocityY _ = do
    let velocityX' = realToFrac velocityX
    let velocityY' = realToFrac velocityY
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  velocityX' velocityY'


-- | Connect a signal handler for the [swipe](#signal:swipe) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' gestureSwipe #swipe callback
-- @
-- 
-- 
onGestureSwipeSwipe :: (IsGestureSwipe a, MonadIO m) => a -> ((?self :: a) => GestureSwipeSwipeCallback) -> m SignalHandlerId
onGestureSwipeSwipe obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_GestureSwipeSwipeCallback wrapped
    wrapped'' <- mk_GestureSwipeSwipeCallback wrapped'
    connectSignalFunPtr obj "swipe" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [swipe](#signal:swipe) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' gestureSwipe #swipe callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterGestureSwipeSwipe :: (IsGestureSwipe a, MonadIO m) => a -> ((?self :: a) => GestureSwipeSwipeCallback) -> m SignalHandlerId
afterGestureSwipeSwipe obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_GestureSwipeSwipeCallback wrapped
    wrapped'' <- mk_GestureSwipeSwipeCallback wrapped'
    connectSignalFunPtr obj "swipe" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data GestureSwipeSwipeSignalInfo
instance SignalInfo GestureSwipeSwipeSignalInfo where
    type HaskellCallbackType GestureSwipeSwipeSignalInfo = GestureSwipeSwipeCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_GestureSwipeSwipeCallback cb
        cb'' <- mk_GestureSwipeSwipeCallback cb'
        connectSignalFunPtr obj "swipe" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.GestureSwipe::swipe"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-GestureSwipe.html#g:signal:swipe"})

#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList GestureSwipe
type instance O.AttributeList GestureSwipe = GestureSwipeAttributeList
type GestureSwipeAttributeList = ('[ '("button", Gtk.GestureSingle.GestureSingleButtonPropertyInfo), '("exclusive", Gtk.GestureSingle.GestureSingleExclusivePropertyInfo), '("nPoints", Gtk.Gesture.GestureNPointsPropertyInfo), '("propagationPhase", Gtk.EventController.EventControllerPropagationPhasePropertyInfo), '("touchOnly", Gtk.GestureSingle.GestureSingleTouchOnlyPropertyInfo), '("widget", Gtk.EventController.EventControllerWidgetPropertyInfo), '("window", Gtk.Gesture.GestureWindowPropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList GestureSwipe = GestureSwipeSignalList
type GestureSwipeSignalList = ('[ '("begin", Gtk.Gesture.GestureBeginSignalInfo), '("cancel", Gtk.Gesture.GestureCancelSignalInfo), '("end", Gtk.Gesture.GestureEndSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo), '("sequenceStateChanged", Gtk.Gesture.GestureSequenceStateChangedSignalInfo), '("swipe", GestureSwipeSwipeSignalInfo), '("update", Gtk.Gesture.GestureUpdateSignalInfo)] :: [(Symbol, *)])

#endif

-- method GestureSwipe::new
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
--               (TInterface Name { namespace = "Gtk" , name = "GestureSwipe" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_gesture_swipe_new" gtk_gesture_swipe_new :: 
    Ptr Gtk.Widget.Widget ->                -- widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    IO (Ptr GestureSwipe)

-- | Returns a newly created t'GI.Gtk.Objects.Gesture.Gesture' that recognizes swipes.
-- 
-- /Since: 3.14/
gestureSwipeNew ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.Widget.IsWidget a) =>
    a
    -- ^ /@widget@/: a t'GI.Gtk.Objects.Widget.Widget'
    -> m GestureSwipe
    -- ^ __Returns:__ a newly created t'GI.Gtk.Objects.GestureSwipe.GestureSwipe'
gestureSwipeNew widget = liftIO $ do
    widget' <- unsafeManagedPtrCastPtr widget
    result <- gtk_gesture_swipe_new widget'
    checkUnexpectedReturnNULL "gestureSwipeNew" result
    result' <- (wrapObject GestureSwipe) result
    touchManagedPtr widget
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method GestureSwipe::get_velocity
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "gesture"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "GestureSwipe" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkGestureSwipe" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "velocity_x"
--           , argType = TBasicType TDouble
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "return value for the velocity in the X axis, in pixels/sec"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       , Arg
--           { argCName = "velocity_y"
--           , argType = TBasicType TDouble
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "return value for the velocity in the Y axis, in pixels/sec"
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

foreign import ccall "gtk_gesture_swipe_get_velocity" gtk_gesture_swipe_get_velocity :: 
    Ptr GestureSwipe ->                     -- gesture : TInterface (Name {namespace = "Gtk", name = "GestureSwipe"})
    Ptr CDouble ->                          -- velocity_x : TBasicType TDouble
    Ptr CDouble ->                          -- velocity_y : TBasicType TDouble
    IO CInt

-- | If the gesture is recognized, this function returns 'P.True' and fill in
-- /@velocityX@/ and /@velocityY@/ with the recorded velocity, as per the
-- last event(s) processed.
-- 
-- /Since: 3.14/
gestureSwipeGetVelocity ::
    (B.CallStack.HasCallStack, MonadIO m, IsGestureSwipe a) =>
    a
    -- ^ /@gesture@/: a t'GI.Gtk.Objects.GestureSwipe.GestureSwipe'
    -> m ((Bool, Double, Double))
    -- ^ __Returns:__ whether velocity could be calculated
gestureSwipeGetVelocity gesture = liftIO $ do
    gesture' <- unsafeManagedPtrCastPtr gesture
    velocityX <- allocMem :: IO (Ptr CDouble)
    velocityY <- allocMem :: IO (Ptr CDouble)
    result <- gtk_gesture_swipe_get_velocity gesture' velocityX velocityY
    let result' = (/= 0) result
    velocityX' <- peek velocityX
    let velocityX'' = realToFrac velocityX'
    velocityY' <- peek velocityY
    let velocityY'' = realToFrac velocityY'
    touchManagedPtr gesture
    freeMem velocityX
    freeMem velocityY
    return (result', velocityX'', velocityY'')

#if defined(ENABLE_OVERLOADING)
data GestureSwipeGetVelocityMethodInfo
instance (signature ~ (m ((Bool, Double, Double))), MonadIO m, IsGestureSwipe a) => O.OverloadedMethod GestureSwipeGetVelocityMethodInfo a signature where
    overloadedMethod = gestureSwipeGetVelocity

instance O.OverloadedMethodInfo GestureSwipeGetVelocityMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.GestureSwipe.gestureSwipeGetVelocity",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-GestureSwipe.html#v:gestureSwipeGetVelocity"
        })


#endif


