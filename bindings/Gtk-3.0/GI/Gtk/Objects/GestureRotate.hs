{-# LANGUAGE ImplicitParams, RankNTypes, TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- t'GI.Gtk.Objects.GestureRotate.GestureRotate' is a t'GI.Gtk.Objects.Gesture.Gesture' implementation able to recognize
-- 2-finger rotations, whenever the angle between both handled sequences
-- changes, the [GestureRotate::angleChanged]("GI.Gtk.Objects.GestureRotate#g:signal:angleChanged") signal is emitted.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.GestureRotate
    ( 

-- * Exported types
    GestureRotate(..)                       ,
    IsGestureRotate                         ,
    toGestureRotate                         ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [group]("GI.Gtk.Objects.Gesture#g:method:group"), [handleEvent]("GI.Gtk.Objects.EventController#g:method:handleEvent"), [handlesSequence]("GI.Gtk.Objects.Gesture#g:method:handlesSequence"), [isActive]("GI.Gtk.Objects.Gesture#g:method:isActive"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [isGroupedWith]("GI.Gtk.Objects.Gesture#g:method:isGroupedWith"), [isRecognized]("GI.Gtk.Objects.Gesture#g:method:isRecognized"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [reset]("GI.Gtk.Objects.EventController#g:method:reset"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [ungroup]("GI.Gtk.Objects.Gesture#g:method:ungroup"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getAngleDelta]("GI.Gtk.Objects.GestureRotate#g:method:getAngleDelta"), [getBoundingBox]("GI.Gtk.Objects.Gesture#g:method:getBoundingBox"), [getBoundingBoxCenter]("GI.Gtk.Objects.Gesture#g:method:getBoundingBoxCenter"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getDevice]("GI.Gtk.Objects.Gesture#g:method:getDevice"), [getGroup]("GI.Gtk.Objects.Gesture#g:method:getGroup"), [getLastEvent]("GI.Gtk.Objects.Gesture#g:method:getLastEvent"), [getLastUpdatedSequence]("GI.Gtk.Objects.Gesture#g:method:getLastUpdatedSequence"), [getPoint]("GI.Gtk.Objects.Gesture#g:method:getPoint"), [getPropagationPhase]("GI.Gtk.Objects.EventController#g:method:getPropagationPhase"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getSequenceState]("GI.Gtk.Objects.Gesture#g:method:getSequenceState"), [getSequences]("GI.Gtk.Objects.Gesture#g:method:getSequences"), [getWidget]("GI.Gtk.Objects.EventController#g:method:getWidget"), [getWindow]("GI.Gtk.Objects.Gesture#g:method:getWindow").
-- 
-- ==== Setters
-- [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setPropagationPhase]("GI.Gtk.Objects.EventController#g:method:setPropagationPhase"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setSequenceState]("GI.Gtk.Objects.Gesture#g:method:setSequenceState"), [setState]("GI.Gtk.Objects.Gesture#g:method:setState"), [setWindow]("GI.Gtk.Objects.Gesture#g:method:setWindow").

#if defined(ENABLE_OVERLOADING)
    ResolveGestureRotateMethod              ,
#endif

-- ** getAngleDelta #method:getAngleDelta#

#if defined(ENABLE_OVERLOADING)
    GestureRotateGetAngleDeltaMethodInfo    ,
#endif
    gestureRotateGetAngleDelta              ,


-- ** new #method:new#

    gestureRotateNew                        ,




 -- * Signals


-- ** angleChanged #signal:angleChanged#

    GestureRotateAngleChangedCallback       ,
#if defined(ENABLE_OVERLOADING)
    GestureRotateAngleChangedSignalInfo     ,
#endif
    afterGestureRotateAngleChanged          ,
    onGestureRotateAngleChanged             ,




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
import {-# SOURCE #-} qualified GI.Gtk.Objects.Widget as Gtk.Widget

-- | Memory-managed wrapper type.
newtype GestureRotate = GestureRotate (SP.ManagedPtr GestureRotate)
    deriving (Eq)

instance SP.ManagedPtrNewtype GestureRotate where
    toManagedPtr (GestureRotate p) = p

foreign import ccall "gtk_gesture_rotate_get_type"
    c_gtk_gesture_rotate_get_type :: IO B.Types.GType

instance B.Types.TypedObject GestureRotate where
    glibType = c_gtk_gesture_rotate_get_type

instance B.Types.GObject GestureRotate

-- | Type class for types which can be safely cast to `GestureRotate`, for instance with `toGestureRotate`.
class (SP.GObject o, O.IsDescendantOf GestureRotate o) => IsGestureRotate o
instance (SP.GObject o, O.IsDescendantOf GestureRotate o) => IsGestureRotate o

instance O.HasParentTypes GestureRotate
type instance O.ParentTypes GestureRotate = '[Gtk.Gesture.Gesture, Gtk.EventController.EventController, GObject.Object.Object]

-- | Cast to `GestureRotate`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toGestureRotate :: (MIO.MonadIO m, IsGestureRotate o) => o -> m GestureRotate
toGestureRotate = MIO.liftIO . B.ManagedPtr.unsafeCastTo GestureRotate

-- | Convert 'GestureRotate' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe GestureRotate) where
    gvalueGType_ = c_gtk_gesture_rotate_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr GestureRotate)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr GestureRotate)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject GestureRotate ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveGestureRotateMethod (t :: Symbol) (o :: *) :: * where
    ResolveGestureRotateMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveGestureRotateMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveGestureRotateMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveGestureRotateMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveGestureRotateMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveGestureRotateMethod "group" o = Gtk.Gesture.GestureGroupMethodInfo
    ResolveGestureRotateMethod "handleEvent" o = Gtk.EventController.EventControllerHandleEventMethodInfo
    ResolveGestureRotateMethod "handlesSequence" o = Gtk.Gesture.GestureHandlesSequenceMethodInfo
    ResolveGestureRotateMethod "isActive" o = Gtk.Gesture.GestureIsActiveMethodInfo
    ResolveGestureRotateMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveGestureRotateMethod "isGroupedWith" o = Gtk.Gesture.GestureIsGroupedWithMethodInfo
    ResolveGestureRotateMethod "isRecognized" o = Gtk.Gesture.GestureIsRecognizedMethodInfo
    ResolveGestureRotateMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveGestureRotateMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveGestureRotateMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveGestureRotateMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveGestureRotateMethod "reset" o = Gtk.EventController.EventControllerResetMethodInfo
    ResolveGestureRotateMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveGestureRotateMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveGestureRotateMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveGestureRotateMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveGestureRotateMethod "ungroup" o = Gtk.Gesture.GestureUngroupMethodInfo
    ResolveGestureRotateMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveGestureRotateMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveGestureRotateMethod "getAngleDelta" o = GestureRotateGetAngleDeltaMethodInfo
    ResolveGestureRotateMethod "getBoundingBox" o = Gtk.Gesture.GestureGetBoundingBoxMethodInfo
    ResolveGestureRotateMethod "getBoundingBoxCenter" o = Gtk.Gesture.GestureGetBoundingBoxCenterMethodInfo
    ResolveGestureRotateMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveGestureRotateMethod "getDevice" o = Gtk.Gesture.GestureGetDeviceMethodInfo
    ResolveGestureRotateMethod "getGroup" o = Gtk.Gesture.GestureGetGroupMethodInfo
    ResolveGestureRotateMethod "getLastEvent" o = Gtk.Gesture.GestureGetLastEventMethodInfo
    ResolveGestureRotateMethod "getLastUpdatedSequence" o = Gtk.Gesture.GestureGetLastUpdatedSequenceMethodInfo
    ResolveGestureRotateMethod "getPoint" o = Gtk.Gesture.GestureGetPointMethodInfo
    ResolveGestureRotateMethod "getPropagationPhase" o = Gtk.EventController.EventControllerGetPropagationPhaseMethodInfo
    ResolveGestureRotateMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveGestureRotateMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveGestureRotateMethod "getSequenceState" o = Gtk.Gesture.GestureGetSequenceStateMethodInfo
    ResolveGestureRotateMethod "getSequences" o = Gtk.Gesture.GestureGetSequencesMethodInfo
    ResolveGestureRotateMethod "getWidget" o = Gtk.EventController.EventControllerGetWidgetMethodInfo
    ResolveGestureRotateMethod "getWindow" o = Gtk.Gesture.GestureGetWindowMethodInfo
    ResolveGestureRotateMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveGestureRotateMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveGestureRotateMethod "setPropagationPhase" o = Gtk.EventController.EventControllerSetPropagationPhaseMethodInfo
    ResolveGestureRotateMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveGestureRotateMethod "setSequenceState" o = Gtk.Gesture.GestureSetSequenceStateMethodInfo
    ResolveGestureRotateMethod "setState" o = Gtk.Gesture.GestureSetStateMethodInfo
    ResolveGestureRotateMethod "setWindow" o = Gtk.Gesture.GestureSetWindowMethodInfo
    ResolveGestureRotateMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveGestureRotateMethod t GestureRotate, O.OverloadedMethod info GestureRotate p) => OL.IsLabel t (GestureRotate -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveGestureRotateMethod t GestureRotate, O.OverloadedMethod info GestureRotate p, R.HasField t GestureRotate p) => R.HasField t GestureRotate p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveGestureRotateMethod t GestureRotate, O.OverloadedMethodInfo info GestureRotate) => OL.IsLabel t (O.MethodProxy info GestureRotate) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- signal GestureRotate::angle-changed
-- | This signal is emitted when the angle between both tracked points
-- changes.
-- 
-- /Since: 3.14/
type GestureRotateAngleChangedCallback =
    Double
    -- ^ /@angle@/: Current angle in radians
    -> Double
    -- ^ /@angleDelta@/: Difference with the starting angle, in radians
    -> IO ()

type C_GestureRotateAngleChangedCallback =
    Ptr GestureRotate ->                    -- object
    CDouble ->
    CDouble ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_GestureRotateAngleChangedCallback`.
foreign import ccall "wrapper"
    mk_GestureRotateAngleChangedCallback :: C_GestureRotateAngleChangedCallback -> IO (FunPtr C_GestureRotateAngleChangedCallback)

wrap_GestureRotateAngleChangedCallback :: 
    GObject a => (a -> GestureRotateAngleChangedCallback) ->
    C_GestureRotateAngleChangedCallback
wrap_GestureRotateAngleChangedCallback gi'cb gi'selfPtr angle angleDelta _ = do
    let angle' = realToFrac angle
    let angleDelta' = realToFrac angleDelta
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  angle' angleDelta'


-- | Connect a signal handler for the [angleChanged](#signal:angleChanged) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' gestureRotate #angleChanged callback
-- @
-- 
-- 
onGestureRotateAngleChanged :: (IsGestureRotate a, MonadIO m) => a -> ((?self :: a) => GestureRotateAngleChangedCallback) -> m SignalHandlerId
onGestureRotateAngleChanged obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_GestureRotateAngleChangedCallback wrapped
    wrapped'' <- mk_GestureRotateAngleChangedCallback wrapped'
    connectSignalFunPtr obj "angle-changed" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [angleChanged](#signal:angleChanged) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' gestureRotate #angleChanged callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterGestureRotateAngleChanged :: (IsGestureRotate a, MonadIO m) => a -> ((?self :: a) => GestureRotateAngleChangedCallback) -> m SignalHandlerId
afterGestureRotateAngleChanged obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_GestureRotateAngleChangedCallback wrapped
    wrapped'' <- mk_GestureRotateAngleChangedCallback wrapped'
    connectSignalFunPtr obj "angle-changed" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data GestureRotateAngleChangedSignalInfo
instance SignalInfo GestureRotateAngleChangedSignalInfo where
    type HaskellCallbackType GestureRotateAngleChangedSignalInfo = GestureRotateAngleChangedCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_GestureRotateAngleChangedCallback cb
        cb'' <- mk_GestureRotateAngleChangedCallback cb'
        connectSignalFunPtr obj "angle-changed" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.GestureRotate::angle-changed"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-GestureRotate.html#g:signal:angleChanged"})

#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList GestureRotate
type instance O.AttributeList GestureRotate = GestureRotateAttributeList
type GestureRotateAttributeList = ('[ '("nPoints", Gtk.Gesture.GestureNPointsPropertyInfo), '("propagationPhase", Gtk.EventController.EventControllerPropagationPhasePropertyInfo), '("widget", Gtk.EventController.EventControllerWidgetPropertyInfo), '("window", Gtk.Gesture.GestureWindowPropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList GestureRotate = GestureRotateSignalList
type GestureRotateSignalList = ('[ '("angleChanged", GestureRotateAngleChangedSignalInfo), '("begin", Gtk.Gesture.GestureBeginSignalInfo), '("cancel", Gtk.Gesture.GestureCancelSignalInfo), '("end", Gtk.Gesture.GestureEndSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo), '("sequenceStateChanged", Gtk.Gesture.GestureSequenceStateChangedSignalInfo), '("update", Gtk.Gesture.GestureUpdateSignalInfo)] :: [(Symbol, *)])

#endif

-- method GestureRotate::new
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
--               (TInterface Name { namespace = "Gtk" , name = "GestureRotate" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_gesture_rotate_new" gtk_gesture_rotate_new :: 
    Ptr Gtk.Widget.Widget ->                -- widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    IO (Ptr GestureRotate)

-- | Returns a newly created t'GI.Gtk.Objects.Gesture.Gesture' that recognizes 2-touch
-- rotation gestures.
-- 
-- /Since: 3.14/
gestureRotateNew ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.Widget.IsWidget a) =>
    a
    -- ^ /@widget@/: a t'GI.Gtk.Objects.Widget.Widget'
    -> m GestureRotate
    -- ^ __Returns:__ a newly created t'GI.Gtk.Objects.GestureRotate.GestureRotate'
gestureRotateNew widget = liftIO $ do
    widget' <- unsafeManagedPtrCastPtr widget
    result <- gtk_gesture_rotate_new widget'
    checkUnexpectedReturnNULL "gestureRotateNew" result
    result' <- (wrapObject GestureRotate) result
    touchManagedPtr widget
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method GestureRotate::get_angle_delta
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "gesture"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "GestureRotate" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkGestureRotate"
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
-- returnType: Just (TBasicType TDouble)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_gesture_rotate_get_angle_delta" gtk_gesture_rotate_get_angle_delta :: 
    Ptr GestureRotate ->                    -- gesture : TInterface (Name {namespace = "Gtk", name = "GestureRotate"})
    IO CDouble

-- | If /@gesture@/ is active, this function returns the angle difference
-- in radians since the gesture was first recognized. If /@gesture@/ is
-- not active, 0 is returned.
-- 
-- /Since: 3.14/
gestureRotateGetAngleDelta ::
    (B.CallStack.HasCallStack, MonadIO m, IsGestureRotate a) =>
    a
    -- ^ /@gesture@/: a t'GI.Gtk.Objects.GestureRotate.GestureRotate'
    -> m Double
    -- ^ __Returns:__ the angle delta in radians
gestureRotateGetAngleDelta gesture = liftIO $ do
    gesture' <- unsafeManagedPtrCastPtr gesture
    result <- gtk_gesture_rotate_get_angle_delta gesture'
    let result' = realToFrac result
    touchManagedPtr gesture
    return result'

#if defined(ENABLE_OVERLOADING)
data GestureRotateGetAngleDeltaMethodInfo
instance (signature ~ (m Double), MonadIO m, IsGestureRotate a) => O.OverloadedMethod GestureRotateGetAngleDeltaMethodInfo a signature where
    overloadedMethod = gestureRotateGetAngleDelta

instance O.OverloadedMethodInfo GestureRotateGetAngleDeltaMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.GestureRotate.gestureRotateGetAngleDelta",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-GestureRotate.html#v:gestureRotateGetAngleDelta"
        })


#endif


