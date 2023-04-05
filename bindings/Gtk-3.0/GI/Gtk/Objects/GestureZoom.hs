{-# LANGUAGE ImplicitParams, RankNTypes, TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- t'GI.Gtk.Objects.GestureZoom.GestureZoom' is a t'GI.Gtk.Objects.Gesture.Gesture' implementation able to recognize
-- pinch\/zoom gestures, whenever the distance between both tracked
-- sequences changes, the [GestureZoom::scaleChanged]("GI.Gtk.Objects.GestureZoom#g:signal:scaleChanged") signal is
-- emitted to report the scale factor.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.GestureZoom
    ( 

-- * Exported types
    GestureZoom(..)                         ,
    IsGestureZoom                           ,
    toGestureZoom                           ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [group]("GI.Gtk.Objects.Gesture#g:method:group"), [handleEvent]("GI.Gtk.Objects.EventController#g:method:handleEvent"), [handlesSequence]("GI.Gtk.Objects.Gesture#g:method:handlesSequence"), [isActive]("GI.Gtk.Objects.Gesture#g:method:isActive"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [isGroupedWith]("GI.Gtk.Objects.Gesture#g:method:isGroupedWith"), [isRecognized]("GI.Gtk.Objects.Gesture#g:method:isRecognized"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [reset]("GI.Gtk.Objects.EventController#g:method:reset"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [ungroup]("GI.Gtk.Objects.Gesture#g:method:ungroup"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getBoundingBox]("GI.Gtk.Objects.Gesture#g:method:getBoundingBox"), [getBoundingBoxCenter]("GI.Gtk.Objects.Gesture#g:method:getBoundingBoxCenter"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getDevice]("GI.Gtk.Objects.Gesture#g:method:getDevice"), [getGroup]("GI.Gtk.Objects.Gesture#g:method:getGroup"), [getLastEvent]("GI.Gtk.Objects.Gesture#g:method:getLastEvent"), [getLastUpdatedSequence]("GI.Gtk.Objects.Gesture#g:method:getLastUpdatedSequence"), [getPoint]("GI.Gtk.Objects.Gesture#g:method:getPoint"), [getPropagationPhase]("GI.Gtk.Objects.EventController#g:method:getPropagationPhase"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getScaleDelta]("GI.Gtk.Objects.GestureZoom#g:method:getScaleDelta"), [getSequenceState]("GI.Gtk.Objects.Gesture#g:method:getSequenceState"), [getSequences]("GI.Gtk.Objects.Gesture#g:method:getSequences"), [getWidget]("GI.Gtk.Objects.EventController#g:method:getWidget"), [getWindow]("GI.Gtk.Objects.Gesture#g:method:getWindow").
-- 
-- ==== Setters
-- [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setPropagationPhase]("GI.Gtk.Objects.EventController#g:method:setPropagationPhase"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setSequenceState]("GI.Gtk.Objects.Gesture#g:method:setSequenceState"), [setState]("GI.Gtk.Objects.Gesture#g:method:setState"), [setWindow]("GI.Gtk.Objects.Gesture#g:method:setWindow").

#if defined(ENABLE_OVERLOADING)
    ResolveGestureZoomMethod                ,
#endif

-- ** getScaleDelta #method:getScaleDelta#

#if defined(ENABLE_OVERLOADING)
    GestureZoomGetScaleDeltaMethodInfo      ,
#endif
    gestureZoomGetScaleDelta                ,


-- ** new #method:new#

    gestureZoomNew                          ,




 -- * Signals


-- ** scaleChanged #signal:scaleChanged#

    GestureZoomScaleChangedCallback         ,
#if defined(ENABLE_OVERLOADING)
    GestureZoomScaleChangedSignalInfo       ,
#endif
    afterGestureZoomScaleChanged            ,
    onGestureZoomScaleChanged               ,




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
newtype GestureZoom = GestureZoom (SP.ManagedPtr GestureZoom)
    deriving (Eq)

instance SP.ManagedPtrNewtype GestureZoom where
    toManagedPtr (GestureZoom p) = p

foreign import ccall "gtk_gesture_zoom_get_type"
    c_gtk_gesture_zoom_get_type :: IO B.Types.GType

instance B.Types.TypedObject GestureZoom where
    glibType = c_gtk_gesture_zoom_get_type

instance B.Types.GObject GestureZoom

-- | Type class for types which can be safely cast to `GestureZoom`, for instance with `toGestureZoom`.
class (SP.GObject o, O.IsDescendantOf GestureZoom o) => IsGestureZoom o
instance (SP.GObject o, O.IsDescendantOf GestureZoom o) => IsGestureZoom o

instance O.HasParentTypes GestureZoom
type instance O.ParentTypes GestureZoom = '[Gtk.Gesture.Gesture, Gtk.EventController.EventController, GObject.Object.Object]

-- | Cast to `GestureZoom`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toGestureZoom :: (MIO.MonadIO m, IsGestureZoom o) => o -> m GestureZoom
toGestureZoom = MIO.liftIO . B.ManagedPtr.unsafeCastTo GestureZoom

-- | Convert 'GestureZoom' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe GestureZoom) where
    gvalueGType_ = c_gtk_gesture_zoom_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr GestureZoom)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr GestureZoom)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject GestureZoom ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveGestureZoomMethod (t :: Symbol) (o :: *) :: * where
    ResolveGestureZoomMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveGestureZoomMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveGestureZoomMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveGestureZoomMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveGestureZoomMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveGestureZoomMethod "group" o = Gtk.Gesture.GestureGroupMethodInfo
    ResolveGestureZoomMethod "handleEvent" o = Gtk.EventController.EventControllerHandleEventMethodInfo
    ResolveGestureZoomMethod "handlesSequence" o = Gtk.Gesture.GestureHandlesSequenceMethodInfo
    ResolveGestureZoomMethod "isActive" o = Gtk.Gesture.GestureIsActiveMethodInfo
    ResolveGestureZoomMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveGestureZoomMethod "isGroupedWith" o = Gtk.Gesture.GestureIsGroupedWithMethodInfo
    ResolveGestureZoomMethod "isRecognized" o = Gtk.Gesture.GestureIsRecognizedMethodInfo
    ResolveGestureZoomMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveGestureZoomMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveGestureZoomMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveGestureZoomMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveGestureZoomMethod "reset" o = Gtk.EventController.EventControllerResetMethodInfo
    ResolveGestureZoomMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveGestureZoomMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveGestureZoomMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveGestureZoomMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveGestureZoomMethod "ungroup" o = Gtk.Gesture.GestureUngroupMethodInfo
    ResolveGestureZoomMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveGestureZoomMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveGestureZoomMethod "getBoundingBox" o = Gtk.Gesture.GestureGetBoundingBoxMethodInfo
    ResolveGestureZoomMethod "getBoundingBoxCenter" o = Gtk.Gesture.GestureGetBoundingBoxCenterMethodInfo
    ResolveGestureZoomMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveGestureZoomMethod "getDevice" o = Gtk.Gesture.GestureGetDeviceMethodInfo
    ResolveGestureZoomMethod "getGroup" o = Gtk.Gesture.GestureGetGroupMethodInfo
    ResolveGestureZoomMethod "getLastEvent" o = Gtk.Gesture.GestureGetLastEventMethodInfo
    ResolveGestureZoomMethod "getLastUpdatedSequence" o = Gtk.Gesture.GestureGetLastUpdatedSequenceMethodInfo
    ResolveGestureZoomMethod "getPoint" o = Gtk.Gesture.GestureGetPointMethodInfo
    ResolveGestureZoomMethod "getPropagationPhase" o = Gtk.EventController.EventControllerGetPropagationPhaseMethodInfo
    ResolveGestureZoomMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveGestureZoomMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveGestureZoomMethod "getScaleDelta" o = GestureZoomGetScaleDeltaMethodInfo
    ResolveGestureZoomMethod "getSequenceState" o = Gtk.Gesture.GestureGetSequenceStateMethodInfo
    ResolveGestureZoomMethod "getSequences" o = Gtk.Gesture.GestureGetSequencesMethodInfo
    ResolveGestureZoomMethod "getWidget" o = Gtk.EventController.EventControllerGetWidgetMethodInfo
    ResolveGestureZoomMethod "getWindow" o = Gtk.Gesture.GestureGetWindowMethodInfo
    ResolveGestureZoomMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveGestureZoomMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveGestureZoomMethod "setPropagationPhase" o = Gtk.EventController.EventControllerSetPropagationPhaseMethodInfo
    ResolveGestureZoomMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveGestureZoomMethod "setSequenceState" o = Gtk.Gesture.GestureSetSequenceStateMethodInfo
    ResolveGestureZoomMethod "setState" o = Gtk.Gesture.GestureSetStateMethodInfo
    ResolveGestureZoomMethod "setWindow" o = Gtk.Gesture.GestureSetWindowMethodInfo
    ResolveGestureZoomMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveGestureZoomMethod t GestureZoom, O.OverloadedMethod info GestureZoom p) => OL.IsLabel t (GestureZoom -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveGestureZoomMethod t GestureZoom, O.OverloadedMethod info GestureZoom p, R.HasField t GestureZoom p) => R.HasField t GestureZoom p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveGestureZoomMethod t GestureZoom, O.OverloadedMethodInfo info GestureZoom) => OL.IsLabel t (O.MethodProxy info GestureZoom) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- signal GestureZoom::scale-changed
-- | This signal is emitted whenever the distance between both tracked
-- sequences changes.
-- 
-- /Since: 3.14/
type GestureZoomScaleChangedCallback =
    Double
    -- ^ /@scale@/: Scale delta, taking the initial state as 1:1
    -> IO ()

type C_GestureZoomScaleChangedCallback =
    Ptr GestureZoom ->                      -- object
    CDouble ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_GestureZoomScaleChangedCallback`.
foreign import ccall "wrapper"
    mk_GestureZoomScaleChangedCallback :: C_GestureZoomScaleChangedCallback -> IO (FunPtr C_GestureZoomScaleChangedCallback)

wrap_GestureZoomScaleChangedCallback :: 
    GObject a => (a -> GestureZoomScaleChangedCallback) ->
    C_GestureZoomScaleChangedCallback
wrap_GestureZoomScaleChangedCallback gi'cb gi'selfPtr scale _ = do
    let scale' = realToFrac scale
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  scale'


-- | Connect a signal handler for the [scaleChanged](#signal:scaleChanged) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' gestureZoom #scaleChanged callback
-- @
-- 
-- 
onGestureZoomScaleChanged :: (IsGestureZoom a, MonadIO m) => a -> ((?self :: a) => GestureZoomScaleChangedCallback) -> m SignalHandlerId
onGestureZoomScaleChanged obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_GestureZoomScaleChangedCallback wrapped
    wrapped'' <- mk_GestureZoomScaleChangedCallback wrapped'
    connectSignalFunPtr obj "scale-changed" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [scaleChanged](#signal:scaleChanged) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' gestureZoom #scaleChanged callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterGestureZoomScaleChanged :: (IsGestureZoom a, MonadIO m) => a -> ((?self :: a) => GestureZoomScaleChangedCallback) -> m SignalHandlerId
afterGestureZoomScaleChanged obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_GestureZoomScaleChangedCallback wrapped
    wrapped'' <- mk_GestureZoomScaleChangedCallback wrapped'
    connectSignalFunPtr obj "scale-changed" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data GestureZoomScaleChangedSignalInfo
instance SignalInfo GestureZoomScaleChangedSignalInfo where
    type HaskellCallbackType GestureZoomScaleChangedSignalInfo = GestureZoomScaleChangedCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_GestureZoomScaleChangedCallback cb
        cb'' <- mk_GestureZoomScaleChangedCallback cb'
        connectSignalFunPtr obj "scale-changed" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.GestureZoom::scale-changed"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-GestureZoom.html#g:signal:scaleChanged"})

#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList GestureZoom
type instance O.AttributeList GestureZoom = GestureZoomAttributeList
type GestureZoomAttributeList = ('[ '("nPoints", Gtk.Gesture.GestureNPointsPropertyInfo), '("propagationPhase", Gtk.EventController.EventControllerPropagationPhasePropertyInfo), '("widget", Gtk.EventController.EventControllerWidgetPropertyInfo), '("window", Gtk.Gesture.GestureWindowPropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList GestureZoom = GestureZoomSignalList
type GestureZoomSignalList = ('[ '("begin", Gtk.Gesture.GestureBeginSignalInfo), '("cancel", Gtk.Gesture.GestureCancelSignalInfo), '("end", Gtk.Gesture.GestureEndSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo), '("scaleChanged", GestureZoomScaleChangedSignalInfo), '("sequenceStateChanged", Gtk.Gesture.GestureSequenceStateChangedSignalInfo), '("update", Gtk.Gesture.GestureUpdateSignalInfo)] :: [(Symbol, *)])

#endif

-- method GestureZoom::new
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "GestureZoom" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_gesture_zoom_new" gtk_gesture_zoom_new :: 
    Ptr Gtk.Widget.Widget ->                -- widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    IO (Ptr GestureZoom)

-- | Returns a newly created t'GI.Gtk.Objects.Gesture.Gesture' that recognizes zoom
-- in\/out gestures (usually known as pinch\/zoom).
-- 
-- /Since: 3.14/
gestureZoomNew ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.Widget.IsWidget a) =>
    a
    -- ^ /@widget@/: a t'GI.Gtk.Objects.Widget.Widget'
    -> m GestureZoom
    -- ^ __Returns:__ a newly created t'GI.Gtk.Objects.GestureZoom.GestureZoom'
gestureZoomNew widget = liftIO $ do
    widget' <- unsafeManagedPtrCastPtr widget
    result <- gtk_gesture_zoom_new widget'
    checkUnexpectedReturnNULL "gestureZoomNew" result
    result' <- (newObject GestureZoom) result
    touchManagedPtr widget
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method GestureZoom::get_scale_delta
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "gesture"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "GestureZoom" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkGestureZoom" , sinceVersion = Nothing }
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

foreign import ccall "gtk_gesture_zoom_get_scale_delta" gtk_gesture_zoom_get_scale_delta :: 
    Ptr GestureZoom ->                      -- gesture : TInterface (Name {namespace = "Gtk", name = "GestureZoom"})
    IO CDouble

-- | If /@gesture@/ is active, this function returns the zooming difference
-- since the gesture was recognized (hence the starting point is
-- considered 1:1). If /@gesture@/ is not active, 1 is returned.
-- 
-- /Since: 3.14/
gestureZoomGetScaleDelta ::
    (B.CallStack.HasCallStack, MonadIO m, IsGestureZoom a) =>
    a
    -- ^ /@gesture@/: a t'GI.Gtk.Objects.GestureZoom.GestureZoom'
    -> m Double
    -- ^ __Returns:__ the scale delta
gestureZoomGetScaleDelta gesture = liftIO $ do
    gesture' <- unsafeManagedPtrCastPtr gesture
    result <- gtk_gesture_zoom_get_scale_delta gesture'
    let result' = realToFrac result
    touchManagedPtr gesture
    return result'

#if defined(ENABLE_OVERLOADING)
data GestureZoomGetScaleDeltaMethodInfo
instance (signature ~ (m Double), MonadIO m, IsGestureZoom a) => O.OverloadedMethod GestureZoomGetScaleDeltaMethodInfo a signature where
    overloadedMethod = gestureZoomGetScaleDelta

instance O.OverloadedMethodInfo GestureZoomGetScaleDeltaMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.GestureZoom.gestureZoomGetScaleDelta",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-GestureZoom.html#v:gestureZoomGetScaleDelta"
        })


#endif


