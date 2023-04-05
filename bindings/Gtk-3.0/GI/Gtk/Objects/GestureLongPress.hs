{-# LANGUAGE ImplicitParams, RankNTypes, TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- t'GI.Gtk.Objects.GestureLongPress.GestureLongPress' is a t'GI.Gtk.Objects.Gesture.Gesture' implementation able to recognize
-- long presses, triggering the [GestureLongPress::pressed]("GI.Gtk.Objects.GestureLongPress#g:signal:pressed") after the
-- timeout is exceeded.
-- 
-- If the touchpoint is lifted before the timeout passes, or if it drifts
-- too far of the initial press point, the [GestureLongPress::cancelled]("GI.Gtk.Objects.GestureLongPress#g:signal:cancelled")
-- signal will be emitted.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.GestureLongPress
    ( 

-- * Exported types
    GestureLongPress(..)                    ,
    IsGestureLongPress                      ,
    toGestureLongPress                      ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [group]("GI.Gtk.Objects.Gesture#g:method:group"), [handleEvent]("GI.Gtk.Objects.EventController#g:method:handleEvent"), [handlesSequence]("GI.Gtk.Objects.Gesture#g:method:handlesSequence"), [isActive]("GI.Gtk.Objects.Gesture#g:method:isActive"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [isGroupedWith]("GI.Gtk.Objects.Gesture#g:method:isGroupedWith"), [isRecognized]("GI.Gtk.Objects.Gesture#g:method:isRecognized"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [reset]("GI.Gtk.Objects.EventController#g:method:reset"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [ungroup]("GI.Gtk.Objects.Gesture#g:method:ungroup"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getBoundingBox]("GI.Gtk.Objects.Gesture#g:method:getBoundingBox"), [getBoundingBoxCenter]("GI.Gtk.Objects.Gesture#g:method:getBoundingBoxCenter"), [getButton]("GI.Gtk.Objects.GestureSingle#g:method:getButton"), [getCurrentButton]("GI.Gtk.Objects.GestureSingle#g:method:getCurrentButton"), [getCurrentSequence]("GI.Gtk.Objects.GestureSingle#g:method:getCurrentSequence"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getDevice]("GI.Gtk.Objects.Gesture#g:method:getDevice"), [getExclusive]("GI.Gtk.Objects.GestureSingle#g:method:getExclusive"), [getGroup]("GI.Gtk.Objects.Gesture#g:method:getGroup"), [getLastEvent]("GI.Gtk.Objects.Gesture#g:method:getLastEvent"), [getLastUpdatedSequence]("GI.Gtk.Objects.Gesture#g:method:getLastUpdatedSequence"), [getPoint]("GI.Gtk.Objects.Gesture#g:method:getPoint"), [getPropagationPhase]("GI.Gtk.Objects.EventController#g:method:getPropagationPhase"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getSequenceState]("GI.Gtk.Objects.Gesture#g:method:getSequenceState"), [getSequences]("GI.Gtk.Objects.Gesture#g:method:getSequences"), [getTouchOnly]("GI.Gtk.Objects.GestureSingle#g:method:getTouchOnly"), [getWidget]("GI.Gtk.Objects.EventController#g:method:getWidget"), [getWindow]("GI.Gtk.Objects.Gesture#g:method:getWindow").
-- 
-- ==== Setters
-- [setButton]("GI.Gtk.Objects.GestureSingle#g:method:setButton"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setExclusive]("GI.Gtk.Objects.GestureSingle#g:method:setExclusive"), [setPropagationPhase]("GI.Gtk.Objects.EventController#g:method:setPropagationPhase"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setSequenceState]("GI.Gtk.Objects.Gesture#g:method:setSequenceState"), [setState]("GI.Gtk.Objects.Gesture#g:method:setState"), [setTouchOnly]("GI.Gtk.Objects.GestureSingle#g:method:setTouchOnly"), [setWindow]("GI.Gtk.Objects.Gesture#g:method:setWindow").

#if defined(ENABLE_OVERLOADING)
    ResolveGestureLongPressMethod           ,
#endif

-- ** new #method:new#

    gestureLongPressNew                     ,




 -- * Properties


-- ** delayFactor #attr:delayFactor#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    GestureLongPressDelayFactorPropertyInfo ,
#endif
    constructGestureLongPressDelayFactor    ,
#if defined(ENABLE_OVERLOADING)
    gestureLongPressDelayFactor             ,
#endif
    getGestureLongPressDelayFactor          ,
    setGestureLongPressDelayFactor          ,




 -- * Signals


-- ** cancelled #signal:cancelled#

    GestureLongPressCancelledCallback       ,
#if defined(ENABLE_OVERLOADING)
    GestureLongPressCancelledSignalInfo     ,
#endif
    afterGestureLongPressCancelled          ,
    onGestureLongPressCancelled             ,


-- ** pressed #signal:pressed#

    GestureLongPressPressedCallback         ,
#if defined(ENABLE_OVERLOADING)
    GestureLongPressPressedSignalInfo       ,
#endif
    afterGestureLongPressPressed            ,
    onGestureLongPressPressed               ,




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
newtype GestureLongPress = GestureLongPress (SP.ManagedPtr GestureLongPress)
    deriving (Eq)

instance SP.ManagedPtrNewtype GestureLongPress where
    toManagedPtr (GestureLongPress p) = p

foreign import ccall "gtk_gesture_long_press_get_type"
    c_gtk_gesture_long_press_get_type :: IO B.Types.GType

instance B.Types.TypedObject GestureLongPress where
    glibType = c_gtk_gesture_long_press_get_type

instance B.Types.GObject GestureLongPress

-- | Type class for types which can be safely cast to `GestureLongPress`, for instance with `toGestureLongPress`.
class (SP.GObject o, O.IsDescendantOf GestureLongPress o) => IsGestureLongPress o
instance (SP.GObject o, O.IsDescendantOf GestureLongPress o) => IsGestureLongPress o

instance O.HasParentTypes GestureLongPress
type instance O.ParentTypes GestureLongPress = '[Gtk.GestureSingle.GestureSingle, Gtk.Gesture.Gesture, Gtk.EventController.EventController, GObject.Object.Object]

-- | Cast to `GestureLongPress`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toGestureLongPress :: (MIO.MonadIO m, IsGestureLongPress o) => o -> m GestureLongPress
toGestureLongPress = MIO.liftIO . B.ManagedPtr.unsafeCastTo GestureLongPress

-- | Convert 'GestureLongPress' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe GestureLongPress) where
    gvalueGType_ = c_gtk_gesture_long_press_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr GestureLongPress)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr GestureLongPress)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject GestureLongPress ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveGestureLongPressMethod (t :: Symbol) (o :: *) :: * where
    ResolveGestureLongPressMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveGestureLongPressMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveGestureLongPressMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveGestureLongPressMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveGestureLongPressMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveGestureLongPressMethod "group" o = Gtk.Gesture.GestureGroupMethodInfo
    ResolveGestureLongPressMethod "handleEvent" o = Gtk.EventController.EventControllerHandleEventMethodInfo
    ResolveGestureLongPressMethod "handlesSequence" o = Gtk.Gesture.GestureHandlesSequenceMethodInfo
    ResolveGestureLongPressMethod "isActive" o = Gtk.Gesture.GestureIsActiveMethodInfo
    ResolveGestureLongPressMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveGestureLongPressMethod "isGroupedWith" o = Gtk.Gesture.GestureIsGroupedWithMethodInfo
    ResolveGestureLongPressMethod "isRecognized" o = Gtk.Gesture.GestureIsRecognizedMethodInfo
    ResolveGestureLongPressMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveGestureLongPressMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveGestureLongPressMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveGestureLongPressMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveGestureLongPressMethod "reset" o = Gtk.EventController.EventControllerResetMethodInfo
    ResolveGestureLongPressMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveGestureLongPressMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveGestureLongPressMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveGestureLongPressMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveGestureLongPressMethod "ungroup" o = Gtk.Gesture.GestureUngroupMethodInfo
    ResolveGestureLongPressMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveGestureLongPressMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveGestureLongPressMethod "getBoundingBox" o = Gtk.Gesture.GestureGetBoundingBoxMethodInfo
    ResolveGestureLongPressMethod "getBoundingBoxCenter" o = Gtk.Gesture.GestureGetBoundingBoxCenterMethodInfo
    ResolveGestureLongPressMethod "getButton" o = Gtk.GestureSingle.GestureSingleGetButtonMethodInfo
    ResolveGestureLongPressMethod "getCurrentButton" o = Gtk.GestureSingle.GestureSingleGetCurrentButtonMethodInfo
    ResolveGestureLongPressMethod "getCurrentSequence" o = Gtk.GestureSingle.GestureSingleGetCurrentSequenceMethodInfo
    ResolveGestureLongPressMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveGestureLongPressMethod "getDevice" o = Gtk.Gesture.GestureGetDeviceMethodInfo
    ResolveGestureLongPressMethod "getExclusive" o = Gtk.GestureSingle.GestureSingleGetExclusiveMethodInfo
    ResolveGestureLongPressMethod "getGroup" o = Gtk.Gesture.GestureGetGroupMethodInfo
    ResolveGestureLongPressMethod "getLastEvent" o = Gtk.Gesture.GestureGetLastEventMethodInfo
    ResolveGestureLongPressMethod "getLastUpdatedSequence" o = Gtk.Gesture.GestureGetLastUpdatedSequenceMethodInfo
    ResolveGestureLongPressMethod "getPoint" o = Gtk.Gesture.GestureGetPointMethodInfo
    ResolveGestureLongPressMethod "getPropagationPhase" o = Gtk.EventController.EventControllerGetPropagationPhaseMethodInfo
    ResolveGestureLongPressMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveGestureLongPressMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveGestureLongPressMethod "getSequenceState" o = Gtk.Gesture.GestureGetSequenceStateMethodInfo
    ResolveGestureLongPressMethod "getSequences" o = Gtk.Gesture.GestureGetSequencesMethodInfo
    ResolveGestureLongPressMethod "getTouchOnly" o = Gtk.GestureSingle.GestureSingleGetTouchOnlyMethodInfo
    ResolveGestureLongPressMethod "getWidget" o = Gtk.EventController.EventControllerGetWidgetMethodInfo
    ResolveGestureLongPressMethod "getWindow" o = Gtk.Gesture.GestureGetWindowMethodInfo
    ResolveGestureLongPressMethod "setButton" o = Gtk.GestureSingle.GestureSingleSetButtonMethodInfo
    ResolveGestureLongPressMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveGestureLongPressMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveGestureLongPressMethod "setExclusive" o = Gtk.GestureSingle.GestureSingleSetExclusiveMethodInfo
    ResolveGestureLongPressMethod "setPropagationPhase" o = Gtk.EventController.EventControllerSetPropagationPhaseMethodInfo
    ResolveGestureLongPressMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveGestureLongPressMethod "setSequenceState" o = Gtk.Gesture.GestureSetSequenceStateMethodInfo
    ResolveGestureLongPressMethod "setState" o = Gtk.Gesture.GestureSetStateMethodInfo
    ResolveGestureLongPressMethod "setTouchOnly" o = Gtk.GestureSingle.GestureSingleSetTouchOnlyMethodInfo
    ResolveGestureLongPressMethod "setWindow" o = Gtk.Gesture.GestureSetWindowMethodInfo
    ResolveGestureLongPressMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveGestureLongPressMethod t GestureLongPress, O.OverloadedMethod info GestureLongPress p) => OL.IsLabel t (GestureLongPress -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveGestureLongPressMethod t GestureLongPress, O.OverloadedMethod info GestureLongPress p, R.HasField t GestureLongPress p) => R.HasField t GestureLongPress p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveGestureLongPressMethod t GestureLongPress, O.OverloadedMethodInfo info GestureLongPress) => OL.IsLabel t (O.MethodProxy info GestureLongPress) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- signal GestureLongPress::cancelled
-- | This signal is emitted whenever a press moved too far, or was released
-- before [GestureLongPress::pressed]("GI.Gtk.Objects.GestureLongPress#g:signal:pressed") happened.
-- 
-- /Since: 3.14/
type GestureLongPressCancelledCallback =
    IO ()

type C_GestureLongPressCancelledCallback =
    Ptr GestureLongPress ->                 -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_GestureLongPressCancelledCallback`.
foreign import ccall "wrapper"
    mk_GestureLongPressCancelledCallback :: C_GestureLongPressCancelledCallback -> IO (FunPtr C_GestureLongPressCancelledCallback)

wrap_GestureLongPressCancelledCallback :: 
    GObject a => (a -> GestureLongPressCancelledCallback) ->
    C_GestureLongPressCancelledCallback
wrap_GestureLongPressCancelledCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [cancelled](#signal:cancelled) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' gestureLongPress #cancelled callback
-- @
-- 
-- 
onGestureLongPressCancelled :: (IsGestureLongPress a, MonadIO m) => a -> ((?self :: a) => GestureLongPressCancelledCallback) -> m SignalHandlerId
onGestureLongPressCancelled obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_GestureLongPressCancelledCallback wrapped
    wrapped'' <- mk_GestureLongPressCancelledCallback wrapped'
    connectSignalFunPtr obj "cancelled" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [cancelled](#signal:cancelled) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' gestureLongPress #cancelled callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterGestureLongPressCancelled :: (IsGestureLongPress a, MonadIO m) => a -> ((?self :: a) => GestureLongPressCancelledCallback) -> m SignalHandlerId
afterGestureLongPressCancelled obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_GestureLongPressCancelledCallback wrapped
    wrapped'' <- mk_GestureLongPressCancelledCallback wrapped'
    connectSignalFunPtr obj "cancelled" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data GestureLongPressCancelledSignalInfo
instance SignalInfo GestureLongPressCancelledSignalInfo where
    type HaskellCallbackType GestureLongPressCancelledSignalInfo = GestureLongPressCancelledCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_GestureLongPressCancelledCallback cb
        cb'' <- mk_GestureLongPressCancelledCallback cb'
        connectSignalFunPtr obj "cancelled" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.GestureLongPress::cancelled"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-GestureLongPress.html#g:signal:cancelled"})

#endif

-- signal GestureLongPress::pressed
-- | This signal is emitted whenever a press goes unmoved\/unreleased longer than
-- what the GTK+ defaults tell.
-- 
-- /Since: 3.14/
type GestureLongPressPressedCallback =
    Double
    -- ^ /@x@/: the X coordinate where the press happened, relative to the widget allocation
    -> Double
    -- ^ /@y@/: the Y coordinate where the press happened, relative to the widget allocation
    -> IO ()

type C_GestureLongPressPressedCallback =
    Ptr GestureLongPress ->                 -- object
    CDouble ->
    CDouble ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_GestureLongPressPressedCallback`.
foreign import ccall "wrapper"
    mk_GestureLongPressPressedCallback :: C_GestureLongPressPressedCallback -> IO (FunPtr C_GestureLongPressPressedCallback)

wrap_GestureLongPressPressedCallback :: 
    GObject a => (a -> GestureLongPressPressedCallback) ->
    C_GestureLongPressPressedCallback
wrap_GestureLongPressPressedCallback gi'cb gi'selfPtr x y _ = do
    let x' = realToFrac x
    let y' = realToFrac y
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  x' y'


-- | Connect a signal handler for the [pressed](#signal:pressed) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' gestureLongPress #pressed callback
-- @
-- 
-- 
onGestureLongPressPressed :: (IsGestureLongPress a, MonadIO m) => a -> ((?self :: a) => GestureLongPressPressedCallback) -> m SignalHandlerId
onGestureLongPressPressed obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_GestureLongPressPressedCallback wrapped
    wrapped'' <- mk_GestureLongPressPressedCallback wrapped'
    connectSignalFunPtr obj "pressed" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [pressed](#signal:pressed) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' gestureLongPress #pressed callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterGestureLongPressPressed :: (IsGestureLongPress a, MonadIO m) => a -> ((?self :: a) => GestureLongPressPressedCallback) -> m SignalHandlerId
afterGestureLongPressPressed obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_GestureLongPressPressedCallback wrapped
    wrapped'' <- mk_GestureLongPressPressedCallback wrapped'
    connectSignalFunPtr obj "pressed" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data GestureLongPressPressedSignalInfo
instance SignalInfo GestureLongPressPressedSignalInfo where
    type HaskellCallbackType GestureLongPressPressedSignalInfo = GestureLongPressPressedCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_GestureLongPressPressedCallback cb
        cb'' <- mk_GestureLongPressPressedCallback cb'
        connectSignalFunPtr obj "pressed" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.GestureLongPress::pressed"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-GestureLongPress.html#g:signal:pressed"})

#endif

-- VVV Prop "delay-factor"
   -- Type: TBasicType TDouble
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@delay-factor@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' gestureLongPress #delayFactor
-- @
getGestureLongPressDelayFactor :: (MonadIO m, IsGestureLongPress o) => o -> m Double
getGestureLongPressDelayFactor obj = MIO.liftIO $ B.Properties.getObjectPropertyDouble obj "delay-factor"

-- | Set the value of the “@delay-factor@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' gestureLongPress [ #delayFactor 'Data.GI.Base.Attributes.:=' value ]
-- @
setGestureLongPressDelayFactor :: (MonadIO m, IsGestureLongPress o) => o -> Double -> m ()
setGestureLongPressDelayFactor obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyDouble obj "delay-factor" val

-- | Construct a `GValueConstruct` with valid value for the “@delay-factor@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructGestureLongPressDelayFactor :: (IsGestureLongPress o, MIO.MonadIO m) => Double -> m (GValueConstruct o)
constructGestureLongPressDelayFactor val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyDouble "delay-factor" val

#if defined(ENABLE_OVERLOADING)
data GestureLongPressDelayFactorPropertyInfo
instance AttrInfo GestureLongPressDelayFactorPropertyInfo where
    type AttrAllowedOps GestureLongPressDelayFactorPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint GestureLongPressDelayFactorPropertyInfo = IsGestureLongPress
    type AttrSetTypeConstraint GestureLongPressDelayFactorPropertyInfo = (~) Double
    type AttrTransferTypeConstraint GestureLongPressDelayFactorPropertyInfo = (~) Double
    type AttrTransferType GestureLongPressDelayFactorPropertyInfo = Double
    type AttrGetType GestureLongPressDelayFactorPropertyInfo = Double
    type AttrLabel GestureLongPressDelayFactorPropertyInfo = "delay-factor"
    type AttrOrigin GestureLongPressDelayFactorPropertyInfo = GestureLongPress
    attrGet = getGestureLongPressDelayFactor
    attrSet = setGestureLongPressDelayFactor
    attrTransfer _ v = do
        return v
    attrConstruct = constructGestureLongPressDelayFactor
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.GestureLongPress.delayFactor"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-GestureLongPress.html#g:attr:delayFactor"
        })
#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList GestureLongPress
type instance O.AttributeList GestureLongPress = GestureLongPressAttributeList
type GestureLongPressAttributeList = ('[ '("button", Gtk.GestureSingle.GestureSingleButtonPropertyInfo), '("delayFactor", GestureLongPressDelayFactorPropertyInfo), '("exclusive", Gtk.GestureSingle.GestureSingleExclusivePropertyInfo), '("nPoints", Gtk.Gesture.GestureNPointsPropertyInfo), '("propagationPhase", Gtk.EventController.EventControllerPropagationPhasePropertyInfo), '("touchOnly", Gtk.GestureSingle.GestureSingleTouchOnlyPropertyInfo), '("widget", Gtk.EventController.EventControllerWidgetPropertyInfo), '("window", Gtk.Gesture.GestureWindowPropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
gestureLongPressDelayFactor :: AttrLabelProxy "delayFactor"
gestureLongPressDelayFactor = AttrLabelProxy

#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList GestureLongPress = GestureLongPressSignalList
type GestureLongPressSignalList = ('[ '("begin", Gtk.Gesture.GestureBeginSignalInfo), '("cancel", Gtk.Gesture.GestureCancelSignalInfo), '("cancelled", GestureLongPressCancelledSignalInfo), '("end", Gtk.Gesture.GestureEndSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo), '("pressed", GestureLongPressPressedSignalInfo), '("sequenceStateChanged", Gtk.Gesture.GestureSequenceStateChangedSignalInfo), '("update", Gtk.Gesture.GestureUpdateSignalInfo)] :: [(Symbol, *)])

#endif

-- method GestureLongPress::new
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
--               (TInterface Name { namespace = "Gtk" , name = "GestureLongPress" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_gesture_long_press_new" gtk_gesture_long_press_new :: 
    Ptr Gtk.Widget.Widget ->                -- widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    IO (Ptr GestureLongPress)

-- | Returns a newly created t'GI.Gtk.Objects.Gesture.Gesture' that recognizes long presses.
-- 
-- /Since: 3.14/
gestureLongPressNew ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.Widget.IsWidget a) =>
    a
    -- ^ /@widget@/: a t'GI.Gtk.Objects.Widget.Widget'
    -> m GestureLongPress
    -- ^ __Returns:__ a newly created t'GI.Gtk.Objects.GestureLongPress.GestureLongPress'
gestureLongPressNew widget = liftIO $ do
    widget' <- unsafeManagedPtrCastPtr widget
    result <- gtk_gesture_long_press_new widget'
    checkUnexpectedReturnNULL "gestureLongPressNew" result
    result' <- (wrapObject GestureLongPress) result
    touchManagedPtr widget
    return result'

#if defined(ENABLE_OVERLOADING)
#endif


