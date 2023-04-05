{-# LANGUAGE ImplicitParams, RankNTypes, TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- t'GI.Gtk.Objects.GestureMultiPress.GestureMultiPress' is a t'GI.Gtk.Objects.Gesture.Gesture' implementation able to recognize
-- multiple clicks on a nearby zone, which can be listened for through the
-- [GestureMultiPress::pressed]("GI.Gtk.Objects.GestureMultiPress#g:signal:pressed") signal. Whenever time or distance between
-- clicks exceed the GTK+ defaults, [GestureMultiPress::stopped]("GI.Gtk.Objects.GestureMultiPress#g:signal:stopped") is emitted,
-- and the click counter is reset.
-- 
-- Callers may also restrict the area that is considered valid for a >1
-- touch\/button press through 'GI.Gtk.Objects.GestureMultiPress.gestureMultiPressSetArea', so any
-- click happening outside that area is considered to be a first click of
-- its own.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.GestureMultiPress
    ( 

-- * Exported types
    GestureMultiPress(..)                   ,
    IsGestureMultiPress                     ,
    toGestureMultiPress                     ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [group]("GI.Gtk.Objects.Gesture#g:method:group"), [handleEvent]("GI.Gtk.Objects.EventController#g:method:handleEvent"), [handlesSequence]("GI.Gtk.Objects.Gesture#g:method:handlesSequence"), [isActive]("GI.Gtk.Objects.Gesture#g:method:isActive"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [isGroupedWith]("GI.Gtk.Objects.Gesture#g:method:isGroupedWith"), [isRecognized]("GI.Gtk.Objects.Gesture#g:method:isRecognized"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [reset]("GI.Gtk.Objects.EventController#g:method:reset"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [ungroup]("GI.Gtk.Objects.Gesture#g:method:ungroup"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getArea]("GI.Gtk.Objects.GestureMultiPress#g:method:getArea"), [getBoundingBox]("GI.Gtk.Objects.Gesture#g:method:getBoundingBox"), [getBoundingBoxCenter]("GI.Gtk.Objects.Gesture#g:method:getBoundingBoxCenter"), [getButton]("GI.Gtk.Objects.GestureSingle#g:method:getButton"), [getCurrentButton]("GI.Gtk.Objects.GestureSingle#g:method:getCurrentButton"), [getCurrentSequence]("GI.Gtk.Objects.GestureSingle#g:method:getCurrentSequence"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getDevice]("GI.Gtk.Objects.Gesture#g:method:getDevice"), [getExclusive]("GI.Gtk.Objects.GestureSingle#g:method:getExclusive"), [getGroup]("GI.Gtk.Objects.Gesture#g:method:getGroup"), [getLastEvent]("GI.Gtk.Objects.Gesture#g:method:getLastEvent"), [getLastUpdatedSequence]("GI.Gtk.Objects.Gesture#g:method:getLastUpdatedSequence"), [getPoint]("GI.Gtk.Objects.Gesture#g:method:getPoint"), [getPropagationPhase]("GI.Gtk.Objects.EventController#g:method:getPropagationPhase"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getSequenceState]("GI.Gtk.Objects.Gesture#g:method:getSequenceState"), [getSequences]("GI.Gtk.Objects.Gesture#g:method:getSequences"), [getTouchOnly]("GI.Gtk.Objects.GestureSingle#g:method:getTouchOnly"), [getWidget]("GI.Gtk.Objects.EventController#g:method:getWidget"), [getWindow]("GI.Gtk.Objects.Gesture#g:method:getWindow").
-- 
-- ==== Setters
-- [setArea]("GI.Gtk.Objects.GestureMultiPress#g:method:setArea"), [setButton]("GI.Gtk.Objects.GestureSingle#g:method:setButton"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setExclusive]("GI.Gtk.Objects.GestureSingle#g:method:setExclusive"), [setPropagationPhase]("GI.Gtk.Objects.EventController#g:method:setPropagationPhase"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setSequenceState]("GI.Gtk.Objects.Gesture#g:method:setSequenceState"), [setState]("GI.Gtk.Objects.Gesture#g:method:setState"), [setTouchOnly]("GI.Gtk.Objects.GestureSingle#g:method:setTouchOnly"), [setWindow]("GI.Gtk.Objects.Gesture#g:method:setWindow").

#if defined(ENABLE_OVERLOADING)
    ResolveGestureMultiPressMethod          ,
#endif

-- ** getArea #method:getArea#

#if defined(ENABLE_OVERLOADING)
    GestureMultiPressGetAreaMethodInfo      ,
#endif
    gestureMultiPressGetArea                ,


-- ** new #method:new#

    gestureMultiPressNew                    ,


-- ** setArea #method:setArea#

#if defined(ENABLE_OVERLOADING)
    GestureMultiPressSetAreaMethodInfo      ,
#endif
    gestureMultiPressSetArea                ,




 -- * Signals


-- ** pressed #signal:pressed#

    GestureMultiPressPressedCallback        ,
#if defined(ENABLE_OVERLOADING)
    GestureMultiPressPressedSignalInfo      ,
#endif
    afterGestureMultiPressPressed           ,
    onGestureMultiPressPressed              ,


-- ** released #signal:released#

    GestureMultiPressReleasedCallback       ,
#if defined(ENABLE_OVERLOADING)
    GestureMultiPressReleasedSignalInfo     ,
#endif
    afterGestureMultiPressReleased          ,
    onGestureMultiPressReleased             ,


-- ** stopped #signal:stopped#

    GestureMultiPressStoppedCallback        ,
#if defined(ENABLE_OVERLOADING)
    GestureMultiPressStoppedSignalInfo      ,
#endif
    afterGestureMultiPressStopped           ,
    onGestureMultiPressStopped              ,




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
import qualified GI.Gdk.Structs.Rectangle as Gdk.Rectangle
import {-# SOURCE #-} qualified GI.Gtk.Objects.EventController as Gtk.EventController
import {-# SOURCE #-} qualified GI.Gtk.Objects.Gesture as Gtk.Gesture
import {-# SOURCE #-} qualified GI.Gtk.Objects.GestureSingle as Gtk.GestureSingle
import {-# SOURCE #-} qualified GI.Gtk.Objects.Widget as Gtk.Widget

-- | Memory-managed wrapper type.
newtype GestureMultiPress = GestureMultiPress (SP.ManagedPtr GestureMultiPress)
    deriving (Eq)

instance SP.ManagedPtrNewtype GestureMultiPress where
    toManagedPtr (GestureMultiPress p) = p

foreign import ccall "gtk_gesture_multi_press_get_type"
    c_gtk_gesture_multi_press_get_type :: IO B.Types.GType

instance B.Types.TypedObject GestureMultiPress where
    glibType = c_gtk_gesture_multi_press_get_type

instance B.Types.GObject GestureMultiPress

-- | Type class for types which can be safely cast to `GestureMultiPress`, for instance with `toGestureMultiPress`.
class (SP.GObject o, O.IsDescendantOf GestureMultiPress o) => IsGestureMultiPress o
instance (SP.GObject o, O.IsDescendantOf GestureMultiPress o) => IsGestureMultiPress o

instance O.HasParentTypes GestureMultiPress
type instance O.ParentTypes GestureMultiPress = '[Gtk.GestureSingle.GestureSingle, Gtk.Gesture.Gesture, Gtk.EventController.EventController, GObject.Object.Object]

-- | Cast to `GestureMultiPress`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toGestureMultiPress :: (MIO.MonadIO m, IsGestureMultiPress o) => o -> m GestureMultiPress
toGestureMultiPress = MIO.liftIO . B.ManagedPtr.unsafeCastTo GestureMultiPress

-- | Convert 'GestureMultiPress' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe GestureMultiPress) where
    gvalueGType_ = c_gtk_gesture_multi_press_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr GestureMultiPress)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr GestureMultiPress)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject GestureMultiPress ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveGestureMultiPressMethod (t :: Symbol) (o :: *) :: * where
    ResolveGestureMultiPressMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveGestureMultiPressMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveGestureMultiPressMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveGestureMultiPressMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveGestureMultiPressMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveGestureMultiPressMethod "group" o = Gtk.Gesture.GestureGroupMethodInfo
    ResolveGestureMultiPressMethod "handleEvent" o = Gtk.EventController.EventControllerHandleEventMethodInfo
    ResolveGestureMultiPressMethod "handlesSequence" o = Gtk.Gesture.GestureHandlesSequenceMethodInfo
    ResolveGestureMultiPressMethod "isActive" o = Gtk.Gesture.GestureIsActiveMethodInfo
    ResolveGestureMultiPressMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveGestureMultiPressMethod "isGroupedWith" o = Gtk.Gesture.GestureIsGroupedWithMethodInfo
    ResolveGestureMultiPressMethod "isRecognized" o = Gtk.Gesture.GestureIsRecognizedMethodInfo
    ResolveGestureMultiPressMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveGestureMultiPressMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveGestureMultiPressMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveGestureMultiPressMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveGestureMultiPressMethod "reset" o = Gtk.EventController.EventControllerResetMethodInfo
    ResolveGestureMultiPressMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveGestureMultiPressMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveGestureMultiPressMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveGestureMultiPressMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveGestureMultiPressMethod "ungroup" o = Gtk.Gesture.GestureUngroupMethodInfo
    ResolveGestureMultiPressMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveGestureMultiPressMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveGestureMultiPressMethod "getArea" o = GestureMultiPressGetAreaMethodInfo
    ResolveGestureMultiPressMethod "getBoundingBox" o = Gtk.Gesture.GestureGetBoundingBoxMethodInfo
    ResolveGestureMultiPressMethod "getBoundingBoxCenter" o = Gtk.Gesture.GestureGetBoundingBoxCenterMethodInfo
    ResolveGestureMultiPressMethod "getButton" o = Gtk.GestureSingle.GestureSingleGetButtonMethodInfo
    ResolveGestureMultiPressMethod "getCurrentButton" o = Gtk.GestureSingle.GestureSingleGetCurrentButtonMethodInfo
    ResolveGestureMultiPressMethod "getCurrentSequence" o = Gtk.GestureSingle.GestureSingleGetCurrentSequenceMethodInfo
    ResolveGestureMultiPressMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveGestureMultiPressMethod "getDevice" o = Gtk.Gesture.GestureGetDeviceMethodInfo
    ResolveGestureMultiPressMethod "getExclusive" o = Gtk.GestureSingle.GestureSingleGetExclusiveMethodInfo
    ResolveGestureMultiPressMethod "getGroup" o = Gtk.Gesture.GestureGetGroupMethodInfo
    ResolveGestureMultiPressMethod "getLastEvent" o = Gtk.Gesture.GestureGetLastEventMethodInfo
    ResolveGestureMultiPressMethod "getLastUpdatedSequence" o = Gtk.Gesture.GestureGetLastUpdatedSequenceMethodInfo
    ResolveGestureMultiPressMethod "getPoint" o = Gtk.Gesture.GestureGetPointMethodInfo
    ResolveGestureMultiPressMethod "getPropagationPhase" o = Gtk.EventController.EventControllerGetPropagationPhaseMethodInfo
    ResolveGestureMultiPressMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveGestureMultiPressMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveGestureMultiPressMethod "getSequenceState" o = Gtk.Gesture.GestureGetSequenceStateMethodInfo
    ResolveGestureMultiPressMethod "getSequences" o = Gtk.Gesture.GestureGetSequencesMethodInfo
    ResolveGestureMultiPressMethod "getTouchOnly" o = Gtk.GestureSingle.GestureSingleGetTouchOnlyMethodInfo
    ResolveGestureMultiPressMethod "getWidget" o = Gtk.EventController.EventControllerGetWidgetMethodInfo
    ResolveGestureMultiPressMethod "getWindow" o = Gtk.Gesture.GestureGetWindowMethodInfo
    ResolveGestureMultiPressMethod "setArea" o = GestureMultiPressSetAreaMethodInfo
    ResolveGestureMultiPressMethod "setButton" o = Gtk.GestureSingle.GestureSingleSetButtonMethodInfo
    ResolveGestureMultiPressMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveGestureMultiPressMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveGestureMultiPressMethod "setExclusive" o = Gtk.GestureSingle.GestureSingleSetExclusiveMethodInfo
    ResolveGestureMultiPressMethod "setPropagationPhase" o = Gtk.EventController.EventControllerSetPropagationPhaseMethodInfo
    ResolveGestureMultiPressMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveGestureMultiPressMethod "setSequenceState" o = Gtk.Gesture.GestureSetSequenceStateMethodInfo
    ResolveGestureMultiPressMethod "setState" o = Gtk.Gesture.GestureSetStateMethodInfo
    ResolveGestureMultiPressMethod "setTouchOnly" o = Gtk.GestureSingle.GestureSingleSetTouchOnlyMethodInfo
    ResolveGestureMultiPressMethod "setWindow" o = Gtk.Gesture.GestureSetWindowMethodInfo
    ResolveGestureMultiPressMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveGestureMultiPressMethod t GestureMultiPress, O.OverloadedMethod info GestureMultiPress p) => OL.IsLabel t (GestureMultiPress -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveGestureMultiPressMethod t GestureMultiPress, O.OverloadedMethod info GestureMultiPress p, R.HasField t GestureMultiPress p) => R.HasField t GestureMultiPress p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveGestureMultiPressMethod t GestureMultiPress, O.OverloadedMethodInfo info GestureMultiPress) => OL.IsLabel t (O.MethodProxy info GestureMultiPress) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- signal GestureMultiPress::pressed
-- | This signal is emitted whenever a button or touch press happens.
-- 
-- /Since: 3.14/
type GestureMultiPressPressedCallback =
    Int32
    -- ^ /@nPress@/: how many touch\/button presses happened with this one
    -> Double
    -- ^ /@x@/: The X coordinate, in widget allocation coordinates
    -> Double
    -- ^ /@y@/: The Y coordinate, in widget allocation coordinates
    -> IO ()

type C_GestureMultiPressPressedCallback =
    Ptr GestureMultiPress ->                -- object
    Int32 ->
    CDouble ->
    CDouble ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_GestureMultiPressPressedCallback`.
foreign import ccall "wrapper"
    mk_GestureMultiPressPressedCallback :: C_GestureMultiPressPressedCallback -> IO (FunPtr C_GestureMultiPressPressedCallback)

wrap_GestureMultiPressPressedCallback :: 
    GObject a => (a -> GestureMultiPressPressedCallback) ->
    C_GestureMultiPressPressedCallback
wrap_GestureMultiPressPressedCallback gi'cb gi'selfPtr nPress x y _ = do
    let x' = realToFrac x
    let y' = realToFrac y
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  nPress x' y'


-- | Connect a signal handler for the [pressed](#signal:pressed) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' gestureMultiPress #pressed callback
-- @
-- 
-- 
onGestureMultiPressPressed :: (IsGestureMultiPress a, MonadIO m) => a -> ((?self :: a) => GestureMultiPressPressedCallback) -> m SignalHandlerId
onGestureMultiPressPressed obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_GestureMultiPressPressedCallback wrapped
    wrapped'' <- mk_GestureMultiPressPressedCallback wrapped'
    connectSignalFunPtr obj "pressed" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [pressed](#signal:pressed) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' gestureMultiPress #pressed callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterGestureMultiPressPressed :: (IsGestureMultiPress a, MonadIO m) => a -> ((?self :: a) => GestureMultiPressPressedCallback) -> m SignalHandlerId
afterGestureMultiPressPressed obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_GestureMultiPressPressedCallback wrapped
    wrapped'' <- mk_GestureMultiPressPressedCallback wrapped'
    connectSignalFunPtr obj "pressed" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data GestureMultiPressPressedSignalInfo
instance SignalInfo GestureMultiPressPressedSignalInfo where
    type HaskellCallbackType GestureMultiPressPressedSignalInfo = GestureMultiPressPressedCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_GestureMultiPressPressedCallback cb
        cb'' <- mk_GestureMultiPressPressedCallback cb'
        connectSignalFunPtr obj "pressed" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.GestureMultiPress::pressed"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-GestureMultiPress.html#g:signal:pressed"})

#endif

-- signal GestureMultiPress::released
-- | This signal is emitted when a button or touch is released. /@nPress@/
-- will report the number of press that is paired to this event, note
-- that [GestureMultiPress::stopped]("GI.Gtk.Objects.GestureMultiPress#g:signal:stopped") may have been emitted between the
-- press and its release, /@nPress@/ will only start over at the next press.
-- 
-- /Since: 3.14/
type GestureMultiPressReleasedCallback =
    Int32
    -- ^ /@nPress@/: number of press that is paired with this release
    -> Double
    -- ^ /@x@/: The X coordinate, in widget allocation coordinates
    -> Double
    -- ^ /@y@/: The Y coordinate, in widget allocation coordinates
    -> IO ()

type C_GestureMultiPressReleasedCallback =
    Ptr GestureMultiPress ->                -- object
    Int32 ->
    CDouble ->
    CDouble ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_GestureMultiPressReleasedCallback`.
foreign import ccall "wrapper"
    mk_GestureMultiPressReleasedCallback :: C_GestureMultiPressReleasedCallback -> IO (FunPtr C_GestureMultiPressReleasedCallback)

wrap_GestureMultiPressReleasedCallback :: 
    GObject a => (a -> GestureMultiPressReleasedCallback) ->
    C_GestureMultiPressReleasedCallback
wrap_GestureMultiPressReleasedCallback gi'cb gi'selfPtr nPress x y _ = do
    let x' = realToFrac x
    let y' = realToFrac y
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  nPress x' y'


-- | Connect a signal handler for the [released](#signal:released) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' gestureMultiPress #released callback
-- @
-- 
-- 
onGestureMultiPressReleased :: (IsGestureMultiPress a, MonadIO m) => a -> ((?self :: a) => GestureMultiPressReleasedCallback) -> m SignalHandlerId
onGestureMultiPressReleased obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_GestureMultiPressReleasedCallback wrapped
    wrapped'' <- mk_GestureMultiPressReleasedCallback wrapped'
    connectSignalFunPtr obj "released" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [released](#signal:released) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' gestureMultiPress #released callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterGestureMultiPressReleased :: (IsGestureMultiPress a, MonadIO m) => a -> ((?self :: a) => GestureMultiPressReleasedCallback) -> m SignalHandlerId
afterGestureMultiPressReleased obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_GestureMultiPressReleasedCallback wrapped
    wrapped'' <- mk_GestureMultiPressReleasedCallback wrapped'
    connectSignalFunPtr obj "released" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data GestureMultiPressReleasedSignalInfo
instance SignalInfo GestureMultiPressReleasedSignalInfo where
    type HaskellCallbackType GestureMultiPressReleasedSignalInfo = GestureMultiPressReleasedCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_GestureMultiPressReleasedCallback cb
        cb'' <- mk_GestureMultiPressReleasedCallback cb'
        connectSignalFunPtr obj "released" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.GestureMultiPress::released"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-GestureMultiPress.html#g:signal:released"})

#endif

-- signal GestureMultiPress::stopped
-- | This signal is emitted whenever any time\/distance threshold has
-- been exceeded.
-- 
-- /Since: 3.14/
type GestureMultiPressStoppedCallback =
    IO ()

type C_GestureMultiPressStoppedCallback =
    Ptr GestureMultiPress ->                -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_GestureMultiPressStoppedCallback`.
foreign import ccall "wrapper"
    mk_GestureMultiPressStoppedCallback :: C_GestureMultiPressStoppedCallback -> IO (FunPtr C_GestureMultiPressStoppedCallback)

wrap_GestureMultiPressStoppedCallback :: 
    GObject a => (a -> GestureMultiPressStoppedCallback) ->
    C_GestureMultiPressStoppedCallback
wrap_GestureMultiPressStoppedCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [stopped](#signal:stopped) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' gestureMultiPress #stopped callback
-- @
-- 
-- 
onGestureMultiPressStopped :: (IsGestureMultiPress a, MonadIO m) => a -> ((?self :: a) => GestureMultiPressStoppedCallback) -> m SignalHandlerId
onGestureMultiPressStopped obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_GestureMultiPressStoppedCallback wrapped
    wrapped'' <- mk_GestureMultiPressStoppedCallback wrapped'
    connectSignalFunPtr obj "stopped" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [stopped](#signal:stopped) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' gestureMultiPress #stopped callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterGestureMultiPressStopped :: (IsGestureMultiPress a, MonadIO m) => a -> ((?self :: a) => GestureMultiPressStoppedCallback) -> m SignalHandlerId
afterGestureMultiPressStopped obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_GestureMultiPressStoppedCallback wrapped
    wrapped'' <- mk_GestureMultiPressStoppedCallback wrapped'
    connectSignalFunPtr obj "stopped" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data GestureMultiPressStoppedSignalInfo
instance SignalInfo GestureMultiPressStoppedSignalInfo where
    type HaskellCallbackType GestureMultiPressStoppedSignalInfo = GestureMultiPressStoppedCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_GestureMultiPressStoppedCallback cb
        cb'' <- mk_GestureMultiPressStoppedCallback cb'
        connectSignalFunPtr obj "stopped" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.GestureMultiPress::stopped"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-GestureMultiPress.html#g:signal:stopped"})

#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList GestureMultiPress
type instance O.AttributeList GestureMultiPress = GestureMultiPressAttributeList
type GestureMultiPressAttributeList = ('[ '("button", Gtk.GestureSingle.GestureSingleButtonPropertyInfo), '("exclusive", Gtk.GestureSingle.GestureSingleExclusivePropertyInfo), '("nPoints", Gtk.Gesture.GestureNPointsPropertyInfo), '("propagationPhase", Gtk.EventController.EventControllerPropagationPhasePropertyInfo), '("touchOnly", Gtk.GestureSingle.GestureSingleTouchOnlyPropertyInfo), '("widget", Gtk.EventController.EventControllerWidgetPropertyInfo), '("window", Gtk.Gesture.GestureWindowPropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList GestureMultiPress = GestureMultiPressSignalList
type GestureMultiPressSignalList = ('[ '("begin", Gtk.Gesture.GestureBeginSignalInfo), '("cancel", Gtk.Gesture.GestureCancelSignalInfo), '("end", Gtk.Gesture.GestureEndSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo), '("pressed", GestureMultiPressPressedSignalInfo), '("released", GestureMultiPressReleasedSignalInfo), '("sequenceStateChanged", Gtk.Gesture.GestureSequenceStateChangedSignalInfo), '("stopped", GestureMultiPressStoppedSignalInfo), '("update", Gtk.Gesture.GestureUpdateSignalInfo)] :: [(Symbol, *)])

#endif

-- method GestureMultiPress::new
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
--                  Name { namespace = "Gtk" , name = "GestureMultiPress" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_gesture_multi_press_new" gtk_gesture_multi_press_new :: 
    Ptr Gtk.Widget.Widget ->                -- widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    IO (Ptr GestureMultiPress)

-- | Returns a newly created t'GI.Gtk.Objects.Gesture.Gesture' that recognizes single and multiple
-- presses.
-- 
-- /Since: 3.14/
gestureMultiPressNew ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.Widget.IsWidget a) =>
    a
    -- ^ /@widget@/: a t'GI.Gtk.Objects.Widget.Widget'
    -> m GestureMultiPress
    -- ^ __Returns:__ a newly created t'GI.Gtk.Objects.GestureMultiPress.GestureMultiPress'
gestureMultiPressNew widget = liftIO $ do
    widget' <- unsafeManagedPtrCastPtr widget
    result <- gtk_gesture_multi_press_new widget'
    checkUnexpectedReturnNULL "gestureMultiPressNew" result
    result' <- (wrapObject GestureMultiPress) result
    touchManagedPtr widget
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method GestureMultiPress::get_area
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "gesture"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "GestureMultiPress" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkGestureMultiPress"
--                 , sinceVersion = Nothing
--                 }
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
--                 { rawDocText = Just "return location for the press area"
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

foreign import ccall "gtk_gesture_multi_press_get_area" gtk_gesture_multi_press_get_area :: 
    Ptr GestureMultiPress ->                -- gesture : TInterface (Name {namespace = "Gtk", name = "GestureMultiPress"})
    Ptr Gdk.Rectangle.Rectangle ->          -- rect : TInterface (Name {namespace = "Gdk", name = "Rectangle"})
    IO CInt

-- | If an area was set through 'GI.Gtk.Objects.GestureMultiPress.gestureMultiPressSetArea',
-- this function will return 'P.True' and fill in /@rect@/ with the
-- press area. See 'GI.Gtk.Objects.GestureMultiPress.gestureMultiPressSetArea' for more
-- details on what the press area represents.
-- 
-- /Since: 3.14/
gestureMultiPressGetArea ::
    (B.CallStack.HasCallStack, MonadIO m, IsGestureMultiPress a) =>
    a
    -- ^ /@gesture@/: a t'GI.Gtk.Objects.GestureMultiPress.GestureMultiPress'
    -> m ((Bool, Gdk.Rectangle.Rectangle))
    -- ^ __Returns:__ 'P.True' if /@rect@/ was filled with the press area
gestureMultiPressGetArea gesture = liftIO $ do
    gesture' <- unsafeManagedPtrCastPtr gesture
    rect <- SP.callocBoxedBytes 16 :: IO (Ptr Gdk.Rectangle.Rectangle)
    result <- gtk_gesture_multi_press_get_area gesture' rect
    let result' = (/= 0) result
    rect' <- (wrapBoxed Gdk.Rectangle.Rectangle) rect
    touchManagedPtr gesture
    return (result', rect')

#if defined(ENABLE_OVERLOADING)
data GestureMultiPressGetAreaMethodInfo
instance (signature ~ (m ((Bool, Gdk.Rectangle.Rectangle))), MonadIO m, IsGestureMultiPress a) => O.OverloadedMethod GestureMultiPressGetAreaMethodInfo a signature where
    overloadedMethod = gestureMultiPressGetArea

instance O.OverloadedMethodInfo GestureMultiPressGetAreaMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.GestureMultiPress.gestureMultiPressGetArea",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-GestureMultiPress.html#v:gestureMultiPressGetArea"
        })


#endif

-- method GestureMultiPress::set_area
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "gesture"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "GestureMultiPress" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkGestureMultiPress"
--                 , sinceVersion = Nothing
--                 }
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
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "rectangle to receive coordinates on"
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

foreign import ccall "gtk_gesture_multi_press_set_area" gtk_gesture_multi_press_set_area :: 
    Ptr GestureMultiPress ->                -- gesture : TInterface (Name {namespace = "Gtk", name = "GestureMultiPress"})
    Ptr Gdk.Rectangle.Rectangle ->          -- rect : TInterface (Name {namespace = "Gdk", name = "Rectangle"})
    IO ()

-- | If /@rect@/ is non-'P.Nothing', the press area will be checked to be
-- confined within the rectangle, otherwise the button count
-- will be reset so the press is seen as being the first one.
-- If /@rect@/ is 'P.Nothing', the area will be reset to an unrestricted
-- state.
-- 
-- Note: The rectangle is only used to determine whether any
-- non-first click falls within the expected area. This is not
-- akin to an input shape.
-- 
-- /Since: 3.14/
gestureMultiPressSetArea ::
    (B.CallStack.HasCallStack, MonadIO m, IsGestureMultiPress a) =>
    a
    -- ^ /@gesture@/: a t'GI.Gtk.Objects.GestureMultiPress.GestureMultiPress'
    -> Maybe (Gdk.Rectangle.Rectangle)
    -- ^ /@rect@/: rectangle to receive coordinates on
    -> m ()
gestureMultiPressSetArea gesture rect = liftIO $ do
    gesture' <- unsafeManagedPtrCastPtr gesture
    maybeRect <- case rect of
        Nothing -> return nullPtr
        Just jRect -> do
            jRect' <- unsafeManagedPtrGetPtr jRect
            return jRect'
    gtk_gesture_multi_press_set_area gesture' maybeRect
    touchManagedPtr gesture
    whenJust rect touchManagedPtr
    return ()

#if defined(ENABLE_OVERLOADING)
data GestureMultiPressSetAreaMethodInfo
instance (signature ~ (Maybe (Gdk.Rectangle.Rectangle) -> m ()), MonadIO m, IsGestureMultiPress a) => O.OverloadedMethod GestureMultiPressSetAreaMethodInfo a signature where
    overloadedMethod = gestureMultiPressSetArea

instance O.OverloadedMethodInfo GestureMultiPressSetAreaMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.GestureMultiPress.gestureMultiPressSetArea",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-GestureMultiPress.html#v:gestureMultiPressSetArea"
        })


#endif


