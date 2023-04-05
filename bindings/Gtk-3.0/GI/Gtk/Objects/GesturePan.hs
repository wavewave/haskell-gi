{-# LANGUAGE ImplicitParams, RankNTypes, TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- t'GI.Gtk.Objects.GesturePan.GesturePan' is a t'GI.Gtk.Objects.Gesture.Gesture' implementation able to recognize
-- pan gestures, those are drags that are locked to happen along one
-- axis. The axis that a t'GI.Gtk.Objects.GesturePan.GesturePan' handles is defined at
-- construct time, and can be changed through
-- 'GI.Gtk.Objects.GesturePan.gesturePanSetOrientation'.
-- 
-- When the gesture starts to be recognized, t'GI.Gtk.Objects.GesturePan.GesturePan' will
-- attempt to determine as early as possible whether the sequence
-- is moving in the expected direction, and denying the sequence if
-- this does not happen.
-- 
-- Once a panning gesture along the expected axis is recognized,
-- the [GesturePan::pan]("GI.Gtk.Objects.GesturePan#g:signal:pan") signal will be emitted as input events
-- are received, containing the offset in the given axis.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.GesturePan
    ( 

-- * Exported types
    GesturePan(..)                          ,
    IsGesturePan                            ,
    toGesturePan                            ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [group]("GI.Gtk.Objects.Gesture#g:method:group"), [handleEvent]("GI.Gtk.Objects.EventController#g:method:handleEvent"), [handlesSequence]("GI.Gtk.Objects.Gesture#g:method:handlesSequence"), [isActive]("GI.Gtk.Objects.Gesture#g:method:isActive"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [isGroupedWith]("GI.Gtk.Objects.Gesture#g:method:isGroupedWith"), [isRecognized]("GI.Gtk.Objects.Gesture#g:method:isRecognized"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [reset]("GI.Gtk.Objects.EventController#g:method:reset"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [ungroup]("GI.Gtk.Objects.Gesture#g:method:ungroup"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getBoundingBox]("GI.Gtk.Objects.Gesture#g:method:getBoundingBox"), [getBoundingBoxCenter]("GI.Gtk.Objects.Gesture#g:method:getBoundingBoxCenter"), [getButton]("GI.Gtk.Objects.GestureSingle#g:method:getButton"), [getCurrentButton]("GI.Gtk.Objects.GestureSingle#g:method:getCurrentButton"), [getCurrentSequence]("GI.Gtk.Objects.GestureSingle#g:method:getCurrentSequence"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getDevice]("GI.Gtk.Objects.Gesture#g:method:getDevice"), [getExclusive]("GI.Gtk.Objects.GestureSingle#g:method:getExclusive"), [getGroup]("GI.Gtk.Objects.Gesture#g:method:getGroup"), [getLastEvent]("GI.Gtk.Objects.Gesture#g:method:getLastEvent"), [getLastUpdatedSequence]("GI.Gtk.Objects.Gesture#g:method:getLastUpdatedSequence"), [getOffset]("GI.Gtk.Objects.GestureDrag#g:method:getOffset"), [getOrientation]("GI.Gtk.Objects.GesturePan#g:method:getOrientation"), [getPoint]("GI.Gtk.Objects.Gesture#g:method:getPoint"), [getPropagationPhase]("GI.Gtk.Objects.EventController#g:method:getPropagationPhase"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getSequenceState]("GI.Gtk.Objects.Gesture#g:method:getSequenceState"), [getSequences]("GI.Gtk.Objects.Gesture#g:method:getSequences"), [getStartPoint]("GI.Gtk.Objects.GestureDrag#g:method:getStartPoint"), [getTouchOnly]("GI.Gtk.Objects.GestureSingle#g:method:getTouchOnly"), [getWidget]("GI.Gtk.Objects.EventController#g:method:getWidget"), [getWindow]("GI.Gtk.Objects.Gesture#g:method:getWindow").
-- 
-- ==== Setters
-- [setButton]("GI.Gtk.Objects.GestureSingle#g:method:setButton"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setExclusive]("GI.Gtk.Objects.GestureSingle#g:method:setExclusive"), [setOrientation]("GI.Gtk.Objects.GesturePan#g:method:setOrientation"), [setPropagationPhase]("GI.Gtk.Objects.EventController#g:method:setPropagationPhase"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setSequenceState]("GI.Gtk.Objects.Gesture#g:method:setSequenceState"), [setState]("GI.Gtk.Objects.Gesture#g:method:setState"), [setTouchOnly]("GI.Gtk.Objects.GestureSingle#g:method:setTouchOnly"), [setWindow]("GI.Gtk.Objects.Gesture#g:method:setWindow").

#if defined(ENABLE_OVERLOADING)
    ResolveGesturePanMethod                 ,
#endif

-- ** getOrientation #method:getOrientation#

#if defined(ENABLE_OVERLOADING)
    GesturePanGetOrientationMethodInfo      ,
#endif
    gesturePanGetOrientation                ,


-- ** new #method:new#

    gesturePanNew                           ,


-- ** setOrientation #method:setOrientation#

#if defined(ENABLE_OVERLOADING)
    GesturePanSetOrientationMethodInfo      ,
#endif
    gesturePanSetOrientation                ,




 -- * Properties


-- ** orientation #attr:orientation#
-- | The expected orientation of pan gestures.
-- 
-- /Since: 3.14/

#if defined(ENABLE_OVERLOADING)
    GesturePanOrientationPropertyInfo       ,
#endif
    constructGesturePanOrientation          ,
#if defined(ENABLE_OVERLOADING)
    gesturePanOrientation                   ,
#endif
    getGesturePanOrientation                ,
    setGesturePanOrientation                ,




 -- * Signals


-- ** pan #signal:pan#

    GesturePanPanCallback                   ,
#if defined(ENABLE_OVERLOADING)
    GesturePanPanSignalInfo                 ,
#endif
    afterGesturePanPan                      ,
    onGesturePanPan                         ,




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
import {-# SOURCE #-} qualified GI.Gtk.Enums as Gtk.Enums
import {-# SOURCE #-} qualified GI.Gtk.Objects.EventController as Gtk.EventController
import {-# SOURCE #-} qualified GI.Gtk.Objects.Gesture as Gtk.Gesture
import {-# SOURCE #-} qualified GI.Gtk.Objects.GestureDrag as Gtk.GestureDrag
import {-# SOURCE #-} qualified GI.Gtk.Objects.GestureSingle as Gtk.GestureSingle
import {-# SOURCE #-} qualified GI.Gtk.Objects.Widget as Gtk.Widget

-- | Memory-managed wrapper type.
newtype GesturePan = GesturePan (SP.ManagedPtr GesturePan)
    deriving (Eq)

instance SP.ManagedPtrNewtype GesturePan where
    toManagedPtr (GesturePan p) = p

foreign import ccall "gtk_gesture_pan_get_type"
    c_gtk_gesture_pan_get_type :: IO B.Types.GType

instance B.Types.TypedObject GesturePan where
    glibType = c_gtk_gesture_pan_get_type

instance B.Types.GObject GesturePan

-- | Type class for types which can be safely cast to `GesturePan`, for instance with `toGesturePan`.
class (SP.GObject o, O.IsDescendantOf GesturePan o) => IsGesturePan o
instance (SP.GObject o, O.IsDescendantOf GesturePan o) => IsGesturePan o

instance O.HasParentTypes GesturePan
type instance O.ParentTypes GesturePan = '[Gtk.GestureDrag.GestureDrag, Gtk.GestureSingle.GestureSingle, Gtk.Gesture.Gesture, Gtk.EventController.EventController, GObject.Object.Object]

-- | Cast to `GesturePan`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toGesturePan :: (MIO.MonadIO m, IsGesturePan o) => o -> m GesturePan
toGesturePan = MIO.liftIO . B.ManagedPtr.unsafeCastTo GesturePan

-- | Convert 'GesturePan' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe GesturePan) where
    gvalueGType_ = c_gtk_gesture_pan_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr GesturePan)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr GesturePan)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject GesturePan ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveGesturePanMethod (t :: Symbol) (o :: *) :: * where
    ResolveGesturePanMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveGesturePanMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveGesturePanMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveGesturePanMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveGesturePanMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveGesturePanMethod "group" o = Gtk.Gesture.GestureGroupMethodInfo
    ResolveGesturePanMethod "handleEvent" o = Gtk.EventController.EventControllerHandleEventMethodInfo
    ResolveGesturePanMethod "handlesSequence" o = Gtk.Gesture.GestureHandlesSequenceMethodInfo
    ResolveGesturePanMethod "isActive" o = Gtk.Gesture.GestureIsActiveMethodInfo
    ResolveGesturePanMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveGesturePanMethod "isGroupedWith" o = Gtk.Gesture.GestureIsGroupedWithMethodInfo
    ResolveGesturePanMethod "isRecognized" o = Gtk.Gesture.GestureIsRecognizedMethodInfo
    ResolveGesturePanMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveGesturePanMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveGesturePanMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveGesturePanMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveGesturePanMethod "reset" o = Gtk.EventController.EventControllerResetMethodInfo
    ResolveGesturePanMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveGesturePanMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveGesturePanMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveGesturePanMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveGesturePanMethod "ungroup" o = Gtk.Gesture.GestureUngroupMethodInfo
    ResolveGesturePanMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveGesturePanMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveGesturePanMethod "getBoundingBox" o = Gtk.Gesture.GestureGetBoundingBoxMethodInfo
    ResolveGesturePanMethod "getBoundingBoxCenter" o = Gtk.Gesture.GestureGetBoundingBoxCenterMethodInfo
    ResolveGesturePanMethod "getButton" o = Gtk.GestureSingle.GestureSingleGetButtonMethodInfo
    ResolveGesturePanMethod "getCurrentButton" o = Gtk.GestureSingle.GestureSingleGetCurrentButtonMethodInfo
    ResolveGesturePanMethod "getCurrentSequence" o = Gtk.GestureSingle.GestureSingleGetCurrentSequenceMethodInfo
    ResolveGesturePanMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveGesturePanMethod "getDevice" o = Gtk.Gesture.GestureGetDeviceMethodInfo
    ResolveGesturePanMethod "getExclusive" o = Gtk.GestureSingle.GestureSingleGetExclusiveMethodInfo
    ResolveGesturePanMethod "getGroup" o = Gtk.Gesture.GestureGetGroupMethodInfo
    ResolveGesturePanMethod "getLastEvent" o = Gtk.Gesture.GestureGetLastEventMethodInfo
    ResolveGesturePanMethod "getLastUpdatedSequence" o = Gtk.Gesture.GestureGetLastUpdatedSequenceMethodInfo
    ResolveGesturePanMethod "getOffset" o = Gtk.GestureDrag.GestureDragGetOffsetMethodInfo
    ResolveGesturePanMethod "getOrientation" o = GesturePanGetOrientationMethodInfo
    ResolveGesturePanMethod "getPoint" o = Gtk.Gesture.GestureGetPointMethodInfo
    ResolveGesturePanMethod "getPropagationPhase" o = Gtk.EventController.EventControllerGetPropagationPhaseMethodInfo
    ResolveGesturePanMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveGesturePanMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveGesturePanMethod "getSequenceState" o = Gtk.Gesture.GestureGetSequenceStateMethodInfo
    ResolveGesturePanMethod "getSequences" o = Gtk.Gesture.GestureGetSequencesMethodInfo
    ResolveGesturePanMethod "getStartPoint" o = Gtk.GestureDrag.GestureDragGetStartPointMethodInfo
    ResolveGesturePanMethod "getTouchOnly" o = Gtk.GestureSingle.GestureSingleGetTouchOnlyMethodInfo
    ResolveGesturePanMethod "getWidget" o = Gtk.EventController.EventControllerGetWidgetMethodInfo
    ResolveGesturePanMethod "getWindow" o = Gtk.Gesture.GestureGetWindowMethodInfo
    ResolveGesturePanMethod "setButton" o = Gtk.GestureSingle.GestureSingleSetButtonMethodInfo
    ResolveGesturePanMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveGesturePanMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveGesturePanMethod "setExclusive" o = Gtk.GestureSingle.GestureSingleSetExclusiveMethodInfo
    ResolveGesturePanMethod "setOrientation" o = GesturePanSetOrientationMethodInfo
    ResolveGesturePanMethod "setPropagationPhase" o = Gtk.EventController.EventControllerSetPropagationPhaseMethodInfo
    ResolveGesturePanMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveGesturePanMethod "setSequenceState" o = Gtk.Gesture.GestureSetSequenceStateMethodInfo
    ResolveGesturePanMethod "setState" o = Gtk.Gesture.GestureSetStateMethodInfo
    ResolveGesturePanMethod "setTouchOnly" o = Gtk.GestureSingle.GestureSingleSetTouchOnlyMethodInfo
    ResolveGesturePanMethod "setWindow" o = Gtk.Gesture.GestureSetWindowMethodInfo
    ResolveGesturePanMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveGesturePanMethod t GesturePan, O.OverloadedMethod info GesturePan p) => OL.IsLabel t (GesturePan -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveGesturePanMethod t GesturePan, O.OverloadedMethod info GesturePan p, R.HasField t GesturePan p) => R.HasField t GesturePan p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveGesturePanMethod t GesturePan, O.OverloadedMethodInfo info GesturePan) => OL.IsLabel t (O.MethodProxy info GesturePan) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- signal GesturePan::pan
-- | This signal is emitted once a panning gesture along the
-- expected axis is detected.
-- 
-- /Since: 3.14/
type GesturePanPanCallback =
    Gtk.Enums.PanDirection
    -- ^ /@direction@/: current direction of the pan gesture
    -> Double
    -- ^ /@offset@/: Offset along the gesture orientation
    -> IO ()

type C_GesturePanPanCallback =
    Ptr GesturePan ->                       -- object
    CUInt ->
    CDouble ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_GesturePanPanCallback`.
foreign import ccall "wrapper"
    mk_GesturePanPanCallback :: C_GesturePanPanCallback -> IO (FunPtr C_GesturePanPanCallback)

wrap_GesturePanPanCallback :: 
    GObject a => (a -> GesturePanPanCallback) ->
    C_GesturePanPanCallback
wrap_GesturePanPanCallback gi'cb gi'selfPtr direction offset _ = do
    let direction' = (toEnum . fromIntegral) direction
    let offset' = realToFrac offset
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  direction' offset'


-- | Connect a signal handler for the [pan](#signal:pan) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' gesturePan #pan callback
-- @
-- 
-- 
onGesturePanPan :: (IsGesturePan a, MonadIO m) => a -> ((?self :: a) => GesturePanPanCallback) -> m SignalHandlerId
onGesturePanPan obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_GesturePanPanCallback wrapped
    wrapped'' <- mk_GesturePanPanCallback wrapped'
    connectSignalFunPtr obj "pan" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [pan](#signal:pan) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' gesturePan #pan callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterGesturePanPan :: (IsGesturePan a, MonadIO m) => a -> ((?self :: a) => GesturePanPanCallback) -> m SignalHandlerId
afterGesturePanPan obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_GesturePanPanCallback wrapped
    wrapped'' <- mk_GesturePanPanCallback wrapped'
    connectSignalFunPtr obj "pan" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data GesturePanPanSignalInfo
instance SignalInfo GesturePanPanSignalInfo where
    type HaskellCallbackType GesturePanPanSignalInfo = GesturePanPanCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_GesturePanPanCallback cb
        cb'' <- mk_GesturePanPanCallback cb'
        connectSignalFunPtr obj "pan" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.GesturePan::pan"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-GesturePan.html#g:signal:pan"})

#endif

-- VVV Prop "orientation"
   -- Type: TInterface (Name {namespace = "Gtk", name = "Orientation"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@orientation@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' gesturePan #orientation
-- @
getGesturePanOrientation :: (MonadIO m, IsGesturePan o) => o -> m Gtk.Enums.Orientation
getGesturePanOrientation obj = MIO.liftIO $ B.Properties.getObjectPropertyEnum obj "orientation"

-- | Set the value of the “@orientation@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' gesturePan [ #orientation 'Data.GI.Base.Attributes.:=' value ]
-- @
setGesturePanOrientation :: (MonadIO m, IsGesturePan o) => o -> Gtk.Enums.Orientation -> m ()
setGesturePanOrientation obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyEnum obj "orientation" val

-- | Construct a `GValueConstruct` with valid value for the “@orientation@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructGesturePanOrientation :: (IsGesturePan o, MIO.MonadIO m) => Gtk.Enums.Orientation -> m (GValueConstruct o)
constructGesturePanOrientation val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyEnum "orientation" val

#if defined(ENABLE_OVERLOADING)
data GesturePanOrientationPropertyInfo
instance AttrInfo GesturePanOrientationPropertyInfo where
    type AttrAllowedOps GesturePanOrientationPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint GesturePanOrientationPropertyInfo = IsGesturePan
    type AttrSetTypeConstraint GesturePanOrientationPropertyInfo = (~) Gtk.Enums.Orientation
    type AttrTransferTypeConstraint GesturePanOrientationPropertyInfo = (~) Gtk.Enums.Orientation
    type AttrTransferType GesturePanOrientationPropertyInfo = Gtk.Enums.Orientation
    type AttrGetType GesturePanOrientationPropertyInfo = Gtk.Enums.Orientation
    type AttrLabel GesturePanOrientationPropertyInfo = "orientation"
    type AttrOrigin GesturePanOrientationPropertyInfo = GesturePan
    attrGet = getGesturePanOrientation
    attrSet = setGesturePanOrientation
    attrTransfer _ v = do
        return v
    attrConstruct = constructGesturePanOrientation
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.GesturePan.orientation"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-GesturePan.html#g:attr:orientation"
        })
#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList GesturePan
type instance O.AttributeList GesturePan = GesturePanAttributeList
type GesturePanAttributeList = ('[ '("button", Gtk.GestureSingle.GestureSingleButtonPropertyInfo), '("exclusive", Gtk.GestureSingle.GestureSingleExclusivePropertyInfo), '("nPoints", Gtk.Gesture.GestureNPointsPropertyInfo), '("orientation", GesturePanOrientationPropertyInfo), '("propagationPhase", Gtk.EventController.EventControllerPropagationPhasePropertyInfo), '("touchOnly", Gtk.GestureSingle.GestureSingleTouchOnlyPropertyInfo), '("widget", Gtk.EventController.EventControllerWidgetPropertyInfo), '("window", Gtk.Gesture.GestureWindowPropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
gesturePanOrientation :: AttrLabelProxy "orientation"
gesturePanOrientation = AttrLabelProxy

#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList GesturePan = GesturePanSignalList
type GesturePanSignalList = ('[ '("begin", Gtk.Gesture.GestureBeginSignalInfo), '("cancel", Gtk.Gesture.GestureCancelSignalInfo), '("dragBegin", Gtk.GestureDrag.GestureDragDragBeginSignalInfo), '("dragEnd", Gtk.GestureDrag.GestureDragDragEndSignalInfo), '("dragUpdate", Gtk.GestureDrag.GestureDragDragUpdateSignalInfo), '("end", Gtk.Gesture.GestureEndSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo), '("pan", GesturePanPanSignalInfo), '("sequenceStateChanged", Gtk.Gesture.GestureSequenceStateChangedSignalInfo), '("update", Gtk.Gesture.GestureUpdateSignalInfo)] :: [(Symbol, *)])

#endif

-- method GesturePan::new
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
--           { argCName = "orientation"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Orientation" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "expected orientation"
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "GesturePan" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_gesture_pan_new" gtk_gesture_pan_new :: 
    Ptr Gtk.Widget.Widget ->                -- widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    CUInt ->                                -- orientation : TInterface (Name {namespace = "Gtk", name = "Orientation"})
    IO (Ptr GesturePan)

-- | Returns a newly created t'GI.Gtk.Objects.Gesture.Gesture' that recognizes pan gestures.
-- 
-- /Since: 3.14/
gesturePanNew ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.Widget.IsWidget a) =>
    a
    -- ^ /@widget@/: a t'GI.Gtk.Objects.Widget.Widget'
    -> Gtk.Enums.Orientation
    -- ^ /@orientation@/: expected orientation
    -> m GesturePan
    -- ^ __Returns:__ a newly created t'GI.Gtk.Objects.GesturePan.GesturePan'
gesturePanNew widget orientation = liftIO $ do
    widget' <- unsafeManagedPtrCastPtr widget
    let orientation' = (fromIntegral . fromEnum) orientation
    result <- gtk_gesture_pan_new widget' orientation'
    checkUnexpectedReturnNULL "gesturePanNew" result
    result' <- (wrapObject GesturePan) result
    touchManagedPtr widget
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method GesturePan::get_orientation
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "gesture"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "GesturePan" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkGesturePan" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "Orientation" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_gesture_pan_get_orientation" gtk_gesture_pan_get_orientation :: 
    Ptr GesturePan ->                       -- gesture : TInterface (Name {namespace = "Gtk", name = "GesturePan"})
    IO CUInt

-- | Returns the orientation of the pan gestures that this /@gesture@/ expects.
-- 
-- /Since: 3.14/
gesturePanGetOrientation ::
    (B.CallStack.HasCallStack, MonadIO m, IsGesturePan a) =>
    a
    -- ^ /@gesture@/: A t'GI.Gtk.Objects.GesturePan.GesturePan'
    -> m Gtk.Enums.Orientation
    -- ^ __Returns:__ the expected orientation for pan gestures
gesturePanGetOrientation gesture = liftIO $ do
    gesture' <- unsafeManagedPtrCastPtr gesture
    result <- gtk_gesture_pan_get_orientation gesture'
    let result' = (toEnum . fromIntegral) result
    touchManagedPtr gesture
    return result'

#if defined(ENABLE_OVERLOADING)
data GesturePanGetOrientationMethodInfo
instance (signature ~ (m Gtk.Enums.Orientation), MonadIO m, IsGesturePan a) => O.OverloadedMethod GesturePanGetOrientationMethodInfo a signature where
    overloadedMethod = gesturePanGetOrientation

instance O.OverloadedMethodInfo GesturePanGetOrientationMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.GesturePan.gesturePanGetOrientation",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-GesturePan.html#v:gesturePanGetOrientation"
        })


#endif

-- method GesturePan::set_orientation
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "gesture"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "GesturePan" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkGesturePan" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "orientation"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Orientation" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "expected orientation"
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

foreign import ccall "gtk_gesture_pan_set_orientation" gtk_gesture_pan_set_orientation :: 
    Ptr GesturePan ->                       -- gesture : TInterface (Name {namespace = "Gtk", name = "GesturePan"})
    CUInt ->                                -- orientation : TInterface (Name {namespace = "Gtk", name = "Orientation"})
    IO ()

-- | Sets the orientation to be expected on pan gestures.
-- 
-- /Since: 3.14/
gesturePanSetOrientation ::
    (B.CallStack.HasCallStack, MonadIO m, IsGesturePan a) =>
    a
    -- ^ /@gesture@/: A t'GI.Gtk.Objects.GesturePan.GesturePan'
    -> Gtk.Enums.Orientation
    -- ^ /@orientation@/: expected orientation
    -> m ()
gesturePanSetOrientation gesture orientation = liftIO $ do
    gesture' <- unsafeManagedPtrCastPtr gesture
    let orientation' = (fromIntegral . fromEnum) orientation
    gtk_gesture_pan_set_orientation gesture' orientation'
    touchManagedPtr gesture
    return ()

#if defined(ENABLE_OVERLOADING)
data GesturePanSetOrientationMethodInfo
instance (signature ~ (Gtk.Enums.Orientation -> m ()), MonadIO m, IsGesturePan a) => O.OverloadedMethod GesturePanSetOrientationMethodInfo a signature where
    overloadedMethod = gesturePanSetOrientation

instance O.OverloadedMethodInfo GesturePanSetOrientationMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.GesturePan.gesturePanSetOrientation",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-GesturePan.html#v:gesturePanSetOrientation"
        })


#endif


