{-# LANGUAGE ImplicitParams, RankNTypes, TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- t'GI.Gtk.Objects.EventControllerKey.EventControllerKey' is an event controller meant for situations
-- where you need access to key events.
-- 
-- This object was added in 3.24.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.EventControllerKey
    ( 

-- * Exported types
    EventControllerKey(..)                  ,
    IsEventControllerKey                    ,
    toEventControllerKey                    ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [forward]("GI.Gtk.Objects.EventControllerKey#g:method:forward"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [handleEvent]("GI.Gtk.Objects.EventController#g:method:handleEvent"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [reset]("GI.Gtk.Objects.EventController#g:method:reset"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getData]("GI.GObject.Objects.Object#g:method:getData"), [getGroup]("GI.Gtk.Objects.EventControllerKey#g:method:getGroup"), [getImContext]("GI.Gtk.Objects.EventControllerKey#g:method:getImContext"), [getPropagationPhase]("GI.Gtk.Objects.EventController#g:method:getPropagationPhase"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getWidget]("GI.Gtk.Objects.EventController#g:method:getWidget").
-- 
-- ==== Setters
-- [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setImContext]("GI.Gtk.Objects.EventControllerKey#g:method:setImContext"), [setPropagationPhase]("GI.Gtk.Objects.EventController#g:method:setPropagationPhase"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty").

#if defined(ENABLE_OVERLOADING)
    ResolveEventControllerKeyMethod         ,
#endif

-- ** forward #method:forward#

#if defined(ENABLE_OVERLOADING)
    EventControllerKeyForwardMethodInfo     ,
#endif
    eventControllerKeyForward               ,


-- ** getGroup #method:getGroup#

#if defined(ENABLE_OVERLOADING)
    EventControllerKeyGetGroupMethodInfo    ,
#endif
    eventControllerKeyGetGroup              ,


-- ** getImContext #method:getImContext#

#if defined(ENABLE_OVERLOADING)
    EventControllerKeyGetImContextMethodInfo,
#endif
    eventControllerKeyGetImContext          ,


-- ** new #method:new#

    eventControllerKeyNew                   ,


-- ** setImContext #method:setImContext#

#if defined(ENABLE_OVERLOADING)
    EventControllerKeySetImContextMethodInfo,
#endif
    eventControllerKeySetImContext          ,




 -- * Signals


-- ** focusIn #signal:focusIn#

    EventControllerKeyFocusInCallback       ,
#if defined(ENABLE_OVERLOADING)
    EventControllerKeyFocusInSignalInfo     ,
#endif
    afterEventControllerKeyFocusIn          ,
    onEventControllerKeyFocusIn             ,


-- ** focusOut #signal:focusOut#

    EventControllerKeyFocusOutCallback      ,
#if defined(ENABLE_OVERLOADING)
    EventControllerKeyFocusOutSignalInfo    ,
#endif
    afterEventControllerKeyFocusOut         ,
    onEventControllerKeyFocusOut            ,


-- ** imUpdate #signal:imUpdate#

    EventControllerKeyImUpdateCallback      ,
#if defined(ENABLE_OVERLOADING)
    EventControllerKeyImUpdateSignalInfo    ,
#endif
    afterEventControllerKeyImUpdate         ,
    onEventControllerKeyImUpdate            ,


-- ** keyPressed #signal:keyPressed#

    EventControllerKeyKeyPressedCallback    ,
#if defined(ENABLE_OVERLOADING)
    EventControllerKeyKeyPressedSignalInfo  ,
#endif
    afterEventControllerKeyKeyPressed       ,
    onEventControllerKeyKeyPressed          ,


-- ** keyReleased #signal:keyReleased#

    EventControllerKeyKeyReleasedCallback   ,
#if defined(ENABLE_OVERLOADING)
    EventControllerKeyKeyReleasedSignalInfo ,
#endif
    afterEventControllerKeyKeyReleased      ,
    onEventControllerKeyKeyReleased         ,


-- ** modifiers #signal:modifiers#

    EventControllerKeyModifiersCallback     ,
#if defined(ENABLE_OVERLOADING)
    EventControllerKeyModifiersSignalInfo   ,
#endif
    afterEventControllerKeyModifiers        ,
    onEventControllerKeyModifiers           ,




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
import qualified GI.Gdk.Flags as Gdk.Flags
import {-# SOURCE #-} qualified GI.Gtk.Objects.EventController as Gtk.EventController
import {-# SOURCE #-} qualified GI.Gtk.Objects.IMContext as Gtk.IMContext
import {-# SOURCE #-} qualified GI.Gtk.Objects.Widget as Gtk.Widget

-- | Memory-managed wrapper type.
newtype EventControllerKey = EventControllerKey (SP.ManagedPtr EventControllerKey)
    deriving (Eq)

instance SP.ManagedPtrNewtype EventControllerKey where
    toManagedPtr (EventControllerKey p) = p

foreign import ccall "gtk_event_controller_key_get_type"
    c_gtk_event_controller_key_get_type :: IO B.Types.GType

instance B.Types.TypedObject EventControllerKey where
    glibType = c_gtk_event_controller_key_get_type

instance B.Types.GObject EventControllerKey

-- | Type class for types which can be safely cast to `EventControllerKey`, for instance with `toEventControllerKey`.
class (SP.GObject o, O.IsDescendantOf EventControllerKey o) => IsEventControllerKey o
instance (SP.GObject o, O.IsDescendantOf EventControllerKey o) => IsEventControllerKey o

instance O.HasParentTypes EventControllerKey
type instance O.ParentTypes EventControllerKey = '[Gtk.EventController.EventController, GObject.Object.Object]

-- | Cast to `EventControllerKey`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toEventControllerKey :: (MIO.MonadIO m, IsEventControllerKey o) => o -> m EventControllerKey
toEventControllerKey = MIO.liftIO . B.ManagedPtr.unsafeCastTo EventControllerKey

-- | Convert 'EventControllerKey' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe EventControllerKey) where
    gvalueGType_ = c_gtk_event_controller_key_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr EventControllerKey)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr EventControllerKey)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject EventControllerKey ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveEventControllerKeyMethod (t :: Symbol) (o :: *) :: * where
    ResolveEventControllerKeyMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveEventControllerKeyMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveEventControllerKeyMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveEventControllerKeyMethod "forward" o = EventControllerKeyForwardMethodInfo
    ResolveEventControllerKeyMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveEventControllerKeyMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveEventControllerKeyMethod "handleEvent" o = Gtk.EventController.EventControllerHandleEventMethodInfo
    ResolveEventControllerKeyMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveEventControllerKeyMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveEventControllerKeyMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveEventControllerKeyMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveEventControllerKeyMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveEventControllerKeyMethod "reset" o = Gtk.EventController.EventControllerResetMethodInfo
    ResolveEventControllerKeyMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveEventControllerKeyMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveEventControllerKeyMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveEventControllerKeyMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveEventControllerKeyMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveEventControllerKeyMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveEventControllerKeyMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveEventControllerKeyMethod "getGroup" o = EventControllerKeyGetGroupMethodInfo
    ResolveEventControllerKeyMethod "getImContext" o = EventControllerKeyGetImContextMethodInfo
    ResolveEventControllerKeyMethod "getPropagationPhase" o = Gtk.EventController.EventControllerGetPropagationPhaseMethodInfo
    ResolveEventControllerKeyMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveEventControllerKeyMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveEventControllerKeyMethod "getWidget" o = Gtk.EventController.EventControllerGetWidgetMethodInfo
    ResolveEventControllerKeyMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveEventControllerKeyMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveEventControllerKeyMethod "setImContext" o = EventControllerKeySetImContextMethodInfo
    ResolveEventControllerKeyMethod "setPropagationPhase" o = Gtk.EventController.EventControllerSetPropagationPhaseMethodInfo
    ResolveEventControllerKeyMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveEventControllerKeyMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveEventControllerKeyMethod t EventControllerKey, O.OverloadedMethod info EventControllerKey p) => OL.IsLabel t (EventControllerKey -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveEventControllerKeyMethod t EventControllerKey, O.OverloadedMethod info EventControllerKey p, R.HasField t EventControllerKey p) => R.HasField t EventControllerKey p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveEventControllerKeyMethod t EventControllerKey, O.OverloadedMethodInfo info EventControllerKey) => OL.IsLabel t (O.MethodProxy info EventControllerKey) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- signal EventControllerKey::focus-in
-- | /No description available in the introspection data./
type EventControllerKeyFocusInCallback =
    IO ()

type C_EventControllerKeyFocusInCallback =
    Ptr EventControllerKey ->               -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_EventControllerKeyFocusInCallback`.
foreign import ccall "wrapper"
    mk_EventControllerKeyFocusInCallback :: C_EventControllerKeyFocusInCallback -> IO (FunPtr C_EventControllerKeyFocusInCallback)

wrap_EventControllerKeyFocusInCallback :: 
    GObject a => (a -> EventControllerKeyFocusInCallback) ->
    C_EventControllerKeyFocusInCallback
wrap_EventControllerKeyFocusInCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [focusIn](#signal:focusIn) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' eventControllerKey #focusIn callback
-- @
-- 
-- 
onEventControllerKeyFocusIn :: (IsEventControllerKey a, MonadIO m) => a -> ((?self :: a) => EventControllerKeyFocusInCallback) -> m SignalHandlerId
onEventControllerKeyFocusIn obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_EventControllerKeyFocusInCallback wrapped
    wrapped'' <- mk_EventControllerKeyFocusInCallback wrapped'
    connectSignalFunPtr obj "focus-in" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [focusIn](#signal:focusIn) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' eventControllerKey #focusIn callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterEventControllerKeyFocusIn :: (IsEventControllerKey a, MonadIO m) => a -> ((?self :: a) => EventControllerKeyFocusInCallback) -> m SignalHandlerId
afterEventControllerKeyFocusIn obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_EventControllerKeyFocusInCallback wrapped
    wrapped'' <- mk_EventControllerKeyFocusInCallback wrapped'
    connectSignalFunPtr obj "focus-in" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data EventControllerKeyFocusInSignalInfo
instance SignalInfo EventControllerKeyFocusInSignalInfo where
    type HaskellCallbackType EventControllerKeyFocusInSignalInfo = EventControllerKeyFocusInCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_EventControllerKeyFocusInCallback cb
        cb'' <- mk_EventControllerKeyFocusInCallback cb'
        connectSignalFunPtr obj "focus-in" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.EventControllerKey::focus-in"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-EventControllerKey.html#g:signal:focusIn"})

#endif

-- signal EventControllerKey::focus-out
-- | /No description available in the introspection data./
type EventControllerKeyFocusOutCallback =
    IO ()

type C_EventControllerKeyFocusOutCallback =
    Ptr EventControllerKey ->               -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_EventControllerKeyFocusOutCallback`.
foreign import ccall "wrapper"
    mk_EventControllerKeyFocusOutCallback :: C_EventControllerKeyFocusOutCallback -> IO (FunPtr C_EventControllerKeyFocusOutCallback)

wrap_EventControllerKeyFocusOutCallback :: 
    GObject a => (a -> EventControllerKeyFocusOutCallback) ->
    C_EventControllerKeyFocusOutCallback
wrap_EventControllerKeyFocusOutCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [focusOut](#signal:focusOut) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' eventControllerKey #focusOut callback
-- @
-- 
-- 
onEventControllerKeyFocusOut :: (IsEventControllerKey a, MonadIO m) => a -> ((?self :: a) => EventControllerKeyFocusOutCallback) -> m SignalHandlerId
onEventControllerKeyFocusOut obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_EventControllerKeyFocusOutCallback wrapped
    wrapped'' <- mk_EventControllerKeyFocusOutCallback wrapped'
    connectSignalFunPtr obj "focus-out" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [focusOut](#signal:focusOut) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' eventControllerKey #focusOut callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterEventControllerKeyFocusOut :: (IsEventControllerKey a, MonadIO m) => a -> ((?self :: a) => EventControllerKeyFocusOutCallback) -> m SignalHandlerId
afterEventControllerKeyFocusOut obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_EventControllerKeyFocusOutCallback wrapped
    wrapped'' <- mk_EventControllerKeyFocusOutCallback wrapped'
    connectSignalFunPtr obj "focus-out" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data EventControllerKeyFocusOutSignalInfo
instance SignalInfo EventControllerKeyFocusOutSignalInfo where
    type HaskellCallbackType EventControllerKeyFocusOutSignalInfo = EventControllerKeyFocusOutCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_EventControllerKeyFocusOutCallback cb
        cb'' <- mk_EventControllerKeyFocusOutCallback cb'
        connectSignalFunPtr obj "focus-out" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.EventControllerKey::focus-out"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-EventControllerKey.html#g:signal:focusOut"})

#endif

-- signal EventControllerKey::im-update
-- | /No description available in the introspection data./
type EventControllerKeyImUpdateCallback =
    IO ()

type C_EventControllerKeyImUpdateCallback =
    Ptr EventControllerKey ->               -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_EventControllerKeyImUpdateCallback`.
foreign import ccall "wrapper"
    mk_EventControllerKeyImUpdateCallback :: C_EventControllerKeyImUpdateCallback -> IO (FunPtr C_EventControllerKeyImUpdateCallback)

wrap_EventControllerKeyImUpdateCallback :: 
    GObject a => (a -> EventControllerKeyImUpdateCallback) ->
    C_EventControllerKeyImUpdateCallback
wrap_EventControllerKeyImUpdateCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [imUpdate](#signal:imUpdate) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' eventControllerKey #imUpdate callback
-- @
-- 
-- 
onEventControllerKeyImUpdate :: (IsEventControllerKey a, MonadIO m) => a -> ((?self :: a) => EventControllerKeyImUpdateCallback) -> m SignalHandlerId
onEventControllerKeyImUpdate obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_EventControllerKeyImUpdateCallback wrapped
    wrapped'' <- mk_EventControllerKeyImUpdateCallback wrapped'
    connectSignalFunPtr obj "im-update" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [imUpdate](#signal:imUpdate) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' eventControllerKey #imUpdate callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterEventControllerKeyImUpdate :: (IsEventControllerKey a, MonadIO m) => a -> ((?self :: a) => EventControllerKeyImUpdateCallback) -> m SignalHandlerId
afterEventControllerKeyImUpdate obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_EventControllerKeyImUpdateCallback wrapped
    wrapped'' <- mk_EventControllerKeyImUpdateCallback wrapped'
    connectSignalFunPtr obj "im-update" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data EventControllerKeyImUpdateSignalInfo
instance SignalInfo EventControllerKeyImUpdateSignalInfo where
    type HaskellCallbackType EventControllerKeyImUpdateSignalInfo = EventControllerKeyImUpdateCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_EventControllerKeyImUpdateCallback cb
        cb'' <- mk_EventControllerKeyImUpdateCallback cb'
        connectSignalFunPtr obj "im-update" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.EventControllerKey::im-update"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-EventControllerKey.html#g:signal:imUpdate"})

#endif

-- signal EventControllerKey::key-pressed
-- | This signal is emitted whenever a key is pressed.
-- 
-- /Since: 3.24/
type EventControllerKeyKeyPressedCallback =
    Word32
    -- ^ /@keyval@/: the pressed key.
    -> Word32
    -- ^ /@keycode@/: the raw code of the pressed key.
    -> [Gdk.Flags.ModifierType]
    -- ^ /@state@/: the bitmask, representing the state of modifier keys and pointer buttons. See t'GI.Gdk.Flags.ModifierType'.
    -> IO Bool
    -- ^ __Returns:__ 'P.True' if the key press was handled, 'P.False' otherwise.

type C_EventControllerKeyKeyPressedCallback =
    Ptr EventControllerKey ->               -- object
    Word32 ->
    Word32 ->
    CUInt ->
    Ptr () ->                               -- user_data
    IO CInt

-- | Generate a function pointer callable from C code, from a `C_EventControllerKeyKeyPressedCallback`.
foreign import ccall "wrapper"
    mk_EventControllerKeyKeyPressedCallback :: C_EventControllerKeyKeyPressedCallback -> IO (FunPtr C_EventControllerKeyKeyPressedCallback)

wrap_EventControllerKeyKeyPressedCallback :: 
    GObject a => (a -> EventControllerKeyKeyPressedCallback) ->
    C_EventControllerKeyKeyPressedCallback
wrap_EventControllerKeyKeyPressedCallback gi'cb gi'selfPtr keyval keycode state _ = do
    let state' = wordToGFlags state
    result <- B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  keyval keycode state'
    let result' = (fromIntegral . fromEnum) result
    return result'


-- | Connect a signal handler for the [keyPressed](#signal:keyPressed) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' eventControllerKey #keyPressed callback
-- @
-- 
-- 
onEventControllerKeyKeyPressed :: (IsEventControllerKey a, MonadIO m) => a -> ((?self :: a) => EventControllerKeyKeyPressedCallback) -> m SignalHandlerId
onEventControllerKeyKeyPressed obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_EventControllerKeyKeyPressedCallback wrapped
    wrapped'' <- mk_EventControllerKeyKeyPressedCallback wrapped'
    connectSignalFunPtr obj "key-pressed" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [keyPressed](#signal:keyPressed) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' eventControllerKey #keyPressed callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterEventControllerKeyKeyPressed :: (IsEventControllerKey a, MonadIO m) => a -> ((?self :: a) => EventControllerKeyKeyPressedCallback) -> m SignalHandlerId
afterEventControllerKeyKeyPressed obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_EventControllerKeyKeyPressedCallback wrapped
    wrapped'' <- mk_EventControllerKeyKeyPressedCallback wrapped'
    connectSignalFunPtr obj "key-pressed" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data EventControllerKeyKeyPressedSignalInfo
instance SignalInfo EventControllerKeyKeyPressedSignalInfo where
    type HaskellCallbackType EventControllerKeyKeyPressedSignalInfo = EventControllerKeyKeyPressedCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_EventControllerKeyKeyPressedCallback cb
        cb'' <- mk_EventControllerKeyKeyPressedCallback cb'
        connectSignalFunPtr obj "key-pressed" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.EventControllerKey::key-pressed"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-EventControllerKey.html#g:signal:keyPressed"})

#endif

-- signal EventControllerKey::key-released
-- | This signal is emitted whenever a key is released.
-- 
-- /Since: 3.24/
type EventControllerKeyKeyReleasedCallback =
    Word32
    -- ^ /@keyval@/: the released key.
    -> Word32
    -- ^ /@keycode@/: the raw code of the released key.
    -> [Gdk.Flags.ModifierType]
    -- ^ /@state@/: the bitmask, representing the state of modifier keys and pointer buttons. See t'GI.Gdk.Flags.ModifierType'.
    -> IO ()

type C_EventControllerKeyKeyReleasedCallback =
    Ptr EventControllerKey ->               -- object
    Word32 ->
    Word32 ->
    CUInt ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_EventControllerKeyKeyReleasedCallback`.
foreign import ccall "wrapper"
    mk_EventControllerKeyKeyReleasedCallback :: C_EventControllerKeyKeyReleasedCallback -> IO (FunPtr C_EventControllerKeyKeyReleasedCallback)

wrap_EventControllerKeyKeyReleasedCallback :: 
    GObject a => (a -> EventControllerKeyKeyReleasedCallback) ->
    C_EventControllerKeyKeyReleasedCallback
wrap_EventControllerKeyKeyReleasedCallback gi'cb gi'selfPtr keyval keycode state _ = do
    let state' = wordToGFlags state
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  keyval keycode state'


-- | Connect a signal handler for the [keyReleased](#signal:keyReleased) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' eventControllerKey #keyReleased callback
-- @
-- 
-- 
onEventControllerKeyKeyReleased :: (IsEventControllerKey a, MonadIO m) => a -> ((?self :: a) => EventControllerKeyKeyReleasedCallback) -> m SignalHandlerId
onEventControllerKeyKeyReleased obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_EventControllerKeyKeyReleasedCallback wrapped
    wrapped'' <- mk_EventControllerKeyKeyReleasedCallback wrapped'
    connectSignalFunPtr obj "key-released" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [keyReleased](#signal:keyReleased) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' eventControllerKey #keyReleased callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterEventControllerKeyKeyReleased :: (IsEventControllerKey a, MonadIO m) => a -> ((?self :: a) => EventControllerKeyKeyReleasedCallback) -> m SignalHandlerId
afterEventControllerKeyKeyReleased obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_EventControllerKeyKeyReleasedCallback wrapped
    wrapped'' <- mk_EventControllerKeyKeyReleasedCallback wrapped'
    connectSignalFunPtr obj "key-released" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data EventControllerKeyKeyReleasedSignalInfo
instance SignalInfo EventControllerKeyKeyReleasedSignalInfo where
    type HaskellCallbackType EventControllerKeyKeyReleasedSignalInfo = EventControllerKeyKeyReleasedCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_EventControllerKeyKeyReleasedCallback cb
        cb'' <- mk_EventControllerKeyKeyReleasedCallback cb'
        connectSignalFunPtr obj "key-released" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.EventControllerKey::key-released"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-EventControllerKey.html#g:signal:keyReleased"})

#endif

-- signal EventControllerKey::modifiers
-- | /No description available in the introspection data./
type EventControllerKeyModifiersCallback =
    [Gdk.Flags.ModifierType]
    -> IO Bool

type C_EventControllerKeyModifiersCallback =
    Ptr EventControllerKey ->               -- object
    CUInt ->
    Ptr () ->                               -- user_data
    IO CInt

-- | Generate a function pointer callable from C code, from a `C_EventControllerKeyModifiersCallback`.
foreign import ccall "wrapper"
    mk_EventControllerKeyModifiersCallback :: C_EventControllerKeyModifiersCallback -> IO (FunPtr C_EventControllerKeyModifiersCallback)

wrap_EventControllerKeyModifiersCallback :: 
    GObject a => (a -> EventControllerKeyModifiersCallback) ->
    C_EventControllerKeyModifiersCallback
wrap_EventControllerKeyModifiersCallback gi'cb gi'selfPtr object _ = do
    let object' = wordToGFlags object
    result <- B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  object'
    let result' = (fromIntegral . fromEnum) result
    return result'


-- | Connect a signal handler for the [modifiers](#signal:modifiers) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' eventControllerKey #modifiers callback
-- @
-- 
-- 
onEventControllerKeyModifiers :: (IsEventControllerKey a, MonadIO m) => a -> ((?self :: a) => EventControllerKeyModifiersCallback) -> m SignalHandlerId
onEventControllerKeyModifiers obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_EventControllerKeyModifiersCallback wrapped
    wrapped'' <- mk_EventControllerKeyModifiersCallback wrapped'
    connectSignalFunPtr obj "modifiers" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [modifiers](#signal:modifiers) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' eventControllerKey #modifiers callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterEventControllerKeyModifiers :: (IsEventControllerKey a, MonadIO m) => a -> ((?self :: a) => EventControllerKeyModifiersCallback) -> m SignalHandlerId
afterEventControllerKeyModifiers obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_EventControllerKeyModifiersCallback wrapped
    wrapped'' <- mk_EventControllerKeyModifiersCallback wrapped'
    connectSignalFunPtr obj "modifiers" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data EventControllerKeyModifiersSignalInfo
instance SignalInfo EventControllerKeyModifiersSignalInfo where
    type HaskellCallbackType EventControllerKeyModifiersSignalInfo = EventControllerKeyModifiersCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_EventControllerKeyModifiersCallback cb
        cb'' <- mk_EventControllerKeyModifiersCallback cb'
        connectSignalFunPtr obj "modifiers" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.EventControllerKey::modifiers"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-EventControllerKey.html#g:signal:modifiers"})

#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList EventControllerKey
type instance O.AttributeList EventControllerKey = EventControllerKeyAttributeList
type EventControllerKeyAttributeList = ('[ '("propagationPhase", Gtk.EventController.EventControllerPropagationPhasePropertyInfo), '("widget", Gtk.EventController.EventControllerWidgetPropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList EventControllerKey = EventControllerKeySignalList
type EventControllerKeySignalList = ('[ '("focusIn", EventControllerKeyFocusInSignalInfo), '("focusOut", EventControllerKeyFocusOutSignalInfo), '("imUpdate", EventControllerKeyImUpdateSignalInfo), '("keyPressed", EventControllerKeyKeyPressedSignalInfo), '("keyReleased", EventControllerKeyKeyReleasedSignalInfo), '("modifiers", EventControllerKeyModifiersSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo)] :: [(Symbol, *)])

#endif

-- method EventControllerKey::new
-- method type : Constructor
-- Args: [ Arg
--           { argCName = "widget"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Widget" }
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
--                  Name { namespace = "Gtk" , name = "EventControllerKey" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_event_controller_key_new" gtk_event_controller_key_new :: 
    Ptr Gtk.Widget.Widget ->                -- widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    IO (Ptr EventControllerKey)

-- | /No description available in the introspection data./
eventControllerKeyNew ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.Widget.IsWidget a) =>
    a
    -> m EventControllerKey
eventControllerKeyNew widget = liftIO $ do
    widget' <- unsafeManagedPtrCastPtr widget
    result <- gtk_event_controller_key_new widget'
    checkUnexpectedReturnNULL "eventControllerKeyNew" result
    result' <- (wrapObject EventControllerKey) result
    touchManagedPtr widget
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method EventControllerKey::forward
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "controller"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "EventControllerKey" }
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
--           { argCName = "widget"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Widget" }
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
-- returnType: Just (TBasicType TBoolean)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_event_controller_key_forward" gtk_event_controller_key_forward :: 
    Ptr EventControllerKey ->               -- controller : TInterface (Name {namespace = "Gtk", name = "EventControllerKey"})
    Ptr Gtk.Widget.Widget ->                -- widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    IO CInt

-- | /No description available in the introspection data./
eventControllerKeyForward ::
    (B.CallStack.HasCallStack, MonadIO m, IsEventControllerKey a, Gtk.Widget.IsWidget b) =>
    a
    -> b
    -> m Bool
eventControllerKeyForward controller widget = liftIO $ do
    controller' <- unsafeManagedPtrCastPtr controller
    widget' <- unsafeManagedPtrCastPtr widget
    result <- gtk_event_controller_key_forward controller' widget'
    let result' = (/= 0) result
    touchManagedPtr controller
    touchManagedPtr widget
    return result'

#if defined(ENABLE_OVERLOADING)
data EventControllerKeyForwardMethodInfo
instance (signature ~ (b -> m Bool), MonadIO m, IsEventControllerKey a, Gtk.Widget.IsWidget b) => O.OverloadedMethod EventControllerKeyForwardMethodInfo a signature where
    overloadedMethod = eventControllerKeyForward

instance O.OverloadedMethodInfo EventControllerKeyForwardMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.EventControllerKey.eventControllerKeyForward",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-EventControllerKey.html#v:eventControllerKeyForward"
        })


#endif

-- method EventControllerKey::get_group
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "controller"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "EventControllerKey" }
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
-- returnType: Just (TBasicType TUInt)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_event_controller_key_get_group" gtk_event_controller_key_get_group :: 
    Ptr EventControllerKey ->               -- controller : TInterface (Name {namespace = "Gtk", name = "EventControllerKey"})
    IO Word32

-- | /No description available in the introspection data./
eventControllerKeyGetGroup ::
    (B.CallStack.HasCallStack, MonadIO m, IsEventControllerKey a) =>
    a
    -> m Word32
eventControllerKeyGetGroup controller = liftIO $ do
    controller' <- unsafeManagedPtrCastPtr controller
    result <- gtk_event_controller_key_get_group controller'
    touchManagedPtr controller
    return result

#if defined(ENABLE_OVERLOADING)
data EventControllerKeyGetGroupMethodInfo
instance (signature ~ (m Word32), MonadIO m, IsEventControllerKey a) => O.OverloadedMethod EventControllerKeyGetGroupMethodInfo a signature where
    overloadedMethod = eventControllerKeyGetGroup

instance O.OverloadedMethodInfo EventControllerKeyGetGroupMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.EventControllerKey.eventControllerKeyGetGroup",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-EventControllerKey.html#v:eventControllerKeyGetGroup"
        })


#endif

-- method EventControllerKey::get_im_context
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "controller"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "EventControllerKey" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEventControllerKey"
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "IMContext" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_event_controller_key_get_im_context" gtk_event_controller_key_get_im_context :: 
    Ptr EventControllerKey ->               -- controller : TInterface (Name {namespace = "Gtk", name = "EventControllerKey"})
    IO (Ptr Gtk.IMContext.IMContext)

-- | Gets the IM context of a key controller.
-- 
-- /Since: 3.24/
eventControllerKeyGetImContext ::
    (B.CallStack.HasCallStack, MonadIO m, IsEventControllerKey a) =>
    a
    -- ^ /@controller@/: a t'GI.Gtk.Objects.EventControllerKey.EventControllerKey'
    -> m Gtk.IMContext.IMContext
    -- ^ __Returns:__ the IM context
eventControllerKeyGetImContext controller = liftIO $ do
    controller' <- unsafeManagedPtrCastPtr controller
    result <- gtk_event_controller_key_get_im_context controller'
    checkUnexpectedReturnNULL "eventControllerKeyGetImContext" result
    result' <- (newObject Gtk.IMContext.IMContext) result
    touchManagedPtr controller
    return result'

#if defined(ENABLE_OVERLOADING)
data EventControllerKeyGetImContextMethodInfo
instance (signature ~ (m Gtk.IMContext.IMContext), MonadIO m, IsEventControllerKey a) => O.OverloadedMethod EventControllerKeyGetImContextMethodInfo a signature where
    overloadedMethod = eventControllerKeyGetImContext

instance O.OverloadedMethodInfo EventControllerKeyGetImContextMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.EventControllerKey.eventControllerKeyGetImContext",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-EventControllerKey.html#v:eventControllerKeyGetImContext"
        })


#endif

-- method EventControllerKey::set_im_context
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "controller"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "EventControllerKey" }
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
--           { argCName = "im_context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "IMContext" }
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

foreign import ccall "gtk_event_controller_key_set_im_context" gtk_event_controller_key_set_im_context :: 
    Ptr EventControllerKey ->               -- controller : TInterface (Name {namespace = "Gtk", name = "EventControllerKey"})
    Ptr Gtk.IMContext.IMContext ->          -- im_context : TInterface (Name {namespace = "Gtk", name = "IMContext"})
    IO ()

-- | /No description available in the introspection data./
eventControllerKeySetImContext ::
    (B.CallStack.HasCallStack, MonadIO m, IsEventControllerKey a, Gtk.IMContext.IsIMContext b) =>
    a
    -> b
    -> m ()
eventControllerKeySetImContext controller imContext = liftIO $ do
    controller' <- unsafeManagedPtrCastPtr controller
    imContext' <- unsafeManagedPtrCastPtr imContext
    gtk_event_controller_key_set_im_context controller' imContext'
    touchManagedPtr controller
    touchManagedPtr imContext
    return ()

#if defined(ENABLE_OVERLOADING)
data EventControllerKeySetImContextMethodInfo
instance (signature ~ (b -> m ()), MonadIO m, IsEventControllerKey a, Gtk.IMContext.IsIMContext b) => O.OverloadedMethod EventControllerKeySetImContextMethodInfo a signature where
    overloadedMethod = eventControllerKeySetImContext

instance O.OverloadedMethodInfo EventControllerKeySetImContextMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.EventControllerKey.eventControllerKeySetImContext",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-EventControllerKey.html#v:eventControllerKeySetImContext"
        })


#endif


