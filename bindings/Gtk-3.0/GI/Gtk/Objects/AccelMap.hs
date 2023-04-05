{-# LANGUAGE ImplicitParams, RankNTypes, TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- Accelerator maps are used to define runtime configurable accelerators.
-- Functions for manipulating them are are usually used by higher level
-- convenience mechanisms like t'GI.Gtk.Objects.UIManager.UIManager' and are thus considered
-- “low-level”. You’ll want to use them if you’re manually creating menus that
-- should have user-configurable accelerators.
-- 
-- An accelerator is uniquely defined by:
-- 
-- * accelerator path
-- * accelerator key
-- * accelerator modifiers
-- 
-- 
-- The accelerator path must consist of
-- “\<WINDOWTYPE>\/Category1\/Category2\/...\/Action”, where WINDOWTYPE
-- should be a unique application-specific identifier that corresponds
-- to the kind of window the accelerator is being used in, e.g.
-- “Gimp-Image”, “Abiword-Document” or “Gnumeric-Settings”.
-- The “Category1\/...\/Action” portion is most appropriately chosen by
-- the action the accelerator triggers, i.e. for accelerators on menu
-- items, choose the item’s menu path, e.g. “File\/Save As”,
-- “Image\/View\/Zoom” or “Edit\/Select All”. So a full valid accelerator
-- path may look like: “\<Gimp-Toolbox>\/File\/Dialogs\/Tool Options...”.
-- 
-- All accelerators are stored inside one global t'GI.Gtk.Objects.AccelMap.AccelMap' that can
-- be obtained using 'GI.Gtk.Objects.AccelMap.accelMapGet'. See
-- [Monitoring changes][monitoring-changes] for additional
-- details.
-- 
-- = Manipulating accelerators
-- 
-- New accelerators can be added using 'GI.Gtk.Objects.AccelMap.accelMapAddEntry'.
-- To search for specific accelerator, use 'GI.Gtk.Objects.AccelMap.accelMapLookupEntry'.
-- Modifications of existing accelerators should be done using
-- 'GI.Gtk.Objects.AccelMap.accelMapChangeEntry'.
-- 
-- In order to avoid having some accelerators changed, they can be
-- locked using 'GI.Gtk.Objects.AccelMap.accelMapLockPath'. Unlocking is done using
-- 'GI.Gtk.Objects.AccelMap.accelMapUnlockPath'.
-- 
-- = Saving and loading accelerator maps
-- 
-- Accelerator maps can be saved to and loaded from some external
-- resource. For simple saving and loading from file,
-- 'GI.Gtk.Objects.AccelMap.accelMapSave' and 'GI.Gtk.Objects.AccelMap.accelMapLoad' are provided.
-- Saving and loading can also be done by providing file descriptor
-- to 'GI.Gtk.Objects.AccelMap.accelMapSaveFd' and 'GI.Gtk.Objects.AccelMap.accelMapLoadFd'.
-- 
-- = Monitoring changes
-- 
-- t'GI.Gtk.Objects.AccelMap.AccelMap' object is only useful for monitoring changes of
-- accelerators. By connecting to [AccelMap::changed]("GI.Gtk.Objects.AccelMap#g:signal:changed") signal, one
-- can monitor changes of all accelerators. It is also possible to
-- monitor only single accelerator path by using it as a detail of
-- the [AccelMap::changed]("GI.Gtk.Objects.AccelMap#g:signal:changed") signal.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.AccelMap
    ( 

-- * Exported types
    AccelMap(..)                            ,
    IsAccelMap                              ,
    toAccelMap                              ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getData]("GI.GObject.Objects.Object#g:method:getData"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata").
-- 
-- ==== Setters
-- [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty").

#if defined(ENABLE_OVERLOADING)
    ResolveAccelMapMethod                   ,
#endif

-- ** addEntry #method:addEntry#

    accelMapAddEntry                        ,


-- ** addFilter #method:addFilter#

    accelMapAddFilter                       ,


-- ** changeEntry #method:changeEntry#

    accelMapChangeEntry                     ,


-- ** foreach #method:foreach#

    accelMapForeach                         ,


-- ** foreachUnfiltered #method:foreachUnfiltered#

    accelMapForeachUnfiltered               ,


-- ** get #method:get#

    accelMapGet                             ,


-- ** load #method:load#

    accelMapLoad                            ,


-- ** loadFd #method:loadFd#

    accelMapLoadFd                          ,


-- ** loadScanner #method:loadScanner#

    accelMapLoadScanner                     ,


-- ** lockPath #method:lockPath#

    accelMapLockPath                        ,


-- ** lookupEntry #method:lookupEntry#

    accelMapLookupEntry                     ,


-- ** save #method:save#

    accelMapSave                            ,


-- ** saveFd #method:saveFd#

    accelMapSaveFd                          ,


-- ** unlockPath #method:unlockPath#

    accelMapUnlockPath                      ,




 -- * Signals


-- ** changed #signal:changed#

    AccelMapChangedCallback                 ,
#if defined(ENABLE_OVERLOADING)
    AccelMapChangedSignalInfo               ,
#endif
    afterAccelMapChanged                    ,
    onAccelMapChanged                       ,




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

import qualified GI.GLib.Structs.Scanner as GLib.Scanner
import qualified GI.GObject.Objects.Object as GObject.Object
import qualified GI.Gdk.Flags as Gdk.Flags
import qualified GI.Gtk.Callbacks as Gtk.Callbacks
import {-# SOURCE #-} qualified GI.Gtk.Structs.AccelKey as Gtk.AccelKey

-- | Memory-managed wrapper type.
newtype AccelMap = AccelMap (SP.ManagedPtr AccelMap)
    deriving (Eq)

instance SP.ManagedPtrNewtype AccelMap where
    toManagedPtr (AccelMap p) = p

foreign import ccall "gtk_accel_map_get_type"
    c_gtk_accel_map_get_type :: IO B.Types.GType

instance B.Types.TypedObject AccelMap where
    glibType = c_gtk_accel_map_get_type

instance B.Types.GObject AccelMap

-- | Type class for types which can be safely cast to `AccelMap`, for instance with `toAccelMap`.
class (SP.GObject o, O.IsDescendantOf AccelMap o) => IsAccelMap o
instance (SP.GObject o, O.IsDescendantOf AccelMap o) => IsAccelMap o

instance O.HasParentTypes AccelMap
type instance O.ParentTypes AccelMap = '[GObject.Object.Object]

-- | Cast to `AccelMap`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toAccelMap :: (MIO.MonadIO m, IsAccelMap o) => o -> m AccelMap
toAccelMap = MIO.liftIO . B.ManagedPtr.unsafeCastTo AccelMap

-- | Convert 'AccelMap' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe AccelMap) where
    gvalueGType_ = c_gtk_accel_map_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr AccelMap)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr AccelMap)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject AccelMap ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveAccelMapMethod (t :: Symbol) (o :: *) :: * where
    ResolveAccelMapMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveAccelMapMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveAccelMapMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveAccelMapMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveAccelMapMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveAccelMapMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveAccelMapMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveAccelMapMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveAccelMapMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveAccelMapMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveAccelMapMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveAccelMapMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveAccelMapMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveAccelMapMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveAccelMapMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveAccelMapMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveAccelMapMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveAccelMapMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveAccelMapMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveAccelMapMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveAccelMapMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveAccelMapMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveAccelMapMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveAccelMapMethod t AccelMap, O.OverloadedMethod info AccelMap p) => OL.IsLabel t (AccelMap -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveAccelMapMethod t AccelMap, O.OverloadedMethod info AccelMap p, R.HasField t AccelMap p) => R.HasField t AccelMap p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveAccelMapMethod t AccelMap, O.OverloadedMethodInfo info AccelMap) => OL.IsLabel t (O.MethodProxy info AccelMap) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- signal AccelMap::changed
-- | Notifies of a change in the global accelerator map.
-- The path is also used as the detail for the signal,
-- so it is possible to connect to
-- changed::@accel_path@.
-- 
-- /Since: 2.4/
type AccelMapChangedCallback =
    T.Text
    -- ^ /@accelPath@/: the path of the accelerator that changed
    -> Word32
    -- ^ /@accelKey@/: the key value for the new accelerator
    -> [Gdk.Flags.ModifierType]
    -- ^ /@accelMods@/: the modifier mask for the new accelerator
    -> IO ()

type C_AccelMapChangedCallback =
    Ptr AccelMap ->                         -- object
    CString ->
    Word32 ->
    CUInt ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_AccelMapChangedCallback`.
foreign import ccall "wrapper"
    mk_AccelMapChangedCallback :: C_AccelMapChangedCallback -> IO (FunPtr C_AccelMapChangedCallback)

wrap_AccelMapChangedCallback :: 
    GObject a => (a -> AccelMapChangedCallback) ->
    C_AccelMapChangedCallback
wrap_AccelMapChangedCallback gi'cb gi'selfPtr accelPath accelKey accelMods _ = do
    accelPath' <- cstringToText accelPath
    let accelMods' = wordToGFlags accelMods
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  accelPath' accelKey accelMods'


-- | Connect a signal handler for the [changed](#signal:changed) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' accelMap #changed callback
-- @
-- 
-- This signal admits a optional parameter @detail@.
-- If it's not @Nothing@, we will connect to “@changed::detail@” instead.
-- 
onAccelMapChanged :: (IsAccelMap a, MonadIO m) => a -> P.Maybe T.Text -> ((?self :: a) => AccelMapChangedCallback) -> m SignalHandlerId
onAccelMapChanged obj detail cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_AccelMapChangedCallback wrapped
    wrapped'' <- mk_AccelMapChangedCallback wrapped'
    connectSignalFunPtr obj "changed" wrapped'' SignalConnectBefore detail

-- | Connect a signal handler for the [changed](#signal:changed) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' accelMap #changed callback
-- @
-- 
-- This signal admits a optional parameter @detail@.
-- If it's not @Nothing@, we will connect to “@changed::detail@” instead.
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterAccelMapChanged :: (IsAccelMap a, MonadIO m) => a -> P.Maybe T.Text -> ((?self :: a) => AccelMapChangedCallback) -> m SignalHandlerId
afterAccelMapChanged obj detail cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_AccelMapChangedCallback wrapped
    wrapped'' <- mk_AccelMapChangedCallback wrapped'
    connectSignalFunPtr obj "changed" wrapped'' SignalConnectAfter detail


#if defined(ENABLE_OVERLOADING)
data AccelMapChangedSignalInfo
instance SignalInfo AccelMapChangedSignalInfo where
    type HaskellCallbackType AccelMapChangedSignalInfo = AccelMapChangedCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_AccelMapChangedCallback cb
        cb'' <- mk_AccelMapChangedCallback cb'
        connectSignalFunPtr obj "changed" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.AccelMap::changed"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-AccelMap.html#g:signal:changed"})

#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList AccelMap
type instance O.AttributeList AccelMap = AccelMapAttributeList
type AccelMapAttributeList = ('[ ] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList AccelMap = AccelMapSignalList
type AccelMapSignalList = ('[ '("changed", AccelMapChangedSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo)] :: [(Symbol, *)])

#endif

-- method AccelMap::add_entry
-- method type : MemberFunction
-- Args: [ Arg
--           { argCName = "accel_path"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "valid accelerator path"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "accel_key"
--           , argType = TBasicType TUInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the accelerator key"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "accel_mods"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "ModifierType" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the accelerator modifiers"
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

foreign import ccall "gtk_accel_map_add_entry" gtk_accel_map_add_entry :: 
    CString ->                              -- accel_path : TBasicType TUTF8
    Word32 ->                               -- accel_key : TBasicType TUInt
    CUInt ->                                -- accel_mods : TInterface (Name {namespace = "Gdk", name = "ModifierType"})
    IO ()

-- | Registers a new accelerator with the global accelerator map.
-- This function should only be called once per /@accelPath@/
-- with the canonical /@accelKey@/ and /@accelMods@/ for this path.
-- To change the accelerator during runtime programatically, use
-- 'GI.Gtk.Objects.AccelMap.accelMapChangeEntry'.
-- 
-- Set /@accelKey@/ and /@accelMods@/ to 0 to request a removal of
-- the accelerator.
-- 
-- Note that /@accelPath@/ string will be stored in a @/GQuark/@. Therefore, if you
-- pass a static string, you can save some memory by interning it first with
-- 'GI.GLib.Functions.internStaticString'.
accelMapAddEntry ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    T.Text
    -- ^ /@accelPath@/: valid accelerator path
    -> Word32
    -- ^ /@accelKey@/: the accelerator key
    -> [Gdk.Flags.ModifierType]
    -- ^ /@accelMods@/: the accelerator modifiers
    -> m ()
accelMapAddEntry accelPath accelKey accelMods = liftIO $ do
    accelPath' <- textToCString accelPath
    let accelMods' = gflagsToWord accelMods
    gtk_accel_map_add_entry accelPath' accelKey accelMods'
    freeMem accelPath'
    return ()

#if defined(ENABLE_OVERLOADING)
#endif

-- method AccelMap::add_filter
-- method type : MemberFunction
-- Args: [ Arg
--           { argCName = "filter_pattern"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a pattern (see #GPatternSpec)"
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

foreign import ccall "gtk_accel_map_add_filter" gtk_accel_map_add_filter :: 
    CString ->                              -- filter_pattern : TBasicType TUTF8
    IO ()

-- | Adds a filter to the global list of accel path filters.
-- 
-- Accel map entries whose accel path matches one of the filters
-- are skipped by 'GI.Gtk.Objects.AccelMap.accelMapForeach'.
-- 
-- This function is intended for GTK+ modules that create their own
-- menus, but don’t want them to be saved into the applications accelerator
-- map dump.
accelMapAddFilter ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    T.Text
    -- ^ /@filterPattern@/: a pattern (see t'GI.GLib.Structs.PatternSpec.PatternSpec')
    -> m ()
accelMapAddFilter filterPattern = liftIO $ do
    filterPattern' <- textToCString filterPattern
    gtk_accel_map_add_filter filterPattern'
    freeMem filterPattern'
    return ()

#if defined(ENABLE_OVERLOADING)
#endif

-- method AccelMap::change_entry
-- method type : MemberFunction
-- Args: [ Arg
--           { argCName = "accel_path"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a valid accelerator path"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "accel_key"
--           , argType = TBasicType TUInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the new accelerator key"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "accel_mods"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "ModifierType" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the new accelerator modifiers"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "replace"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "%TRUE if other accelerators may be deleted upon conflicts"
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

foreign import ccall "gtk_accel_map_change_entry" gtk_accel_map_change_entry :: 
    CString ->                              -- accel_path : TBasicType TUTF8
    Word32 ->                               -- accel_key : TBasicType TUInt
    CUInt ->                                -- accel_mods : TInterface (Name {namespace = "Gdk", name = "ModifierType"})
    CInt ->                                 -- replace : TBasicType TBoolean
    IO CInt

-- | Changes the /@accelKey@/ and /@accelMods@/ currently associated with /@accelPath@/.
-- Due to conflicts with other accelerators, a change may not always be possible,
-- /@replace@/ indicates whether other accelerators may be deleted to resolve such
-- conflicts. A change will only occur if all conflicts could be resolved (which
-- might not be the case if conflicting accelerators are locked). Successful
-- changes are indicated by a 'P.True' return value.
-- 
-- Note that /@accelPath@/ string will be stored in a @/GQuark/@. Therefore, if you
-- pass a static string, you can save some memory by interning it first with
-- 'GI.GLib.Functions.internStaticString'.
accelMapChangeEntry ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    T.Text
    -- ^ /@accelPath@/: a valid accelerator path
    -> Word32
    -- ^ /@accelKey@/: the new accelerator key
    -> [Gdk.Flags.ModifierType]
    -- ^ /@accelMods@/: the new accelerator modifiers
    -> Bool
    -- ^ /@replace@/: 'P.True' if other accelerators may be deleted upon conflicts
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the accelerator could be changed, 'P.False' otherwise
accelMapChangeEntry accelPath accelKey accelMods replace = liftIO $ do
    accelPath' <- textToCString accelPath
    let accelMods' = gflagsToWord accelMods
    let replace' = (fromIntegral . fromEnum) replace
    result <- gtk_accel_map_change_entry accelPath' accelKey accelMods' replace'
    let result' = (/= 0) result
    freeMem accelPath'
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method AccelMap::foreach
-- method type : MemberFunction
-- Args: [ Arg
--           { argCName = "data"
--           , argType = TBasicType TPtr
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "data to be passed into @foreach_func"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "foreach_func"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "AccelMapForeach" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "function to be executed for each accel\n               map entry which is not filtered out"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeCall
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

foreign import ccall "gtk_accel_map_foreach" gtk_accel_map_foreach :: 
    Ptr () ->                               -- data : TBasicType TPtr
    FunPtr Gtk.Callbacks.C_AccelMapForeach -> -- foreach_func : TInterface (Name {namespace = "Gtk", name = "AccelMapForeach"})
    IO ()

-- | Loops over the entries in the accelerator map whose accel path
-- doesn’t match any of the filters added with 'GI.Gtk.Objects.AccelMap.accelMapAddFilter',
-- and execute /@foreachFunc@/ on each. The signature of /@foreachFunc@/ is
-- that of t'GI.Gtk.Callbacks.AccelMapForeach', the /@changed@/ parameter indicates whether
-- this accelerator was changed during runtime (thus, would need
-- saving during an accelerator map dump).
accelMapForeach ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    Ptr ()
    -- ^ /@data@/: data to be passed into /@foreachFunc@/
    -> Gtk.Callbacks.AccelMapForeach
    -- ^ /@foreachFunc@/: function to be executed for each accel
    --                map entry which is not filtered out
    -> m ()
accelMapForeach data_ foreachFunc = liftIO $ do
    foreachFunc' <- Gtk.Callbacks.mk_AccelMapForeach (Gtk.Callbacks.wrap_AccelMapForeach Nothing foreachFunc)
    gtk_accel_map_foreach data_ foreachFunc'
    safeFreeFunPtr $ castFunPtrToPtr foreachFunc'
    return ()

#if defined(ENABLE_OVERLOADING)
#endif

-- method AccelMap::foreach_unfiltered
-- method type : MemberFunction
-- Args: [ Arg
--           { argCName = "data"
--           , argType = TBasicType TPtr
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "data to be passed into @foreach_func"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "foreach_func"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "AccelMapForeach" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "function to be executed for each accel\n               map entry"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeCall
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

foreign import ccall "gtk_accel_map_foreach_unfiltered" gtk_accel_map_foreach_unfiltered :: 
    Ptr () ->                               -- data : TBasicType TPtr
    FunPtr Gtk.Callbacks.C_AccelMapForeach -> -- foreach_func : TInterface (Name {namespace = "Gtk", name = "AccelMapForeach"})
    IO ()

-- | Loops over all entries in the accelerator map, and execute
-- /@foreachFunc@/ on each. The signature of /@foreachFunc@/ is that of
-- t'GI.Gtk.Callbacks.AccelMapForeach', the /@changed@/ parameter indicates whether
-- this accelerator was changed during runtime (thus, would need
-- saving during an accelerator map dump).
accelMapForeachUnfiltered ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    Ptr ()
    -- ^ /@data@/: data to be passed into /@foreachFunc@/
    -> Gtk.Callbacks.AccelMapForeach
    -- ^ /@foreachFunc@/: function to be executed for each accel
    --                map entry
    -> m ()
accelMapForeachUnfiltered data_ foreachFunc = liftIO $ do
    foreachFunc' <- Gtk.Callbacks.mk_AccelMapForeach (Gtk.Callbacks.wrap_AccelMapForeach Nothing foreachFunc)
    gtk_accel_map_foreach_unfiltered data_ foreachFunc'
    safeFreeFunPtr $ castFunPtrToPtr foreachFunc'
    return ()

#if defined(ENABLE_OVERLOADING)
#endif

-- method AccelMap::get
-- method type : MemberFunction
-- Args: []
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "AccelMap" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_accel_map_get" gtk_accel_map_get :: 
    IO (Ptr AccelMap)

-- | Gets the singleton global t'GI.Gtk.Objects.AccelMap.AccelMap' object. This object
-- is useful only for notification of changes to the accelerator
-- map via the [changed](#g:signal:changed) signal; it isn’t a parameter to the
-- other accelerator map functions.
-- 
-- /Since: 2.4/
accelMapGet ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m AccelMap
    -- ^ __Returns:__ the global t'GI.Gtk.Objects.AccelMap.AccelMap' object
accelMapGet  = liftIO $ do
    result <- gtk_accel_map_get
    checkUnexpectedReturnNULL "accelMapGet" result
    result' <- (newObject AccelMap) result
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method AccelMap::load
-- method type : MemberFunction
-- Args: [ Arg
--           { argCName = "file_name"
--           , argType = TBasicType TFileName
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "a file containing accelerator specifications,\n  in the GLib file name encoding"
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

foreign import ccall "gtk_accel_map_load" gtk_accel_map_load :: 
    CString ->                              -- file_name : TBasicType TFileName
    IO ()

-- | Parses a file previously saved with 'GI.Gtk.Objects.AccelMap.accelMapSave' for
-- accelerator specifications, and propagates them accordingly.
accelMapLoad ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    [Char]
    -- ^ /@fileName@/: a file containing accelerator specifications,
    --   in the GLib file name encoding
    -> m ()
accelMapLoad fileName = liftIO $ do
    fileName' <- stringToCString fileName
    gtk_accel_map_load fileName'
    freeMem fileName'
    return ()

#if defined(ENABLE_OVERLOADING)
#endif

-- method AccelMap::load_fd
-- method type : MemberFunction
-- Args: [ Arg
--           { argCName = "fd"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a valid readable file descriptor"
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

foreign import ccall "gtk_accel_map_load_fd" gtk_accel_map_load_fd :: 
    Int32 ->                                -- fd : TBasicType TInt
    IO ()

-- | Filedescriptor variant of 'GI.Gtk.Objects.AccelMap.accelMapLoad'.
-- 
-- Note that the file descriptor will not be closed by this function.
accelMapLoadFd ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    Int32
    -- ^ /@fd@/: a valid readable file descriptor
    -> m ()
accelMapLoadFd fd = liftIO $ do
    gtk_accel_map_load_fd fd
    return ()

#if defined(ENABLE_OVERLOADING)
#endif

-- method AccelMap::load_scanner
-- method type : MemberFunction
-- Args: [ Arg
--           { argCName = "scanner"
--           , argType =
--               TInterface Name { namespace = "GLib" , name = "Scanner" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "a #GScanner which has already been provided with an input file"
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

foreign import ccall "gtk_accel_map_load_scanner" gtk_accel_map_load_scanner :: 
    Ptr GLib.Scanner.Scanner ->             -- scanner : TInterface (Name {namespace = "GLib", name = "Scanner"})
    IO ()

-- | t'GI.GLib.Structs.Scanner.Scanner' variant of 'GI.Gtk.Objects.AccelMap.accelMapLoad'.
accelMapLoadScanner ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    GLib.Scanner.Scanner
    -- ^ /@scanner@/: a t'GI.GLib.Structs.Scanner.Scanner' which has already been provided with an input file
    -> m ()
accelMapLoadScanner scanner = liftIO $ do
    scanner' <- unsafeManagedPtrGetPtr scanner
    gtk_accel_map_load_scanner scanner'
    touchManagedPtr scanner
    return ()

#if defined(ENABLE_OVERLOADING)
#endif

-- method AccelMap::lock_path
-- method type : MemberFunction
-- Args: [ Arg
--           { argCName = "accel_path"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a valid accelerator path"
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

foreign import ccall "gtk_accel_map_lock_path" gtk_accel_map_lock_path :: 
    CString ->                              -- accel_path : TBasicType TUTF8
    IO ()

-- | Locks the given accelerator path. If the accelerator map doesn’t yet contain
-- an entry for /@accelPath@/, a new one is created.
-- 
-- Locking an accelerator path prevents its accelerator from being changed
-- during runtime. A locked accelerator path can be unlocked by
-- 'GI.Gtk.Objects.AccelMap.accelMapUnlockPath'. Refer to 'GI.Gtk.Objects.AccelMap.accelMapChangeEntry'
-- for information about runtime accelerator changes.
-- 
-- If called more than once, /@accelPath@/ remains locked until
-- 'GI.Gtk.Objects.AccelMap.accelMapUnlockPath' has been called an equivalent number
-- of times.
-- 
-- Note that locking of individual accelerator paths is independent from
-- locking the t'GI.Gtk.Objects.AccelGroup.AccelGroup' containing them. For runtime accelerator
-- changes to be possible, both the accelerator path and its t'GI.Gtk.Objects.AccelGroup.AccelGroup'
-- have to be unlocked.
-- 
-- /Since: 2.4/
accelMapLockPath ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    T.Text
    -- ^ /@accelPath@/: a valid accelerator path
    -> m ()
accelMapLockPath accelPath = liftIO $ do
    accelPath' <- textToCString accelPath
    gtk_accel_map_lock_path accelPath'
    freeMem accelPath'
    return ()

#if defined(ENABLE_OVERLOADING)
#endif

-- method AccelMap::lookup_entry
-- method type : MemberFunction
-- Args: [ Arg
--           { argCName = "accel_path"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a valid accelerator path"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "key"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "AccelKey" }
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "the accelerator key to be filled in (optional)"
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

foreign import ccall "gtk_accel_map_lookup_entry" gtk_accel_map_lookup_entry :: 
    CString ->                              -- accel_path : TBasicType TUTF8
    Ptr Gtk.AccelKey.AccelKey ->            -- key : TInterface (Name {namespace = "Gtk", name = "AccelKey"})
    IO CInt

-- | Looks up the accelerator entry for /@accelPath@/ and fills in /@key@/.
accelMapLookupEntry ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    T.Text
    -- ^ /@accelPath@/: a valid accelerator path
    -> m ((Bool, Gtk.AccelKey.AccelKey))
    -- ^ __Returns:__ 'P.True' if /@accelPath@/ is known, 'P.False' otherwise
accelMapLookupEntry accelPath = liftIO $ do
    accelPath' <- textToCString accelPath
    key <- SP.callocBytes 12 :: IO (Ptr Gtk.AccelKey.AccelKey)
    result <- gtk_accel_map_lookup_entry accelPath' key
    let result' = (/= 0) result
    key' <- (wrapPtr Gtk.AccelKey.AccelKey) key
    freeMem accelPath'
    return (result', key')

#if defined(ENABLE_OVERLOADING)
#endif

-- method AccelMap::save
-- method type : MemberFunction
-- Args: [ Arg
--           { argCName = "file_name"
--           , argType = TBasicType TFileName
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the name of the file to contain\n  accelerator specifications, in the GLib file name encoding"
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

foreign import ccall "gtk_accel_map_save" gtk_accel_map_save :: 
    CString ->                              -- file_name : TBasicType TFileName
    IO ()

-- | Saves current accelerator specifications (accelerator path, key
-- and modifiers) to /@fileName@/.
-- The file is written in a format suitable to be read back in by
-- 'GI.Gtk.Objects.AccelMap.accelMapLoad'.
accelMapSave ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    [Char]
    -- ^ /@fileName@/: the name of the file to contain
    --   accelerator specifications, in the GLib file name encoding
    -> m ()
accelMapSave fileName = liftIO $ do
    fileName' <- stringToCString fileName
    gtk_accel_map_save fileName'
    freeMem fileName'
    return ()

#if defined(ENABLE_OVERLOADING)
#endif

-- method AccelMap::save_fd
-- method type : MemberFunction
-- Args: [ Arg
--           { argCName = "fd"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a valid writable file descriptor"
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

foreign import ccall "gtk_accel_map_save_fd" gtk_accel_map_save_fd :: 
    Int32 ->                                -- fd : TBasicType TInt
    IO ()

-- | Filedescriptor variant of 'GI.Gtk.Objects.AccelMap.accelMapSave'.
-- 
-- Note that the file descriptor will not be closed by this function.
accelMapSaveFd ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    Int32
    -- ^ /@fd@/: a valid writable file descriptor
    -> m ()
accelMapSaveFd fd = liftIO $ do
    gtk_accel_map_save_fd fd
    return ()

#if defined(ENABLE_OVERLOADING)
#endif

-- method AccelMap::unlock_path
-- method type : MemberFunction
-- Args: [ Arg
--           { argCName = "accel_path"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a valid accelerator path"
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

foreign import ccall "gtk_accel_map_unlock_path" gtk_accel_map_unlock_path :: 
    CString ->                              -- accel_path : TBasicType TUTF8
    IO ()

-- | Undoes the last call to 'GI.Gtk.Objects.AccelMap.accelMapLockPath' on this /@accelPath@/.
-- Refer to 'GI.Gtk.Objects.AccelMap.accelMapLockPath' for information about accelerator path locking.
-- 
-- /Since: 2.4/
accelMapUnlockPath ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    T.Text
    -- ^ /@accelPath@/: a valid accelerator path
    -> m ()
accelMapUnlockPath accelPath = liftIO $ do
    accelPath' <- textToCString accelPath
    gtk_accel_map_unlock_path accelPath'
    freeMem accelPath'
    return ()

#if defined(ENABLE_OVERLOADING)
#endif


