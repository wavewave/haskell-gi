{-# LANGUAGE ImplicitParams, RankNTypes, TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- t'GI.Gtk.Objects.CellRendererAccel.CellRendererAccel' displays a keyboard accelerator (i.e. a key
-- combination like @Control + a@). If the cell renderer is editable,
-- the accelerator can be changed by simply typing the new combination.
-- 
-- The t'GI.Gtk.Objects.CellRendererAccel.CellRendererAccel' cell renderer was added in GTK+ 2.10.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.CellRendererAccel
    ( 

-- * Exported types
    CellRendererAccel(..)                   ,
    IsCellRendererAccel                     ,
    toCellRendererAccel                     ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [activate]("GI.Gtk.Objects.CellRenderer#g:method:activate"), [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [isActivatable]("GI.Gtk.Objects.CellRenderer#g:method:isActivatable"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [render]("GI.Gtk.Objects.CellRenderer#g:method:render"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [startEditing]("GI.Gtk.Objects.CellRenderer#g:method:startEditing"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [stopEditing]("GI.Gtk.Objects.CellRenderer#g:method:stopEditing"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getAlignedArea]("GI.Gtk.Objects.CellRenderer#g:method:getAlignedArea"), [getAlignment]("GI.Gtk.Objects.CellRenderer#g:method:getAlignment"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getFixedSize]("GI.Gtk.Objects.CellRenderer#g:method:getFixedSize"), [getPadding]("GI.Gtk.Objects.CellRenderer#g:method:getPadding"), [getPreferredHeight]("GI.Gtk.Objects.CellRenderer#g:method:getPreferredHeight"), [getPreferredHeightForWidth]("GI.Gtk.Objects.CellRenderer#g:method:getPreferredHeightForWidth"), [getPreferredSize]("GI.Gtk.Objects.CellRenderer#g:method:getPreferredSize"), [getPreferredWidth]("GI.Gtk.Objects.CellRenderer#g:method:getPreferredWidth"), [getPreferredWidthForHeight]("GI.Gtk.Objects.CellRenderer#g:method:getPreferredWidthForHeight"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getRequestMode]("GI.Gtk.Objects.CellRenderer#g:method:getRequestMode"), [getSensitive]("GI.Gtk.Objects.CellRenderer#g:method:getSensitive"), [getSize]("GI.Gtk.Objects.CellRenderer#g:method:getSize"), [getState]("GI.Gtk.Objects.CellRenderer#g:method:getState"), [getVisible]("GI.Gtk.Objects.CellRenderer#g:method:getVisible").
-- 
-- ==== Setters
-- [setAlignment]("GI.Gtk.Objects.CellRenderer#g:method:setAlignment"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setFixedHeightFromFont]("GI.Gtk.Objects.CellRendererText#g:method:setFixedHeightFromFont"), [setFixedSize]("GI.Gtk.Objects.CellRenderer#g:method:setFixedSize"), [setPadding]("GI.Gtk.Objects.CellRenderer#g:method:setPadding"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setSensitive]("GI.Gtk.Objects.CellRenderer#g:method:setSensitive"), [setVisible]("GI.Gtk.Objects.CellRenderer#g:method:setVisible").

#if defined(ENABLE_OVERLOADING)
    ResolveCellRendererAccelMethod          ,
#endif

-- ** new #method:new#

    cellRendererAccelNew                    ,




 -- * Properties


-- ** accelKey #attr:accelKey#
-- | The keyval of the accelerator.
-- 
-- /Since: 2.10/

#if defined(ENABLE_OVERLOADING)
    CellRendererAccelAccelKeyPropertyInfo   ,
#endif
#if defined(ENABLE_OVERLOADING)
    cellRendererAccelAccelKey               ,
#endif
    constructCellRendererAccelAccelKey      ,
    getCellRendererAccelAccelKey            ,
    setCellRendererAccelAccelKey            ,


-- ** accelMode #attr:accelMode#
-- | Determines if the edited accelerators are GTK+ accelerators. If
-- they are, consumed modifiers are suppressed, only accelerators
-- accepted by GTK+ are allowed, and the accelerators are rendered
-- in the same way as they are in menus.
-- 
-- /Since: 2.10/

#if defined(ENABLE_OVERLOADING)
    CellRendererAccelAccelModePropertyInfo  ,
#endif
#if defined(ENABLE_OVERLOADING)
    cellRendererAccelAccelMode              ,
#endif
    constructCellRendererAccelAccelMode     ,
    getCellRendererAccelAccelMode           ,
    setCellRendererAccelAccelMode           ,


-- ** accelMods #attr:accelMods#
-- | The modifier mask of the accelerator.
-- 
-- /Since: 2.10/

#if defined(ENABLE_OVERLOADING)
    CellRendererAccelAccelModsPropertyInfo  ,
#endif
#if defined(ENABLE_OVERLOADING)
    cellRendererAccelAccelMods              ,
#endif
    constructCellRendererAccelAccelMods     ,
    getCellRendererAccelAccelMods           ,
    setCellRendererAccelAccelMods           ,


-- ** keycode #attr:keycode#
-- | The hardware keycode of the accelerator. Note that the hardware keycode is
-- only relevant if the key does not have a keyval. Normally, the keyboard
-- configuration should assign keyvals to all keys.
-- 
-- /Since: 2.10/

#if defined(ENABLE_OVERLOADING)
    CellRendererAccelKeycodePropertyInfo    ,
#endif
#if defined(ENABLE_OVERLOADING)
    cellRendererAccelKeycode                ,
#endif
    constructCellRendererAccelKeycode       ,
    getCellRendererAccelKeycode             ,
    setCellRendererAccelKeycode             ,




 -- * Signals


-- ** accelCleared #signal:accelCleared#

    CellRendererAccelAccelClearedCallback   ,
#if defined(ENABLE_OVERLOADING)
    CellRendererAccelAccelClearedSignalInfo ,
#endif
    afterCellRendererAccelAccelCleared      ,
    onCellRendererAccelAccelCleared         ,


-- ** accelEdited #signal:accelEdited#

    CellRendererAccelAccelEditedCallback    ,
#if defined(ENABLE_OVERLOADING)
    CellRendererAccelAccelEditedSignalInfo  ,
#endif
    afterCellRendererAccelAccelEdited       ,
    onCellRendererAccelAccelEdited          ,




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
import {-# SOURCE #-} qualified GI.Gtk.Enums as Gtk.Enums
import {-# SOURCE #-} qualified GI.Gtk.Objects.CellRenderer as Gtk.CellRenderer
import {-# SOURCE #-} qualified GI.Gtk.Objects.CellRendererText as Gtk.CellRendererText

-- | Memory-managed wrapper type.
newtype CellRendererAccel = CellRendererAccel (SP.ManagedPtr CellRendererAccel)
    deriving (Eq)

instance SP.ManagedPtrNewtype CellRendererAccel where
    toManagedPtr (CellRendererAccel p) = p

foreign import ccall "gtk_cell_renderer_accel_get_type"
    c_gtk_cell_renderer_accel_get_type :: IO B.Types.GType

instance B.Types.TypedObject CellRendererAccel where
    glibType = c_gtk_cell_renderer_accel_get_type

instance B.Types.GObject CellRendererAccel

-- | Type class for types which can be safely cast to `CellRendererAccel`, for instance with `toCellRendererAccel`.
class (SP.GObject o, O.IsDescendantOf CellRendererAccel o) => IsCellRendererAccel o
instance (SP.GObject o, O.IsDescendantOf CellRendererAccel o) => IsCellRendererAccel o

instance O.HasParentTypes CellRendererAccel
type instance O.ParentTypes CellRendererAccel = '[Gtk.CellRendererText.CellRendererText, Gtk.CellRenderer.CellRenderer, GObject.Object.Object]

-- | Cast to `CellRendererAccel`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toCellRendererAccel :: (MIO.MonadIO m, IsCellRendererAccel o) => o -> m CellRendererAccel
toCellRendererAccel = MIO.liftIO . B.ManagedPtr.unsafeCastTo CellRendererAccel

-- | Convert 'CellRendererAccel' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe CellRendererAccel) where
    gvalueGType_ = c_gtk_cell_renderer_accel_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr CellRendererAccel)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr CellRendererAccel)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject CellRendererAccel ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveCellRendererAccelMethod (t :: Symbol) (o :: *) :: * where
    ResolveCellRendererAccelMethod "activate" o = Gtk.CellRenderer.CellRendererActivateMethodInfo
    ResolveCellRendererAccelMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveCellRendererAccelMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveCellRendererAccelMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveCellRendererAccelMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveCellRendererAccelMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveCellRendererAccelMethod "isActivatable" o = Gtk.CellRenderer.CellRendererIsActivatableMethodInfo
    ResolveCellRendererAccelMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveCellRendererAccelMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveCellRendererAccelMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveCellRendererAccelMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveCellRendererAccelMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveCellRendererAccelMethod "render" o = Gtk.CellRenderer.CellRendererRenderMethodInfo
    ResolveCellRendererAccelMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveCellRendererAccelMethod "startEditing" o = Gtk.CellRenderer.CellRendererStartEditingMethodInfo
    ResolveCellRendererAccelMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveCellRendererAccelMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveCellRendererAccelMethod "stopEditing" o = Gtk.CellRenderer.CellRendererStopEditingMethodInfo
    ResolveCellRendererAccelMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveCellRendererAccelMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveCellRendererAccelMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveCellRendererAccelMethod "getAlignedArea" o = Gtk.CellRenderer.CellRendererGetAlignedAreaMethodInfo
    ResolveCellRendererAccelMethod "getAlignment" o = Gtk.CellRenderer.CellRendererGetAlignmentMethodInfo
    ResolveCellRendererAccelMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveCellRendererAccelMethod "getFixedSize" o = Gtk.CellRenderer.CellRendererGetFixedSizeMethodInfo
    ResolveCellRendererAccelMethod "getPadding" o = Gtk.CellRenderer.CellRendererGetPaddingMethodInfo
    ResolveCellRendererAccelMethod "getPreferredHeight" o = Gtk.CellRenderer.CellRendererGetPreferredHeightMethodInfo
    ResolveCellRendererAccelMethod "getPreferredHeightForWidth" o = Gtk.CellRenderer.CellRendererGetPreferredHeightForWidthMethodInfo
    ResolveCellRendererAccelMethod "getPreferredSize" o = Gtk.CellRenderer.CellRendererGetPreferredSizeMethodInfo
    ResolveCellRendererAccelMethod "getPreferredWidth" o = Gtk.CellRenderer.CellRendererGetPreferredWidthMethodInfo
    ResolveCellRendererAccelMethod "getPreferredWidthForHeight" o = Gtk.CellRenderer.CellRendererGetPreferredWidthForHeightMethodInfo
    ResolveCellRendererAccelMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveCellRendererAccelMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveCellRendererAccelMethod "getRequestMode" o = Gtk.CellRenderer.CellRendererGetRequestModeMethodInfo
    ResolveCellRendererAccelMethod "getSensitive" o = Gtk.CellRenderer.CellRendererGetSensitiveMethodInfo
    ResolveCellRendererAccelMethod "getSize" o = Gtk.CellRenderer.CellRendererGetSizeMethodInfo
    ResolveCellRendererAccelMethod "getState" o = Gtk.CellRenderer.CellRendererGetStateMethodInfo
    ResolveCellRendererAccelMethod "getVisible" o = Gtk.CellRenderer.CellRendererGetVisibleMethodInfo
    ResolveCellRendererAccelMethod "setAlignment" o = Gtk.CellRenderer.CellRendererSetAlignmentMethodInfo
    ResolveCellRendererAccelMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveCellRendererAccelMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveCellRendererAccelMethod "setFixedHeightFromFont" o = Gtk.CellRendererText.CellRendererTextSetFixedHeightFromFontMethodInfo
    ResolveCellRendererAccelMethod "setFixedSize" o = Gtk.CellRenderer.CellRendererSetFixedSizeMethodInfo
    ResolveCellRendererAccelMethod "setPadding" o = Gtk.CellRenderer.CellRendererSetPaddingMethodInfo
    ResolveCellRendererAccelMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveCellRendererAccelMethod "setSensitive" o = Gtk.CellRenderer.CellRendererSetSensitiveMethodInfo
    ResolveCellRendererAccelMethod "setVisible" o = Gtk.CellRenderer.CellRendererSetVisibleMethodInfo
    ResolveCellRendererAccelMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveCellRendererAccelMethod t CellRendererAccel, O.OverloadedMethod info CellRendererAccel p) => OL.IsLabel t (CellRendererAccel -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveCellRendererAccelMethod t CellRendererAccel, O.OverloadedMethod info CellRendererAccel p, R.HasField t CellRendererAccel p) => R.HasField t CellRendererAccel p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveCellRendererAccelMethod t CellRendererAccel, O.OverloadedMethodInfo info CellRendererAccel) => OL.IsLabel t (O.MethodProxy info CellRendererAccel) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- signal CellRendererAccel::accel-cleared
-- | Gets emitted when the user has removed the accelerator.
-- 
-- /Since: 2.10/
type CellRendererAccelAccelClearedCallback =
    T.Text
    -- ^ /@pathString@/: the path identifying the row of the edited cell
    -> IO ()

type C_CellRendererAccelAccelClearedCallback =
    Ptr CellRendererAccel ->                -- object
    CString ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_CellRendererAccelAccelClearedCallback`.
foreign import ccall "wrapper"
    mk_CellRendererAccelAccelClearedCallback :: C_CellRendererAccelAccelClearedCallback -> IO (FunPtr C_CellRendererAccelAccelClearedCallback)

wrap_CellRendererAccelAccelClearedCallback :: 
    GObject a => (a -> CellRendererAccelAccelClearedCallback) ->
    C_CellRendererAccelAccelClearedCallback
wrap_CellRendererAccelAccelClearedCallback gi'cb gi'selfPtr pathString _ = do
    pathString' <- cstringToText pathString
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  pathString'


-- | Connect a signal handler for the [accelCleared](#signal:accelCleared) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' cellRendererAccel #accelCleared callback
-- @
-- 
-- 
onCellRendererAccelAccelCleared :: (IsCellRendererAccel a, MonadIO m) => a -> ((?self :: a) => CellRendererAccelAccelClearedCallback) -> m SignalHandlerId
onCellRendererAccelAccelCleared obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_CellRendererAccelAccelClearedCallback wrapped
    wrapped'' <- mk_CellRendererAccelAccelClearedCallback wrapped'
    connectSignalFunPtr obj "accel-cleared" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [accelCleared](#signal:accelCleared) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' cellRendererAccel #accelCleared callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterCellRendererAccelAccelCleared :: (IsCellRendererAccel a, MonadIO m) => a -> ((?self :: a) => CellRendererAccelAccelClearedCallback) -> m SignalHandlerId
afterCellRendererAccelAccelCleared obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_CellRendererAccelAccelClearedCallback wrapped
    wrapped'' <- mk_CellRendererAccelAccelClearedCallback wrapped'
    connectSignalFunPtr obj "accel-cleared" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data CellRendererAccelAccelClearedSignalInfo
instance SignalInfo CellRendererAccelAccelClearedSignalInfo where
    type HaskellCallbackType CellRendererAccelAccelClearedSignalInfo = CellRendererAccelAccelClearedCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_CellRendererAccelAccelClearedCallback cb
        cb'' <- mk_CellRendererAccelAccelClearedCallback cb'
        connectSignalFunPtr obj "accel-cleared" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRendererAccel::accel-cleared"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRendererAccel.html#g:signal:accelCleared"})

#endif

-- signal CellRendererAccel::accel-edited
-- | Gets emitted when the user has selected a new accelerator.
-- 
-- /Since: 2.10/
type CellRendererAccelAccelEditedCallback =
    T.Text
    -- ^ /@pathString@/: the path identifying the row of the edited cell
    -> Word32
    -- ^ /@accelKey@/: the new accelerator keyval
    -> [Gdk.Flags.ModifierType]
    -- ^ /@accelMods@/: the new acclerator modifier mask
    -> Word32
    -- ^ /@hardwareKeycode@/: the keycode of the new accelerator
    -> IO ()

type C_CellRendererAccelAccelEditedCallback =
    Ptr CellRendererAccel ->                -- object
    CString ->
    Word32 ->
    CUInt ->
    Word32 ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_CellRendererAccelAccelEditedCallback`.
foreign import ccall "wrapper"
    mk_CellRendererAccelAccelEditedCallback :: C_CellRendererAccelAccelEditedCallback -> IO (FunPtr C_CellRendererAccelAccelEditedCallback)

wrap_CellRendererAccelAccelEditedCallback :: 
    GObject a => (a -> CellRendererAccelAccelEditedCallback) ->
    C_CellRendererAccelAccelEditedCallback
wrap_CellRendererAccelAccelEditedCallback gi'cb gi'selfPtr pathString accelKey accelMods hardwareKeycode _ = do
    pathString' <- cstringToText pathString
    let accelMods' = wordToGFlags accelMods
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  pathString' accelKey accelMods' hardwareKeycode


-- | Connect a signal handler for the [accelEdited](#signal:accelEdited) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' cellRendererAccel #accelEdited callback
-- @
-- 
-- 
onCellRendererAccelAccelEdited :: (IsCellRendererAccel a, MonadIO m) => a -> ((?self :: a) => CellRendererAccelAccelEditedCallback) -> m SignalHandlerId
onCellRendererAccelAccelEdited obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_CellRendererAccelAccelEditedCallback wrapped
    wrapped'' <- mk_CellRendererAccelAccelEditedCallback wrapped'
    connectSignalFunPtr obj "accel-edited" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [accelEdited](#signal:accelEdited) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' cellRendererAccel #accelEdited callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterCellRendererAccelAccelEdited :: (IsCellRendererAccel a, MonadIO m) => a -> ((?self :: a) => CellRendererAccelAccelEditedCallback) -> m SignalHandlerId
afterCellRendererAccelAccelEdited obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_CellRendererAccelAccelEditedCallback wrapped
    wrapped'' <- mk_CellRendererAccelAccelEditedCallback wrapped'
    connectSignalFunPtr obj "accel-edited" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data CellRendererAccelAccelEditedSignalInfo
instance SignalInfo CellRendererAccelAccelEditedSignalInfo where
    type HaskellCallbackType CellRendererAccelAccelEditedSignalInfo = CellRendererAccelAccelEditedCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_CellRendererAccelAccelEditedCallback cb
        cb'' <- mk_CellRendererAccelAccelEditedCallback cb'
        connectSignalFunPtr obj "accel-edited" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRendererAccel::accel-edited"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRendererAccel.html#g:signal:accelEdited"})

#endif

-- VVV Prop "accel-key"
   -- Type: TBasicType TUInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@accel-key@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' cellRendererAccel #accelKey
-- @
getCellRendererAccelAccelKey :: (MonadIO m, IsCellRendererAccel o) => o -> m Word32
getCellRendererAccelAccelKey obj = MIO.liftIO $ B.Properties.getObjectPropertyUInt32 obj "accel-key"

-- | Set the value of the “@accel-key@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' cellRendererAccel [ #accelKey 'Data.GI.Base.Attributes.:=' value ]
-- @
setCellRendererAccelAccelKey :: (MonadIO m, IsCellRendererAccel o) => o -> Word32 -> m ()
setCellRendererAccelAccelKey obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyUInt32 obj "accel-key" val

-- | Construct a `GValueConstruct` with valid value for the “@accel-key@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructCellRendererAccelAccelKey :: (IsCellRendererAccel o, MIO.MonadIO m) => Word32 -> m (GValueConstruct o)
constructCellRendererAccelAccelKey val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyUInt32 "accel-key" val

#if defined(ENABLE_OVERLOADING)
data CellRendererAccelAccelKeyPropertyInfo
instance AttrInfo CellRendererAccelAccelKeyPropertyInfo where
    type AttrAllowedOps CellRendererAccelAccelKeyPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint CellRendererAccelAccelKeyPropertyInfo = IsCellRendererAccel
    type AttrSetTypeConstraint CellRendererAccelAccelKeyPropertyInfo = (~) Word32
    type AttrTransferTypeConstraint CellRendererAccelAccelKeyPropertyInfo = (~) Word32
    type AttrTransferType CellRendererAccelAccelKeyPropertyInfo = Word32
    type AttrGetType CellRendererAccelAccelKeyPropertyInfo = Word32
    type AttrLabel CellRendererAccelAccelKeyPropertyInfo = "accel-key"
    type AttrOrigin CellRendererAccelAccelKeyPropertyInfo = CellRendererAccel
    attrGet = getCellRendererAccelAccelKey
    attrSet = setCellRendererAccelAccelKey
    attrTransfer _ v = do
        return v
    attrConstruct = constructCellRendererAccelAccelKey
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRendererAccel.accelKey"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRendererAccel.html#g:attr:accelKey"
        })
#endif

-- VVV Prop "accel-mode"
   -- Type: TInterface (Name {namespace = "Gtk", name = "CellRendererAccelMode"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@accel-mode@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' cellRendererAccel #accelMode
-- @
getCellRendererAccelAccelMode :: (MonadIO m, IsCellRendererAccel o) => o -> m Gtk.Enums.CellRendererAccelMode
getCellRendererAccelAccelMode obj = MIO.liftIO $ B.Properties.getObjectPropertyEnum obj "accel-mode"

-- | Set the value of the “@accel-mode@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' cellRendererAccel [ #accelMode 'Data.GI.Base.Attributes.:=' value ]
-- @
setCellRendererAccelAccelMode :: (MonadIO m, IsCellRendererAccel o) => o -> Gtk.Enums.CellRendererAccelMode -> m ()
setCellRendererAccelAccelMode obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyEnum obj "accel-mode" val

-- | Construct a `GValueConstruct` with valid value for the “@accel-mode@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructCellRendererAccelAccelMode :: (IsCellRendererAccel o, MIO.MonadIO m) => Gtk.Enums.CellRendererAccelMode -> m (GValueConstruct o)
constructCellRendererAccelAccelMode val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyEnum "accel-mode" val

#if defined(ENABLE_OVERLOADING)
data CellRendererAccelAccelModePropertyInfo
instance AttrInfo CellRendererAccelAccelModePropertyInfo where
    type AttrAllowedOps CellRendererAccelAccelModePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint CellRendererAccelAccelModePropertyInfo = IsCellRendererAccel
    type AttrSetTypeConstraint CellRendererAccelAccelModePropertyInfo = (~) Gtk.Enums.CellRendererAccelMode
    type AttrTransferTypeConstraint CellRendererAccelAccelModePropertyInfo = (~) Gtk.Enums.CellRendererAccelMode
    type AttrTransferType CellRendererAccelAccelModePropertyInfo = Gtk.Enums.CellRendererAccelMode
    type AttrGetType CellRendererAccelAccelModePropertyInfo = Gtk.Enums.CellRendererAccelMode
    type AttrLabel CellRendererAccelAccelModePropertyInfo = "accel-mode"
    type AttrOrigin CellRendererAccelAccelModePropertyInfo = CellRendererAccel
    attrGet = getCellRendererAccelAccelMode
    attrSet = setCellRendererAccelAccelMode
    attrTransfer _ v = do
        return v
    attrConstruct = constructCellRendererAccelAccelMode
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRendererAccel.accelMode"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRendererAccel.html#g:attr:accelMode"
        })
#endif

-- VVV Prop "accel-mods"
   -- Type: TInterface (Name {namespace = "Gdk", name = "ModifierType"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@accel-mods@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' cellRendererAccel #accelMods
-- @
getCellRendererAccelAccelMods :: (MonadIO m, IsCellRendererAccel o) => o -> m [Gdk.Flags.ModifierType]
getCellRendererAccelAccelMods obj = MIO.liftIO $ B.Properties.getObjectPropertyFlags obj "accel-mods"

-- | Set the value of the “@accel-mods@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' cellRendererAccel [ #accelMods 'Data.GI.Base.Attributes.:=' value ]
-- @
setCellRendererAccelAccelMods :: (MonadIO m, IsCellRendererAccel o) => o -> [Gdk.Flags.ModifierType] -> m ()
setCellRendererAccelAccelMods obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyFlags obj "accel-mods" val

-- | Construct a `GValueConstruct` with valid value for the “@accel-mods@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructCellRendererAccelAccelMods :: (IsCellRendererAccel o, MIO.MonadIO m) => [Gdk.Flags.ModifierType] -> m (GValueConstruct o)
constructCellRendererAccelAccelMods val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyFlags "accel-mods" val

#if defined(ENABLE_OVERLOADING)
data CellRendererAccelAccelModsPropertyInfo
instance AttrInfo CellRendererAccelAccelModsPropertyInfo where
    type AttrAllowedOps CellRendererAccelAccelModsPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint CellRendererAccelAccelModsPropertyInfo = IsCellRendererAccel
    type AttrSetTypeConstraint CellRendererAccelAccelModsPropertyInfo = (~) [Gdk.Flags.ModifierType]
    type AttrTransferTypeConstraint CellRendererAccelAccelModsPropertyInfo = (~) [Gdk.Flags.ModifierType]
    type AttrTransferType CellRendererAccelAccelModsPropertyInfo = [Gdk.Flags.ModifierType]
    type AttrGetType CellRendererAccelAccelModsPropertyInfo = [Gdk.Flags.ModifierType]
    type AttrLabel CellRendererAccelAccelModsPropertyInfo = "accel-mods"
    type AttrOrigin CellRendererAccelAccelModsPropertyInfo = CellRendererAccel
    attrGet = getCellRendererAccelAccelMods
    attrSet = setCellRendererAccelAccelMods
    attrTransfer _ v = do
        return v
    attrConstruct = constructCellRendererAccelAccelMods
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRendererAccel.accelMods"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRendererAccel.html#g:attr:accelMods"
        })
#endif

-- VVV Prop "keycode"
   -- Type: TBasicType TUInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@keycode@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' cellRendererAccel #keycode
-- @
getCellRendererAccelKeycode :: (MonadIO m, IsCellRendererAccel o) => o -> m Word32
getCellRendererAccelKeycode obj = MIO.liftIO $ B.Properties.getObjectPropertyUInt32 obj "keycode"

-- | Set the value of the “@keycode@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' cellRendererAccel [ #keycode 'Data.GI.Base.Attributes.:=' value ]
-- @
setCellRendererAccelKeycode :: (MonadIO m, IsCellRendererAccel o) => o -> Word32 -> m ()
setCellRendererAccelKeycode obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyUInt32 obj "keycode" val

-- | Construct a `GValueConstruct` with valid value for the “@keycode@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructCellRendererAccelKeycode :: (IsCellRendererAccel o, MIO.MonadIO m) => Word32 -> m (GValueConstruct o)
constructCellRendererAccelKeycode val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyUInt32 "keycode" val

#if defined(ENABLE_OVERLOADING)
data CellRendererAccelKeycodePropertyInfo
instance AttrInfo CellRendererAccelKeycodePropertyInfo where
    type AttrAllowedOps CellRendererAccelKeycodePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint CellRendererAccelKeycodePropertyInfo = IsCellRendererAccel
    type AttrSetTypeConstraint CellRendererAccelKeycodePropertyInfo = (~) Word32
    type AttrTransferTypeConstraint CellRendererAccelKeycodePropertyInfo = (~) Word32
    type AttrTransferType CellRendererAccelKeycodePropertyInfo = Word32
    type AttrGetType CellRendererAccelKeycodePropertyInfo = Word32
    type AttrLabel CellRendererAccelKeycodePropertyInfo = "keycode"
    type AttrOrigin CellRendererAccelKeycodePropertyInfo = CellRendererAccel
    attrGet = getCellRendererAccelKeycode
    attrSet = setCellRendererAccelKeycode
    attrTransfer _ v = do
        return v
    attrConstruct = constructCellRendererAccelKeycode
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRendererAccel.keycode"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRendererAccel.html#g:attr:keycode"
        })
#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList CellRendererAccel
type instance O.AttributeList CellRendererAccel = CellRendererAccelAttributeList
type CellRendererAccelAttributeList = ('[ '("accelKey", CellRendererAccelAccelKeyPropertyInfo), '("accelMode", CellRendererAccelAccelModePropertyInfo), '("accelMods", CellRendererAccelAccelModsPropertyInfo), '("alignSet", Gtk.CellRendererText.CellRendererTextAlignSetPropertyInfo), '("alignment", Gtk.CellRendererText.CellRendererTextAlignmentPropertyInfo), '("attributes", Gtk.CellRendererText.CellRendererTextAttributesPropertyInfo), '("background", Gtk.CellRendererText.CellRendererTextBackgroundPropertyInfo), '("backgroundGdk", Gtk.CellRendererText.CellRendererTextBackgroundGdkPropertyInfo), '("backgroundRgba", Gtk.CellRendererText.CellRendererTextBackgroundRgbaPropertyInfo), '("backgroundSet", Gtk.CellRendererText.CellRendererTextBackgroundSetPropertyInfo), '("cellBackground", Gtk.CellRenderer.CellRendererCellBackgroundPropertyInfo), '("cellBackgroundGdk", Gtk.CellRenderer.CellRendererCellBackgroundGdkPropertyInfo), '("cellBackgroundRgba", Gtk.CellRenderer.CellRendererCellBackgroundRgbaPropertyInfo), '("cellBackgroundSet", Gtk.CellRenderer.CellRendererCellBackgroundSetPropertyInfo), '("editable", Gtk.CellRendererText.CellRendererTextEditablePropertyInfo), '("editableSet", Gtk.CellRendererText.CellRendererTextEditableSetPropertyInfo), '("editing", Gtk.CellRenderer.CellRendererEditingPropertyInfo), '("ellipsize", Gtk.CellRendererText.CellRendererTextEllipsizePropertyInfo), '("ellipsizeSet", Gtk.CellRendererText.CellRendererTextEllipsizeSetPropertyInfo), '("family", Gtk.CellRendererText.CellRendererTextFamilyPropertyInfo), '("familySet", Gtk.CellRendererText.CellRendererTextFamilySetPropertyInfo), '("font", Gtk.CellRendererText.CellRendererTextFontPropertyInfo), '("fontDesc", Gtk.CellRendererText.CellRendererTextFontDescPropertyInfo), '("foreground", Gtk.CellRendererText.CellRendererTextForegroundPropertyInfo), '("foregroundGdk", Gtk.CellRendererText.CellRendererTextForegroundGdkPropertyInfo), '("foregroundRgba", Gtk.CellRendererText.CellRendererTextForegroundRgbaPropertyInfo), '("foregroundSet", Gtk.CellRendererText.CellRendererTextForegroundSetPropertyInfo), '("height", Gtk.CellRenderer.CellRendererHeightPropertyInfo), '("isExpanded", Gtk.CellRenderer.CellRendererIsExpandedPropertyInfo), '("isExpander", Gtk.CellRenderer.CellRendererIsExpanderPropertyInfo), '("keycode", CellRendererAccelKeycodePropertyInfo), '("language", Gtk.CellRendererText.CellRendererTextLanguagePropertyInfo), '("languageSet", Gtk.CellRendererText.CellRendererTextLanguageSetPropertyInfo), '("markup", Gtk.CellRendererText.CellRendererTextMarkupPropertyInfo), '("maxWidthChars", Gtk.CellRendererText.CellRendererTextMaxWidthCharsPropertyInfo), '("mode", Gtk.CellRenderer.CellRendererModePropertyInfo), '("placeholderText", Gtk.CellRendererText.CellRendererTextPlaceholderTextPropertyInfo), '("rise", Gtk.CellRendererText.CellRendererTextRisePropertyInfo), '("riseSet", Gtk.CellRendererText.CellRendererTextRiseSetPropertyInfo), '("scale", Gtk.CellRendererText.CellRendererTextScalePropertyInfo), '("scaleSet", Gtk.CellRendererText.CellRendererTextScaleSetPropertyInfo), '("sensitive", Gtk.CellRenderer.CellRendererSensitivePropertyInfo), '("singleParagraphMode", Gtk.CellRendererText.CellRendererTextSingleParagraphModePropertyInfo), '("size", Gtk.CellRendererText.CellRendererTextSizePropertyInfo), '("sizePoints", Gtk.CellRendererText.CellRendererTextSizePointsPropertyInfo), '("sizeSet", Gtk.CellRendererText.CellRendererTextSizeSetPropertyInfo), '("stretch", Gtk.CellRendererText.CellRendererTextStretchPropertyInfo), '("stretchSet", Gtk.CellRendererText.CellRendererTextStretchSetPropertyInfo), '("strikethrough", Gtk.CellRendererText.CellRendererTextStrikethroughPropertyInfo), '("strikethroughSet", Gtk.CellRendererText.CellRendererTextStrikethroughSetPropertyInfo), '("style", Gtk.CellRendererText.CellRendererTextStylePropertyInfo), '("styleSet", Gtk.CellRendererText.CellRendererTextStyleSetPropertyInfo), '("text", Gtk.CellRendererText.CellRendererTextTextPropertyInfo), '("underline", Gtk.CellRendererText.CellRendererTextUnderlinePropertyInfo), '("underlineSet", Gtk.CellRendererText.CellRendererTextUnderlineSetPropertyInfo), '("variant", Gtk.CellRendererText.CellRendererTextVariantPropertyInfo), '("variantSet", Gtk.CellRendererText.CellRendererTextVariantSetPropertyInfo), '("visible", Gtk.CellRenderer.CellRendererVisiblePropertyInfo), '("weight", Gtk.CellRendererText.CellRendererTextWeightPropertyInfo), '("weightSet", Gtk.CellRendererText.CellRendererTextWeightSetPropertyInfo), '("width", Gtk.CellRenderer.CellRendererWidthPropertyInfo), '("widthChars", Gtk.CellRendererText.CellRendererTextWidthCharsPropertyInfo), '("wrapMode", Gtk.CellRendererText.CellRendererTextWrapModePropertyInfo), '("wrapWidth", Gtk.CellRendererText.CellRendererTextWrapWidthPropertyInfo), '("xalign", Gtk.CellRenderer.CellRendererXalignPropertyInfo), '("xpad", Gtk.CellRenderer.CellRendererXpadPropertyInfo), '("yalign", Gtk.CellRenderer.CellRendererYalignPropertyInfo), '("ypad", Gtk.CellRenderer.CellRendererYpadPropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
cellRendererAccelAccelKey :: AttrLabelProxy "accelKey"
cellRendererAccelAccelKey = AttrLabelProxy

cellRendererAccelAccelMode :: AttrLabelProxy "accelMode"
cellRendererAccelAccelMode = AttrLabelProxy

cellRendererAccelAccelMods :: AttrLabelProxy "accelMods"
cellRendererAccelAccelMods = AttrLabelProxy

cellRendererAccelKeycode :: AttrLabelProxy "keycode"
cellRendererAccelKeycode = AttrLabelProxy

#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList CellRendererAccel = CellRendererAccelSignalList
type CellRendererAccelSignalList = ('[ '("accelCleared", CellRendererAccelAccelClearedSignalInfo), '("accelEdited", CellRendererAccelAccelEditedSignalInfo), '("edited", Gtk.CellRendererText.CellRendererTextEditedSignalInfo), '("editingCanceled", Gtk.CellRenderer.CellRendererEditingCanceledSignalInfo), '("editingStarted", Gtk.CellRenderer.CellRendererEditingStartedSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo)] :: [(Symbol, *)])

#endif

-- method CellRendererAccel::new
-- method type : Constructor
-- Args: []
-- Lengths: []
-- returnType: Just
--               (TInterface
--                  Name { namespace = "Gtk" , name = "CellRendererAccel" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_cell_renderer_accel_new" gtk_cell_renderer_accel_new :: 
    IO (Ptr CellRendererAccel)

-- | Creates a new t'GI.Gtk.Objects.CellRendererAccel.CellRendererAccel'.
-- 
-- /Since: 2.10/
cellRendererAccelNew ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m CellRendererAccel
    -- ^ __Returns:__ the new cell renderer
cellRendererAccelNew  = liftIO $ do
    result <- gtk_cell_renderer_accel_new
    checkUnexpectedReturnNULL "cellRendererAccelNew" result
    result' <- (newObject CellRendererAccel) result
    return result'

#if defined(ENABLE_OVERLOADING)
#endif


