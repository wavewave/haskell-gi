{-# LANGUAGE ImplicitParams, RankNTypes, TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- t'GI.Gtk.Objects.CellRendererToggle.CellRendererToggle' renders a toggle button in a cell. The
-- button is drawn as a radio or a checkbutton, depending on the
-- [CellRendererToggle:radio]("GI.Gtk.Objects.CellRendererToggle#g:attr:radio") property.
-- When activated, it emits the [CellRendererToggle::toggled]("GI.Gtk.Objects.CellRendererToggle#g:signal:toggled") signal.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.CellRendererToggle
    ( 

-- * Exported types
    CellRendererToggle(..)                  ,
    IsCellRendererToggle                    ,
    toCellRendererToggle                    ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [activate]("GI.Gtk.Objects.CellRenderer#g:method:activate"), [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [isActivatable]("GI.Gtk.Objects.CellRenderer#g:method:isActivatable"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [render]("GI.Gtk.Objects.CellRenderer#g:method:render"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [startEditing]("GI.Gtk.Objects.CellRenderer#g:method:startEditing"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [stopEditing]("GI.Gtk.Objects.CellRenderer#g:method:stopEditing"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getActivatable]("GI.Gtk.Objects.CellRendererToggle#g:method:getActivatable"), [getActive]("GI.Gtk.Objects.CellRendererToggle#g:method:getActive"), [getAlignedArea]("GI.Gtk.Objects.CellRenderer#g:method:getAlignedArea"), [getAlignment]("GI.Gtk.Objects.CellRenderer#g:method:getAlignment"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getFixedSize]("GI.Gtk.Objects.CellRenderer#g:method:getFixedSize"), [getPadding]("GI.Gtk.Objects.CellRenderer#g:method:getPadding"), [getPreferredHeight]("GI.Gtk.Objects.CellRenderer#g:method:getPreferredHeight"), [getPreferredHeightForWidth]("GI.Gtk.Objects.CellRenderer#g:method:getPreferredHeightForWidth"), [getPreferredSize]("GI.Gtk.Objects.CellRenderer#g:method:getPreferredSize"), [getPreferredWidth]("GI.Gtk.Objects.CellRenderer#g:method:getPreferredWidth"), [getPreferredWidthForHeight]("GI.Gtk.Objects.CellRenderer#g:method:getPreferredWidthForHeight"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getRadio]("GI.Gtk.Objects.CellRendererToggle#g:method:getRadio"), [getRequestMode]("GI.Gtk.Objects.CellRenderer#g:method:getRequestMode"), [getSensitive]("GI.Gtk.Objects.CellRenderer#g:method:getSensitive"), [getSize]("GI.Gtk.Objects.CellRenderer#g:method:getSize"), [getState]("GI.Gtk.Objects.CellRenderer#g:method:getState"), [getVisible]("GI.Gtk.Objects.CellRenderer#g:method:getVisible").
-- 
-- ==== Setters
-- [setActivatable]("GI.Gtk.Objects.CellRendererToggle#g:method:setActivatable"), [setActive]("GI.Gtk.Objects.CellRendererToggle#g:method:setActive"), [setAlignment]("GI.Gtk.Objects.CellRenderer#g:method:setAlignment"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setFixedSize]("GI.Gtk.Objects.CellRenderer#g:method:setFixedSize"), [setPadding]("GI.Gtk.Objects.CellRenderer#g:method:setPadding"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setRadio]("GI.Gtk.Objects.CellRendererToggle#g:method:setRadio"), [setSensitive]("GI.Gtk.Objects.CellRenderer#g:method:setSensitive"), [setVisible]("GI.Gtk.Objects.CellRenderer#g:method:setVisible").

#if defined(ENABLE_OVERLOADING)
    ResolveCellRendererToggleMethod         ,
#endif

-- ** getActivatable #method:getActivatable#

#if defined(ENABLE_OVERLOADING)
    CellRendererToggleGetActivatableMethodInfo,
#endif
    cellRendererToggleGetActivatable        ,


-- ** getActive #method:getActive#

#if defined(ENABLE_OVERLOADING)
    CellRendererToggleGetActiveMethodInfo   ,
#endif
    cellRendererToggleGetActive             ,


-- ** getRadio #method:getRadio#

#if defined(ENABLE_OVERLOADING)
    CellRendererToggleGetRadioMethodInfo    ,
#endif
    cellRendererToggleGetRadio              ,


-- ** new #method:new#

    cellRendererToggleNew                   ,


-- ** setActivatable #method:setActivatable#

#if defined(ENABLE_OVERLOADING)
    CellRendererToggleSetActivatableMethodInfo,
#endif
    cellRendererToggleSetActivatable        ,


-- ** setActive #method:setActive#

#if defined(ENABLE_OVERLOADING)
    CellRendererToggleSetActiveMethodInfo   ,
#endif
    cellRendererToggleSetActive             ,


-- ** setRadio #method:setRadio#

#if defined(ENABLE_OVERLOADING)
    CellRendererToggleSetRadioMethodInfo    ,
#endif
    cellRendererToggleSetRadio              ,




 -- * Properties


-- ** activatable #attr:activatable#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    CellRendererToggleActivatablePropertyInfo,
#endif
#if defined(ENABLE_OVERLOADING)
    cellRendererToggleActivatable           ,
#endif
    constructCellRendererToggleActivatable  ,
    getCellRendererToggleActivatable        ,
    setCellRendererToggleActivatable        ,


-- ** active #attr:active#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    CellRendererToggleActivePropertyInfo    ,
#endif
#if defined(ENABLE_OVERLOADING)
    cellRendererToggleActive                ,
#endif
    constructCellRendererToggleActive       ,
    getCellRendererToggleActive             ,
    setCellRendererToggleActive             ,


-- ** inconsistent #attr:inconsistent#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    CellRendererToggleInconsistentPropertyInfo,
#endif
#if defined(ENABLE_OVERLOADING)
    cellRendererToggleInconsistent          ,
#endif
    constructCellRendererToggleInconsistent ,
    getCellRendererToggleInconsistent       ,
    setCellRendererToggleInconsistent       ,


-- ** indicatorSize #attr:indicatorSize#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    CellRendererToggleIndicatorSizePropertyInfo,
#endif
#if defined(ENABLE_OVERLOADING)
    cellRendererToggleIndicatorSize         ,
#endif
    constructCellRendererToggleIndicatorSize,
    getCellRendererToggleIndicatorSize      ,
    setCellRendererToggleIndicatorSize      ,


-- ** radio #attr:radio#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    CellRendererToggleRadioPropertyInfo     ,
#endif
#if defined(ENABLE_OVERLOADING)
    cellRendererToggleRadio                 ,
#endif
    constructCellRendererToggleRadio        ,
    getCellRendererToggleRadio              ,
    setCellRendererToggleRadio              ,




 -- * Signals


-- ** toggled #signal:toggled#

    CellRendererToggleToggledCallback       ,
#if defined(ENABLE_OVERLOADING)
    CellRendererToggleToggledSignalInfo     ,
#endif
    afterCellRendererToggleToggled          ,
    onCellRendererToggleToggled             ,




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
import {-# SOURCE #-} qualified GI.Gtk.Objects.CellRenderer as Gtk.CellRenderer

-- | Memory-managed wrapper type.
newtype CellRendererToggle = CellRendererToggle (SP.ManagedPtr CellRendererToggle)
    deriving (Eq)

instance SP.ManagedPtrNewtype CellRendererToggle where
    toManagedPtr (CellRendererToggle p) = p

foreign import ccall "gtk_cell_renderer_toggle_get_type"
    c_gtk_cell_renderer_toggle_get_type :: IO B.Types.GType

instance B.Types.TypedObject CellRendererToggle where
    glibType = c_gtk_cell_renderer_toggle_get_type

instance B.Types.GObject CellRendererToggle

-- | Type class for types which can be safely cast to `CellRendererToggle`, for instance with `toCellRendererToggle`.
class (SP.GObject o, O.IsDescendantOf CellRendererToggle o) => IsCellRendererToggle o
instance (SP.GObject o, O.IsDescendantOf CellRendererToggle o) => IsCellRendererToggle o

instance O.HasParentTypes CellRendererToggle
type instance O.ParentTypes CellRendererToggle = '[Gtk.CellRenderer.CellRenderer, GObject.Object.Object]

-- | Cast to `CellRendererToggle`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toCellRendererToggle :: (MIO.MonadIO m, IsCellRendererToggle o) => o -> m CellRendererToggle
toCellRendererToggle = MIO.liftIO . B.ManagedPtr.unsafeCastTo CellRendererToggle

-- | Convert 'CellRendererToggle' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe CellRendererToggle) where
    gvalueGType_ = c_gtk_cell_renderer_toggle_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr CellRendererToggle)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr CellRendererToggle)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject CellRendererToggle ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveCellRendererToggleMethod (t :: Symbol) (o :: *) :: * where
    ResolveCellRendererToggleMethod "activate" o = Gtk.CellRenderer.CellRendererActivateMethodInfo
    ResolveCellRendererToggleMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveCellRendererToggleMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveCellRendererToggleMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveCellRendererToggleMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveCellRendererToggleMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveCellRendererToggleMethod "isActivatable" o = Gtk.CellRenderer.CellRendererIsActivatableMethodInfo
    ResolveCellRendererToggleMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveCellRendererToggleMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveCellRendererToggleMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveCellRendererToggleMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveCellRendererToggleMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveCellRendererToggleMethod "render" o = Gtk.CellRenderer.CellRendererRenderMethodInfo
    ResolveCellRendererToggleMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveCellRendererToggleMethod "startEditing" o = Gtk.CellRenderer.CellRendererStartEditingMethodInfo
    ResolveCellRendererToggleMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveCellRendererToggleMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveCellRendererToggleMethod "stopEditing" o = Gtk.CellRenderer.CellRendererStopEditingMethodInfo
    ResolveCellRendererToggleMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveCellRendererToggleMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveCellRendererToggleMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveCellRendererToggleMethod "getActivatable" o = CellRendererToggleGetActivatableMethodInfo
    ResolveCellRendererToggleMethod "getActive" o = CellRendererToggleGetActiveMethodInfo
    ResolveCellRendererToggleMethod "getAlignedArea" o = Gtk.CellRenderer.CellRendererGetAlignedAreaMethodInfo
    ResolveCellRendererToggleMethod "getAlignment" o = Gtk.CellRenderer.CellRendererGetAlignmentMethodInfo
    ResolveCellRendererToggleMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveCellRendererToggleMethod "getFixedSize" o = Gtk.CellRenderer.CellRendererGetFixedSizeMethodInfo
    ResolveCellRendererToggleMethod "getPadding" o = Gtk.CellRenderer.CellRendererGetPaddingMethodInfo
    ResolveCellRendererToggleMethod "getPreferredHeight" o = Gtk.CellRenderer.CellRendererGetPreferredHeightMethodInfo
    ResolveCellRendererToggleMethod "getPreferredHeightForWidth" o = Gtk.CellRenderer.CellRendererGetPreferredHeightForWidthMethodInfo
    ResolveCellRendererToggleMethod "getPreferredSize" o = Gtk.CellRenderer.CellRendererGetPreferredSizeMethodInfo
    ResolveCellRendererToggleMethod "getPreferredWidth" o = Gtk.CellRenderer.CellRendererGetPreferredWidthMethodInfo
    ResolveCellRendererToggleMethod "getPreferredWidthForHeight" o = Gtk.CellRenderer.CellRendererGetPreferredWidthForHeightMethodInfo
    ResolveCellRendererToggleMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveCellRendererToggleMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveCellRendererToggleMethod "getRadio" o = CellRendererToggleGetRadioMethodInfo
    ResolveCellRendererToggleMethod "getRequestMode" o = Gtk.CellRenderer.CellRendererGetRequestModeMethodInfo
    ResolveCellRendererToggleMethod "getSensitive" o = Gtk.CellRenderer.CellRendererGetSensitiveMethodInfo
    ResolveCellRendererToggleMethod "getSize" o = Gtk.CellRenderer.CellRendererGetSizeMethodInfo
    ResolveCellRendererToggleMethod "getState" o = Gtk.CellRenderer.CellRendererGetStateMethodInfo
    ResolveCellRendererToggleMethod "getVisible" o = Gtk.CellRenderer.CellRendererGetVisibleMethodInfo
    ResolveCellRendererToggleMethod "setActivatable" o = CellRendererToggleSetActivatableMethodInfo
    ResolveCellRendererToggleMethod "setActive" o = CellRendererToggleSetActiveMethodInfo
    ResolveCellRendererToggleMethod "setAlignment" o = Gtk.CellRenderer.CellRendererSetAlignmentMethodInfo
    ResolveCellRendererToggleMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveCellRendererToggleMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveCellRendererToggleMethod "setFixedSize" o = Gtk.CellRenderer.CellRendererSetFixedSizeMethodInfo
    ResolveCellRendererToggleMethod "setPadding" o = Gtk.CellRenderer.CellRendererSetPaddingMethodInfo
    ResolveCellRendererToggleMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveCellRendererToggleMethod "setRadio" o = CellRendererToggleSetRadioMethodInfo
    ResolveCellRendererToggleMethod "setSensitive" o = Gtk.CellRenderer.CellRendererSetSensitiveMethodInfo
    ResolveCellRendererToggleMethod "setVisible" o = Gtk.CellRenderer.CellRendererSetVisibleMethodInfo
    ResolveCellRendererToggleMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveCellRendererToggleMethod t CellRendererToggle, O.OverloadedMethod info CellRendererToggle p) => OL.IsLabel t (CellRendererToggle -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveCellRendererToggleMethod t CellRendererToggle, O.OverloadedMethod info CellRendererToggle p, R.HasField t CellRendererToggle p) => R.HasField t CellRendererToggle p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveCellRendererToggleMethod t CellRendererToggle, O.OverloadedMethodInfo info CellRendererToggle) => OL.IsLabel t (O.MethodProxy info CellRendererToggle) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- signal CellRendererToggle::toggled
-- | The [toggled](#g:signal:toggled) signal is emitted when the cell is toggled.
-- 
-- It is the responsibility of the application to update the model
-- with the correct value to store at /@path@/.  Often this is simply the
-- opposite of the value currently stored at /@path@/.
type CellRendererToggleToggledCallback =
    T.Text
    -- ^ /@path@/: string representation of t'GI.Gtk.Structs.TreePath.TreePath' describing the
    --        event location
    -> IO ()

type C_CellRendererToggleToggledCallback =
    Ptr CellRendererToggle ->               -- object
    CString ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_CellRendererToggleToggledCallback`.
foreign import ccall "wrapper"
    mk_CellRendererToggleToggledCallback :: C_CellRendererToggleToggledCallback -> IO (FunPtr C_CellRendererToggleToggledCallback)

wrap_CellRendererToggleToggledCallback :: 
    GObject a => (a -> CellRendererToggleToggledCallback) ->
    C_CellRendererToggleToggledCallback
wrap_CellRendererToggleToggledCallback gi'cb gi'selfPtr path _ = do
    path' <- cstringToText path
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  path'


-- | Connect a signal handler for the [toggled](#signal:toggled) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' cellRendererToggle #toggled callback
-- @
-- 
-- 
onCellRendererToggleToggled :: (IsCellRendererToggle a, MonadIO m) => a -> ((?self :: a) => CellRendererToggleToggledCallback) -> m SignalHandlerId
onCellRendererToggleToggled obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_CellRendererToggleToggledCallback wrapped
    wrapped'' <- mk_CellRendererToggleToggledCallback wrapped'
    connectSignalFunPtr obj "toggled" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [toggled](#signal:toggled) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' cellRendererToggle #toggled callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterCellRendererToggleToggled :: (IsCellRendererToggle a, MonadIO m) => a -> ((?self :: a) => CellRendererToggleToggledCallback) -> m SignalHandlerId
afterCellRendererToggleToggled obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_CellRendererToggleToggledCallback wrapped
    wrapped'' <- mk_CellRendererToggleToggledCallback wrapped'
    connectSignalFunPtr obj "toggled" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data CellRendererToggleToggledSignalInfo
instance SignalInfo CellRendererToggleToggledSignalInfo where
    type HaskellCallbackType CellRendererToggleToggledSignalInfo = CellRendererToggleToggledCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_CellRendererToggleToggledCallback cb
        cb'' <- mk_CellRendererToggleToggledCallback cb'
        connectSignalFunPtr obj "toggled" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRendererToggle::toggled"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRendererToggle.html#g:signal:toggled"})

#endif

-- VVV Prop "activatable"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@activatable@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' cellRendererToggle #activatable
-- @
getCellRendererToggleActivatable :: (MonadIO m, IsCellRendererToggle o) => o -> m Bool
getCellRendererToggleActivatable obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "activatable"

-- | Set the value of the “@activatable@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' cellRendererToggle [ #activatable 'Data.GI.Base.Attributes.:=' value ]
-- @
setCellRendererToggleActivatable :: (MonadIO m, IsCellRendererToggle o) => o -> Bool -> m ()
setCellRendererToggleActivatable obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "activatable" val

-- | Construct a `GValueConstruct` with valid value for the “@activatable@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructCellRendererToggleActivatable :: (IsCellRendererToggle o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructCellRendererToggleActivatable val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "activatable" val

#if defined(ENABLE_OVERLOADING)
data CellRendererToggleActivatablePropertyInfo
instance AttrInfo CellRendererToggleActivatablePropertyInfo where
    type AttrAllowedOps CellRendererToggleActivatablePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint CellRendererToggleActivatablePropertyInfo = IsCellRendererToggle
    type AttrSetTypeConstraint CellRendererToggleActivatablePropertyInfo = (~) Bool
    type AttrTransferTypeConstraint CellRendererToggleActivatablePropertyInfo = (~) Bool
    type AttrTransferType CellRendererToggleActivatablePropertyInfo = Bool
    type AttrGetType CellRendererToggleActivatablePropertyInfo = Bool
    type AttrLabel CellRendererToggleActivatablePropertyInfo = "activatable"
    type AttrOrigin CellRendererToggleActivatablePropertyInfo = CellRendererToggle
    attrGet = getCellRendererToggleActivatable
    attrSet = setCellRendererToggleActivatable
    attrTransfer _ v = do
        return v
    attrConstruct = constructCellRendererToggleActivatable
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRendererToggle.activatable"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRendererToggle.html#g:attr:activatable"
        })
#endif

-- VVV Prop "active"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@active@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' cellRendererToggle #active
-- @
getCellRendererToggleActive :: (MonadIO m, IsCellRendererToggle o) => o -> m Bool
getCellRendererToggleActive obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "active"

-- | Set the value of the “@active@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' cellRendererToggle [ #active 'Data.GI.Base.Attributes.:=' value ]
-- @
setCellRendererToggleActive :: (MonadIO m, IsCellRendererToggle o) => o -> Bool -> m ()
setCellRendererToggleActive obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "active" val

-- | Construct a `GValueConstruct` with valid value for the “@active@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructCellRendererToggleActive :: (IsCellRendererToggle o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructCellRendererToggleActive val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "active" val

#if defined(ENABLE_OVERLOADING)
data CellRendererToggleActivePropertyInfo
instance AttrInfo CellRendererToggleActivePropertyInfo where
    type AttrAllowedOps CellRendererToggleActivePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint CellRendererToggleActivePropertyInfo = IsCellRendererToggle
    type AttrSetTypeConstraint CellRendererToggleActivePropertyInfo = (~) Bool
    type AttrTransferTypeConstraint CellRendererToggleActivePropertyInfo = (~) Bool
    type AttrTransferType CellRendererToggleActivePropertyInfo = Bool
    type AttrGetType CellRendererToggleActivePropertyInfo = Bool
    type AttrLabel CellRendererToggleActivePropertyInfo = "active"
    type AttrOrigin CellRendererToggleActivePropertyInfo = CellRendererToggle
    attrGet = getCellRendererToggleActive
    attrSet = setCellRendererToggleActive
    attrTransfer _ v = do
        return v
    attrConstruct = constructCellRendererToggleActive
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRendererToggle.active"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRendererToggle.html#g:attr:active"
        })
#endif

-- VVV Prop "inconsistent"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@inconsistent@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' cellRendererToggle #inconsistent
-- @
getCellRendererToggleInconsistent :: (MonadIO m, IsCellRendererToggle o) => o -> m Bool
getCellRendererToggleInconsistent obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "inconsistent"

-- | Set the value of the “@inconsistent@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' cellRendererToggle [ #inconsistent 'Data.GI.Base.Attributes.:=' value ]
-- @
setCellRendererToggleInconsistent :: (MonadIO m, IsCellRendererToggle o) => o -> Bool -> m ()
setCellRendererToggleInconsistent obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "inconsistent" val

-- | Construct a `GValueConstruct` with valid value for the “@inconsistent@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructCellRendererToggleInconsistent :: (IsCellRendererToggle o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructCellRendererToggleInconsistent val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "inconsistent" val

#if defined(ENABLE_OVERLOADING)
data CellRendererToggleInconsistentPropertyInfo
instance AttrInfo CellRendererToggleInconsistentPropertyInfo where
    type AttrAllowedOps CellRendererToggleInconsistentPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint CellRendererToggleInconsistentPropertyInfo = IsCellRendererToggle
    type AttrSetTypeConstraint CellRendererToggleInconsistentPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint CellRendererToggleInconsistentPropertyInfo = (~) Bool
    type AttrTransferType CellRendererToggleInconsistentPropertyInfo = Bool
    type AttrGetType CellRendererToggleInconsistentPropertyInfo = Bool
    type AttrLabel CellRendererToggleInconsistentPropertyInfo = "inconsistent"
    type AttrOrigin CellRendererToggleInconsistentPropertyInfo = CellRendererToggle
    attrGet = getCellRendererToggleInconsistent
    attrSet = setCellRendererToggleInconsistent
    attrTransfer _ v = do
        return v
    attrConstruct = constructCellRendererToggleInconsistent
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRendererToggle.inconsistent"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRendererToggle.html#g:attr:inconsistent"
        })
#endif

-- VVV Prop "indicator-size"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@indicator-size@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' cellRendererToggle #indicatorSize
-- @
getCellRendererToggleIndicatorSize :: (MonadIO m, IsCellRendererToggle o) => o -> m Int32
getCellRendererToggleIndicatorSize obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "indicator-size"

-- | Set the value of the “@indicator-size@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' cellRendererToggle [ #indicatorSize 'Data.GI.Base.Attributes.:=' value ]
-- @
setCellRendererToggleIndicatorSize :: (MonadIO m, IsCellRendererToggle o) => o -> Int32 -> m ()
setCellRendererToggleIndicatorSize obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyInt32 obj "indicator-size" val

-- | Construct a `GValueConstruct` with valid value for the “@indicator-size@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructCellRendererToggleIndicatorSize :: (IsCellRendererToggle o, MIO.MonadIO m) => Int32 -> m (GValueConstruct o)
constructCellRendererToggleIndicatorSize val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyInt32 "indicator-size" val

#if defined(ENABLE_OVERLOADING)
data CellRendererToggleIndicatorSizePropertyInfo
instance AttrInfo CellRendererToggleIndicatorSizePropertyInfo where
    type AttrAllowedOps CellRendererToggleIndicatorSizePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint CellRendererToggleIndicatorSizePropertyInfo = IsCellRendererToggle
    type AttrSetTypeConstraint CellRendererToggleIndicatorSizePropertyInfo = (~) Int32
    type AttrTransferTypeConstraint CellRendererToggleIndicatorSizePropertyInfo = (~) Int32
    type AttrTransferType CellRendererToggleIndicatorSizePropertyInfo = Int32
    type AttrGetType CellRendererToggleIndicatorSizePropertyInfo = Int32
    type AttrLabel CellRendererToggleIndicatorSizePropertyInfo = "indicator-size"
    type AttrOrigin CellRendererToggleIndicatorSizePropertyInfo = CellRendererToggle
    attrGet = getCellRendererToggleIndicatorSize
    attrSet = setCellRendererToggleIndicatorSize
    attrTransfer _ v = do
        return v
    attrConstruct = constructCellRendererToggleIndicatorSize
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRendererToggle.indicatorSize"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRendererToggle.html#g:attr:indicatorSize"
        })
#endif

-- VVV Prop "radio"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@radio@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' cellRendererToggle #radio
-- @
getCellRendererToggleRadio :: (MonadIO m, IsCellRendererToggle o) => o -> m Bool
getCellRendererToggleRadio obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "radio"

-- | Set the value of the “@radio@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' cellRendererToggle [ #radio 'Data.GI.Base.Attributes.:=' value ]
-- @
setCellRendererToggleRadio :: (MonadIO m, IsCellRendererToggle o) => o -> Bool -> m ()
setCellRendererToggleRadio obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "radio" val

-- | Construct a `GValueConstruct` with valid value for the “@radio@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructCellRendererToggleRadio :: (IsCellRendererToggle o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructCellRendererToggleRadio val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "radio" val

#if defined(ENABLE_OVERLOADING)
data CellRendererToggleRadioPropertyInfo
instance AttrInfo CellRendererToggleRadioPropertyInfo where
    type AttrAllowedOps CellRendererToggleRadioPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint CellRendererToggleRadioPropertyInfo = IsCellRendererToggle
    type AttrSetTypeConstraint CellRendererToggleRadioPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint CellRendererToggleRadioPropertyInfo = (~) Bool
    type AttrTransferType CellRendererToggleRadioPropertyInfo = Bool
    type AttrGetType CellRendererToggleRadioPropertyInfo = Bool
    type AttrLabel CellRendererToggleRadioPropertyInfo = "radio"
    type AttrOrigin CellRendererToggleRadioPropertyInfo = CellRendererToggle
    attrGet = getCellRendererToggleRadio
    attrSet = setCellRendererToggleRadio
    attrTransfer _ v = do
        return v
    attrConstruct = constructCellRendererToggleRadio
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRendererToggle.radio"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRendererToggle.html#g:attr:radio"
        })
#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList CellRendererToggle
type instance O.AttributeList CellRendererToggle = CellRendererToggleAttributeList
type CellRendererToggleAttributeList = ('[ '("activatable", CellRendererToggleActivatablePropertyInfo), '("active", CellRendererToggleActivePropertyInfo), '("cellBackground", Gtk.CellRenderer.CellRendererCellBackgroundPropertyInfo), '("cellBackgroundGdk", Gtk.CellRenderer.CellRendererCellBackgroundGdkPropertyInfo), '("cellBackgroundRgba", Gtk.CellRenderer.CellRendererCellBackgroundRgbaPropertyInfo), '("cellBackgroundSet", Gtk.CellRenderer.CellRendererCellBackgroundSetPropertyInfo), '("editing", Gtk.CellRenderer.CellRendererEditingPropertyInfo), '("height", Gtk.CellRenderer.CellRendererHeightPropertyInfo), '("inconsistent", CellRendererToggleInconsistentPropertyInfo), '("indicatorSize", CellRendererToggleIndicatorSizePropertyInfo), '("isExpanded", Gtk.CellRenderer.CellRendererIsExpandedPropertyInfo), '("isExpander", Gtk.CellRenderer.CellRendererIsExpanderPropertyInfo), '("mode", Gtk.CellRenderer.CellRendererModePropertyInfo), '("radio", CellRendererToggleRadioPropertyInfo), '("sensitive", Gtk.CellRenderer.CellRendererSensitivePropertyInfo), '("visible", Gtk.CellRenderer.CellRendererVisiblePropertyInfo), '("width", Gtk.CellRenderer.CellRendererWidthPropertyInfo), '("xalign", Gtk.CellRenderer.CellRendererXalignPropertyInfo), '("xpad", Gtk.CellRenderer.CellRendererXpadPropertyInfo), '("yalign", Gtk.CellRenderer.CellRendererYalignPropertyInfo), '("ypad", Gtk.CellRenderer.CellRendererYpadPropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
cellRendererToggleActivatable :: AttrLabelProxy "activatable"
cellRendererToggleActivatable = AttrLabelProxy

cellRendererToggleActive :: AttrLabelProxy "active"
cellRendererToggleActive = AttrLabelProxy

cellRendererToggleInconsistent :: AttrLabelProxy "inconsistent"
cellRendererToggleInconsistent = AttrLabelProxy

cellRendererToggleIndicatorSize :: AttrLabelProxy "indicatorSize"
cellRendererToggleIndicatorSize = AttrLabelProxy

cellRendererToggleRadio :: AttrLabelProxy "radio"
cellRendererToggleRadio = AttrLabelProxy

#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList CellRendererToggle = CellRendererToggleSignalList
type CellRendererToggleSignalList = ('[ '("editingCanceled", Gtk.CellRenderer.CellRendererEditingCanceledSignalInfo), '("editingStarted", Gtk.CellRenderer.CellRendererEditingStartedSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo), '("toggled", CellRendererToggleToggledSignalInfo)] :: [(Symbol, *)])

#endif

-- method CellRendererToggle::new
-- method type : Constructor
-- Args: []
-- Lengths: []
-- returnType: Just
--               (TInterface
--                  Name { namespace = "Gtk" , name = "CellRendererToggle" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_cell_renderer_toggle_new" gtk_cell_renderer_toggle_new :: 
    IO (Ptr CellRendererToggle)

-- | Creates a new t'GI.Gtk.Objects.CellRendererToggle.CellRendererToggle'. Adjust rendering
-- parameters using object properties. Object properties can be set
-- globally (with @/g_object_set()/@). Also, with t'GI.Gtk.Objects.TreeViewColumn.TreeViewColumn', you
-- can bind a property to a value in a t'GI.Gtk.Interfaces.TreeModel.TreeModel'. For example, you
-- can bind the “active” property on the cell renderer to a boolean value
-- in the model, thus causing the check button to reflect the state of
-- the model.
cellRendererToggleNew ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m CellRendererToggle
    -- ^ __Returns:__ the new cell renderer
cellRendererToggleNew  = liftIO $ do
    result <- gtk_cell_renderer_toggle_new
    checkUnexpectedReturnNULL "cellRendererToggleNew" result
    result' <- (newObject CellRendererToggle) result
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method CellRendererToggle::get_activatable
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "toggle"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellRendererToggle" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCellRendererToggle"
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

foreign import ccall "gtk_cell_renderer_toggle_get_activatable" gtk_cell_renderer_toggle_get_activatable :: 
    Ptr CellRendererToggle ->               -- toggle : TInterface (Name {namespace = "Gtk", name = "CellRendererToggle"})
    IO CInt

-- | Returns whether the cell renderer is activatable. See
-- 'GI.Gtk.Objects.CellRendererToggle.cellRendererToggleSetActivatable'.
-- 
-- /Since: 2.18/
cellRendererToggleGetActivatable ::
    (B.CallStack.HasCallStack, MonadIO m, IsCellRendererToggle a) =>
    a
    -- ^ /@toggle@/: a t'GI.Gtk.Objects.CellRendererToggle.CellRendererToggle'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the cell renderer is activatable.
cellRendererToggleGetActivatable toggle = liftIO $ do
    toggle' <- unsafeManagedPtrCastPtr toggle
    result <- gtk_cell_renderer_toggle_get_activatable toggle'
    let result' = (/= 0) result
    touchManagedPtr toggle
    return result'

#if defined(ENABLE_OVERLOADING)
data CellRendererToggleGetActivatableMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsCellRendererToggle a) => O.OverloadedMethod CellRendererToggleGetActivatableMethodInfo a signature where
    overloadedMethod = cellRendererToggleGetActivatable

instance O.OverloadedMethodInfo CellRendererToggleGetActivatableMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRendererToggle.cellRendererToggleGetActivatable",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRendererToggle.html#v:cellRendererToggleGetActivatable"
        })


#endif

-- method CellRendererToggle::get_active
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "toggle"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellRendererToggle" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCellRendererToggle"
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

foreign import ccall "gtk_cell_renderer_toggle_get_active" gtk_cell_renderer_toggle_get_active :: 
    Ptr CellRendererToggle ->               -- toggle : TInterface (Name {namespace = "Gtk", name = "CellRendererToggle"})
    IO CInt

-- | Returns whether the cell renderer is active. See
-- 'GI.Gtk.Objects.CellRendererToggle.cellRendererToggleSetActive'.
cellRendererToggleGetActive ::
    (B.CallStack.HasCallStack, MonadIO m, IsCellRendererToggle a) =>
    a
    -- ^ /@toggle@/: a t'GI.Gtk.Objects.CellRendererToggle.CellRendererToggle'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the cell renderer is active.
cellRendererToggleGetActive toggle = liftIO $ do
    toggle' <- unsafeManagedPtrCastPtr toggle
    result <- gtk_cell_renderer_toggle_get_active toggle'
    let result' = (/= 0) result
    touchManagedPtr toggle
    return result'

#if defined(ENABLE_OVERLOADING)
data CellRendererToggleGetActiveMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsCellRendererToggle a) => O.OverloadedMethod CellRendererToggleGetActiveMethodInfo a signature where
    overloadedMethod = cellRendererToggleGetActive

instance O.OverloadedMethodInfo CellRendererToggleGetActiveMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRendererToggle.cellRendererToggleGetActive",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRendererToggle.html#v:cellRendererToggleGetActive"
        })


#endif

-- method CellRendererToggle::get_radio
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "toggle"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellRendererToggle" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCellRendererToggle"
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

foreign import ccall "gtk_cell_renderer_toggle_get_radio" gtk_cell_renderer_toggle_get_radio :: 
    Ptr CellRendererToggle ->               -- toggle : TInterface (Name {namespace = "Gtk", name = "CellRendererToggle"})
    IO CInt

-- | Returns whether we’re rendering radio toggles rather than checkboxes.
cellRendererToggleGetRadio ::
    (B.CallStack.HasCallStack, MonadIO m, IsCellRendererToggle a) =>
    a
    -- ^ /@toggle@/: a t'GI.Gtk.Objects.CellRendererToggle.CellRendererToggle'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if we’re rendering radio toggles rather than checkboxes
cellRendererToggleGetRadio toggle = liftIO $ do
    toggle' <- unsafeManagedPtrCastPtr toggle
    result <- gtk_cell_renderer_toggle_get_radio toggle'
    let result' = (/= 0) result
    touchManagedPtr toggle
    return result'

#if defined(ENABLE_OVERLOADING)
data CellRendererToggleGetRadioMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsCellRendererToggle a) => O.OverloadedMethod CellRendererToggleGetRadioMethodInfo a signature where
    overloadedMethod = cellRendererToggleGetRadio

instance O.OverloadedMethodInfo CellRendererToggleGetRadioMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRendererToggle.cellRendererToggleGetRadio",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRendererToggle.html#v:cellRendererToggleGetRadio"
        })


#endif

-- method CellRendererToggle::set_activatable
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "toggle"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellRendererToggle" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCellRendererToggle."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "setting"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the value to set." , sinceVersion = Nothing }
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

foreign import ccall "gtk_cell_renderer_toggle_set_activatable" gtk_cell_renderer_toggle_set_activatable :: 
    Ptr CellRendererToggle ->               -- toggle : TInterface (Name {namespace = "Gtk", name = "CellRendererToggle"})
    CInt ->                                 -- setting : TBasicType TBoolean
    IO ()

-- | Makes the cell renderer activatable.
-- 
-- /Since: 2.18/
cellRendererToggleSetActivatable ::
    (B.CallStack.HasCallStack, MonadIO m, IsCellRendererToggle a) =>
    a
    -- ^ /@toggle@/: a t'GI.Gtk.Objects.CellRendererToggle.CellRendererToggle'.
    -> Bool
    -- ^ /@setting@/: the value to set.
    -> m ()
cellRendererToggleSetActivatable toggle setting = liftIO $ do
    toggle' <- unsafeManagedPtrCastPtr toggle
    let setting' = (fromIntegral . fromEnum) setting
    gtk_cell_renderer_toggle_set_activatable toggle' setting'
    touchManagedPtr toggle
    return ()

#if defined(ENABLE_OVERLOADING)
data CellRendererToggleSetActivatableMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsCellRendererToggle a) => O.OverloadedMethod CellRendererToggleSetActivatableMethodInfo a signature where
    overloadedMethod = cellRendererToggleSetActivatable

instance O.OverloadedMethodInfo CellRendererToggleSetActivatableMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRendererToggle.cellRendererToggleSetActivatable",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRendererToggle.html#v:cellRendererToggleSetActivatable"
        })


#endif

-- method CellRendererToggle::set_active
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "toggle"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellRendererToggle" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCellRendererToggle."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "setting"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the value to set." , sinceVersion = Nothing }
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

foreign import ccall "gtk_cell_renderer_toggle_set_active" gtk_cell_renderer_toggle_set_active :: 
    Ptr CellRendererToggle ->               -- toggle : TInterface (Name {namespace = "Gtk", name = "CellRendererToggle"})
    CInt ->                                 -- setting : TBasicType TBoolean
    IO ()

-- | Activates or deactivates a cell renderer.
cellRendererToggleSetActive ::
    (B.CallStack.HasCallStack, MonadIO m, IsCellRendererToggle a) =>
    a
    -- ^ /@toggle@/: a t'GI.Gtk.Objects.CellRendererToggle.CellRendererToggle'.
    -> Bool
    -- ^ /@setting@/: the value to set.
    -> m ()
cellRendererToggleSetActive toggle setting = liftIO $ do
    toggle' <- unsafeManagedPtrCastPtr toggle
    let setting' = (fromIntegral . fromEnum) setting
    gtk_cell_renderer_toggle_set_active toggle' setting'
    touchManagedPtr toggle
    return ()

#if defined(ENABLE_OVERLOADING)
data CellRendererToggleSetActiveMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsCellRendererToggle a) => O.OverloadedMethod CellRendererToggleSetActiveMethodInfo a signature where
    overloadedMethod = cellRendererToggleSetActive

instance O.OverloadedMethodInfo CellRendererToggleSetActiveMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRendererToggle.cellRendererToggleSetActive",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRendererToggle.html#v:cellRendererToggleSetActive"
        })


#endif

-- method CellRendererToggle::set_radio
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "toggle"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellRendererToggle" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCellRendererToggle"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "radio"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "%TRUE to make the toggle look like a radio button"
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

foreign import ccall "gtk_cell_renderer_toggle_set_radio" gtk_cell_renderer_toggle_set_radio :: 
    Ptr CellRendererToggle ->               -- toggle : TInterface (Name {namespace = "Gtk", name = "CellRendererToggle"})
    CInt ->                                 -- radio : TBasicType TBoolean
    IO ()

-- | If /@radio@/ is 'P.True', the cell renderer renders a radio toggle
-- (i.e. a toggle in a group of mutually-exclusive toggles).
-- If 'P.False', it renders a check toggle (a standalone boolean option).
-- This can be set globally for the cell renderer, or changed just
-- before rendering each cell in the model (for t'GI.Gtk.Objects.TreeView.TreeView', you set
-- up a per-row setting using t'GI.Gtk.Objects.TreeViewColumn.TreeViewColumn' to associate model
-- columns with cell renderer properties).
cellRendererToggleSetRadio ::
    (B.CallStack.HasCallStack, MonadIO m, IsCellRendererToggle a) =>
    a
    -- ^ /@toggle@/: a t'GI.Gtk.Objects.CellRendererToggle.CellRendererToggle'
    -> Bool
    -- ^ /@radio@/: 'P.True' to make the toggle look like a radio button
    -> m ()
cellRendererToggleSetRadio toggle radio = liftIO $ do
    toggle' <- unsafeManagedPtrCastPtr toggle
    let radio' = (fromIntegral . fromEnum) radio
    gtk_cell_renderer_toggle_set_radio toggle' radio'
    touchManagedPtr toggle
    return ()

#if defined(ENABLE_OVERLOADING)
data CellRendererToggleSetRadioMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsCellRendererToggle a) => O.OverloadedMethod CellRendererToggleSetRadioMethodInfo a signature where
    overloadedMethod = cellRendererToggleSetRadio

instance O.OverloadedMethodInfo CellRendererToggleSetRadioMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRendererToggle.cellRendererToggleSetRadio",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRendererToggle.html#v:cellRendererToggleSetRadio"
        })


#endif


