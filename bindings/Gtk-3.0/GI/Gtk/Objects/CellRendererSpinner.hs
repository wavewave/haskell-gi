{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- GtkCellRendererSpinner renders a spinning animation in a cell, very
-- similar to t'GI.Gtk.Objects.Spinner.Spinner'. It can often be used as an alternative
-- to a t'GI.Gtk.Objects.CellRendererProgress.CellRendererProgress' for displaying indefinite activity,
-- instead of actual progress.
-- 
-- To start the animation in a cell, set the [CellRendererSpinner:active]("GI.Gtk.Objects.CellRendererSpinner#g:attr:active")
-- property to 'P.True' and increment the [CellRendererSpinner:pulse]("GI.Gtk.Objects.CellRendererSpinner#g:attr:pulse") property
-- at regular intervals. The usual way to set the cell renderer properties
-- for each cell is to bind them to columns in your tree model using e.g.
-- 'GI.Gtk.Objects.TreeViewColumn.treeViewColumnAddAttribute'.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.CellRendererSpinner
    ( 

-- * Exported types
    CellRendererSpinner(..)                 ,
    IsCellRendererSpinner                   ,
    toCellRendererSpinner                   ,


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
-- [setAlignment]("GI.Gtk.Objects.CellRenderer#g:method:setAlignment"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setFixedSize]("GI.Gtk.Objects.CellRenderer#g:method:setFixedSize"), [setPadding]("GI.Gtk.Objects.CellRenderer#g:method:setPadding"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setSensitive]("GI.Gtk.Objects.CellRenderer#g:method:setSensitive"), [setVisible]("GI.Gtk.Objects.CellRenderer#g:method:setVisible").

#if defined(ENABLE_OVERLOADING)
    ResolveCellRendererSpinnerMethod        ,
#endif

-- ** new #method:new#

    cellRendererSpinnerNew                  ,




 -- * Properties


-- ** active #attr:active#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    CellRendererSpinnerActivePropertyInfo   ,
#endif
#if defined(ENABLE_OVERLOADING)
    cellRendererSpinnerActive               ,
#endif
    constructCellRendererSpinnerActive      ,
    getCellRendererSpinnerActive            ,
    setCellRendererSpinnerActive            ,


-- ** pulse #attr:pulse#
-- | Pulse of the spinner. Increment this value to draw the next frame of the
-- spinner animation. Usually, you would update this value in a timeout.
-- 
-- By default, the t'GI.Gtk.Objects.Spinner.Spinner' widget draws one full cycle of the animation,
-- consisting of 12 frames, in 750 milliseconds.
-- 
-- /Since: 2.20/

#if defined(ENABLE_OVERLOADING)
    CellRendererSpinnerPulsePropertyInfo    ,
#endif
#if defined(ENABLE_OVERLOADING)
    cellRendererSpinnerPulse                ,
#endif
    constructCellRendererSpinnerPulse       ,
    getCellRendererSpinnerPulse             ,
    setCellRendererSpinnerPulse             ,


-- ** size #attr:size#
-- | The t'GI.Gtk.Enums.IconSize' value that specifies the size of the rendered spinner.
-- 
-- /Since: 2.20/

#if defined(ENABLE_OVERLOADING)
    CellRendererSpinnerSizePropertyInfo     ,
#endif
#if defined(ENABLE_OVERLOADING)
    cellRendererSpinnerSize                 ,
#endif
    constructCellRendererSpinnerSize        ,
    getCellRendererSpinnerSize              ,
    setCellRendererSpinnerSize              ,




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
import {-# SOURCE #-} qualified GI.Gtk.Objects.CellRenderer as Gtk.CellRenderer

-- | Memory-managed wrapper type.
newtype CellRendererSpinner = CellRendererSpinner (SP.ManagedPtr CellRendererSpinner)
    deriving (Eq)

instance SP.ManagedPtrNewtype CellRendererSpinner where
    toManagedPtr (CellRendererSpinner p) = p

foreign import ccall "gtk_cell_renderer_spinner_get_type"
    c_gtk_cell_renderer_spinner_get_type :: IO B.Types.GType

instance B.Types.TypedObject CellRendererSpinner where
    glibType = c_gtk_cell_renderer_spinner_get_type

instance B.Types.GObject CellRendererSpinner

-- | Type class for types which can be safely cast to `CellRendererSpinner`, for instance with `toCellRendererSpinner`.
class (SP.GObject o, O.IsDescendantOf CellRendererSpinner o) => IsCellRendererSpinner o
instance (SP.GObject o, O.IsDescendantOf CellRendererSpinner o) => IsCellRendererSpinner o

instance O.HasParentTypes CellRendererSpinner
type instance O.ParentTypes CellRendererSpinner = '[Gtk.CellRenderer.CellRenderer, GObject.Object.Object]

-- | Cast to `CellRendererSpinner`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toCellRendererSpinner :: (MIO.MonadIO m, IsCellRendererSpinner o) => o -> m CellRendererSpinner
toCellRendererSpinner = MIO.liftIO . B.ManagedPtr.unsafeCastTo CellRendererSpinner

-- | Convert 'CellRendererSpinner' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe CellRendererSpinner) where
    gvalueGType_ = c_gtk_cell_renderer_spinner_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr CellRendererSpinner)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr CellRendererSpinner)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject CellRendererSpinner ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveCellRendererSpinnerMethod (t :: Symbol) (o :: *) :: * where
    ResolveCellRendererSpinnerMethod "activate" o = Gtk.CellRenderer.CellRendererActivateMethodInfo
    ResolveCellRendererSpinnerMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveCellRendererSpinnerMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveCellRendererSpinnerMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveCellRendererSpinnerMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveCellRendererSpinnerMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveCellRendererSpinnerMethod "isActivatable" o = Gtk.CellRenderer.CellRendererIsActivatableMethodInfo
    ResolveCellRendererSpinnerMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveCellRendererSpinnerMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveCellRendererSpinnerMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveCellRendererSpinnerMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveCellRendererSpinnerMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveCellRendererSpinnerMethod "render" o = Gtk.CellRenderer.CellRendererRenderMethodInfo
    ResolveCellRendererSpinnerMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveCellRendererSpinnerMethod "startEditing" o = Gtk.CellRenderer.CellRendererStartEditingMethodInfo
    ResolveCellRendererSpinnerMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveCellRendererSpinnerMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveCellRendererSpinnerMethod "stopEditing" o = Gtk.CellRenderer.CellRendererStopEditingMethodInfo
    ResolveCellRendererSpinnerMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveCellRendererSpinnerMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveCellRendererSpinnerMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveCellRendererSpinnerMethod "getAlignedArea" o = Gtk.CellRenderer.CellRendererGetAlignedAreaMethodInfo
    ResolveCellRendererSpinnerMethod "getAlignment" o = Gtk.CellRenderer.CellRendererGetAlignmentMethodInfo
    ResolveCellRendererSpinnerMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveCellRendererSpinnerMethod "getFixedSize" o = Gtk.CellRenderer.CellRendererGetFixedSizeMethodInfo
    ResolveCellRendererSpinnerMethod "getPadding" o = Gtk.CellRenderer.CellRendererGetPaddingMethodInfo
    ResolveCellRendererSpinnerMethod "getPreferredHeight" o = Gtk.CellRenderer.CellRendererGetPreferredHeightMethodInfo
    ResolveCellRendererSpinnerMethod "getPreferredHeightForWidth" o = Gtk.CellRenderer.CellRendererGetPreferredHeightForWidthMethodInfo
    ResolveCellRendererSpinnerMethod "getPreferredSize" o = Gtk.CellRenderer.CellRendererGetPreferredSizeMethodInfo
    ResolveCellRendererSpinnerMethod "getPreferredWidth" o = Gtk.CellRenderer.CellRendererGetPreferredWidthMethodInfo
    ResolveCellRendererSpinnerMethod "getPreferredWidthForHeight" o = Gtk.CellRenderer.CellRendererGetPreferredWidthForHeightMethodInfo
    ResolveCellRendererSpinnerMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveCellRendererSpinnerMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveCellRendererSpinnerMethod "getRequestMode" o = Gtk.CellRenderer.CellRendererGetRequestModeMethodInfo
    ResolveCellRendererSpinnerMethod "getSensitive" o = Gtk.CellRenderer.CellRendererGetSensitiveMethodInfo
    ResolveCellRendererSpinnerMethod "getSize" o = Gtk.CellRenderer.CellRendererGetSizeMethodInfo
    ResolveCellRendererSpinnerMethod "getState" o = Gtk.CellRenderer.CellRendererGetStateMethodInfo
    ResolveCellRendererSpinnerMethod "getVisible" o = Gtk.CellRenderer.CellRendererGetVisibleMethodInfo
    ResolveCellRendererSpinnerMethod "setAlignment" o = Gtk.CellRenderer.CellRendererSetAlignmentMethodInfo
    ResolveCellRendererSpinnerMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveCellRendererSpinnerMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveCellRendererSpinnerMethod "setFixedSize" o = Gtk.CellRenderer.CellRendererSetFixedSizeMethodInfo
    ResolveCellRendererSpinnerMethod "setPadding" o = Gtk.CellRenderer.CellRendererSetPaddingMethodInfo
    ResolveCellRendererSpinnerMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveCellRendererSpinnerMethod "setSensitive" o = Gtk.CellRenderer.CellRendererSetSensitiveMethodInfo
    ResolveCellRendererSpinnerMethod "setVisible" o = Gtk.CellRenderer.CellRendererSetVisibleMethodInfo
    ResolveCellRendererSpinnerMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveCellRendererSpinnerMethod t CellRendererSpinner, O.OverloadedMethod info CellRendererSpinner p) => OL.IsLabel t (CellRendererSpinner -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveCellRendererSpinnerMethod t CellRendererSpinner, O.OverloadedMethod info CellRendererSpinner p, R.HasField t CellRendererSpinner p) => R.HasField t CellRendererSpinner p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveCellRendererSpinnerMethod t CellRendererSpinner, O.OverloadedMethodInfo info CellRendererSpinner) => OL.IsLabel t (O.MethodProxy info CellRendererSpinner) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- VVV Prop "active"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@active@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' cellRendererSpinner #active
-- @
getCellRendererSpinnerActive :: (MonadIO m, IsCellRendererSpinner o) => o -> m Bool
getCellRendererSpinnerActive obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "active"

-- | Set the value of the “@active@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' cellRendererSpinner [ #active 'Data.GI.Base.Attributes.:=' value ]
-- @
setCellRendererSpinnerActive :: (MonadIO m, IsCellRendererSpinner o) => o -> Bool -> m ()
setCellRendererSpinnerActive obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "active" val

-- | Construct a `GValueConstruct` with valid value for the “@active@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructCellRendererSpinnerActive :: (IsCellRendererSpinner o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructCellRendererSpinnerActive val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "active" val

#if defined(ENABLE_OVERLOADING)
data CellRendererSpinnerActivePropertyInfo
instance AttrInfo CellRendererSpinnerActivePropertyInfo where
    type AttrAllowedOps CellRendererSpinnerActivePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint CellRendererSpinnerActivePropertyInfo = IsCellRendererSpinner
    type AttrSetTypeConstraint CellRendererSpinnerActivePropertyInfo = (~) Bool
    type AttrTransferTypeConstraint CellRendererSpinnerActivePropertyInfo = (~) Bool
    type AttrTransferType CellRendererSpinnerActivePropertyInfo = Bool
    type AttrGetType CellRendererSpinnerActivePropertyInfo = Bool
    type AttrLabel CellRendererSpinnerActivePropertyInfo = "active"
    type AttrOrigin CellRendererSpinnerActivePropertyInfo = CellRendererSpinner
    attrGet = getCellRendererSpinnerActive
    attrSet = setCellRendererSpinnerActive
    attrTransfer _ v = do
        return v
    attrConstruct = constructCellRendererSpinnerActive
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRendererSpinner.active"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRendererSpinner.html#g:attr:active"
        })
#endif

-- VVV Prop "pulse"
   -- Type: TBasicType TUInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@pulse@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' cellRendererSpinner #pulse
-- @
getCellRendererSpinnerPulse :: (MonadIO m, IsCellRendererSpinner o) => o -> m Word32
getCellRendererSpinnerPulse obj = MIO.liftIO $ B.Properties.getObjectPropertyUInt32 obj "pulse"

-- | Set the value of the “@pulse@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' cellRendererSpinner [ #pulse 'Data.GI.Base.Attributes.:=' value ]
-- @
setCellRendererSpinnerPulse :: (MonadIO m, IsCellRendererSpinner o) => o -> Word32 -> m ()
setCellRendererSpinnerPulse obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyUInt32 obj "pulse" val

-- | Construct a `GValueConstruct` with valid value for the “@pulse@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructCellRendererSpinnerPulse :: (IsCellRendererSpinner o, MIO.MonadIO m) => Word32 -> m (GValueConstruct o)
constructCellRendererSpinnerPulse val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyUInt32 "pulse" val

#if defined(ENABLE_OVERLOADING)
data CellRendererSpinnerPulsePropertyInfo
instance AttrInfo CellRendererSpinnerPulsePropertyInfo where
    type AttrAllowedOps CellRendererSpinnerPulsePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint CellRendererSpinnerPulsePropertyInfo = IsCellRendererSpinner
    type AttrSetTypeConstraint CellRendererSpinnerPulsePropertyInfo = (~) Word32
    type AttrTransferTypeConstraint CellRendererSpinnerPulsePropertyInfo = (~) Word32
    type AttrTransferType CellRendererSpinnerPulsePropertyInfo = Word32
    type AttrGetType CellRendererSpinnerPulsePropertyInfo = Word32
    type AttrLabel CellRendererSpinnerPulsePropertyInfo = "pulse"
    type AttrOrigin CellRendererSpinnerPulsePropertyInfo = CellRendererSpinner
    attrGet = getCellRendererSpinnerPulse
    attrSet = setCellRendererSpinnerPulse
    attrTransfer _ v = do
        return v
    attrConstruct = constructCellRendererSpinnerPulse
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRendererSpinner.pulse"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRendererSpinner.html#g:attr:pulse"
        })
#endif

-- VVV Prop "size"
   -- Type: TInterface (Name {namespace = "Gtk", name = "IconSize"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@size@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' cellRendererSpinner #size
-- @
getCellRendererSpinnerSize :: (MonadIO m, IsCellRendererSpinner o) => o -> m Gtk.Enums.IconSize
getCellRendererSpinnerSize obj = MIO.liftIO $ B.Properties.getObjectPropertyEnum obj "size"

-- | Set the value of the “@size@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' cellRendererSpinner [ #size 'Data.GI.Base.Attributes.:=' value ]
-- @
setCellRendererSpinnerSize :: (MonadIO m, IsCellRendererSpinner o) => o -> Gtk.Enums.IconSize -> m ()
setCellRendererSpinnerSize obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyEnum obj "size" val

-- | Construct a `GValueConstruct` with valid value for the “@size@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructCellRendererSpinnerSize :: (IsCellRendererSpinner o, MIO.MonadIO m) => Gtk.Enums.IconSize -> m (GValueConstruct o)
constructCellRendererSpinnerSize val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyEnum "size" val

#if defined(ENABLE_OVERLOADING)
data CellRendererSpinnerSizePropertyInfo
instance AttrInfo CellRendererSpinnerSizePropertyInfo where
    type AttrAllowedOps CellRendererSpinnerSizePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint CellRendererSpinnerSizePropertyInfo = IsCellRendererSpinner
    type AttrSetTypeConstraint CellRendererSpinnerSizePropertyInfo = (~) Gtk.Enums.IconSize
    type AttrTransferTypeConstraint CellRendererSpinnerSizePropertyInfo = (~) Gtk.Enums.IconSize
    type AttrTransferType CellRendererSpinnerSizePropertyInfo = Gtk.Enums.IconSize
    type AttrGetType CellRendererSpinnerSizePropertyInfo = Gtk.Enums.IconSize
    type AttrLabel CellRendererSpinnerSizePropertyInfo = "size"
    type AttrOrigin CellRendererSpinnerSizePropertyInfo = CellRendererSpinner
    attrGet = getCellRendererSpinnerSize
    attrSet = setCellRendererSpinnerSize
    attrTransfer _ v = do
        return v
    attrConstruct = constructCellRendererSpinnerSize
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRendererSpinner.size"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRendererSpinner.html#g:attr:size"
        })
#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList CellRendererSpinner
type instance O.AttributeList CellRendererSpinner = CellRendererSpinnerAttributeList
type CellRendererSpinnerAttributeList = ('[ '("active", CellRendererSpinnerActivePropertyInfo), '("cellBackground", Gtk.CellRenderer.CellRendererCellBackgroundPropertyInfo), '("cellBackgroundGdk", Gtk.CellRenderer.CellRendererCellBackgroundGdkPropertyInfo), '("cellBackgroundRgba", Gtk.CellRenderer.CellRendererCellBackgroundRgbaPropertyInfo), '("cellBackgroundSet", Gtk.CellRenderer.CellRendererCellBackgroundSetPropertyInfo), '("editing", Gtk.CellRenderer.CellRendererEditingPropertyInfo), '("height", Gtk.CellRenderer.CellRendererHeightPropertyInfo), '("isExpanded", Gtk.CellRenderer.CellRendererIsExpandedPropertyInfo), '("isExpander", Gtk.CellRenderer.CellRendererIsExpanderPropertyInfo), '("mode", Gtk.CellRenderer.CellRendererModePropertyInfo), '("pulse", CellRendererSpinnerPulsePropertyInfo), '("sensitive", Gtk.CellRenderer.CellRendererSensitivePropertyInfo), '("size", CellRendererSpinnerSizePropertyInfo), '("visible", Gtk.CellRenderer.CellRendererVisiblePropertyInfo), '("width", Gtk.CellRenderer.CellRendererWidthPropertyInfo), '("xalign", Gtk.CellRenderer.CellRendererXalignPropertyInfo), '("xpad", Gtk.CellRenderer.CellRendererXpadPropertyInfo), '("yalign", Gtk.CellRenderer.CellRendererYalignPropertyInfo), '("ypad", Gtk.CellRenderer.CellRendererYpadPropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
cellRendererSpinnerActive :: AttrLabelProxy "active"
cellRendererSpinnerActive = AttrLabelProxy

cellRendererSpinnerPulse :: AttrLabelProxy "pulse"
cellRendererSpinnerPulse = AttrLabelProxy

cellRendererSpinnerSize :: AttrLabelProxy "size"
cellRendererSpinnerSize = AttrLabelProxy

#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList CellRendererSpinner = CellRendererSpinnerSignalList
type CellRendererSpinnerSignalList = ('[ '("editingCanceled", Gtk.CellRenderer.CellRendererEditingCanceledSignalInfo), '("editingStarted", Gtk.CellRenderer.CellRendererEditingStartedSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo)] :: [(Symbol, *)])

#endif

-- method CellRendererSpinner::new
-- method type : Constructor
-- Args: []
-- Lengths: []
-- returnType: Just
--               (TInterface
--                  Name { namespace = "Gtk" , name = "CellRendererSpinner" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_cell_renderer_spinner_new" gtk_cell_renderer_spinner_new :: 
    IO (Ptr CellRendererSpinner)

-- | Returns a new cell renderer which will show a spinner to indicate
-- activity.
-- 
-- /Since: 2.20/
cellRendererSpinnerNew ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m CellRendererSpinner
    -- ^ __Returns:__ a new t'GI.Gtk.Objects.CellRenderer.CellRenderer'
cellRendererSpinnerNew  = liftIO $ do
    result <- gtk_cell_renderer_spinner_new
    checkUnexpectedReturnNULL "cellRendererSpinnerNew" result
    result' <- (newObject CellRendererSpinner) result
    return result'

#if defined(ENABLE_OVERLOADING)
#endif


