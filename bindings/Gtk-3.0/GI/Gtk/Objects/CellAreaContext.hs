{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- The t'GI.Gtk.Objects.CellAreaContext.CellAreaContext' object is created by a given t'GI.Gtk.Objects.CellArea.CellArea'
-- implementation via its t'GI.Gtk.Structs.CellAreaClass.CellAreaClass'.@/create_context/@() virtual
-- method and is used to store cell sizes and alignments for a series of
-- t'GI.Gtk.Interfaces.TreeModel.TreeModel' rows that are requested and rendered in the same context.
-- 
-- t'GI.Gtk.Interfaces.CellLayout.CellLayout' widgets can create any number of contexts in which to
-- request and render groups of data rows. However, it’s important that the
-- same context which was used to request sizes for a given t'GI.Gtk.Interfaces.TreeModel.TreeModel'
-- row also be used for the same row when calling other t'GI.Gtk.Objects.CellArea.CellArea' APIs
-- such as 'GI.Gtk.Objects.CellArea.cellAreaRender' and 'GI.Gtk.Objects.CellArea.cellAreaEvent'.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.CellAreaContext
    ( 

-- * Exported types
    CellAreaContext(..)                     ,
    IsCellAreaContext                       ,
    toCellAreaContext                       ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [allocate]("GI.Gtk.Objects.CellAreaContext#g:method:allocate"), [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [pushPreferredHeight]("GI.Gtk.Objects.CellAreaContext#g:method:pushPreferredHeight"), [pushPreferredWidth]("GI.Gtk.Objects.CellAreaContext#g:method:pushPreferredWidth"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [reset]("GI.Gtk.Objects.CellAreaContext#g:method:reset"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getAllocation]("GI.Gtk.Objects.CellAreaContext#g:method:getAllocation"), [getArea]("GI.Gtk.Objects.CellAreaContext#g:method:getArea"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getPreferredHeight]("GI.Gtk.Objects.CellAreaContext#g:method:getPreferredHeight"), [getPreferredHeightForWidth]("GI.Gtk.Objects.CellAreaContext#g:method:getPreferredHeightForWidth"), [getPreferredWidth]("GI.Gtk.Objects.CellAreaContext#g:method:getPreferredWidth"), [getPreferredWidthForHeight]("GI.Gtk.Objects.CellAreaContext#g:method:getPreferredWidthForHeight"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata").
-- 
-- ==== Setters
-- [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty").

#if defined(ENABLE_OVERLOADING)
    ResolveCellAreaContextMethod            ,
#endif

-- ** allocate #method:allocate#

#if defined(ENABLE_OVERLOADING)
    CellAreaContextAllocateMethodInfo       ,
#endif
    cellAreaContextAllocate                 ,


-- ** getAllocation #method:getAllocation#

#if defined(ENABLE_OVERLOADING)
    CellAreaContextGetAllocationMethodInfo  ,
#endif
    cellAreaContextGetAllocation            ,


-- ** getArea #method:getArea#

#if defined(ENABLE_OVERLOADING)
    CellAreaContextGetAreaMethodInfo        ,
#endif
    cellAreaContextGetArea                  ,


-- ** getPreferredHeight #method:getPreferredHeight#

#if defined(ENABLE_OVERLOADING)
    CellAreaContextGetPreferredHeightMethodInfo,
#endif
    cellAreaContextGetPreferredHeight       ,


-- ** getPreferredHeightForWidth #method:getPreferredHeightForWidth#

#if defined(ENABLE_OVERLOADING)
    CellAreaContextGetPreferredHeightForWidthMethodInfo,
#endif
    cellAreaContextGetPreferredHeightForWidth,


-- ** getPreferredWidth #method:getPreferredWidth#

#if defined(ENABLE_OVERLOADING)
    CellAreaContextGetPreferredWidthMethodInfo,
#endif
    cellAreaContextGetPreferredWidth        ,


-- ** getPreferredWidthForHeight #method:getPreferredWidthForHeight#

#if defined(ENABLE_OVERLOADING)
    CellAreaContextGetPreferredWidthForHeightMethodInfo,
#endif
    cellAreaContextGetPreferredWidthForHeight,


-- ** pushPreferredHeight #method:pushPreferredHeight#

#if defined(ENABLE_OVERLOADING)
    CellAreaContextPushPreferredHeightMethodInfo,
#endif
    cellAreaContextPushPreferredHeight      ,


-- ** pushPreferredWidth #method:pushPreferredWidth#

#if defined(ENABLE_OVERLOADING)
    CellAreaContextPushPreferredWidthMethodInfo,
#endif
    cellAreaContextPushPreferredWidth       ,


-- ** reset #method:reset#

#if defined(ENABLE_OVERLOADING)
    CellAreaContextResetMethodInfo          ,
#endif
    cellAreaContextReset                    ,




 -- * Properties


-- ** area #attr:area#
-- | The t'GI.Gtk.Objects.CellArea.CellArea' this context was created by
-- 
-- /Since: 3.0/

#if defined(ENABLE_OVERLOADING)
    CellAreaContextAreaPropertyInfo         ,
#endif
#if defined(ENABLE_OVERLOADING)
    cellAreaContextArea                     ,
#endif
    constructCellAreaContextArea            ,
    getCellAreaContextArea                  ,


-- ** minimumHeight #attr:minimumHeight#
-- | The minimum height for the t'GI.Gtk.Objects.CellArea.CellArea' in this context
-- for all t'GI.Gtk.Interfaces.TreeModel.TreeModel' rows that this context was requested
-- for using 'GI.Gtk.Objects.CellArea.cellAreaGetPreferredHeight'.
-- 
-- /Since: 3.0/

#if defined(ENABLE_OVERLOADING)
    CellAreaContextMinimumHeightPropertyInfo,
#endif
#if defined(ENABLE_OVERLOADING)
    cellAreaContextMinimumHeight            ,
#endif
    getCellAreaContextMinimumHeight         ,


-- ** minimumWidth #attr:minimumWidth#
-- | The minimum width for the t'GI.Gtk.Objects.CellArea.CellArea' in this context
-- for all t'GI.Gtk.Interfaces.TreeModel.TreeModel' rows that this context was requested
-- for using 'GI.Gtk.Objects.CellArea.cellAreaGetPreferredWidth'.
-- 
-- /Since: 3.0/

#if defined(ENABLE_OVERLOADING)
    CellAreaContextMinimumWidthPropertyInfo ,
#endif
#if defined(ENABLE_OVERLOADING)
    cellAreaContextMinimumWidth             ,
#endif
    getCellAreaContextMinimumWidth          ,


-- ** naturalHeight #attr:naturalHeight#
-- | The natural height for the t'GI.Gtk.Objects.CellArea.CellArea' in this context
-- for all t'GI.Gtk.Interfaces.TreeModel.TreeModel' rows that this context was requested
-- for using 'GI.Gtk.Objects.CellArea.cellAreaGetPreferredHeight'.
-- 
-- /Since: 3.0/

#if defined(ENABLE_OVERLOADING)
    CellAreaContextNaturalHeightPropertyInfo,
#endif
#if defined(ENABLE_OVERLOADING)
    cellAreaContextNaturalHeight            ,
#endif
    getCellAreaContextNaturalHeight         ,


-- ** naturalWidth #attr:naturalWidth#
-- | The natural width for the t'GI.Gtk.Objects.CellArea.CellArea' in this context
-- for all t'GI.Gtk.Interfaces.TreeModel.TreeModel' rows that this context was requested
-- for using 'GI.Gtk.Objects.CellArea.cellAreaGetPreferredWidth'.
-- 
-- /Since: 3.0/

#if defined(ENABLE_OVERLOADING)
    CellAreaContextNaturalWidthPropertyInfo ,
#endif
#if defined(ENABLE_OVERLOADING)
    cellAreaContextNaturalWidth             ,
#endif
    getCellAreaContextNaturalWidth          ,




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
import {-# SOURCE #-} qualified GI.Gtk.Objects.CellArea as Gtk.CellArea

-- | Memory-managed wrapper type.
newtype CellAreaContext = CellAreaContext (SP.ManagedPtr CellAreaContext)
    deriving (Eq)

instance SP.ManagedPtrNewtype CellAreaContext where
    toManagedPtr (CellAreaContext p) = p

foreign import ccall "gtk_cell_area_context_get_type"
    c_gtk_cell_area_context_get_type :: IO B.Types.GType

instance B.Types.TypedObject CellAreaContext where
    glibType = c_gtk_cell_area_context_get_type

instance B.Types.GObject CellAreaContext

-- | Type class for types which can be safely cast to `CellAreaContext`, for instance with `toCellAreaContext`.
class (SP.GObject o, O.IsDescendantOf CellAreaContext o) => IsCellAreaContext o
instance (SP.GObject o, O.IsDescendantOf CellAreaContext o) => IsCellAreaContext o

instance O.HasParentTypes CellAreaContext
type instance O.ParentTypes CellAreaContext = '[GObject.Object.Object]

-- | Cast to `CellAreaContext`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toCellAreaContext :: (MIO.MonadIO m, IsCellAreaContext o) => o -> m CellAreaContext
toCellAreaContext = MIO.liftIO . B.ManagedPtr.unsafeCastTo CellAreaContext

-- | Convert 'CellAreaContext' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe CellAreaContext) where
    gvalueGType_ = c_gtk_cell_area_context_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr CellAreaContext)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr CellAreaContext)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject CellAreaContext ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveCellAreaContextMethod (t :: Symbol) (o :: *) :: * where
    ResolveCellAreaContextMethod "allocate" o = CellAreaContextAllocateMethodInfo
    ResolveCellAreaContextMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveCellAreaContextMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveCellAreaContextMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveCellAreaContextMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveCellAreaContextMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveCellAreaContextMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveCellAreaContextMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveCellAreaContextMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveCellAreaContextMethod "pushPreferredHeight" o = CellAreaContextPushPreferredHeightMethodInfo
    ResolveCellAreaContextMethod "pushPreferredWidth" o = CellAreaContextPushPreferredWidthMethodInfo
    ResolveCellAreaContextMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveCellAreaContextMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveCellAreaContextMethod "reset" o = CellAreaContextResetMethodInfo
    ResolveCellAreaContextMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveCellAreaContextMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveCellAreaContextMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveCellAreaContextMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveCellAreaContextMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveCellAreaContextMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveCellAreaContextMethod "getAllocation" o = CellAreaContextGetAllocationMethodInfo
    ResolveCellAreaContextMethod "getArea" o = CellAreaContextGetAreaMethodInfo
    ResolveCellAreaContextMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveCellAreaContextMethod "getPreferredHeight" o = CellAreaContextGetPreferredHeightMethodInfo
    ResolveCellAreaContextMethod "getPreferredHeightForWidth" o = CellAreaContextGetPreferredHeightForWidthMethodInfo
    ResolveCellAreaContextMethod "getPreferredWidth" o = CellAreaContextGetPreferredWidthMethodInfo
    ResolveCellAreaContextMethod "getPreferredWidthForHeight" o = CellAreaContextGetPreferredWidthForHeightMethodInfo
    ResolveCellAreaContextMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveCellAreaContextMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveCellAreaContextMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveCellAreaContextMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveCellAreaContextMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveCellAreaContextMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveCellAreaContextMethod t CellAreaContext, O.OverloadedMethod info CellAreaContext p) => OL.IsLabel t (CellAreaContext -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveCellAreaContextMethod t CellAreaContext, O.OverloadedMethod info CellAreaContext p, R.HasField t CellAreaContext p) => R.HasField t CellAreaContext p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveCellAreaContextMethod t CellAreaContext, O.OverloadedMethodInfo info CellAreaContext) => OL.IsLabel t (O.MethodProxy info CellAreaContext) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- VVV Prop "area"
   -- Type: TInterface (Name {namespace = "Gtk", name = "CellArea"})
   -- Flags: [PropertyReadable,PropertyWritable,PropertyConstructOnly]
   -- Nullable: (Just False,Nothing)

-- | Get the value of the “@area@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' cellAreaContext #area
-- @
getCellAreaContextArea :: (MonadIO m, IsCellAreaContext o) => o -> m Gtk.CellArea.CellArea
getCellAreaContextArea obj = MIO.liftIO $ checkUnexpectedNothing "getCellAreaContextArea" $ B.Properties.getObjectPropertyObject obj "area" Gtk.CellArea.CellArea

-- | Construct a `GValueConstruct` with valid value for the “@area@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructCellAreaContextArea :: (IsCellAreaContext o, MIO.MonadIO m, Gtk.CellArea.IsCellArea a) => a -> m (GValueConstruct o)
constructCellAreaContextArea val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyObject "area" (P.Just val)

#if defined(ENABLE_OVERLOADING)
data CellAreaContextAreaPropertyInfo
instance AttrInfo CellAreaContextAreaPropertyInfo where
    type AttrAllowedOps CellAreaContextAreaPropertyInfo = '[ 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint CellAreaContextAreaPropertyInfo = IsCellAreaContext
    type AttrSetTypeConstraint CellAreaContextAreaPropertyInfo = Gtk.CellArea.IsCellArea
    type AttrTransferTypeConstraint CellAreaContextAreaPropertyInfo = Gtk.CellArea.IsCellArea
    type AttrTransferType CellAreaContextAreaPropertyInfo = Gtk.CellArea.CellArea
    type AttrGetType CellAreaContextAreaPropertyInfo = Gtk.CellArea.CellArea
    type AttrLabel CellAreaContextAreaPropertyInfo = "area"
    type AttrOrigin CellAreaContextAreaPropertyInfo = CellAreaContext
    attrGet = getCellAreaContextArea
    attrSet = undefined
    attrTransfer _ v = do
        unsafeCastTo Gtk.CellArea.CellArea v
    attrConstruct = constructCellAreaContextArea
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellAreaContext.area"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellAreaContext.html#g:attr:area"
        })
#endif

-- VVV Prop "minimum-height"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@minimum-height@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' cellAreaContext #minimumHeight
-- @
getCellAreaContextMinimumHeight :: (MonadIO m, IsCellAreaContext o) => o -> m Int32
getCellAreaContextMinimumHeight obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "minimum-height"

#if defined(ENABLE_OVERLOADING)
data CellAreaContextMinimumHeightPropertyInfo
instance AttrInfo CellAreaContextMinimumHeightPropertyInfo where
    type AttrAllowedOps CellAreaContextMinimumHeightPropertyInfo = '[ 'AttrGet]
    type AttrBaseTypeConstraint CellAreaContextMinimumHeightPropertyInfo = IsCellAreaContext
    type AttrSetTypeConstraint CellAreaContextMinimumHeightPropertyInfo = (~) ()
    type AttrTransferTypeConstraint CellAreaContextMinimumHeightPropertyInfo = (~) ()
    type AttrTransferType CellAreaContextMinimumHeightPropertyInfo = ()
    type AttrGetType CellAreaContextMinimumHeightPropertyInfo = Int32
    type AttrLabel CellAreaContextMinimumHeightPropertyInfo = "minimum-height"
    type AttrOrigin CellAreaContextMinimumHeightPropertyInfo = CellAreaContext
    attrGet = getCellAreaContextMinimumHeight
    attrSet = undefined
    attrTransfer _ = undefined
    attrConstruct = undefined
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellAreaContext.minimumHeight"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellAreaContext.html#g:attr:minimumHeight"
        })
#endif

-- VVV Prop "minimum-width"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@minimum-width@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' cellAreaContext #minimumWidth
-- @
getCellAreaContextMinimumWidth :: (MonadIO m, IsCellAreaContext o) => o -> m Int32
getCellAreaContextMinimumWidth obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "minimum-width"

#if defined(ENABLE_OVERLOADING)
data CellAreaContextMinimumWidthPropertyInfo
instance AttrInfo CellAreaContextMinimumWidthPropertyInfo where
    type AttrAllowedOps CellAreaContextMinimumWidthPropertyInfo = '[ 'AttrGet]
    type AttrBaseTypeConstraint CellAreaContextMinimumWidthPropertyInfo = IsCellAreaContext
    type AttrSetTypeConstraint CellAreaContextMinimumWidthPropertyInfo = (~) ()
    type AttrTransferTypeConstraint CellAreaContextMinimumWidthPropertyInfo = (~) ()
    type AttrTransferType CellAreaContextMinimumWidthPropertyInfo = ()
    type AttrGetType CellAreaContextMinimumWidthPropertyInfo = Int32
    type AttrLabel CellAreaContextMinimumWidthPropertyInfo = "minimum-width"
    type AttrOrigin CellAreaContextMinimumWidthPropertyInfo = CellAreaContext
    attrGet = getCellAreaContextMinimumWidth
    attrSet = undefined
    attrTransfer _ = undefined
    attrConstruct = undefined
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellAreaContext.minimumWidth"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellAreaContext.html#g:attr:minimumWidth"
        })
#endif

-- VVV Prop "natural-height"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@natural-height@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' cellAreaContext #naturalHeight
-- @
getCellAreaContextNaturalHeight :: (MonadIO m, IsCellAreaContext o) => o -> m Int32
getCellAreaContextNaturalHeight obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "natural-height"

#if defined(ENABLE_OVERLOADING)
data CellAreaContextNaturalHeightPropertyInfo
instance AttrInfo CellAreaContextNaturalHeightPropertyInfo where
    type AttrAllowedOps CellAreaContextNaturalHeightPropertyInfo = '[ 'AttrGet]
    type AttrBaseTypeConstraint CellAreaContextNaturalHeightPropertyInfo = IsCellAreaContext
    type AttrSetTypeConstraint CellAreaContextNaturalHeightPropertyInfo = (~) ()
    type AttrTransferTypeConstraint CellAreaContextNaturalHeightPropertyInfo = (~) ()
    type AttrTransferType CellAreaContextNaturalHeightPropertyInfo = ()
    type AttrGetType CellAreaContextNaturalHeightPropertyInfo = Int32
    type AttrLabel CellAreaContextNaturalHeightPropertyInfo = "natural-height"
    type AttrOrigin CellAreaContextNaturalHeightPropertyInfo = CellAreaContext
    attrGet = getCellAreaContextNaturalHeight
    attrSet = undefined
    attrTransfer _ = undefined
    attrConstruct = undefined
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellAreaContext.naturalHeight"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellAreaContext.html#g:attr:naturalHeight"
        })
#endif

-- VVV Prop "natural-width"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@natural-width@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' cellAreaContext #naturalWidth
-- @
getCellAreaContextNaturalWidth :: (MonadIO m, IsCellAreaContext o) => o -> m Int32
getCellAreaContextNaturalWidth obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "natural-width"

#if defined(ENABLE_OVERLOADING)
data CellAreaContextNaturalWidthPropertyInfo
instance AttrInfo CellAreaContextNaturalWidthPropertyInfo where
    type AttrAllowedOps CellAreaContextNaturalWidthPropertyInfo = '[ 'AttrGet]
    type AttrBaseTypeConstraint CellAreaContextNaturalWidthPropertyInfo = IsCellAreaContext
    type AttrSetTypeConstraint CellAreaContextNaturalWidthPropertyInfo = (~) ()
    type AttrTransferTypeConstraint CellAreaContextNaturalWidthPropertyInfo = (~) ()
    type AttrTransferType CellAreaContextNaturalWidthPropertyInfo = ()
    type AttrGetType CellAreaContextNaturalWidthPropertyInfo = Int32
    type AttrLabel CellAreaContextNaturalWidthPropertyInfo = "natural-width"
    type AttrOrigin CellAreaContextNaturalWidthPropertyInfo = CellAreaContext
    attrGet = getCellAreaContextNaturalWidth
    attrSet = undefined
    attrTransfer _ = undefined
    attrConstruct = undefined
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellAreaContext.naturalWidth"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellAreaContext.html#g:attr:naturalWidth"
        })
#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList CellAreaContext
type instance O.AttributeList CellAreaContext = CellAreaContextAttributeList
type CellAreaContextAttributeList = ('[ '("area", CellAreaContextAreaPropertyInfo), '("minimumHeight", CellAreaContextMinimumHeightPropertyInfo), '("minimumWidth", CellAreaContextMinimumWidthPropertyInfo), '("naturalHeight", CellAreaContextNaturalHeightPropertyInfo), '("naturalWidth", CellAreaContextNaturalWidthPropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
cellAreaContextArea :: AttrLabelProxy "area"
cellAreaContextArea = AttrLabelProxy

cellAreaContextMinimumHeight :: AttrLabelProxy "minimumHeight"
cellAreaContextMinimumHeight = AttrLabelProxy

cellAreaContextMinimumWidth :: AttrLabelProxy "minimumWidth"
cellAreaContextMinimumWidth = AttrLabelProxy

cellAreaContextNaturalHeight :: AttrLabelProxy "naturalHeight"
cellAreaContextNaturalHeight = AttrLabelProxy

cellAreaContextNaturalWidth :: AttrLabelProxy "naturalWidth"
cellAreaContextNaturalWidth = AttrLabelProxy

#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList CellAreaContext = CellAreaContextSignalList
type CellAreaContextSignalList = ('[ '("notify", GObject.Object.ObjectNotifySignalInfo)] :: [(Symbol, *)])

#endif

-- method CellAreaContext::allocate
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellAreaContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCellAreaContext"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "width"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the allocated width for all #GtkTreeModel rows rendered\n    with @context, or -1."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "height"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the allocated height for all #GtkTreeModel rows rendered\n    with @context, or -1."
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

foreign import ccall "gtk_cell_area_context_allocate" gtk_cell_area_context_allocate :: 
    Ptr CellAreaContext ->                  -- context : TInterface (Name {namespace = "Gtk", name = "CellAreaContext"})
    Int32 ->                                -- width : TBasicType TInt
    Int32 ->                                -- height : TBasicType TInt
    IO ()

-- | Allocates a width and\/or a height for all rows which are to be
-- rendered with /@context@/.
-- 
-- Usually allocation is performed only horizontally or sometimes
-- vertically since a group of rows are usually rendered side by
-- side vertically or horizontally and share either the same width
-- or the same height. Sometimes they are allocated in both horizontal
-- and vertical orientations producing a homogeneous effect of the
-- rows. This is generally the case for t'GI.Gtk.Objects.TreeView.TreeView' when
-- [TreeView:fixedHeightMode]("GI.Gtk.Objects.TreeView#g:attr:fixedHeightMode") is enabled.
-- 
-- Since 3.0
cellAreaContextAllocate ::
    (B.CallStack.HasCallStack, MonadIO m, IsCellAreaContext a) =>
    a
    -- ^ /@context@/: a t'GI.Gtk.Objects.CellAreaContext.CellAreaContext'
    -> Int32
    -- ^ /@width@/: the allocated width for all t'GI.Gtk.Interfaces.TreeModel.TreeModel' rows rendered
    --     with /@context@/, or -1.
    -> Int32
    -- ^ /@height@/: the allocated height for all t'GI.Gtk.Interfaces.TreeModel.TreeModel' rows rendered
    --     with /@context@/, or -1.
    -> m ()
cellAreaContextAllocate context width height = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    gtk_cell_area_context_allocate context' width height
    touchManagedPtr context
    return ()

#if defined(ENABLE_OVERLOADING)
data CellAreaContextAllocateMethodInfo
instance (signature ~ (Int32 -> Int32 -> m ()), MonadIO m, IsCellAreaContext a) => O.OverloadedMethod CellAreaContextAllocateMethodInfo a signature where
    overloadedMethod = cellAreaContextAllocate

instance O.OverloadedMethodInfo CellAreaContextAllocateMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellAreaContext.cellAreaContextAllocate",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellAreaContext.html#v:cellAreaContextAllocate"
        })


#endif

-- method CellAreaContext::get_allocation
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellAreaContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCellAreaContext"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "width"
--           , argType = TBasicType TInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "location to store the allocated width, or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       , Arg
--           { argCName = "height"
--           , argType = TBasicType TInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "location to store the allocated height, or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       ]
-- Lengths: []
-- returnType: Nothing
-- throws : False
-- Skip return : False

foreign import ccall "gtk_cell_area_context_get_allocation" gtk_cell_area_context_get_allocation :: 
    Ptr CellAreaContext ->                  -- context : TInterface (Name {namespace = "Gtk", name = "CellAreaContext"})
    Ptr Int32 ->                            -- width : TBasicType TInt
    Ptr Int32 ->                            -- height : TBasicType TInt
    IO ()

-- | Fetches the current allocation size for /@context@/.
-- 
-- If the context was not allocated in width or height, or if the
-- context was recently reset with 'GI.Gtk.Objects.CellAreaContext.cellAreaContextReset',
-- the returned value will be -1.
-- 
-- /Since: 3.0/
cellAreaContextGetAllocation ::
    (B.CallStack.HasCallStack, MonadIO m, IsCellAreaContext a) =>
    a
    -- ^ /@context@/: a t'GI.Gtk.Objects.CellAreaContext.CellAreaContext'
    -> m ((Int32, Int32))
cellAreaContextGetAllocation context = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    width <- allocMem :: IO (Ptr Int32)
    height <- allocMem :: IO (Ptr Int32)
    gtk_cell_area_context_get_allocation context' width height
    width' <- peek width
    height' <- peek height
    touchManagedPtr context
    freeMem width
    freeMem height
    return (width', height')

#if defined(ENABLE_OVERLOADING)
data CellAreaContextGetAllocationMethodInfo
instance (signature ~ (m ((Int32, Int32))), MonadIO m, IsCellAreaContext a) => O.OverloadedMethod CellAreaContextGetAllocationMethodInfo a signature where
    overloadedMethod = cellAreaContextGetAllocation

instance O.OverloadedMethodInfo CellAreaContextGetAllocationMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellAreaContext.cellAreaContextGetAllocation",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellAreaContext.html#v:cellAreaContextGetAllocation"
        })


#endif

-- method CellAreaContext::get_area
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellAreaContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCellAreaContext"
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "CellArea" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_cell_area_context_get_area" gtk_cell_area_context_get_area :: 
    Ptr CellAreaContext ->                  -- context : TInterface (Name {namespace = "Gtk", name = "CellAreaContext"})
    IO (Ptr Gtk.CellArea.CellArea)

-- | Fetches the t'GI.Gtk.Objects.CellArea.CellArea' this /@context@/ was created by.
-- 
-- This is generally unneeded by layouting widgets; however,
-- it is important for the context implementation itself to
-- fetch information about the area it is being used for.
-- 
-- For instance at t'GI.Gtk.Structs.CellAreaContextClass.CellAreaContextClass'.@/allocate/@() time
-- it’s important to know details about any cell spacing
-- that the t'GI.Gtk.Objects.CellArea.CellArea' is configured with in order to
-- compute a proper allocation.
-- 
-- /Since: 3.0/
cellAreaContextGetArea ::
    (B.CallStack.HasCallStack, MonadIO m, IsCellAreaContext a) =>
    a
    -- ^ /@context@/: a t'GI.Gtk.Objects.CellAreaContext.CellAreaContext'
    -> m Gtk.CellArea.CellArea
    -- ^ __Returns:__ the t'GI.Gtk.Objects.CellArea.CellArea' this context was created by.
cellAreaContextGetArea context = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    result <- gtk_cell_area_context_get_area context'
    checkUnexpectedReturnNULL "cellAreaContextGetArea" result
    result' <- (newObject Gtk.CellArea.CellArea) result
    touchManagedPtr context
    return result'

#if defined(ENABLE_OVERLOADING)
data CellAreaContextGetAreaMethodInfo
instance (signature ~ (m Gtk.CellArea.CellArea), MonadIO m, IsCellAreaContext a) => O.OverloadedMethod CellAreaContextGetAreaMethodInfo a signature where
    overloadedMethod = cellAreaContextGetArea

instance O.OverloadedMethodInfo CellAreaContextGetAreaMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellAreaContext.cellAreaContextGetArea",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellAreaContext.html#v:cellAreaContextGetArea"
        })


#endif

-- method CellAreaContext::get_preferred_height
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellAreaContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCellAreaContext"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "minimum_height"
--           , argType = TBasicType TInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "location to store the minimum height,\n    or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       , Arg
--           { argCName = "natural_height"
--           , argType = TBasicType TInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "location to store the natural height,\n    or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       ]
-- Lengths: []
-- returnType: Nothing
-- throws : False
-- Skip return : False

foreign import ccall "gtk_cell_area_context_get_preferred_height" gtk_cell_area_context_get_preferred_height :: 
    Ptr CellAreaContext ->                  -- context : TInterface (Name {namespace = "Gtk", name = "CellAreaContext"})
    Ptr Int32 ->                            -- minimum_height : TBasicType TInt
    Ptr Int32 ->                            -- natural_height : TBasicType TInt
    IO ()

-- | Gets the accumulative preferred height for all rows which have been
-- requested with this context.
-- 
-- After 'GI.Gtk.Objects.CellAreaContext.cellAreaContextReset' is called and\/or before ever
-- requesting the size of a t'GI.Gtk.Objects.CellArea.CellArea', the returned values are 0.
-- 
-- /Since: 3.0/
cellAreaContextGetPreferredHeight ::
    (B.CallStack.HasCallStack, MonadIO m, IsCellAreaContext a) =>
    a
    -- ^ /@context@/: a t'GI.Gtk.Objects.CellAreaContext.CellAreaContext'
    -> m ((Int32, Int32))
cellAreaContextGetPreferredHeight context = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    minimumHeight <- allocMem :: IO (Ptr Int32)
    naturalHeight <- allocMem :: IO (Ptr Int32)
    gtk_cell_area_context_get_preferred_height context' minimumHeight naturalHeight
    minimumHeight' <- peek minimumHeight
    naturalHeight' <- peek naturalHeight
    touchManagedPtr context
    freeMem minimumHeight
    freeMem naturalHeight
    return (minimumHeight', naturalHeight')

#if defined(ENABLE_OVERLOADING)
data CellAreaContextGetPreferredHeightMethodInfo
instance (signature ~ (m ((Int32, Int32))), MonadIO m, IsCellAreaContext a) => O.OverloadedMethod CellAreaContextGetPreferredHeightMethodInfo a signature where
    overloadedMethod = cellAreaContextGetPreferredHeight

instance O.OverloadedMethodInfo CellAreaContextGetPreferredHeightMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellAreaContext.cellAreaContextGetPreferredHeight",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellAreaContext.html#v:cellAreaContextGetPreferredHeight"
        })


#endif

-- method CellAreaContext::get_preferred_height_for_width
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellAreaContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCellAreaContext"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "width"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a proposed width for allocation"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "minimum_height"
--           , argType = TBasicType TInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "location to store the minimum height,\n    or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       , Arg
--           { argCName = "natural_height"
--           , argType = TBasicType TInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "location to store the natural height,\n    or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       ]
-- Lengths: []
-- returnType: Nothing
-- throws : False
-- Skip return : False

foreign import ccall "gtk_cell_area_context_get_preferred_height_for_width" gtk_cell_area_context_get_preferred_height_for_width :: 
    Ptr CellAreaContext ->                  -- context : TInterface (Name {namespace = "Gtk", name = "CellAreaContext"})
    Int32 ->                                -- width : TBasicType TInt
    Ptr Int32 ->                            -- minimum_height : TBasicType TInt
    Ptr Int32 ->                            -- natural_height : TBasicType TInt
    IO ()

-- | Gets the accumulative preferred height for /@width@/ for all rows
-- which have been requested for the same said /@width@/ with this context.
-- 
-- After 'GI.Gtk.Objects.CellAreaContext.cellAreaContextReset' is called and\/or before ever
-- requesting the size of a t'GI.Gtk.Objects.CellArea.CellArea', the returned values are -1.
-- 
-- /Since: 3.0/
cellAreaContextGetPreferredHeightForWidth ::
    (B.CallStack.HasCallStack, MonadIO m, IsCellAreaContext a) =>
    a
    -- ^ /@context@/: a t'GI.Gtk.Objects.CellAreaContext.CellAreaContext'
    -> Int32
    -- ^ /@width@/: a proposed width for allocation
    -> m ((Int32, Int32))
cellAreaContextGetPreferredHeightForWidth context width = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    minimumHeight <- allocMem :: IO (Ptr Int32)
    naturalHeight <- allocMem :: IO (Ptr Int32)
    gtk_cell_area_context_get_preferred_height_for_width context' width minimumHeight naturalHeight
    minimumHeight' <- peek minimumHeight
    naturalHeight' <- peek naturalHeight
    touchManagedPtr context
    freeMem minimumHeight
    freeMem naturalHeight
    return (minimumHeight', naturalHeight')

#if defined(ENABLE_OVERLOADING)
data CellAreaContextGetPreferredHeightForWidthMethodInfo
instance (signature ~ (Int32 -> m ((Int32, Int32))), MonadIO m, IsCellAreaContext a) => O.OverloadedMethod CellAreaContextGetPreferredHeightForWidthMethodInfo a signature where
    overloadedMethod = cellAreaContextGetPreferredHeightForWidth

instance O.OverloadedMethodInfo CellAreaContextGetPreferredHeightForWidthMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellAreaContext.cellAreaContextGetPreferredHeightForWidth",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellAreaContext.html#v:cellAreaContextGetPreferredHeightForWidth"
        })


#endif

-- method CellAreaContext::get_preferred_width
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellAreaContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCellAreaContext"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "minimum_width"
--           , argType = TBasicType TInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "location to store the minimum width,\n    or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       , Arg
--           { argCName = "natural_width"
--           , argType = TBasicType TInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "location to store the natural width,\n    or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       ]
-- Lengths: []
-- returnType: Nothing
-- throws : False
-- Skip return : False

foreign import ccall "gtk_cell_area_context_get_preferred_width" gtk_cell_area_context_get_preferred_width :: 
    Ptr CellAreaContext ->                  -- context : TInterface (Name {namespace = "Gtk", name = "CellAreaContext"})
    Ptr Int32 ->                            -- minimum_width : TBasicType TInt
    Ptr Int32 ->                            -- natural_width : TBasicType TInt
    IO ()

-- | Gets the accumulative preferred width for all rows which have been
-- requested with this context.
-- 
-- After 'GI.Gtk.Objects.CellAreaContext.cellAreaContextReset' is called and\/or before ever
-- requesting the size of a t'GI.Gtk.Objects.CellArea.CellArea', the returned values are 0.
-- 
-- /Since: 3.0/
cellAreaContextGetPreferredWidth ::
    (B.CallStack.HasCallStack, MonadIO m, IsCellAreaContext a) =>
    a
    -- ^ /@context@/: a t'GI.Gtk.Objects.CellAreaContext.CellAreaContext'
    -> m ((Int32, Int32))
cellAreaContextGetPreferredWidth context = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    minimumWidth <- allocMem :: IO (Ptr Int32)
    naturalWidth <- allocMem :: IO (Ptr Int32)
    gtk_cell_area_context_get_preferred_width context' minimumWidth naturalWidth
    minimumWidth' <- peek minimumWidth
    naturalWidth' <- peek naturalWidth
    touchManagedPtr context
    freeMem minimumWidth
    freeMem naturalWidth
    return (minimumWidth', naturalWidth')

#if defined(ENABLE_OVERLOADING)
data CellAreaContextGetPreferredWidthMethodInfo
instance (signature ~ (m ((Int32, Int32))), MonadIO m, IsCellAreaContext a) => O.OverloadedMethod CellAreaContextGetPreferredWidthMethodInfo a signature where
    overloadedMethod = cellAreaContextGetPreferredWidth

instance O.OverloadedMethodInfo CellAreaContextGetPreferredWidthMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellAreaContext.cellAreaContextGetPreferredWidth",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellAreaContext.html#v:cellAreaContextGetPreferredWidth"
        })


#endif

-- method CellAreaContext::get_preferred_width_for_height
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellAreaContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCellAreaContext"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "height"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a proposed height for allocation"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "minimum_width"
--           , argType = TBasicType TInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "location to store the minimum width,\n    or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       , Arg
--           { argCName = "natural_width"
--           , argType = TBasicType TInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "location to store the natural width,\n    or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       ]
-- Lengths: []
-- returnType: Nothing
-- throws : False
-- Skip return : False

foreign import ccall "gtk_cell_area_context_get_preferred_width_for_height" gtk_cell_area_context_get_preferred_width_for_height :: 
    Ptr CellAreaContext ->                  -- context : TInterface (Name {namespace = "Gtk", name = "CellAreaContext"})
    Int32 ->                                -- height : TBasicType TInt
    Ptr Int32 ->                            -- minimum_width : TBasicType TInt
    Ptr Int32 ->                            -- natural_width : TBasicType TInt
    IO ()

-- | Gets the accumulative preferred width for /@height@/ for all rows which
-- have been requested for the same said /@height@/ with this context.
-- 
-- After 'GI.Gtk.Objects.CellAreaContext.cellAreaContextReset' is called and\/or before ever
-- requesting the size of a t'GI.Gtk.Objects.CellArea.CellArea', the returned values are -1.
-- 
-- /Since: 3.0/
cellAreaContextGetPreferredWidthForHeight ::
    (B.CallStack.HasCallStack, MonadIO m, IsCellAreaContext a) =>
    a
    -- ^ /@context@/: a t'GI.Gtk.Objects.CellAreaContext.CellAreaContext'
    -> Int32
    -- ^ /@height@/: a proposed height for allocation
    -> m ((Int32, Int32))
cellAreaContextGetPreferredWidthForHeight context height = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    minimumWidth <- allocMem :: IO (Ptr Int32)
    naturalWidth <- allocMem :: IO (Ptr Int32)
    gtk_cell_area_context_get_preferred_width_for_height context' height minimumWidth naturalWidth
    minimumWidth' <- peek minimumWidth
    naturalWidth' <- peek naturalWidth
    touchManagedPtr context
    freeMem minimumWidth
    freeMem naturalWidth
    return (minimumWidth', naturalWidth')

#if defined(ENABLE_OVERLOADING)
data CellAreaContextGetPreferredWidthForHeightMethodInfo
instance (signature ~ (Int32 -> m ((Int32, Int32))), MonadIO m, IsCellAreaContext a) => O.OverloadedMethod CellAreaContextGetPreferredWidthForHeightMethodInfo a signature where
    overloadedMethod = cellAreaContextGetPreferredWidthForHeight

instance O.OverloadedMethodInfo CellAreaContextGetPreferredWidthForHeightMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellAreaContext.cellAreaContextGetPreferredWidthForHeight",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellAreaContext.html#v:cellAreaContextGetPreferredWidthForHeight"
        })


#endif

-- method CellAreaContext::push_preferred_height
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellAreaContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCellAreaContext"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "minimum_height"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the proposed new minimum height for @context"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "natural_height"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the proposed new natural height for @context"
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

foreign import ccall "gtk_cell_area_context_push_preferred_height" gtk_cell_area_context_push_preferred_height :: 
    Ptr CellAreaContext ->                  -- context : TInterface (Name {namespace = "Gtk", name = "CellAreaContext"})
    Int32 ->                                -- minimum_height : TBasicType TInt
    Int32 ->                                -- natural_height : TBasicType TInt
    IO ()

-- | Causes the minimum and\/or natural height to grow if the new
-- proposed sizes exceed the current minimum and natural height.
-- 
-- This is used by t'GI.Gtk.Objects.CellAreaContext.CellAreaContext' implementations during
-- the request process over a series of t'GI.Gtk.Interfaces.TreeModel.TreeModel' rows to
-- progressively push the requested height over a series of
-- 'GI.Gtk.Objects.CellArea.cellAreaGetPreferredHeight' requests.
-- 
-- /Since: 3.0/
cellAreaContextPushPreferredHeight ::
    (B.CallStack.HasCallStack, MonadIO m, IsCellAreaContext a) =>
    a
    -- ^ /@context@/: a t'GI.Gtk.Objects.CellAreaContext.CellAreaContext'
    -> Int32
    -- ^ /@minimumHeight@/: the proposed new minimum height for /@context@/
    -> Int32
    -- ^ /@naturalHeight@/: the proposed new natural height for /@context@/
    -> m ()
cellAreaContextPushPreferredHeight context minimumHeight naturalHeight = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    gtk_cell_area_context_push_preferred_height context' minimumHeight naturalHeight
    touchManagedPtr context
    return ()

#if defined(ENABLE_OVERLOADING)
data CellAreaContextPushPreferredHeightMethodInfo
instance (signature ~ (Int32 -> Int32 -> m ()), MonadIO m, IsCellAreaContext a) => O.OverloadedMethod CellAreaContextPushPreferredHeightMethodInfo a signature where
    overloadedMethod = cellAreaContextPushPreferredHeight

instance O.OverloadedMethodInfo CellAreaContextPushPreferredHeightMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellAreaContext.cellAreaContextPushPreferredHeight",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellAreaContext.html#v:cellAreaContextPushPreferredHeight"
        })


#endif

-- method CellAreaContext::push_preferred_width
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellAreaContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCellAreaContext"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "minimum_width"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the proposed new minimum width for @context"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "natural_width"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the proposed new natural width for @context"
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

foreign import ccall "gtk_cell_area_context_push_preferred_width" gtk_cell_area_context_push_preferred_width :: 
    Ptr CellAreaContext ->                  -- context : TInterface (Name {namespace = "Gtk", name = "CellAreaContext"})
    Int32 ->                                -- minimum_width : TBasicType TInt
    Int32 ->                                -- natural_width : TBasicType TInt
    IO ()

-- | Causes the minimum and\/or natural width to grow if the new
-- proposed sizes exceed the current minimum and natural width.
-- 
-- This is used by t'GI.Gtk.Objects.CellAreaContext.CellAreaContext' implementations during
-- the request process over a series of t'GI.Gtk.Interfaces.TreeModel.TreeModel' rows to
-- progressively push the requested width over a series of
-- 'GI.Gtk.Objects.CellArea.cellAreaGetPreferredWidth' requests.
-- 
-- /Since: 3.0/
cellAreaContextPushPreferredWidth ::
    (B.CallStack.HasCallStack, MonadIO m, IsCellAreaContext a) =>
    a
    -- ^ /@context@/: a t'GI.Gtk.Objects.CellAreaContext.CellAreaContext'
    -> Int32
    -- ^ /@minimumWidth@/: the proposed new minimum width for /@context@/
    -> Int32
    -- ^ /@naturalWidth@/: the proposed new natural width for /@context@/
    -> m ()
cellAreaContextPushPreferredWidth context minimumWidth naturalWidth = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    gtk_cell_area_context_push_preferred_width context' minimumWidth naturalWidth
    touchManagedPtr context
    return ()

#if defined(ENABLE_OVERLOADING)
data CellAreaContextPushPreferredWidthMethodInfo
instance (signature ~ (Int32 -> Int32 -> m ()), MonadIO m, IsCellAreaContext a) => O.OverloadedMethod CellAreaContextPushPreferredWidthMethodInfo a signature where
    overloadedMethod = cellAreaContextPushPreferredWidth

instance O.OverloadedMethodInfo CellAreaContextPushPreferredWidthMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellAreaContext.cellAreaContextPushPreferredWidth",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellAreaContext.html#v:cellAreaContextPushPreferredWidth"
        })


#endif

-- method CellAreaContext::reset
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellAreaContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCellAreaContext"
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

foreign import ccall "gtk_cell_area_context_reset" gtk_cell_area_context_reset :: 
    Ptr CellAreaContext ->                  -- context : TInterface (Name {namespace = "Gtk", name = "CellAreaContext"})
    IO ()

-- | Resets any previously cached request and allocation
-- data.
-- 
-- When underlying t'GI.Gtk.Interfaces.TreeModel.TreeModel' data changes its
-- important to reset the context if the content
-- size is allowed to shrink. If the content size
-- is only allowed to grow (this is usually an option
-- for views rendering large data stores as a measure
-- of optimization), then only the row that changed
-- or was inserted needs to be (re)requested with
-- 'GI.Gtk.Objects.CellArea.cellAreaGetPreferredWidth'.
-- 
-- When the new overall size of the context requires
-- that the allocated size changes (or whenever this
-- allocation changes at all), the variable row
-- sizes need to be re-requested for every row.
-- 
-- For instance, if the rows are displayed all with
-- the same width from top to bottom then a change
-- in the allocated width necessitates a recalculation
-- of all the displayed row heights using
-- 'GI.Gtk.Objects.CellArea.cellAreaGetPreferredHeightForWidth'.
-- 
-- Since 3.0
cellAreaContextReset ::
    (B.CallStack.HasCallStack, MonadIO m, IsCellAreaContext a) =>
    a
    -- ^ /@context@/: a t'GI.Gtk.Objects.CellAreaContext.CellAreaContext'
    -> m ()
cellAreaContextReset context = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    gtk_cell_area_context_reset context'
    touchManagedPtr context
    return ()

#if defined(ENABLE_OVERLOADING)
data CellAreaContextResetMethodInfo
instance (signature ~ (m ()), MonadIO m, IsCellAreaContext a) => O.OverloadedMethod CellAreaContextResetMethodInfo a signature where
    overloadedMethod = cellAreaContextReset

instance O.OverloadedMethodInfo CellAreaContextResetMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellAreaContext.cellAreaContextReset",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellAreaContext.html#v:cellAreaContextReset"
        })


#endif


