{-# LANGUAGE ImplicitParams, RankNTypes, TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- The t'GI.Gtk.Objects.TreeSelection.TreeSelection' object is a helper object to manage the selection
-- for a t'GI.Gtk.Objects.TreeView.TreeView' widget.  The t'GI.Gtk.Objects.TreeSelection.TreeSelection' object is
-- automatically created when a new t'GI.Gtk.Objects.TreeView.TreeView' widget is created, and
-- cannot exist independently of this widget.  The primary reason the
-- t'GI.Gtk.Objects.TreeSelection.TreeSelection' objects exists is for cleanliness of code and API.
-- That is, there is no conceptual reason all these functions could not be
-- methods on the t'GI.Gtk.Objects.TreeView.TreeView' widget instead of a separate function.
-- 
-- The t'GI.Gtk.Objects.TreeSelection.TreeSelection' object is gotten from a t'GI.Gtk.Objects.TreeView.TreeView' by calling
-- 'GI.Gtk.Objects.TreeView.treeViewGetSelection'.  It can be manipulated to check the
-- selection status of the tree, as well as select and deselect individual
-- rows.  Selection is done completely view side.  As a result, multiple
-- views of the same model can have completely different selections.
-- Additionally, you cannot change the selection of a row on the model that
-- is not currently displayed by the view without expanding its parents
-- first.
-- 
-- One of the important things to remember when monitoring the selection of
-- a view is that the [TreeSelection::changed]("GI.Gtk.Objects.TreeSelection#g:signal:changed") signal is mostly a hint.
-- That is, it may only emit one signal when a range of rows is selected.
-- Additionally, it may on occasion emit a [TreeSelection::changed]("GI.Gtk.Objects.TreeSelection#g:signal:changed") signal
-- when nothing has happened (mostly as a result of programmers calling
-- select_row on an already selected row).

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.TreeSelection
    ( 

-- * Exported types
    TreeSelection(..)                       ,
    IsTreeSelection                         ,
    toTreeSelection                         ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [countSelectedRows]("GI.Gtk.Objects.TreeSelection#g:method:countSelectedRows"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [iterIsSelected]("GI.Gtk.Objects.TreeSelection#g:method:iterIsSelected"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [pathIsSelected]("GI.Gtk.Objects.TreeSelection#g:method:pathIsSelected"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [selectAll]("GI.Gtk.Objects.TreeSelection#g:method:selectAll"), [selectIter]("GI.Gtk.Objects.TreeSelection#g:method:selectIter"), [selectPath]("GI.Gtk.Objects.TreeSelection#g:method:selectPath"), [selectRange]("GI.Gtk.Objects.TreeSelection#g:method:selectRange"), [selectedForeach]("GI.Gtk.Objects.TreeSelection#g:method:selectedForeach"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [unselectAll]("GI.Gtk.Objects.TreeSelection#g:method:unselectAll"), [unselectIter]("GI.Gtk.Objects.TreeSelection#g:method:unselectIter"), [unselectPath]("GI.Gtk.Objects.TreeSelection#g:method:unselectPath"), [unselectRange]("GI.Gtk.Objects.TreeSelection#g:method:unselectRange"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getData]("GI.GObject.Objects.Object#g:method:getData"), [getMode]("GI.Gtk.Objects.TreeSelection#g:method:getMode"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getSelected]("GI.Gtk.Objects.TreeSelection#g:method:getSelected"), [getSelectedRows]("GI.Gtk.Objects.TreeSelection#g:method:getSelectedRows"), [getTreeView]("GI.Gtk.Objects.TreeSelection#g:method:getTreeView").
-- 
-- ==== Setters
-- [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setMode]("GI.Gtk.Objects.TreeSelection#g:method:setMode"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setSelectFunction]("GI.Gtk.Objects.TreeSelection#g:method:setSelectFunction").

#if defined(ENABLE_OVERLOADING)
    ResolveTreeSelectionMethod              ,
#endif

-- ** countSelectedRows #method:countSelectedRows#

#if defined(ENABLE_OVERLOADING)
    TreeSelectionCountSelectedRowsMethodInfo,
#endif
    treeSelectionCountSelectedRows          ,


-- ** getMode #method:getMode#

#if defined(ENABLE_OVERLOADING)
    TreeSelectionGetModeMethodInfo          ,
#endif
    treeSelectionGetMode                    ,


-- ** getSelected #method:getSelected#

#if defined(ENABLE_OVERLOADING)
    TreeSelectionGetSelectedMethodInfo      ,
#endif
    treeSelectionGetSelected                ,


-- ** getSelectedRows #method:getSelectedRows#

#if defined(ENABLE_OVERLOADING)
    TreeSelectionGetSelectedRowsMethodInfo  ,
#endif
    treeSelectionGetSelectedRows            ,


-- ** getTreeView #method:getTreeView#

#if defined(ENABLE_OVERLOADING)
    TreeSelectionGetTreeViewMethodInfo      ,
#endif
    treeSelectionGetTreeView                ,


-- ** iterIsSelected #method:iterIsSelected#

#if defined(ENABLE_OVERLOADING)
    TreeSelectionIterIsSelectedMethodInfo   ,
#endif
    treeSelectionIterIsSelected             ,


-- ** pathIsSelected #method:pathIsSelected#

#if defined(ENABLE_OVERLOADING)
    TreeSelectionPathIsSelectedMethodInfo   ,
#endif
    treeSelectionPathIsSelected             ,


-- ** selectAll #method:selectAll#

#if defined(ENABLE_OVERLOADING)
    TreeSelectionSelectAllMethodInfo        ,
#endif
    treeSelectionSelectAll                  ,


-- ** selectIter #method:selectIter#

#if defined(ENABLE_OVERLOADING)
    TreeSelectionSelectIterMethodInfo       ,
#endif
    treeSelectionSelectIter                 ,


-- ** selectPath #method:selectPath#

#if defined(ENABLE_OVERLOADING)
    TreeSelectionSelectPathMethodInfo       ,
#endif
    treeSelectionSelectPath                 ,


-- ** selectRange #method:selectRange#

#if defined(ENABLE_OVERLOADING)
    TreeSelectionSelectRangeMethodInfo      ,
#endif
    treeSelectionSelectRange                ,


-- ** selectedForeach #method:selectedForeach#

#if defined(ENABLE_OVERLOADING)
    TreeSelectionSelectedForeachMethodInfo  ,
#endif
    treeSelectionSelectedForeach            ,


-- ** setMode #method:setMode#

#if defined(ENABLE_OVERLOADING)
    TreeSelectionSetModeMethodInfo          ,
#endif
    treeSelectionSetMode                    ,


-- ** setSelectFunction #method:setSelectFunction#

#if defined(ENABLE_OVERLOADING)
    TreeSelectionSetSelectFunctionMethodInfo,
#endif
    treeSelectionSetSelectFunction          ,


-- ** unselectAll #method:unselectAll#

#if defined(ENABLE_OVERLOADING)
    TreeSelectionUnselectAllMethodInfo      ,
#endif
    treeSelectionUnselectAll                ,


-- ** unselectIter #method:unselectIter#

#if defined(ENABLE_OVERLOADING)
    TreeSelectionUnselectIterMethodInfo     ,
#endif
    treeSelectionUnselectIter               ,


-- ** unselectPath #method:unselectPath#

#if defined(ENABLE_OVERLOADING)
    TreeSelectionUnselectPathMethodInfo     ,
#endif
    treeSelectionUnselectPath               ,


-- ** unselectRange #method:unselectRange#

#if defined(ENABLE_OVERLOADING)
    TreeSelectionUnselectRangeMethodInfo    ,
#endif
    treeSelectionUnselectRange              ,




 -- * Properties


-- ** mode #attr:mode#
-- | Selection mode.
-- See 'GI.Gtk.Objects.TreeSelection.treeSelectionSetMode' for more information on this property.
-- 
-- /Since: 3.2/

#if defined(ENABLE_OVERLOADING)
    TreeSelectionModePropertyInfo           ,
#endif
    constructTreeSelectionMode              ,
    getTreeSelectionMode                    ,
    setTreeSelectionMode                    ,
#if defined(ENABLE_OVERLOADING)
    treeSelectionMode                       ,
#endif




 -- * Signals


-- ** changed #signal:changed#

    TreeSelectionChangedCallback            ,
#if defined(ENABLE_OVERLOADING)
    TreeSelectionChangedSignalInfo          ,
#endif
    afterTreeSelectionChanged               ,
    onTreeSelectionChanged                  ,




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

import qualified GI.GLib.Callbacks as GLib.Callbacks
import qualified GI.GObject.Objects.Object as GObject.Object
import qualified GI.Gtk.Callbacks as Gtk.Callbacks
import {-# SOURCE #-} qualified GI.Gtk.Enums as Gtk.Enums
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.TreeModel as Gtk.TreeModel
import {-# SOURCE #-} qualified GI.Gtk.Objects.TreeView as Gtk.TreeView
import {-# SOURCE #-} qualified GI.Gtk.Structs.TreeIter as Gtk.TreeIter
import {-# SOURCE #-} qualified GI.Gtk.Structs.TreePath as Gtk.TreePath

-- | Memory-managed wrapper type.
newtype TreeSelection = TreeSelection (SP.ManagedPtr TreeSelection)
    deriving (Eq)

instance SP.ManagedPtrNewtype TreeSelection where
    toManagedPtr (TreeSelection p) = p

foreign import ccall "gtk_tree_selection_get_type"
    c_gtk_tree_selection_get_type :: IO B.Types.GType

instance B.Types.TypedObject TreeSelection where
    glibType = c_gtk_tree_selection_get_type

instance B.Types.GObject TreeSelection

-- | Type class for types which can be safely cast to `TreeSelection`, for instance with `toTreeSelection`.
class (SP.GObject o, O.IsDescendantOf TreeSelection o) => IsTreeSelection o
instance (SP.GObject o, O.IsDescendantOf TreeSelection o) => IsTreeSelection o

instance O.HasParentTypes TreeSelection
type instance O.ParentTypes TreeSelection = '[GObject.Object.Object]

-- | Cast to `TreeSelection`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toTreeSelection :: (MIO.MonadIO m, IsTreeSelection o) => o -> m TreeSelection
toTreeSelection = MIO.liftIO . B.ManagedPtr.unsafeCastTo TreeSelection

-- | Convert 'TreeSelection' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe TreeSelection) where
    gvalueGType_ = c_gtk_tree_selection_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr TreeSelection)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr TreeSelection)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject TreeSelection ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveTreeSelectionMethod (t :: Symbol) (o :: *) :: * where
    ResolveTreeSelectionMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveTreeSelectionMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveTreeSelectionMethod "countSelectedRows" o = TreeSelectionCountSelectedRowsMethodInfo
    ResolveTreeSelectionMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveTreeSelectionMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveTreeSelectionMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveTreeSelectionMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveTreeSelectionMethod "iterIsSelected" o = TreeSelectionIterIsSelectedMethodInfo
    ResolveTreeSelectionMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveTreeSelectionMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveTreeSelectionMethod "pathIsSelected" o = TreeSelectionPathIsSelectedMethodInfo
    ResolveTreeSelectionMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveTreeSelectionMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveTreeSelectionMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveTreeSelectionMethod "selectAll" o = TreeSelectionSelectAllMethodInfo
    ResolveTreeSelectionMethod "selectIter" o = TreeSelectionSelectIterMethodInfo
    ResolveTreeSelectionMethod "selectPath" o = TreeSelectionSelectPathMethodInfo
    ResolveTreeSelectionMethod "selectRange" o = TreeSelectionSelectRangeMethodInfo
    ResolveTreeSelectionMethod "selectedForeach" o = TreeSelectionSelectedForeachMethodInfo
    ResolveTreeSelectionMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveTreeSelectionMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveTreeSelectionMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveTreeSelectionMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveTreeSelectionMethod "unselectAll" o = TreeSelectionUnselectAllMethodInfo
    ResolveTreeSelectionMethod "unselectIter" o = TreeSelectionUnselectIterMethodInfo
    ResolveTreeSelectionMethod "unselectPath" o = TreeSelectionUnselectPathMethodInfo
    ResolveTreeSelectionMethod "unselectRange" o = TreeSelectionUnselectRangeMethodInfo
    ResolveTreeSelectionMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveTreeSelectionMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveTreeSelectionMethod "getMode" o = TreeSelectionGetModeMethodInfo
    ResolveTreeSelectionMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveTreeSelectionMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveTreeSelectionMethod "getSelected" o = TreeSelectionGetSelectedMethodInfo
    ResolveTreeSelectionMethod "getSelectedRows" o = TreeSelectionGetSelectedRowsMethodInfo
    ResolveTreeSelectionMethod "getTreeView" o = TreeSelectionGetTreeViewMethodInfo
    ResolveTreeSelectionMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveTreeSelectionMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveTreeSelectionMethod "setMode" o = TreeSelectionSetModeMethodInfo
    ResolveTreeSelectionMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveTreeSelectionMethod "setSelectFunction" o = TreeSelectionSetSelectFunctionMethodInfo
    ResolveTreeSelectionMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveTreeSelectionMethod t TreeSelection, O.OverloadedMethod info TreeSelection p) => OL.IsLabel t (TreeSelection -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveTreeSelectionMethod t TreeSelection, O.OverloadedMethod info TreeSelection p, R.HasField t TreeSelection p) => R.HasField t TreeSelection p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveTreeSelectionMethod t TreeSelection, O.OverloadedMethodInfo info TreeSelection) => OL.IsLabel t (O.MethodProxy info TreeSelection) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- signal TreeSelection::changed
-- | Emitted whenever the selection has (possibly) changed. Please note that
-- this signal is mostly a hint.  It may only be emitted once when a range
-- of rows are selected, and it may occasionally be emitted when nothing
-- has happened.
type TreeSelectionChangedCallback =
    IO ()

type C_TreeSelectionChangedCallback =
    Ptr TreeSelection ->                    -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_TreeSelectionChangedCallback`.
foreign import ccall "wrapper"
    mk_TreeSelectionChangedCallback :: C_TreeSelectionChangedCallback -> IO (FunPtr C_TreeSelectionChangedCallback)

wrap_TreeSelectionChangedCallback :: 
    GObject a => (a -> TreeSelectionChangedCallback) ->
    C_TreeSelectionChangedCallback
wrap_TreeSelectionChangedCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [changed](#signal:changed) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' treeSelection #changed callback
-- @
-- 
-- 
onTreeSelectionChanged :: (IsTreeSelection a, MonadIO m) => a -> ((?self :: a) => TreeSelectionChangedCallback) -> m SignalHandlerId
onTreeSelectionChanged obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TreeSelectionChangedCallback wrapped
    wrapped'' <- mk_TreeSelectionChangedCallback wrapped'
    connectSignalFunPtr obj "changed" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [changed](#signal:changed) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' treeSelection #changed callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterTreeSelectionChanged :: (IsTreeSelection a, MonadIO m) => a -> ((?self :: a) => TreeSelectionChangedCallback) -> m SignalHandlerId
afterTreeSelectionChanged obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TreeSelectionChangedCallback wrapped
    wrapped'' <- mk_TreeSelectionChangedCallback wrapped'
    connectSignalFunPtr obj "changed" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data TreeSelectionChangedSignalInfo
instance SignalInfo TreeSelectionChangedSignalInfo where
    type HaskellCallbackType TreeSelectionChangedSignalInfo = TreeSelectionChangedCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_TreeSelectionChangedCallback cb
        cb'' <- mk_TreeSelectionChangedCallback cb'
        connectSignalFunPtr obj "changed" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeSelection::changed"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeSelection.html#g:signal:changed"})

#endif

-- VVV Prop "mode"
   -- Type: TInterface (Name {namespace = "Gtk", name = "SelectionMode"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@mode@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' treeSelection #mode
-- @
getTreeSelectionMode :: (MonadIO m, IsTreeSelection o) => o -> m Gtk.Enums.SelectionMode
getTreeSelectionMode obj = MIO.liftIO $ B.Properties.getObjectPropertyEnum obj "mode"

-- | Set the value of the “@mode@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' treeSelection [ #mode 'Data.GI.Base.Attributes.:=' value ]
-- @
setTreeSelectionMode :: (MonadIO m, IsTreeSelection o) => o -> Gtk.Enums.SelectionMode -> m ()
setTreeSelectionMode obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyEnum obj "mode" val

-- | Construct a `GValueConstruct` with valid value for the “@mode@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTreeSelectionMode :: (IsTreeSelection o, MIO.MonadIO m) => Gtk.Enums.SelectionMode -> m (GValueConstruct o)
constructTreeSelectionMode val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyEnum "mode" val

#if defined(ENABLE_OVERLOADING)
data TreeSelectionModePropertyInfo
instance AttrInfo TreeSelectionModePropertyInfo where
    type AttrAllowedOps TreeSelectionModePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TreeSelectionModePropertyInfo = IsTreeSelection
    type AttrSetTypeConstraint TreeSelectionModePropertyInfo = (~) Gtk.Enums.SelectionMode
    type AttrTransferTypeConstraint TreeSelectionModePropertyInfo = (~) Gtk.Enums.SelectionMode
    type AttrTransferType TreeSelectionModePropertyInfo = Gtk.Enums.SelectionMode
    type AttrGetType TreeSelectionModePropertyInfo = Gtk.Enums.SelectionMode
    type AttrLabel TreeSelectionModePropertyInfo = "mode"
    type AttrOrigin TreeSelectionModePropertyInfo = TreeSelection
    attrGet = getTreeSelectionMode
    attrSet = setTreeSelectionMode
    attrTransfer _ v = do
        return v
    attrConstruct = constructTreeSelectionMode
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeSelection.mode"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeSelection.html#g:attr:mode"
        })
#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList TreeSelection
type instance O.AttributeList TreeSelection = TreeSelectionAttributeList
type TreeSelectionAttributeList = ('[ '("mode", TreeSelectionModePropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
treeSelectionMode :: AttrLabelProxy "mode"
treeSelectionMode = AttrLabelProxy

#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList TreeSelection = TreeSelectionSignalList
type TreeSelectionSignalList = ('[ '("changed", TreeSelectionChangedSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo)] :: [(Symbol, *)])

#endif

-- method TreeSelection::count_selected_rows
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "selection"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeSelection" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeSelection."
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
-- returnType: Just (TBasicType TInt)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_tree_selection_count_selected_rows" gtk_tree_selection_count_selected_rows :: 
    Ptr TreeSelection ->                    -- selection : TInterface (Name {namespace = "Gtk", name = "TreeSelection"})
    IO Int32

-- | Returns the number of rows that have been selected in /@tree@/.
-- 
-- /Since: 2.2/
treeSelectionCountSelectedRows ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeSelection a) =>
    a
    -- ^ /@selection@/: A t'GI.Gtk.Objects.TreeSelection.TreeSelection'.
    -> m Int32
    -- ^ __Returns:__ The number of rows selected.
treeSelectionCountSelectedRows selection = liftIO $ do
    selection' <- unsafeManagedPtrCastPtr selection
    result <- gtk_tree_selection_count_selected_rows selection'
    touchManagedPtr selection
    return result

#if defined(ENABLE_OVERLOADING)
data TreeSelectionCountSelectedRowsMethodInfo
instance (signature ~ (m Int32), MonadIO m, IsTreeSelection a) => O.OverloadedMethod TreeSelectionCountSelectedRowsMethodInfo a signature where
    overloadedMethod = treeSelectionCountSelectedRows

instance O.OverloadedMethodInfo TreeSelectionCountSelectedRowsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeSelection.treeSelectionCountSelectedRows",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeSelection.html#v:treeSelectionCountSelectedRows"
        })


#endif

-- method TreeSelection::get_mode
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "selection"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeSelection" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeSelection"
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
-- returnType: Just
--               (TInterface Name { namespace = "Gtk" , name = "SelectionMode" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_tree_selection_get_mode" gtk_tree_selection_get_mode :: 
    Ptr TreeSelection ->                    -- selection : TInterface (Name {namespace = "Gtk", name = "TreeSelection"})
    IO CUInt

-- | Gets the selection mode for /@selection@/. See
-- 'GI.Gtk.Objects.TreeSelection.treeSelectionSetMode'.
treeSelectionGetMode ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeSelection a) =>
    a
    -- ^ /@selection@/: a t'GI.Gtk.Objects.TreeSelection.TreeSelection'
    -> m Gtk.Enums.SelectionMode
    -- ^ __Returns:__ the current selection mode
treeSelectionGetMode selection = liftIO $ do
    selection' <- unsafeManagedPtrCastPtr selection
    result <- gtk_tree_selection_get_mode selection'
    let result' = (toEnum . fromIntegral) result
    touchManagedPtr selection
    return result'

#if defined(ENABLE_OVERLOADING)
data TreeSelectionGetModeMethodInfo
instance (signature ~ (m Gtk.Enums.SelectionMode), MonadIO m, IsTreeSelection a) => O.OverloadedMethod TreeSelectionGetModeMethodInfo a signature where
    overloadedMethod = treeSelectionGetMode

instance O.OverloadedMethodInfo TreeSelectionGetModeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeSelection.treeSelectionGetMode",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeSelection.html#v:treeSelectionGetMode"
        })


#endif

-- method TreeSelection::get_selected
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "selection"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeSelection" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeSelection."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "model"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeModel" }
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the model" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeIter" }
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the iterator for the selected row"
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

foreign import ccall "gtk_tree_selection_get_selected" gtk_tree_selection_get_selected :: 
    Ptr TreeSelection ->                    -- selection : TInterface (Name {namespace = "Gtk", name = "TreeSelection"})
    Ptr (Ptr Gtk.TreeModel.TreeModel) ->    -- model : TInterface (Name {namespace = "Gtk", name = "TreeModel"})
    Ptr Gtk.TreeIter.TreeIter ->            -- iter : TInterface (Name {namespace = "Gtk", name = "TreeIter"})
    IO CInt

-- | Sets /@iter@/ to the currently selected node, if /@selection@/ is set to
-- 'GI.Gtk.Enums.SelectionModeSingle' or 'GI.Gtk.Enums.SelectionModeBrowse'.
-- 
-- The /@iter@/ argument may be 'P.Nothing' if you just want to test if /@selection@/
-- has any selected nodes.
-- 
-- The /@model@/ argument is filled with the current model as a convenience.
-- 
-- This function will not work with 'GI.Gtk.Enums.SelectionModeMultiple'. See
-- 'GI.Gtk.Objects.TreeSelection.treeSelectionGetSelectedRows' instead.
treeSelectionGetSelected ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeSelection a) =>
    a
    -- ^ /@selection@/: A t'GI.Gtk.Objects.TreeSelection.TreeSelection'.
    -> m ((Bool, Gtk.TreeModel.TreeModel, Gtk.TreeIter.TreeIter))
    -- ^ __Returns:__ 'P.True', if there is a selected node.
treeSelectionGetSelected selection = liftIO $ do
    selection' <- unsafeManagedPtrCastPtr selection
    model <- callocMem :: IO (Ptr (Ptr Gtk.TreeModel.TreeModel))
    iter <- SP.callocBoxedBytes 32 :: IO (Ptr Gtk.TreeIter.TreeIter)
    result <- gtk_tree_selection_get_selected selection' model iter
    let result' = (/= 0) result
    model' <- peek model
    model'' <- (newObject Gtk.TreeModel.TreeModel) model'
    iter' <- (wrapBoxed Gtk.TreeIter.TreeIter) iter
    touchManagedPtr selection
    freeMem model
    return (result', model'', iter')

#if defined(ENABLE_OVERLOADING)
data TreeSelectionGetSelectedMethodInfo
instance (signature ~ (m ((Bool, Gtk.TreeModel.TreeModel, Gtk.TreeIter.TreeIter))), MonadIO m, IsTreeSelection a) => O.OverloadedMethod TreeSelectionGetSelectedMethodInfo a signature where
    overloadedMethod = treeSelectionGetSelected

instance O.OverloadedMethodInfo TreeSelectionGetSelectedMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeSelection.treeSelectionGetSelected",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeSelection.html#v:treeSelectionGetSelected"
        })


#endif

-- method TreeSelection::get_selected_rows
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "selection"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeSelection" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeSelection."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "model"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeModel" }
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "A pointer to set to the #GtkTreeModel, or %NULL."
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
-- returnType: Just
--               (TGList
--                  (TInterface Name { namespace = "Gtk" , name = "TreePath" }))
-- throws : False
-- Skip return : False

foreign import ccall "gtk_tree_selection_get_selected_rows" gtk_tree_selection_get_selected_rows :: 
    Ptr TreeSelection ->                    -- selection : TInterface (Name {namespace = "Gtk", name = "TreeSelection"})
    Ptr (Ptr Gtk.TreeModel.TreeModel) ->    -- model : TInterface (Name {namespace = "Gtk", name = "TreeModel"})
    IO (Ptr (GList (Ptr Gtk.TreePath.TreePath)))

-- | Creates a list of path of all selected rows.
-- 
-- Additionally, if you are planning on modifying the model after calling
-- this function, you may want to convert the returned list into a list
-- of @/GtkTreeRowReferences/@.
-- 
-- To do this, you can use 'GI.Gtk.Structs.TreeRowReference.treeRowReferenceNew'.
-- 
-- To free the return value, use:
-- 
-- 
-- === /C code/
-- >
-- >g_list_free_full (list, (GDestroyNotify) gtk_tree_path_free);
-- 
-- 
-- /Since: 2.2/
treeSelectionGetSelectedRows ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeSelection a) =>
    a
    -- ^ /@selection@/: A t'GI.Gtk.Objects.TreeSelection.TreeSelection'.
    -> m (([Gtk.TreePath.TreePath], Gtk.TreeModel.TreeModel))
    -- ^ __Returns:__ the selected paths
treeSelectionGetSelectedRows selection = liftIO $ do
    selection' <- unsafeManagedPtrCastPtr selection
    model <- callocMem :: IO (Ptr (Ptr Gtk.TreeModel.TreeModel))
    result <- gtk_tree_selection_get_selected_rows selection' model
    result' <- unpackGList result
    result'' <- mapM (wrapBoxed Gtk.TreePath.TreePath) result'
    g_list_free result
    model' <- peek model
    model'' <- (newObject Gtk.TreeModel.TreeModel) model'
    touchManagedPtr selection
    freeMem model
    return (result'', model'')

#if defined(ENABLE_OVERLOADING)
data TreeSelectionGetSelectedRowsMethodInfo
instance (signature ~ (m (([Gtk.TreePath.TreePath], Gtk.TreeModel.TreeModel))), MonadIO m, IsTreeSelection a) => O.OverloadedMethod TreeSelectionGetSelectedRowsMethodInfo a signature where
    overloadedMethod = treeSelectionGetSelectedRows

instance O.OverloadedMethodInfo TreeSelectionGetSelectedRowsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeSelection.treeSelectionGetSelectedRows",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeSelection.html#v:treeSelectionGetSelectedRows"
        })


#endif

-- method TreeSelection::get_tree_view
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "selection"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeSelection" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeSelection"
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "TreeView" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_tree_selection_get_tree_view" gtk_tree_selection_get_tree_view :: 
    Ptr TreeSelection ->                    -- selection : TInterface (Name {namespace = "Gtk", name = "TreeSelection"})
    IO (Ptr Gtk.TreeView.TreeView)

-- | Returns the tree view associated with /@selection@/.
treeSelectionGetTreeView ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeSelection a) =>
    a
    -- ^ /@selection@/: A t'GI.Gtk.Objects.TreeSelection.TreeSelection'
    -> m Gtk.TreeView.TreeView
    -- ^ __Returns:__ A t'GI.Gtk.Objects.TreeView.TreeView'
treeSelectionGetTreeView selection = liftIO $ do
    selection' <- unsafeManagedPtrCastPtr selection
    result <- gtk_tree_selection_get_tree_view selection'
    checkUnexpectedReturnNULL "treeSelectionGetTreeView" result
    result' <- (newObject Gtk.TreeView.TreeView) result
    touchManagedPtr selection
    return result'

#if defined(ENABLE_OVERLOADING)
data TreeSelectionGetTreeViewMethodInfo
instance (signature ~ (m Gtk.TreeView.TreeView), MonadIO m, IsTreeSelection a) => O.OverloadedMethod TreeSelectionGetTreeViewMethodInfo a signature where
    overloadedMethod = treeSelectionGetTreeView

instance O.OverloadedMethodInfo TreeSelectionGetTreeViewMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeSelection.treeSelectionGetTreeView",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeSelection.html#v:treeSelectionGetTreeView"
        })


#endif

-- method TreeSelection::iter_is_selected
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "selection"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeSelection" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeSelection"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A valid #GtkTreeIter"
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

foreign import ccall "gtk_tree_selection_iter_is_selected" gtk_tree_selection_iter_is_selected :: 
    Ptr TreeSelection ->                    -- selection : TInterface (Name {namespace = "Gtk", name = "TreeSelection"})
    Ptr Gtk.TreeIter.TreeIter ->            -- iter : TInterface (Name {namespace = "Gtk", name = "TreeIter"})
    IO CInt

-- | Returns 'P.True' if the row at /@iter@/ is currently selected.
treeSelectionIterIsSelected ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeSelection a) =>
    a
    -- ^ /@selection@/: A t'GI.Gtk.Objects.TreeSelection.TreeSelection'
    -> Gtk.TreeIter.TreeIter
    -- ^ /@iter@/: A valid t'GI.Gtk.Structs.TreeIter.TreeIter'
    -> m Bool
    -- ^ __Returns:__ 'P.True', if /@iter@/ is selected
treeSelectionIterIsSelected selection iter = liftIO $ do
    selection' <- unsafeManagedPtrCastPtr selection
    iter' <- unsafeManagedPtrGetPtr iter
    result <- gtk_tree_selection_iter_is_selected selection' iter'
    let result' = (/= 0) result
    touchManagedPtr selection
    touchManagedPtr iter
    return result'

#if defined(ENABLE_OVERLOADING)
data TreeSelectionIterIsSelectedMethodInfo
instance (signature ~ (Gtk.TreeIter.TreeIter -> m Bool), MonadIO m, IsTreeSelection a) => O.OverloadedMethod TreeSelectionIterIsSelectedMethodInfo a signature where
    overloadedMethod = treeSelectionIterIsSelected

instance O.OverloadedMethodInfo TreeSelectionIterIsSelectedMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeSelection.treeSelectionIterIsSelected",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeSelection.html#v:treeSelectionIterIsSelected"
        })


#endif

-- method TreeSelection::path_is_selected
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "selection"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeSelection" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeSelection."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "path"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreePath" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreePath to check selection on."
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

foreign import ccall "gtk_tree_selection_path_is_selected" gtk_tree_selection_path_is_selected :: 
    Ptr TreeSelection ->                    -- selection : TInterface (Name {namespace = "Gtk", name = "TreeSelection"})
    Ptr Gtk.TreePath.TreePath ->            -- path : TInterface (Name {namespace = "Gtk", name = "TreePath"})
    IO CInt

-- | Returns 'P.True' if the row pointed to by /@path@/ is currently selected.  If /@path@/
-- does not point to a valid location, 'P.False' is returned
treeSelectionPathIsSelected ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeSelection a) =>
    a
    -- ^ /@selection@/: A t'GI.Gtk.Objects.TreeSelection.TreeSelection'.
    -> Gtk.TreePath.TreePath
    -- ^ /@path@/: A t'GI.Gtk.Structs.TreePath.TreePath' to check selection on.
    -> m Bool
    -- ^ __Returns:__ 'P.True' if /@path@/ is selected.
treeSelectionPathIsSelected selection path = liftIO $ do
    selection' <- unsafeManagedPtrCastPtr selection
    path' <- unsafeManagedPtrGetPtr path
    result <- gtk_tree_selection_path_is_selected selection' path'
    let result' = (/= 0) result
    touchManagedPtr selection
    touchManagedPtr path
    return result'

#if defined(ENABLE_OVERLOADING)
data TreeSelectionPathIsSelectedMethodInfo
instance (signature ~ (Gtk.TreePath.TreePath -> m Bool), MonadIO m, IsTreeSelection a) => O.OverloadedMethod TreeSelectionPathIsSelectedMethodInfo a signature where
    overloadedMethod = treeSelectionPathIsSelected

instance O.OverloadedMethodInfo TreeSelectionPathIsSelectedMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeSelection.treeSelectionPathIsSelected",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeSelection.html#v:treeSelectionPathIsSelected"
        })


#endif

-- method TreeSelection::select_all
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "selection"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeSelection" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeSelection."
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

foreign import ccall "gtk_tree_selection_select_all" gtk_tree_selection_select_all :: 
    Ptr TreeSelection ->                    -- selection : TInterface (Name {namespace = "Gtk", name = "TreeSelection"})
    IO ()

-- | Selects all the nodes. /@selection@/ must be set to @/GTK_SELECTION_MULTIPLE/@
-- mode.
treeSelectionSelectAll ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeSelection a) =>
    a
    -- ^ /@selection@/: A t'GI.Gtk.Objects.TreeSelection.TreeSelection'.
    -> m ()
treeSelectionSelectAll selection = liftIO $ do
    selection' <- unsafeManagedPtrCastPtr selection
    gtk_tree_selection_select_all selection'
    touchManagedPtr selection
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeSelectionSelectAllMethodInfo
instance (signature ~ (m ()), MonadIO m, IsTreeSelection a) => O.OverloadedMethod TreeSelectionSelectAllMethodInfo a signature where
    overloadedMethod = treeSelectionSelectAll

instance O.OverloadedMethodInfo TreeSelectionSelectAllMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeSelection.treeSelectionSelectAll",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeSelection.html#v:treeSelectionSelectAll"
        })


#endif

-- method TreeSelection::select_iter
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "selection"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeSelection" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeSelection."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "The #GtkTreeIter to be selected."
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

foreign import ccall "gtk_tree_selection_select_iter" gtk_tree_selection_select_iter :: 
    Ptr TreeSelection ->                    -- selection : TInterface (Name {namespace = "Gtk", name = "TreeSelection"})
    Ptr Gtk.TreeIter.TreeIter ->            -- iter : TInterface (Name {namespace = "Gtk", name = "TreeIter"})
    IO ()

-- | Selects the specified iterator.
treeSelectionSelectIter ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeSelection a) =>
    a
    -- ^ /@selection@/: A t'GI.Gtk.Objects.TreeSelection.TreeSelection'.
    -> Gtk.TreeIter.TreeIter
    -- ^ /@iter@/: The t'GI.Gtk.Structs.TreeIter.TreeIter' to be selected.
    -> m ()
treeSelectionSelectIter selection iter = liftIO $ do
    selection' <- unsafeManagedPtrCastPtr selection
    iter' <- unsafeManagedPtrGetPtr iter
    gtk_tree_selection_select_iter selection' iter'
    touchManagedPtr selection
    touchManagedPtr iter
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeSelectionSelectIterMethodInfo
instance (signature ~ (Gtk.TreeIter.TreeIter -> m ()), MonadIO m, IsTreeSelection a) => O.OverloadedMethod TreeSelectionSelectIterMethodInfo a signature where
    overloadedMethod = treeSelectionSelectIter

instance O.OverloadedMethodInfo TreeSelectionSelectIterMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeSelection.treeSelectionSelectIter",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeSelection.html#v:treeSelectionSelectIter"
        })


#endif

-- method TreeSelection::select_path
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "selection"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeSelection" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeSelection."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "path"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreePath" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "The #GtkTreePath to be selected."
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

foreign import ccall "gtk_tree_selection_select_path" gtk_tree_selection_select_path :: 
    Ptr TreeSelection ->                    -- selection : TInterface (Name {namespace = "Gtk", name = "TreeSelection"})
    Ptr Gtk.TreePath.TreePath ->            -- path : TInterface (Name {namespace = "Gtk", name = "TreePath"})
    IO ()

-- | Select the row at /@path@/.
treeSelectionSelectPath ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeSelection a) =>
    a
    -- ^ /@selection@/: A t'GI.Gtk.Objects.TreeSelection.TreeSelection'.
    -> Gtk.TreePath.TreePath
    -- ^ /@path@/: The t'GI.Gtk.Structs.TreePath.TreePath' to be selected.
    -> m ()
treeSelectionSelectPath selection path = liftIO $ do
    selection' <- unsafeManagedPtrCastPtr selection
    path' <- unsafeManagedPtrGetPtr path
    gtk_tree_selection_select_path selection' path'
    touchManagedPtr selection
    touchManagedPtr path
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeSelectionSelectPathMethodInfo
instance (signature ~ (Gtk.TreePath.TreePath -> m ()), MonadIO m, IsTreeSelection a) => O.OverloadedMethod TreeSelectionSelectPathMethodInfo a signature where
    overloadedMethod = treeSelectionSelectPath

instance O.OverloadedMethodInfo TreeSelectionSelectPathMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeSelection.treeSelectionSelectPath",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeSelection.html#v:treeSelectionSelectPath"
        })


#endif

-- method TreeSelection::select_range
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "selection"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeSelection" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeSelection."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "start_path"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreePath" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "The initial node of the range."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "end_path"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreePath" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "The final node of the range."
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

foreign import ccall "gtk_tree_selection_select_range" gtk_tree_selection_select_range :: 
    Ptr TreeSelection ->                    -- selection : TInterface (Name {namespace = "Gtk", name = "TreeSelection"})
    Ptr Gtk.TreePath.TreePath ->            -- start_path : TInterface (Name {namespace = "Gtk", name = "TreePath"})
    Ptr Gtk.TreePath.TreePath ->            -- end_path : TInterface (Name {namespace = "Gtk", name = "TreePath"})
    IO ()

-- | Selects a range of nodes, determined by /@startPath@/ and /@endPath@/ inclusive.
-- /@selection@/ must be set to @/GTK_SELECTION_MULTIPLE/@ mode.
treeSelectionSelectRange ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeSelection a) =>
    a
    -- ^ /@selection@/: A t'GI.Gtk.Objects.TreeSelection.TreeSelection'.
    -> Gtk.TreePath.TreePath
    -- ^ /@startPath@/: The initial node of the range.
    -> Gtk.TreePath.TreePath
    -- ^ /@endPath@/: The final node of the range.
    -> m ()
treeSelectionSelectRange selection startPath endPath = liftIO $ do
    selection' <- unsafeManagedPtrCastPtr selection
    startPath' <- unsafeManagedPtrGetPtr startPath
    endPath' <- unsafeManagedPtrGetPtr endPath
    gtk_tree_selection_select_range selection' startPath' endPath'
    touchManagedPtr selection
    touchManagedPtr startPath
    touchManagedPtr endPath
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeSelectionSelectRangeMethodInfo
instance (signature ~ (Gtk.TreePath.TreePath -> Gtk.TreePath.TreePath -> m ()), MonadIO m, IsTreeSelection a) => O.OverloadedMethod TreeSelectionSelectRangeMethodInfo a signature where
    overloadedMethod = treeSelectionSelectRange

instance O.OverloadedMethodInfo TreeSelectionSelectRangeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeSelection.treeSelectionSelectRange",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeSelection.html#v:treeSelectionSelectRange"
        })


#endif

-- method TreeSelection::selected_foreach
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "selection"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeSelection" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeSelection."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "func"
--           , argType =
--               TInterface
--                 Name { namespace = "Gtk" , name = "TreeSelectionForeachFunc" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "The function to call for each selected node."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeCall
--           , argClosure = 2
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "data"
--           , argType = TBasicType TPtr
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "user data to pass to the function."
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

foreign import ccall "gtk_tree_selection_selected_foreach" gtk_tree_selection_selected_foreach :: 
    Ptr TreeSelection ->                    -- selection : TInterface (Name {namespace = "Gtk", name = "TreeSelection"})
    FunPtr Gtk.Callbacks.C_TreeSelectionForeachFunc -> -- func : TInterface (Name {namespace = "Gtk", name = "TreeSelectionForeachFunc"})
    Ptr () ->                               -- data : TBasicType TPtr
    IO ()

-- | Calls a function for each selected node. Note that you cannot modify
-- the tree or selection from within this function. As a result,
-- 'GI.Gtk.Objects.TreeSelection.treeSelectionGetSelectedRows' might be more useful.
treeSelectionSelectedForeach ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeSelection a) =>
    a
    -- ^ /@selection@/: A t'GI.Gtk.Objects.TreeSelection.TreeSelection'.
    -> Gtk.Callbacks.TreeSelectionForeachFunc
    -- ^ /@func@/: The function to call for each selected node.
    -> m ()
treeSelectionSelectedForeach selection func = liftIO $ do
    selection' <- unsafeManagedPtrCastPtr selection
    func' <- Gtk.Callbacks.mk_TreeSelectionForeachFunc (Gtk.Callbacks.wrap_TreeSelectionForeachFunc Nothing (Gtk.Callbacks.drop_closures_TreeSelectionForeachFunc func))
    let data_ = nullPtr
    gtk_tree_selection_selected_foreach selection' func' data_
    safeFreeFunPtr $ castFunPtrToPtr func'
    touchManagedPtr selection
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeSelectionSelectedForeachMethodInfo
instance (signature ~ (Gtk.Callbacks.TreeSelectionForeachFunc -> m ()), MonadIO m, IsTreeSelection a) => O.OverloadedMethod TreeSelectionSelectedForeachMethodInfo a signature where
    overloadedMethod = treeSelectionSelectedForeach

instance O.OverloadedMethodInfo TreeSelectionSelectedForeachMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeSelection.treeSelectionSelectedForeach",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeSelection.html#v:treeSelectionSelectedForeach"
        })


#endif

-- method TreeSelection::set_mode
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "selection"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeSelection" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeSelection."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "type"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "SelectionMode" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "The selection mode" , sinceVersion = Nothing }
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

foreign import ccall "gtk_tree_selection_set_mode" gtk_tree_selection_set_mode :: 
    Ptr TreeSelection ->                    -- selection : TInterface (Name {namespace = "Gtk", name = "TreeSelection"})
    CUInt ->                                -- type : TInterface (Name {namespace = "Gtk", name = "SelectionMode"})
    IO ()

-- | Sets the selection mode of the /@selection@/.  If the previous type was
-- @/GTK_SELECTION_MULTIPLE/@, then the anchor is kept selected, if it was
-- previously selected.
treeSelectionSetMode ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeSelection a) =>
    a
    -- ^ /@selection@/: A t'GI.Gtk.Objects.TreeSelection.TreeSelection'.
    -> Gtk.Enums.SelectionMode
    -- ^ /@type@/: The selection mode
    -> m ()
treeSelectionSetMode selection type_ = liftIO $ do
    selection' <- unsafeManagedPtrCastPtr selection
    let type_' = (fromIntegral . fromEnum) type_
    gtk_tree_selection_set_mode selection' type_'
    touchManagedPtr selection
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeSelectionSetModeMethodInfo
instance (signature ~ (Gtk.Enums.SelectionMode -> m ()), MonadIO m, IsTreeSelection a) => O.OverloadedMethod TreeSelectionSetModeMethodInfo a signature where
    overloadedMethod = treeSelectionSetMode

instance O.OverloadedMethodInfo TreeSelectionSetModeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeSelection.treeSelectionSetMode",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeSelection.html#v:treeSelectionSetMode"
        })


#endif

-- method TreeSelection::set_select_function
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "selection"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeSelection" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeSelection."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "func"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeSelectionFunc" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "The selection function. May be %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeNotified
--           , argClosure = 2
--           , argDestroy = 3
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "data"
--           , argType = TBasicType TPtr
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "The selection function\8217s data. May be %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "destroy"
--           , argType =
--               TInterface Name { namespace = "GLib" , name = "DestroyNotify" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "The destroy function for user data.  May be %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeAsync
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

foreign import ccall "gtk_tree_selection_set_select_function" gtk_tree_selection_set_select_function :: 
    Ptr TreeSelection ->                    -- selection : TInterface (Name {namespace = "Gtk", name = "TreeSelection"})
    FunPtr Gtk.Callbacks.C_TreeSelectionFunc -> -- func : TInterface (Name {namespace = "Gtk", name = "TreeSelectionFunc"})
    Ptr () ->                               -- data : TBasicType TPtr
    FunPtr GLib.Callbacks.C_DestroyNotify -> -- destroy : TInterface (Name {namespace = "GLib", name = "DestroyNotify"})
    IO ()

-- | Sets the selection function.
-- 
-- If set, this function is called before any node is selected or unselected,
-- giving some control over which nodes are selected. The select function
-- should return 'P.True' if the state of the node may be toggled, and 'P.False'
-- if the state of the node should be left unchanged.
treeSelectionSetSelectFunction ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeSelection a) =>
    a
    -- ^ /@selection@/: A t'GI.Gtk.Objects.TreeSelection.TreeSelection'.
    -> Maybe (Gtk.Callbacks.TreeSelectionFunc)
    -- ^ /@func@/: The selection function. May be 'P.Nothing'
    -> m ()
treeSelectionSetSelectFunction selection func = liftIO $ do
    selection' <- unsafeManagedPtrCastPtr selection
    maybeFunc <- case func of
        Nothing -> return (castPtrToFunPtr nullPtr)
        Just jFunc -> do
            jFunc' <- Gtk.Callbacks.mk_TreeSelectionFunc (Gtk.Callbacks.wrap_TreeSelectionFunc Nothing (Gtk.Callbacks.drop_closures_TreeSelectionFunc jFunc))
            return jFunc'
    let data_ = castFunPtrToPtr maybeFunc
    let destroy = SP.safeFreeFunPtrPtr
    gtk_tree_selection_set_select_function selection' maybeFunc data_ destroy
    touchManagedPtr selection
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeSelectionSetSelectFunctionMethodInfo
instance (signature ~ (Maybe (Gtk.Callbacks.TreeSelectionFunc) -> m ()), MonadIO m, IsTreeSelection a) => O.OverloadedMethod TreeSelectionSetSelectFunctionMethodInfo a signature where
    overloadedMethod = treeSelectionSetSelectFunction

instance O.OverloadedMethodInfo TreeSelectionSetSelectFunctionMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeSelection.treeSelectionSetSelectFunction",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeSelection.html#v:treeSelectionSetSelectFunction"
        })


#endif

-- method TreeSelection::unselect_all
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "selection"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeSelection" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeSelection."
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

foreign import ccall "gtk_tree_selection_unselect_all" gtk_tree_selection_unselect_all :: 
    Ptr TreeSelection ->                    -- selection : TInterface (Name {namespace = "Gtk", name = "TreeSelection"})
    IO ()

-- | Unselects all the nodes.
treeSelectionUnselectAll ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeSelection a) =>
    a
    -- ^ /@selection@/: A t'GI.Gtk.Objects.TreeSelection.TreeSelection'.
    -> m ()
treeSelectionUnselectAll selection = liftIO $ do
    selection' <- unsafeManagedPtrCastPtr selection
    gtk_tree_selection_unselect_all selection'
    touchManagedPtr selection
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeSelectionUnselectAllMethodInfo
instance (signature ~ (m ()), MonadIO m, IsTreeSelection a) => O.OverloadedMethod TreeSelectionUnselectAllMethodInfo a signature where
    overloadedMethod = treeSelectionUnselectAll

instance O.OverloadedMethodInfo TreeSelectionUnselectAllMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeSelection.treeSelectionUnselectAll",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeSelection.html#v:treeSelectionUnselectAll"
        })


#endif

-- method TreeSelection::unselect_iter
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "selection"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeSelection" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeSelection."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "The #GtkTreeIter to be unselected."
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

foreign import ccall "gtk_tree_selection_unselect_iter" gtk_tree_selection_unselect_iter :: 
    Ptr TreeSelection ->                    -- selection : TInterface (Name {namespace = "Gtk", name = "TreeSelection"})
    Ptr Gtk.TreeIter.TreeIter ->            -- iter : TInterface (Name {namespace = "Gtk", name = "TreeIter"})
    IO ()

-- | Unselects the specified iterator.
treeSelectionUnselectIter ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeSelection a) =>
    a
    -- ^ /@selection@/: A t'GI.Gtk.Objects.TreeSelection.TreeSelection'.
    -> Gtk.TreeIter.TreeIter
    -- ^ /@iter@/: The t'GI.Gtk.Structs.TreeIter.TreeIter' to be unselected.
    -> m ()
treeSelectionUnselectIter selection iter = liftIO $ do
    selection' <- unsafeManagedPtrCastPtr selection
    iter' <- unsafeManagedPtrGetPtr iter
    gtk_tree_selection_unselect_iter selection' iter'
    touchManagedPtr selection
    touchManagedPtr iter
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeSelectionUnselectIterMethodInfo
instance (signature ~ (Gtk.TreeIter.TreeIter -> m ()), MonadIO m, IsTreeSelection a) => O.OverloadedMethod TreeSelectionUnselectIterMethodInfo a signature where
    overloadedMethod = treeSelectionUnselectIter

instance O.OverloadedMethodInfo TreeSelectionUnselectIterMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeSelection.treeSelectionUnselectIter",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeSelection.html#v:treeSelectionUnselectIter"
        })


#endif

-- method TreeSelection::unselect_path
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "selection"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeSelection" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeSelection."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "path"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreePath" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "The #GtkTreePath to be unselected."
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

foreign import ccall "gtk_tree_selection_unselect_path" gtk_tree_selection_unselect_path :: 
    Ptr TreeSelection ->                    -- selection : TInterface (Name {namespace = "Gtk", name = "TreeSelection"})
    Ptr Gtk.TreePath.TreePath ->            -- path : TInterface (Name {namespace = "Gtk", name = "TreePath"})
    IO ()

-- | Unselects the row at /@path@/.
treeSelectionUnselectPath ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeSelection a) =>
    a
    -- ^ /@selection@/: A t'GI.Gtk.Objects.TreeSelection.TreeSelection'.
    -> Gtk.TreePath.TreePath
    -- ^ /@path@/: The t'GI.Gtk.Structs.TreePath.TreePath' to be unselected.
    -> m ()
treeSelectionUnselectPath selection path = liftIO $ do
    selection' <- unsafeManagedPtrCastPtr selection
    path' <- unsafeManagedPtrGetPtr path
    gtk_tree_selection_unselect_path selection' path'
    touchManagedPtr selection
    touchManagedPtr path
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeSelectionUnselectPathMethodInfo
instance (signature ~ (Gtk.TreePath.TreePath -> m ()), MonadIO m, IsTreeSelection a) => O.OverloadedMethod TreeSelectionUnselectPathMethodInfo a signature where
    overloadedMethod = treeSelectionUnselectPath

instance O.OverloadedMethodInfo TreeSelectionUnselectPathMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeSelection.treeSelectionUnselectPath",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeSelection.html#v:treeSelectionUnselectPath"
        })


#endif

-- method TreeSelection::unselect_range
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "selection"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeSelection" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeSelection."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "start_path"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreePath" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "The initial node of the range."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "end_path"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreePath" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "The initial node of the range."
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

foreign import ccall "gtk_tree_selection_unselect_range" gtk_tree_selection_unselect_range :: 
    Ptr TreeSelection ->                    -- selection : TInterface (Name {namespace = "Gtk", name = "TreeSelection"})
    Ptr Gtk.TreePath.TreePath ->            -- start_path : TInterface (Name {namespace = "Gtk", name = "TreePath"})
    Ptr Gtk.TreePath.TreePath ->            -- end_path : TInterface (Name {namespace = "Gtk", name = "TreePath"})
    IO ()

-- | Unselects a range of nodes, determined by /@startPath@/ and /@endPath@/
-- inclusive.
-- 
-- /Since: 2.2/
treeSelectionUnselectRange ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeSelection a) =>
    a
    -- ^ /@selection@/: A t'GI.Gtk.Objects.TreeSelection.TreeSelection'.
    -> Gtk.TreePath.TreePath
    -- ^ /@startPath@/: The initial node of the range.
    -> Gtk.TreePath.TreePath
    -- ^ /@endPath@/: The initial node of the range.
    -> m ()
treeSelectionUnselectRange selection startPath endPath = liftIO $ do
    selection' <- unsafeManagedPtrCastPtr selection
    startPath' <- unsafeManagedPtrGetPtr startPath
    endPath' <- unsafeManagedPtrGetPtr endPath
    gtk_tree_selection_unselect_range selection' startPath' endPath'
    touchManagedPtr selection
    touchManagedPtr startPath
    touchManagedPtr endPath
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeSelectionUnselectRangeMethodInfo
instance (signature ~ (Gtk.TreePath.TreePath -> Gtk.TreePath.TreePath -> m ()), MonadIO m, IsTreeSelection a) => O.OverloadedMethod TreeSelectionUnselectRangeMethodInfo a signature where
    overloadedMethod = treeSelectionUnselectRange

instance O.OverloadedMethodInfo TreeSelectionUnselectRangeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeSelection.treeSelectionUnselectRange",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeSelection.html#v:treeSelectionUnselectRange"
        })


#endif


