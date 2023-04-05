{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- t'GI.Gtk.Interfaces.CellLayout.CellLayout' is an interface to be implemented by all objects which
-- want to provide a t'GI.Gtk.Objects.TreeViewColumn.TreeViewColumn' like API for packing cells,
-- setting attributes and data funcs.
-- 
-- One of the notable features provided by implementations of
-- GtkCellLayout are attributes. Attributes let you set the properties
-- in flexible ways. They can just be set to constant values like regular
-- properties. But they can also be mapped to a column of the underlying
-- tree model with @/gtk_cell_layout_set_attributes()/@, which means that the value
-- of the attribute can change from cell to cell as they are rendered by
-- the cell renderer. Finally, it is possible to specify a function with
-- 'GI.Gtk.Interfaces.CellLayout.cellLayoutSetCellDataFunc' that is called to determine the
-- value of the attribute for each cell that is rendered.
-- 
-- = GtkCellLayouts as GtkBuildable
-- 
-- Implementations of GtkCellLayout which also implement the GtkBuildable
-- interface (t'GI.Gtk.Objects.CellView.CellView', t'GI.Gtk.Objects.IconView.IconView', t'GI.Gtk.Objects.ComboBox.ComboBox',
-- t'GI.Gtk.Objects.EntryCompletion.EntryCompletion', t'GI.Gtk.Objects.TreeViewColumn.TreeViewColumn') accept GtkCellRenderer objects
-- as @\<child>@ elements in UI definitions. They support a custom @\<attributes>@
-- element for their children, which can contain multiple @\<attribute>@
-- elements. Each @\<attribute>@ element has a name attribute which specifies
-- a property of the cell renderer; the content of the element is the
-- attribute value.
-- 
-- This is an example of a UI definition fragment specifying attributes:
-- 
-- 
-- === /xml code/
-- >
-- ><object class="GtkCellView">
-- >  <child>
-- >    <object class="GtkCellRendererText"/>
-- >    <attributes>
-- >      <attribute name="text">0</attribute>
-- >    </attributes>
-- >  </child>
-- ></object>
-- 
-- 
-- Furthermore for implementations of GtkCellLayout that use a t'GI.Gtk.Objects.CellArea.CellArea'
-- to lay out cells (all GtkCellLayouts in GTK+ use a GtkCellArea)
-- [cell properties][cell-properties] can also be defined in the format by
-- specifying the custom @\<cell-packing>@ attribute which can contain multiple
-- @\<property>@ elements defined in the normal way.
-- 
-- Here is a UI definition fragment specifying cell properties:
-- 
-- 
-- === /xml code/
-- >
-- ><object class="GtkTreeViewColumn">
-- >  <child>
-- >    <object class="GtkCellRendererText"/>
-- >    <cell-packing>
-- >      <property name="align">True</property>
-- >      <property name="expand">False</property>
-- >    </cell-packing>
-- >  </child>
-- ></object>
-- 
-- 
-- = Subclassing GtkCellLayout implementations
-- 
-- When subclassing a widget that implements t'GI.Gtk.Interfaces.CellLayout.CellLayout' like
-- t'GI.Gtk.Objects.IconView.IconView' or t'GI.Gtk.Objects.ComboBox.ComboBox', there are some considerations related
-- to the fact that these widgets internally use a t'GI.Gtk.Objects.CellArea.CellArea'.
-- The cell area is exposed as a construct-only property by these
-- widgets. This means that it is possible to e.g. do
-- 
-- 
-- === /C code/
-- >
-- >combo = g_object_new (GTK_TYPE_COMBO_BOX, "cell-area", my_cell_area, NULL);
-- 
-- 
-- to use a custom cell area with a combo box. But construct properties
-- are only initialized after instance @/init()/@
-- functions have run, which means that using functions which rely on
-- the existence of the cell area in your subclass’ @/init()/@ function will
-- cause the default cell area to be instantiated. In this case, a provided
-- construct property value will be ignored (with a warning, to alert
-- you to the problem).
-- 
-- 
-- === /C code/
-- >
-- >static void
-- >my_combo_box_init (MyComboBox *b)
-- >{
-- >  GtkCellRenderer *cell;
-- >
-- >  cell = gtk_cell_renderer_pixbuf_new ();
-- >  // The following call causes the default cell area for combo boxes,
-- >  // a GtkCellAreaBox, to be instantiated
-- >  gtk_cell_layout_pack_start (GTK_CELL_LAYOUT (b), cell, FALSE);
-- >  ...
-- >}
-- >
-- >GtkWidget *
-- >my_combo_box_new (GtkCellArea *area)
-- >{
-- >  // This call is going to cause a warning about area being ignored
-- >  return g_object_new (MY_TYPE_COMBO_BOX, "cell-area", area, NULL);
-- >}
-- 
-- 
-- If supporting alternative cell areas with your derived widget is
-- not important, then this does not have to concern you. If you want
-- to support alternative cell areas, you can do so by moving the
-- problematic calls out of @/init()/@ and into a @/constructor()/@
-- for your class.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Interfaces.CellLayout
    ( 

-- * Exported types
    CellLayout(..)                          ,
    IsCellLayout                            ,
    toCellLayout                            ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [addAttribute]("GI.Gtk.Interfaces.CellLayout#g:method:addAttribute"), [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [clear]("GI.Gtk.Interfaces.CellLayout#g:method:clear"), [clearAttributes]("GI.Gtk.Interfaces.CellLayout#g:method:clearAttributes"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [packEnd]("GI.Gtk.Interfaces.CellLayout#g:method:packEnd"), [packStart]("GI.Gtk.Interfaces.CellLayout#g:method:packStart"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [reorder]("GI.Gtk.Interfaces.CellLayout#g:method:reorder"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getArea]("GI.Gtk.Interfaces.CellLayout#g:method:getArea"), [getCells]("GI.Gtk.Interfaces.CellLayout#g:method:getCells"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata").
-- 
-- ==== Setters
-- [setCellDataFunc]("GI.Gtk.Interfaces.CellLayout#g:method:setCellDataFunc"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty").

#if defined(ENABLE_OVERLOADING)
    ResolveCellLayoutMethod                 ,
#endif

-- ** addAttribute #method:addAttribute#

#if defined(ENABLE_OVERLOADING)
    CellLayoutAddAttributeMethodInfo        ,
#endif
    cellLayoutAddAttribute                  ,


-- ** clear #method:clear#

#if defined(ENABLE_OVERLOADING)
    CellLayoutClearMethodInfo               ,
#endif
    cellLayoutClear                         ,


-- ** clearAttributes #method:clearAttributes#

#if defined(ENABLE_OVERLOADING)
    CellLayoutClearAttributesMethodInfo     ,
#endif
    cellLayoutClearAttributes               ,


-- ** getArea #method:getArea#

#if defined(ENABLE_OVERLOADING)
    CellLayoutGetAreaMethodInfo             ,
#endif
    cellLayoutGetArea                       ,


-- ** getCells #method:getCells#

#if defined(ENABLE_OVERLOADING)
    CellLayoutGetCellsMethodInfo            ,
#endif
    cellLayoutGetCells                      ,


-- ** packEnd #method:packEnd#

#if defined(ENABLE_OVERLOADING)
    CellLayoutPackEndMethodInfo             ,
#endif
    cellLayoutPackEnd                       ,


-- ** packStart #method:packStart#

#if defined(ENABLE_OVERLOADING)
    CellLayoutPackStartMethodInfo           ,
#endif
    cellLayoutPackStart                     ,


-- ** reorder #method:reorder#

#if defined(ENABLE_OVERLOADING)
    CellLayoutReorderMethodInfo             ,
#endif
    cellLayoutReorder                       ,


-- ** setCellDataFunc #method:setCellDataFunc#

#if defined(ENABLE_OVERLOADING)
    CellLayoutSetCellDataFuncMethodInfo     ,
#endif
    cellLayoutSetCellDataFunc               ,




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
import {-# SOURCE #-} qualified GI.Gtk.Objects.CellArea as Gtk.CellArea
import {-# SOURCE #-} qualified GI.Gtk.Objects.CellRenderer as Gtk.CellRenderer

-- interface CellLayout 
-- | Memory-managed wrapper type.
newtype CellLayout = CellLayout (SP.ManagedPtr CellLayout)
    deriving (Eq)

instance SP.ManagedPtrNewtype CellLayout where
    toManagedPtr (CellLayout p) = p

foreign import ccall "gtk_cell_layout_get_type"
    c_gtk_cell_layout_get_type :: IO B.Types.GType

instance B.Types.TypedObject CellLayout where
    glibType = c_gtk_cell_layout_get_type

instance B.Types.GObject CellLayout

-- | Type class for types which can be safely cast to `CellLayout`, for instance with `toCellLayout`.
class (SP.GObject o, O.IsDescendantOf CellLayout o) => IsCellLayout o
instance (SP.GObject o, O.IsDescendantOf CellLayout o) => IsCellLayout o

instance O.HasParentTypes CellLayout
type instance O.ParentTypes CellLayout = '[GObject.Object.Object]

-- | Cast to `CellLayout`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toCellLayout :: (MIO.MonadIO m, IsCellLayout o) => o -> m CellLayout
toCellLayout = MIO.liftIO . B.ManagedPtr.unsafeCastTo CellLayout

-- | Convert 'CellLayout' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe CellLayout) where
    gvalueGType_ = c_gtk_cell_layout_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr CellLayout)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr CellLayout)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject CellLayout ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList CellLayout
type instance O.AttributeList CellLayout = CellLayoutAttributeList
type CellLayoutAttributeList = ('[ ] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
#endif

#if defined(ENABLE_OVERLOADING)
type family ResolveCellLayoutMethod (t :: Symbol) (o :: *) :: * where
    ResolveCellLayoutMethod "addAttribute" o = CellLayoutAddAttributeMethodInfo
    ResolveCellLayoutMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveCellLayoutMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveCellLayoutMethod "clear" o = CellLayoutClearMethodInfo
    ResolveCellLayoutMethod "clearAttributes" o = CellLayoutClearAttributesMethodInfo
    ResolveCellLayoutMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveCellLayoutMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveCellLayoutMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveCellLayoutMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveCellLayoutMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveCellLayoutMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveCellLayoutMethod "packEnd" o = CellLayoutPackEndMethodInfo
    ResolveCellLayoutMethod "packStart" o = CellLayoutPackStartMethodInfo
    ResolveCellLayoutMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveCellLayoutMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveCellLayoutMethod "reorder" o = CellLayoutReorderMethodInfo
    ResolveCellLayoutMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveCellLayoutMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveCellLayoutMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveCellLayoutMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveCellLayoutMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveCellLayoutMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveCellLayoutMethod "getArea" o = CellLayoutGetAreaMethodInfo
    ResolveCellLayoutMethod "getCells" o = CellLayoutGetCellsMethodInfo
    ResolveCellLayoutMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveCellLayoutMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveCellLayoutMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveCellLayoutMethod "setCellDataFunc" o = CellLayoutSetCellDataFuncMethodInfo
    ResolveCellLayoutMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveCellLayoutMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveCellLayoutMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveCellLayoutMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveCellLayoutMethod t CellLayout, O.OverloadedMethod info CellLayout p) => OL.IsLabel t (CellLayout -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveCellLayoutMethod t CellLayout, O.OverloadedMethod info CellLayout p, R.HasField t CellLayout p) => R.HasField t CellLayout p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveCellLayoutMethod t CellLayout, O.OverloadedMethodInfo info CellLayout) => OL.IsLabel t (O.MethodProxy info CellLayout) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- method CellLayout::add_attribute
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "cell_layout"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellLayout" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCellLayout" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "cell"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellRenderer" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCellRenderer" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "attribute"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "an attribute on the renderer"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "column"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "the column position on the model to get the attribute from"
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

foreign import ccall "gtk_cell_layout_add_attribute" gtk_cell_layout_add_attribute :: 
    Ptr CellLayout ->                       -- cell_layout : TInterface (Name {namespace = "Gtk", name = "CellLayout"})
    Ptr Gtk.CellRenderer.CellRenderer ->    -- cell : TInterface (Name {namespace = "Gtk", name = "CellRenderer"})
    CString ->                              -- attribute : TBasicType TUTF8
    Int32 ->                                -- column : TBasicType TInt
    IO ()

-- | Adds an attribute mapping to the list in /@cellLayout@/.
-- 
-- The /@column@/ is the column of the model to get a value from, and the
-- /@attribute@/ is the parameter on /@cell@/ to be set from the value. So for
-- example if column 2 of the model contains strings, you could have the
-- “text” attribute of a t'GI.Gtk.Objects.CellRendererText.CellRendererText' get its values from column 2.
-- 
-- /Since: 2.4/
cellLayoutAddAttribute ::
    (B.CallStack.HasCallStack, MonadIO m, IsCellLayout a, Gtk.CellRenderer.IsCellRenderer b) =>
    a
    -- ^ /@cellLayout@/: a t'GI.Gtk.Interfaces.CellLayout.CellLayout'
    -> b
    -- ^ /@cell@/: a t'GI.Gtk.Objects.CellRenderer.CellRenderer'
    -> T.Text
    -- ^ /@attribute@/: an attribute on the renderer
    -> Int32
    -- ^ /@column@/: the column position on the model to get the attribute from
    -> m ()
cellLayoutAddAttribute cellLayout cell attribute column = liftIO $ do
    cellLayout' <- unsafeManagedPtrCastPtr cellLayout
    cell' <- unsafeManagedPtrCastPtr cell
    attribute' <- textToCString attribute
    gtk_cell_layout_add_attribute cellLayout' cell' attribute' column
    touchManagedPtr cellLayout
    touchManagedPtr cell
    freeMem attribute'
    return ()

#if defined(ENABLE_OVERLOADING)
data CellLayoutAddAttributeMethodInfo
instance (signature ~ (b -> T.Text -> Int32 -> m ()), MonadIO m, IsCellLayout a, Gtk.CellRenderer.IsCellRenderer b) => O.OverloadedMethod CellLayoutAddAttributeMethodInfo a signature where
    overloadedMethod = cellLayoutAddAttribute

instance O.OverloadedMethodInfo CellLayoutAddAttributeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.CellLayout.cellLayoutAddAttribute",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-CellLayout.html#v:cellLayoutAddAttribute"
        })


#endif

-- method CellLayout::clear
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "cell_layout"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellLayout" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCellLayout" , sinceVersion = Nothing }
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

foreign import ccall "gtk_cell_layout_clear" gtk_cell_layout_clear :: 
    Ptr CellLayout ->                       -- cell_layout : TInterface (Name {namespace = "Gtk", name = "CellLayout"})
    IO ()

-- | Unsets all the mappings on all renderers on /@cellLayout@/ and
-- removes all renderers from /@cellLayout@/.
-- 
-- /Since: 2.4/
cellLayoutClear ::
    (B.CallStack.HasCallStack, MonadIO m, IsCellLayout a) =>
    a
    -- ^ /@cellLayout@/: a t'GI.Gtk.Interfaces.CellLayout.CellLayout'
    -> m ()
cellLayoutClear cellLayout = liftIO $ do
    cellLayout' <- unsafeManagedPtrCastPtr cellLayout
    gtk_cell_layout_clear cellLayout'
    touchManagedPtr cellLayout
    return ()

#if defined(ENABLE_OVERLOADING)
data CellLayoutClearMethodInfo
instance (signature ~ (m ()), MonadIO m, IsCellLayout a) => O.OverloadedMethod CellLayoutClearMethodInfo a signature where
    overloadedMethod = cellLayoutClear

instance O.OverloadedMethodInfo CellLayoutClearMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.CellLayout.cellLayoutClear",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-CellLayout.html#v:cellLayoutClear"
        })


#endif

-- method CellLayout::clear_attributes
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "cell_layout"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellLayout" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCellLayout" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "cell"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellRenderer" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "a #GtkCellRenderer to clear the attribute mapping on"
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

foreign import ccall "gtk_cell_layout_clear_attributes" gtk_cell_layout_clear_attributes :: 
    Ptr CellLayout ->                       -- cell_layout : TInterface (Name {namespace = "Gtk", name = "CellLayout"})
    Ptr Gtk.CellRenderer.CellRenderer ->    -- cell : TInterface (Name {namespace = "Gtk", name = "CellRenderer"})
    IO ()

-- | Clears all existing attributes previously set with
-- @/gtk_cell_layout_set_attributes()/@.
-- 
-- /Since: 2.4/
cellLayoutClearAttributes ::
    (B.CallStack.HasCallStack, MonadIO m, IsCellLayout a, Gtk.CellRenderer.IsCellRenderer b) =>
    a
    -- ^ /@cellLayout@/: a t'GI.Gtk.Interfaces.CellLayout.CellLayout'
    -> b
    -- ^ /@cell@/: a t'GI.Gtk.Objects.CellRenderer.CellRenderer' to clear the attribute mapping on
    -> m ()
cellLayoutClearAttributes cellLayout cell = liftIO $ do
    cellLayout' <- unsafeManagedPtrCastPtr cellLayout
    cell' <- unsafeManagedPtrCastPtr cell
    gtk_cell_layout_clear_attributes cellLayout' cell'
    touchManagedPtr cellLayout
    touchManagedPtr cell
    return ()

#if defined(ENABLE_OVERLOADING)
data CellLayoutClearAttributesMethodInfo
instance (signature ~ (b -> m ()), MonadIO m, IsCellLayout a, Gtk.CellRenderer.IsCellRenderer b) => O.OverloadedMethod CellLayoutClearAttributesMethodInfo a signature where
    overloadedMethod = cellLayoutClearAttributes

instance O.OverloadedMethodInfo CellLayoutClearAttributesMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.CellLayout.cellLayoutClearAttributes",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-CellLayout.html#v:cellLayoutClearAttributes"
        })


#endif

-- method CellLayout::get_area
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "cell_layout"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellLayout" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCellLayout" , sinceVersion = Nothing }
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

foreign import ccall "gtk_cell_layout_get_area" gtk_cell_layout_get_area :: 
    Ptr CellLayout ->                       -- cell_layout : TInterface (Name {namespace = "Gtk", name = "CellLayout"})
    IO (Ptr Gtk.CellArea.CellArea)

-- | Returns the underlying t'GI.Gtk.Objects.CellArea.CellArea' which might be /@cellLayout@/
-- if called on a t'GI.Gtk.Objects.CellArea.CellArea' or might be 'P.Nothing' if no t'GI.Gtk.Objects.CellArea.CellArea'
-- is used by /@cellLayout@/.
-- 
-- /Since: 3.0/
cellLayoutGetArea ::
    (B.CallStack.HasCallStack, MonadIO m, IsCellLayout a) =>
    a
    -- ^ /@cellLayout@/: a t'GI.Gtk.Interfaces.CellLayout.CellLayout'
    -> m (Maybe Gtk.CellArea.CellArea)
    -- ^ __Returns:__ the cell area used by /@cellLayout@/,
    -- or 'P.Nothing' in case no cell area is used.
cellLayoutGetArea cellLayout = liftIO $ do
    cellLayout' <- unsafeManagedPtrCastPtr cellLayout
    result <- gtk_cell_layout_get_area cellLayout'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (newObject Gtk.CellArea.CellArea) result'
        return result''
    touchManagedPtr cellLayout
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data CellLayoutGetAreaMethodInfo
instance (signature ~ (m (Maybe Gtk.CellArea.CellArea)), MonadIO m, IsCellLayout a) => O.OverloadedMethod CellLayoutGetAreaMethodInfo a signature where
    overloadedMethod = cellLayoutGetArea

instance O.OverloadedMethodInfo CellLayoutGetAreaMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.CellLayout.cellLayoutGetArea",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-CellLayout.html#v:cellLayoutGetArea"
        })


#endif

-- method CellLayout::get_cells
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "cell_layout"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellLayout" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCellLayout" , sinceVersion = Nothing }
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
--                  (TInterface Name { namespace = "Gtk" , name = "CellRenderer" }))
-- throws : False
-- Skip return : False

foreign import ccall "gtk_cell_layout_get_cells" gtk_cell_layout_get_cells :: 
    Ptr CellLayout ->                       -- cell_layout : TInterface (Name {namespace = "Gtk", name = "CellLayout"})
    IO (Ptr (GList (Ptr Gtk.CellRenderer.CellRenderer)))

-- | Returns the cell renderers which have been added to /@cellLayout@/.
-- 
-- /Since: 2.12/
cellLayoutGetCells ::
    (B.CallStack.HasCallStack, MonadIO m, IsCellLayout a) =>
    a
    -- ^ /@cellLayout@/: a t'GI.Gtk.Interfaces.CellLayout.CellLayout'
    -> m [Gtk.CellRenderer.CellRenderer]
    -- ^ __Returns:__ 
    --     a list of cell renderers. The list, but not the renderers has
    --     been newly allocated and should be freed with @/g_list_free()/@
    --     when no longer needed.
cellLayoutGetCells cellLayout = liftIO $ do
    cellLayout' <- unsafeManagedPtrCastPtr cellLayout
    result <- gtk_cell_layout_get_cells cellLayout'
    result' <- unpackGList result
    result'' <- mapM (newObject Gtk.CellRenderer.CellRenderer) result'
    g_list_free result
    touchManagedPtr cellLayout
    return result''

#if defined(ENABLE_OVERLOADING)
data CellLayoutGetCellsMethodInfo
instance (signature ~ (m [Gtk.CellRenderer.CellRenderer]), MonadIO m, IsCellLayout a) => O.OverloadedMethod CellLayoutGetCellsMethodInfo a signature where
    overloadedMethod = cellLayoutGetCells

instance O.OverloadedMethodInfo CellLayoutGetCellsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.CellLayout.cellLayoutGetCells",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-CellLayout.html#v:cellLayoutGetCells"
        })


#endif

-- method CellLayout::pack_end
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "cell_layout"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellLayout" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCellLayout" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "cell"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellRenderer" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCellRenderer" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "expand"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "%TRUE if @cell is to be given extra space allocated to @cell_layout"
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

foreign import ccall "gtk_cell_layout_pack_end" gtk_cell_layout_pack_end :: 
    Ptr CellLayout ->                       -- cell_layout : TInterface (Name {namespace = "Gtk", name = "CellLayout"})
    Ptr Gtk.CellRenderer.CellRenderer ->    -- cell : TInterface (Name {namespace = "Gtk", name = "CellRenderer"})
    CInt ->                                 -- expand : TBasicType TBoolean
    IO ()

-- | Adds the /@cell@/ to the end of /@cellLayout@/. If /@expand@/ is 'P.False', then the
-- /@cell@/ is allocated no more space than it needs. Any unused space is
-- divided evenly between cells for which /@expand@/ is 'P.True'.
-- 
-- Note that reusing the same cell renderer is not supported.
-- 
-- /Since: 2.4/
cellLayoutPackEnd ::
    (B.CallStack.HasCallStack, MonadIO m, IsCellLayout a, Gtk.CellRenderer.IsCellRenderer b) =>
    a
    -- ^ /@cellLayout@/: a t'GI.Gtk.Interfaces.CellLayout.CellLayout'
    -> b
    -- ^ /@cell@/: a t'GI.Gtk.Objects.CellRenderer.CellRenderer'
    -> Bool
    -- ^ /@expand@/: 'P.True' if /@cell@/ is to be given extra space allocated to /@cellLayout@/
    -> m ()
cellLayoutPackEnd cellLayout cell expand = liftIO $ do
    cellLayout' <- unsafeManagedPtrCastPtr cellLayout
    cell' <- unsafeManagedPtrCastPtr cell
    let expand' = (fromIntegral . fromEnum) expand
    gtk_cell_layout_pack_end cellLayout' cell' expand'
    touchManagedPtr cellLayout
    touchManagedPtr cell
    return ()

#if defined(ENABLE_OVERLOADING)
data CellLayoutPackEndMethodInfo
instance (signature ~ (b -> Bool -> m ()), MonadIO m, IsCellLayout a, Gtk.CellRenderer.IsCellRenderer b) => O.OverloadedMethod CellLayoutPackEndMethodInfo a signature where
    overloadedMethod = cellLayoutPackEnd

instance O.OverloadedMethodInfo CellLayoutPackEndMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.CellLayout.cellLayoutPackEnd",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-CellLayout.html#v:cellLayoutPackEnd"
        })


#endif

-- method CellLayout::pack_start
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "cell_layout"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellLayout" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCellLayout" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "cell"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellRenderer" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCellRenderer" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "expand"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "%TRUE if @cell is to be given extra space allocated to @cell_layout"
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

foreign import ccall "gtk_cell_layout_pack_start" gtk_cell_layout_pack_start :: 
    Ptr CellLayout ->                       -- cell_layout : TInterface (Name {namespace = "Gtk", name = "CellLayout"})
    Ptr Gtk.CellRenderer.CellRenderer ->    -- cell : TInterface (Name {namespace = "Gtk", name = "CellRenderer"})
    CInt ->                                 -- expand : TBasicType TBoolean
    IO ()

-- | Packs the /@cell@/ into the beginning of /@cellLayout@/. If /@expand@/ is 'P.False',
-- then the /@cell@/ is allocated no more space than it needs. Any unused space
-- is divided evenly between cells for which /@expand@/ is 'P.True'.
-- 
-- Note that reusing the same cell renderer is not supported.
-- 
-- /Since: 2.4/
cellLayoutPackStart ::
    (B.CallStack.HasCallStack, MonadIO m, IsCellLayout a, Gtk.CellRenderer.IsCellRenderer b) =>
    a
    -- ^ /@cellLayout@/: a t'GI.Gtk.Interfaces.CellLayout.CellLayout'
    -> b
    -- ^ /@cell@/: a t'GI.Gtk.Objects.CellRenderer.CellRenderer'
    -> Bool
    -- ^ /@expand@/: 'P.True' if /@cell@/ is to be given extra space allocated to /@cellLayout@/
    -> m ()
cellLayoutPackStart cellLayout cell expand = liftIO $ do
    cellLayout' <- unsafeManagedPtrCastPtr cellLayout
    cell' <- unsafeManagedPtrCastPtr cell
    let expand' = (fromIntegral . fromEnum) expand
    gtk_cell_layout_pack_start cellLayout' cell' expand'
    touchManagedPtr cellLayout
    touchManagedPtr cell
    return ()

#if defined(ENABLE_OVERLOADING)
data CellLayoutPackStartMethodInfo
instance (signature ~ (b -> Bool -> m ()), MonadIO m, IsCellLayout a, Gtk.CellRenderer.IsCellRenderer b) => O.OverloadedMethod CellLayoutPackStartMethodInfo a signature where
    overloadedMethod = cellLayoutPackStart

instance O.OverloadedMethodInfo CellLayoutPackStartMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.CellLayout.cellLayoutPackStart",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-CellLayout.html#v:cellLayoutPackStart"
        })


#endif

-- method CellLayout::reorder
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "cell_layout"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellLayout" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCellLayout" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "cell"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellRenderer" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCellRenderer to reorder"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "position"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "new position to insert @cell at"
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

foreign import ccall "gtk_cell_layout_reorder" gtk_cell_layout_reorder :: 
    Ptr CellLayout ->                       -- cell_layout : TInterface (Name {namespace = "Gtk", name = "CellLayout"})
    Ptr Gtk.CellRenderer.CellRenderer ->    -- cell : TInterface (Name {namespace = "Gtk", name = "CellRenderer"})
    Int32 ->                                -- position : TBasicType TInt
    IO ()

-- | Re-inserts /@cell@/ at /@position@/.
-- 
-- Note that /@cell@/ has already to be packed into /@cellLayout@/
-- for this to function properly.
-- 
-- /Since: 2.4/
cellLayoutReorder ::
    (B.CallStack.HasCallStack, MonadIO m, IsCellLayout a, Gtk.CellRenderer.IsCellRenderer b) =>
    a
    -- ^ /@cellLayout@/: a t'GI.Gtk.Interfaces.CellLayout.CellLayout'
    -> b
    -- ^ /@cell@/: a t'GI.Gtk.Objects.CellRenderer.CellRenderer' to reorder
    -> Int32
    -- ^ /@position@/: new position to insert /@cell@/ at
    -> m ()
cellLayoutReorder cellLayout cell position = liftIO $ do
    cellLayout' <- unsafeManagedPtrCastPtr cellLayout
    cell' <- unsafeManagedPtrCastPtr cell
    gtk_cell_layout_reorder cellLayout' cell' position
    touchManagedPtr cellLayout
    touchManagedPtr cell
    return ()

#if defined(ENABLE_OVERLOADING)
data CellLayoutReorderMethodInfo
instance (signature ~ (b -> Int32 -> m ()), MonadIO m, IsCellLayout a, Gtk.CellRenderer.IsCellRenderer b) => O.OverloadedMethod CellLayoutReorderMethodInfo a signature where
    overloadedMethod = cellLayoutReorder

instance O.OverloadedMethodInfo CellLayoutReorderMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.CellLayout.cellLayoutReorder",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-CellLayout.html#v:cellLayoutReorder"
        })


#endif

-- method CellLayout::set_cell_data_func
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "cell_layout"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellLayout" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCellLayout" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "cell"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellRenderer" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCellRenderer" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "func"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellLayoutDataFunc" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the #GtkCellLayoutDataFunc to use, or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeNotified
--           , argClosure = 3
--           , argDestroy = 4
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "func_data"
--           , argType = TBasicType TPtr
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "user data for @func"
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
--                 { rawDocText = Just "destroy notify for @func_data"
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

foreign import ccall "gtk_cell_layout_set_cell_data_func" gtk_cell_layout_set_cell_data_func :: 
    Ptr CellLayout ->                       -- cell_layout : TInterface (Name {namespace = "Gtk", name = "CellLayout"})
    Ptr Gtk.CellRenderer.CellRenderer ->    -- cell : TInterface (Name {namespace = "Gtk", name = "CellRenderer"})
    FunPtr Gtk.Callbacks.C_CellLayoutDataFunc -> -- func : TInterface (Name {namespace = "Gtk", name = "CellLayoutDataFunc"})
    Ptr () ->                               -- func_data : TBasicType TPtr
    FunPtr GLib.Callbacks.C_DestroyNotify -> -- destroy : TInterface (Name {namespace = "GLib", name = "DestroyNotify"})
    IO ()

-- | Sets the t'GI.Gtk.Callbacks.CellLayoutDataFunc' to use for /@cellLayout@/.
-- 
-- This function is used instead of the standard attributes mapping
-- for setting the column value, and should set the value of /@cellLayout@/’s
-- cell renderer(s) as appropriate.
-- 
-- /@func@/ may be 'P.Nothing' to remove a previously set function.
-- 
-- /Since: 2.4/
cellLayoutSetCellDataFunc ::
    (B.CallStack.HasCallStack, MonadIO m, IsCellLayout a, Gtk.CellRenderer.IsCellRenderer b) =>
    a
    -- ^ /@cellLayout@/: a t'GI.Gtk.Interfaces.CellLayout.CellLayout'
    -> b
    -- ^ /@cell@/: a t'GI.Gtk.Objects.CellRenderer.CellRenderer'
    -> Maybe (Gtk.Callbacks.CellLayoutDataFunc)
    -- ^ /@func@/: the t'GI.Gtk.Callbacks.CellLayoutDataFunc' to use, or 'P.Nothing'
    -> m ()
cellLayoutSetCellDataFunc cellLayout cell func = liftIO $ do
    cellLayout' <- unsafeManagedPtrCastPtr cellLayout
    cell' <- unsafeManagedPtrCastPtr cell
    maybeFunc <- case func of
        Nothing -> return (castPtrToFunPtr nullPtr)
        Just jFunc -> do
            jFunc' <- Gtk.Callbacks.mk_CellLayoutDataFunc (Gtk.Callbacks.wrap_CellLayoutDataFunc Nothing (Gtk.Callbacks.drop_closures_CellLayoutDataFunc jFunc))
            return jFunc'
    let funcData = castFunPtrToPtr maybeFunc
    let destroy = SP.safeFreeFunPtrPtr
    gtk_cell_layout_set_cell_data_func cellLayout' cell' maybeFunc funcData destroy
    touchManagedPtr cellLayout
    touchManagedPtr cell
    return ()

#if defined(ENABLE_OVERLOADING)
data CellLayoutSetCellDataFuncMethodInfo
instance (signature ~ (b -> Maybe (Gtk.Callbacks.CellLayoutDataFunc) -> m ()), MonadIO m, IsCellLayout a, Gtk.CellRenderer.IsCellRenderer b) => O.OverloadedMethod CellLayoutSetCellDataFuncMethodInfo a signature where
    overloadedMethod = cellLayoutSetCellDataFunc

instance O.OverloadedMethodInfo CellLayoutSetCellDataFuncMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.CellLayout.cellLayoutSetCellDataFunc",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-CellLayout.html#v:cellLayoutSetCellDataFunc"
        })


#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList CellLayout = CellLayoutSignalList
type CellLayoutSignalList = ('[ '("notify", GObject.Object.ObjectNotifySignalInfo)] :: [(Symbol, *)])

#endif


