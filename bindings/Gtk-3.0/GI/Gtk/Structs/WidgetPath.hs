{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- GtkWidgetPath is a boxed type that represents a widget hierarchy from
-- the topmost widget, typically a toplevel, to any child. This widget
-- path abstraction is used in t'GI.Gtk.Objects.StyleContext.StyleContext' on behalf of the real
-- widget in order to query style information.
-- 
-- If you are using GTK+ widgets, you probably will not need to use
-- this API directly, as there is 'GI.Gtk.Objects.Widget.widgetGetPath', and the style
-- context returned by 'GI.Gtk.Objects.Widget.widgetGetStyleContext' will be automatically
-- updated on widget hierarchy changes.
-- 
-- The widget path generation is generally simple:
-- 
-- == Defining a button within a window
-- 
-- 
-- === /C code/
-- >
-- >{
-- >  GtkWidgetPath *path;
-- >
-- >  path = gtk_widget_path_new ();
-- >  gtk_widget_path_append_type (path, GTK_TYPE_WINDOW);
-- >  gtk_widget_path_append_type (path, GTK_TYPE_BUTTON);
-- >}
-- 
-- 
-- Although more complex information, such as widget names, or
-- different classes (property that may be used by other widget
-- types) and intermediate regions may be included:
-- 
-- == Defining the first tab widget in a notebook
-- 
-- 
-- === /C code/
-- >
-- >{
-- >  GtkWidgetPath *path;
-- >  guint pos;
-- >
-- >  path = gtk_widget_path_new ();
-- >
-- >  pos = gtk_widget_path_append_type (path, GTK_TYPE_NOTEBOOK);
-- >  gtk_widget_path_iter_add_region (path, pos, "tab", GTK_REGION_EVEN | GTK_REGION_FIRST);
-- >
-- >  pos = gtk_widget_path_append_type (path, GTK_TYPE_LABEL);
-- >  gtk_widget_path_iter_set_name (path, pos, "first tab label");
-- >}
-- 
-- 
-- All this information will be used to match the style information
-- that applies to the described widget.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Structs.WidgetPath
    ( 

-- * Exported types
    WidgetPath(..)                          ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [appendForWidget]("GI.Gtk.Structs.WidgetPath#g:method:appendForWidget"), [appendType]("GI.Gtk.Structs.WidgetPath#g:method:appendType"), [appendWithSiblings]("GI.Gtk.Structs.WidgetPath#g:method:appendWithSiblings"), [copy]("GI.Gtk.Structs.WidgetPath#g:method:copy"), [free]("GI.Gtk.Structs.WidgetPath#g:method:free"), [hasParent]("GI.Gtk.Structs.WidgetPath#g:method:hasParent"), [isType]("GI.Gtk.Structs.WidgetPath#g:method:isType"), [iterAddClass]("GI.Gtk.Structs.WidgetPath#g:method:iterAddClass"), [iterAddRegion]("GI.Gtk.Structs.WidgetPath#g:method:iterAddRegion"), [iterClearClasses]("GI.Gtk.Structs.WidgetPath#g:method:iterClearClasses"), [iterClearRegions]("GI.Gtk.Structs.WidgetPath#g:method:iterClearRegions"), [iterGetName]("GI.Gtk.Structs.WidgetPath#g:method:iterGetName"), [iterGetObjectName]("GI.Gtk.Structs.WidgetPath#g:method:iterGetObjectName"), [iterGetObjectType]("GI.Gtk.Structs.WidgetPath#g:method:iterGetObjectType"), [iterGetSiblingIndex]("GI.Gtk.Structs.WidgetPath#g:method:iterGetSiblingIndex"), [iterGetSiblings]("GI.Gtk.Structs.WidgetPath#g:method:iterGetSiblings"), [iterGetState]("GI.Gtk.Structs.WidgetPath#g:method:iterGetState"), [iterHasClass]("GI.Gtk.Structs.WidgetPath#g:method:iterHasClass"), [iterHasName]("GI.Gtk.Structs.WidgetPath#g:method:iterHasName"), [iterHasQclass]("GI.Gtk.Structs.WidgetPath#g:method:iterHasQclass"), [iterHasQname]("GI.Gtk.Structs.WidgetPath#g:method:iterHasQname"), [iterHasQregion]("GI.Gtk.Structs.WidgetPath#g:method:iterHasQregion"), [iterHasRegion]("GI.Gtk.Structs.WidgetPath#g:method:iterHasRegion"), [iterListClasses]("GI.Gtk.Structs.WidgetPath#g:method:iterListClasses"), [iterListRegions]("GI.Gtk.Structs.WidgetPath#g:method:iterListRegions"), [iterRemoveClass]("GI.Gtk.Structs.WidgetPath#g:method:iterRemoveClass"), [iterRemoveRegion]("GI.Gtk.Structs.WidgetPath#g:method:iterRemoveRegion"), [iterSetName]("GI.Gtk.Structs.WidgetPath#g:method:iterSetName"), [iterSetObjectName]("GI.Gtk.Structs.WidgetPath#g:method:iterSetObjectName"), [iterSetObjectType]("GI.Gtk.Structs.WidgetPath#g:method:iterSetObjectType"), [iterSetState]("GI.Gtk.Structs.WidgetPath#g:method:iterSetState"), [length]("GI.Gtk.Structs.WidgetPath#g:method:length"), [prependType]("GI.Gtk.Structs.WidgetPath#g:method:prependType"), [ref]("GI.Gtk.Structs.WidgetPath#g:method:ref"), [toString]("GI.Gtk.Structs.WidgetPath#g:method:toString"), [unref]("GI.Gtk.Structs.WidgetPath#g:method:unref").
-- 
-- ==== Getters
-- [getObjectType]("GI.Gtk.Structs.WidgetPath#g:method:getObjectType").
-- 
-- ==== Setters
-- /None/.

#if defined(ENABLE_OVERLOADING)
    ResolveWidgetPathMethod                 ,
#endif

-- ** appendForWidget #method:appendForWidget#

#if defined(ENABLE_OVERLOADING)
    WidgetPathAppendForWidgetMethodInfo     ,
#endif
    widgetPathAppendForWidget               ,


-- ** appendType #method:appendType#

#if defined(ENABLE_OVERLOADING)
    WidgetPathAppendTypeMethodInfo          ,
#endif
    widgetPathAppendType                    ,


-- ** appendWithSiblings #method:appendWithSiblings#

#if defined(ENABLE_OVERLOADING)
    WidgetPathAppendWithSiblingsMethodInfo  ,
#endif
    widgetPathAppendWithSiblings            ,


-- ** copy #method:copy#

#if defined(ENABLE_OVERLOADING)
    WidgetPathCopyMethodInfo                ,
#endif
    widgetPathCopy                          ,


-- ** free #method:free#

#if defined(ENABLE_OVERLOADING)
    WidgetPathFreeMethodInfo                ,
#endif
    widgetPathFree                          ,


-- ** getObjectType #method:getObjectType#

#if defined(ENABLE_OVERLOADING)
    WidgetPathGetObjectTypeMethodInfo       ,
#endif
    widgetPathGetObjectType                 ,


-- ** hasParent #method:hasParent#

#if defined(ENABLE_OVERLOADING)
    WidgetPathHasParentMethodInfo           ,
#endif
    widgetPathHasParent                     ,


-- ** isType #method:isType#

#if defined(ENABLE_OVERLOADING)
    WidgetPathIsTypeMethodInfo              ,
#endif
    widgetPathIsType                        ,


-- ** iterAddClass #method:iterAddClass#

#if defined(ENABLE_OVERLOADING)
    WidgetPathIterAddClassMethodInfo        ,
#endif
    widgetPathIterAddClass                  ,


-- ** iterAddRegion #method:iterAddRegion#

#if defined(ENABLE_OVERLOADING)
    WidgetPathIterAddRegionMethodInfo       ,
#endif
    widgetPathIterAddRegion                 ,


-- ** iterClearClasses #method:iterClearClasses#

#if defined(ENABLE_OVERLOADING)
    WidgetPathIterClearClassesMethodInfo    ,
#endif
    widgetPathIterClearClasses              ,


-- ** iterClearRegions #method:iterClearRegions#

#if defined(ENABLE_OVERLOADING)
    WidgetPathIterClearRegionsMethodInfo    ,
#endif
    widgetPathIterClearRegions              ,


-- ** iterGetName #method:iterGetName#

#if defined(ENABLE_OVERLOADING)
    WidgetPathIterGetNameMethodInfo         ,
#endif
    widgetPathIterGetName                   ,


-- ** iterGetObjectName #method:iterGetObjectName#

#if defined(ENABLE_OVERLOADING)
    WidgetPathIterGetObjectNameMethodInfo   ,
#endif
    widgetPathIterGetObjectName             ,


-- ** iterGetObjectType #method:iterGetObjectType#

#if defined(ENABLE_OVERLOADING)
    WidgetPathIterGetObjectTypeMethodInfo   ,
#endif
    widgetPathIterGetObjectType             ,


-- ** iterGetSiblingIndex #method:iterGetSiblingIndex#

#if defined(ENABLE_OVERLOADING)
    WidgetPathIterGetSiblingIndexMethodInfo ,
#endif
    widgetPathIterGetSiblingIndex           ,


-- ** iterGetSiblings #method:iterGetSiblings#

#if defined(ENABLE_OVERLOADING)
    WidgetPathIterGetSiblingsMethodInfo     ,
#endif
    widgetPathIterGetSiblings               ,


-- ** iterGetState #method:iterGetState#

#if defined(ENABLE_OVERLOADING)
    WidgetPathIterGetStateMethodInfo        ,
#endif
    widgetPathIterGetState                  ,


-- ** iterHasClass #method:iterHasClass#

#if defined(ENABLE_OVERLOADING)
    WidgetPathIterHasClassMethodInfo        ,
#endif
    widgetPathIterHasClass                  ,


-- ** iterHasName #method:iterHasName#

#if defined(ENABLE_OVERLOADING)
    WidgetPathIterHasNameMethodInfo         ,
#endif
    widgetPathIterHasName                   ,


-- ** iterHasQclass #method:iterHasQclass#

#if defined(ENABLE_OVERLOADING)
    WidgetPathIterHasQclassMethodInfo       ,
#endif
    widgetPathIterHasQclass                 ,


-- ** iterHasQname #method:iterHasQname#

#if defined(ENABLE_OVERLOADING)
    WidgetPathIterHasQnameMethodInfo        ,
#endif
    widgetPathIterHasQname                  ,


-- ** iterHasQregion #method:iterHasQregion#

#if defined(ENABLE_OVERLOADING)
    WidgetPathIterHasQregionMethodInfo      ,
#endif
    widgetPathIterHasQregion                ,


-- ** iterHasRegion #method:iterHasRegion#

#if defined(ENABLE_OVERLOADING)
    WidgetPathIterHasRegionMethodInfo       ,
#endif
    widgetPathIterHasRegion                 ,


-- ** iterListClasses #method:iterListClasses#

#if defined(ENABLE_OVERLOADING)
    WidgetPathIterListClassesMethodInfo     ,
#endif
    widgetPathIterListClasses               ,


-- ** iterListRegions #method:iterListRegions#

#if defined(ENABLE_OVERLOADING)
    WidgetPathIterListRegionsMethodInfo     ,
#endif
    widgetPathIterListRegions               ,


-- ** iterRemoveClass #method:iterRemoveClass#

#if defined(ENABLE_OVERLOADING)
    WidgetPathIterRemoveClassMethodInfo     ,
#endif
    widgetPathIterRemoveClass               ,


-- ** iterRemoveRegion #method:iterRemoveRegion#

#if defined(ENABLE_OVERLOADING)
    WidgetPathIterRemoveRegionMethodInfo    ,
#endif
    widgetPathIterRemoveRegion              ,


-- ** iterSetName #method:iterSetName#

#if defined(ENABLE_OVERLOADING)
    WidgetPathIterSetNameMethodInfo         ,
#endif
    widgetPathIterSetName                   ,


-- ** iterSetObjectName #method:iterSetObjectName#

#if defined(ENABLE_OVERLOADING)
    WidgetPathIterSetObjectNameMethodInfo   ,
#endif
    widgetPathIterSetObjectName             ,


-- ** iterSetObjectType #method:iterSetObjectType#

#if defined(ENABLE_OVERLOADING)
    WidgetPathIterSetObjectTypeMethodInfo   ,
#endif
    widgetPathIterSetObjectType             ,


-- ** iterSetState #method:iterSetState#

#if defined(ENABLE_OVERLOADING)
    WidgetPathIterSetStateMethodInfo        ,
#endif
    widgetPathIterSetState                  ,


-- ** length #method:length#

#if defined(ENABLE_OVERLOADING)
    WidgetPathLengthMethodInfo              ,
#endif
    widgetPathLength                        ,


-- ** new #method:new#

    widgetPathNew                           ,


-- ** prependType #method:prependType#

#if defined(ENABLE_OVERLOADING)
    WidgetPathPrependTypeMethodInfo         ,
#endif
    widgetPathPrependType                   ,


-- ** ref #method:ref#

#if defined(ENABLE_OVERLOADING)
    WidgetPathRefMethodInfo                 ,
#endif
    widgetPathRef                           ,


-- ** toString #method:toString#

#if defined(ENABLE_OVERLOADING)
    WidgetPathToStringMethodInfo            ,
#endif
    widgetPathToString                      ,


-- ** unref #method:unref#

#if defined(ENABLE_OVERLOADING)
    WidgetPathUnrefMethodInfo               ,
#endif
    widgetPathUnref                         ,




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

import {-# SOURCE #-} qualified GI.Gtk.Flags as Gtk.Flags
import {-# SOURCE #-} qualified GI.Gtk.Objects.Widget as Gtk.Widget

-- | Memory-managed wrapper type.
newtype WidgetPath = WidgetPath (SP.ManagedPtr WidgetPath)
    deriving (Eq)

instance SP.ManagedPtrNewtype WidgetPath where
    toManagedPtr (WidgetPath p) = p

foreign import ccall "gtk_widget_path_get_type" c_gtk_widget_path_get_type :: 
    IO GType

type instance O.ParentTypes WidgetPath = '[]
instance O.HasParentTypes WidgetPath

instance B.Types.TypedObject WidgetPath where
    glibType = c_gtk_widget_path_get_type

instance B.Types.GBoxed WidgetPath

-- | Convert 'WidgetPath' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe WidgetPath) where
    gvalueGType_ = c_gtk_widget_path_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_boxed gv (FP.nullPtr :: FP.Ptr WidgetPath)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_boxed gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_boxed gv :: IO (Ptr WidgetPath)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newBoxed WidgetPath ptr
        else return P.Nothing
        
    


#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList WidgetPath
type instance O.AttributeList WidgetPath = WidgetPathAttributeList
type WidgetPathAttributeList = ('[ ] :: [(Symbol, *)])
#endif

-- method WidgetPath::new
-- method type : Constructor
-- Args: []
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "WidgetPath" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_widget_path_new" gtk_widget_path_new :: 
    IO (Ptr WidgetPath)

-- | Returns an empty widget path.
-- 
-- /Since: 3.0/
widgetPathNew ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m WidgetPath
    -- ^ __Returns:__ A newly created, empty, t'GI.Gtk.Structs.WidgetPath.WidgetPath'
widgetPathNew  = liftIO $ do
    result <- gtk_widget_path_new
    checkUnexpectedReturnNULL "widgetPathNew" result
    result' <- (wrapBoxed WidgetPath) result
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method WidgetPath::append_for_widget
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "path"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "WidgetPath" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a widget path" , sinceVersion = Nothing }
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
--               Documentation
--                 { rawDocText = Just "the widget to append to the widget path"
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

foreign import ccall "gtk_widget_path_append_for_widget" gtk_widget_path_append_for_widget :: 
    Ptr WidgetPath ->                       -- path : TInterface (Name {namespace = "Gtk", name = "WidgetPath"})
    Ptr Gtk.Widget.Widget ->                -- widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    IO Int32

-- | Appends the data from /@widget@/ to the widget hierarchy represented
-- by /@path@/. This function is a shortcut for adding information from
-- /@widget@/ to the given /@path@/. This includes setting the name or
-- adding the style classes from /@widget@/.
-- 
-- /Since: 3.2/
widgetPathAppendForWidget ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.Widget.IsWidget a) =>
    WidgetPath
    -- ^ /@path@/: a widget path
    -> a
    -- ^ /@widget@/: the widget to append to the widget path
    -> m Int32
    -- ^ __Returns:__ the position where the data was inserted
widgetPathAppendForWidget path widget = liftIO $ do
    path' <- unsafeManagedPtrGetPtr path
    widget' <- unsafeManagedPtrCastPtr widget
    result <- gtk_widget_path_append_for_widget path' widget'
    touchManagedPtr path
    touchManagedPtr widget
    return result

#if defined(ENABLE_OVERLOADING)
data WidgetPathAppendForWidgetMethodInfo
instance (signature ~ (a -> m Int32), MonadIO m, Gtk.Widget.IsWidget a) => O.OverloadedMethod WidgetPathAppendForWidgetMethodInfo WidgetPath signature where
    overloadedMethod = widgetPathAppendForWidget

instance O.OverloadedMethodInfo WidgetPathAppendForWidgetMethodInfo WidgetPath where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetPath.widgetPathAppendForWidget",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetPath.html#v:widgetPathAppendForWidget"
        })


#endif

-- method WidgetPath::append_type
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "path"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "WidgetPath" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWidgetPath" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "type"
--           , argType = TBasicType TGType
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "widget type to append"
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

foreign import ccall "gtk_widget_path_append_type" gtk_widget_path_append_type :: 
    Ptr WidgetPath ->                       -- path : TInterface (Name {namespace = "Gtk", name = "WidgetPath"})
    CGType ->                               -- type : TBasicType TGType
    IO Int32

-- | Appends a widget type to the widget hierarchy represented by /@path@/.
-- 
-- /Since: 3.0/
widgetPathAppendType ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    WidgetPath
    -- ^ /@path@/: a t'GI.Gtk.Structs.WidgetPath.WidgetPath'
    -> GType
    -- ^ /@type@/: widget type to append
    -> m Int32
    -- ^ __Returns:__ the position where the element was inserted
widgetPathAppendType path type_ = liftIO $ do
    path' <- unsafeManagedPtrGetPtr path
    let type_' = gtypeToCGType type_
    result <- gtk_widget_path_append_type path' type_'
    touchManagedPtr path
    return result

#if defined(ENABLE_OVERLOADING)
data WidgetPathAppendTypeMethodInfo
instance (signature ~ (GType -> m Int32), MonadIO m) => O.OverloadedMethod WidgetPathAppendTypeMethodInfo WidgetPath signature where
    overloadedMethod = widgetPathAppendType

instance O.OverloadedMethodInfo WidgetPathAppendTypeMethodInfo WidgetPath where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetPath.widgetPathAppendType",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetPath.html#v:widgetPathAppendType"
        })


#endif

-- method WidgetPath::append_with_siblings
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "path"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "WidgetPath" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the widget path to append to"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "siblings"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "WidgetPath" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "a widget path describing a list of siblings. This path\n  may not contain any siblings itself and it must not be modified\n  afterwards."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "sibling_index"
--           , argType = TBasicType TUInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "index into @siblings for where the added element is\n  positioned."
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

foreign import ccall "gtk_widget_path_append_with_siblings" gtk_widget_path_append_with_siblings :: 
    Ptr WidgetPath ->                       -- path : TInterface (Name {namespace = "Gtk", name = "WidgetPath"})
    Ptr WidgetPath ->                       -- siblings : TInterface (Name {namespace = "Gtk", name = "WidgetPath"})
    Word32 ->                               -- sibling_index : TBasicType TUInt
    IO Int32

-- | Appends a widget type with all its siblings to the widget hierarchy
-- represented by /@path@/. Using this function instead of
-- 'GI.Gtk.Structs.WidgetPath.widgetPathAppendType' will allow the CSS theming to use
-- sibling matches in selectors and apply :nth-@/child()/@ pseudo classes.
-- In turn, it requires a lot more care in widget implementations as
-- widgets need to make sure to call 'GI.Gtk.Objects.Widget.widgetResetStyle' on all
-- involved widgets when the /@siblings@/ path changes.
-- 
-- /Since: 3.2/
widgetPathAppendWithSiblings ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    WidgetPath
    -- ^ /@path@/: the widget path to append to
    -> WidgetPath
    -- ^ /@siblings@/: a widget path describing a list of siblings. This path
    --   may not contain any siblings itself and it must not be modified
    --   afterwards.
    -> Word32
    -- ^ /@siblingIndex@/: index into /@siblings@/ for where the added element is
    --   positioned.
    -> m Int32
    -- ^ __Returns:__ the position where the element was inserted.
widgetPathAppendWithSiblings path siblings siblingIndex = liftIO $ do
    path' <- unsafeManagedPtrGetPtr path
    siblings' <- unsafeManagedPtrGetPtr siblings
    result <- gtk_widget_path_append_with_siblings path' siblings' siblingIndex
    touchManagedPtr path
    touchManagedPtr siblings
    return result

#if defined(ENABLE_OVERLOADING)
data WidgetPathAppendWithSiblingsMethodInfo
instance (signature ~ (WidgetPath -> Word32 -> m Int32), MonadIO m) => O.OverloadedMethod WidgetPathAppendWithSiblingsMethodInfo WidgetPath signature where
    overloadedMethod = widgetPathAppendWithSiblings

instance O.OverloadedMethodInfo WidgetPathAppendWithSiblingsMethodInfo WidgetPath where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetPath.widgetPathAppendWithSiblings",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetPath.html#v:widgetPathAppendWithSiblings"
        })


#endif

-- method WidgetPath::copy
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "path"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "WidgetPath" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWidgetPath" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "WidgetPath" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_widget_path_copy" gtk_widget_path_copy :: 
    Ptr WidgetPath ->                       -- path : TInterface (Name {namespace = "Gtk", name = "WidgetPath"})
    IO (Ptr WidgetPath)

-- | Returns a copy of /@path@/
-- 
-- /Since: 3.0/
widgetPathCopy ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    WidgetPath
    -- ^ /@path@/: a t'GI.Gtk.Structs.WidgetPath.WidgetPath'
    -> m WidgetPath
    -- ^ __Returns:__ a copy of /@path@/
widgetPathCopy path = liftIO $ do
    path' <- unsafeManagedPtrGetPtr path
    result <- gtk_widget_path_copy path'
    checkUnexpectedReturnNULL "widgetPathCopy" result
    result' <- (wrapBoxed WidgetPath) result
    touchManagedPtr path
    return result'

#if defined(ENABLE_OVERLOADING)
data WidgetPathCopyMethodInfo
instance (signature ~ (m WidgetPath), MonadIO m) => O.OverloadedMethod WidgetPathCopyMethodInfo WidgetPath signature where
    overloadedMethod = widgetPathCopy

instance O.OverloadedMethodInfo WidgetPathCopyMethodInfo WidgetPath where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetPath.widgetPathCopy",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetPath.html#v:widgetPathCopy"
        })


#endif

-- method WidgetPath::free
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "path"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "WidgetPath" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWidgetPath" , sinceVersion = Nothing }
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

foreign import ccall "gtk_widget_path_free" gtk_widget_path_free :: 
    Ptr WidgetPath ->                       -- path : TInterface (Name {namespace = "Gtk", name = "WidgetPath"})
    IO ()

-- | Decrements the reference count on /@path@/, freeing the structure
-- if the reference count reaches 0.
-- 
-- /Since: 3.0/
widgetPathFree ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    WidgetPath
    -- ^ /@path@/: a t'GI.Gtk.Structs.WidgetPath.WidgetPath'
    -> m ()
widgetPathFree path = liftIO $ do
    path' <- unsafeManagedPtrGetPtr path
    gtk_widget_path_free path'
    touchManagedPtr path
    return ()

#if defined(ENABLE_OVERLOADING)
data WidgetPathFreeMethodInfo
instance (signature ~ (m ()), MonadIO m) => O.OverloadedMethod WidgetPathFreeMethodInfo WidgetPath signature where
    overloadedMethod = widgetPathFree

instance O.OverloadedMethodInfo WidgetPathFreeMethodInfo WidgetPath where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetPath.widgetPathFree",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetPath.html#v:widgetPathFree"
        })


#endif

-- method WidgetPath::get_object_type
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "path"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "WidgetPath" }
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
-- returnType: Just (TBasicType TGType)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_widget_path_get_object_type" gtk_widget_path_get_object_type :: 
    Ptr WidgetPath ->                       -- path : TInterface (Name {namespace = "Gtk", name = "WidgetPath"})
    IO CGType

-- | Returns the topmost object type, that is, the object type this path
-- is representing.
-- 
-- /Since: 3.0/
widgetPathGetObjectType ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    WidgetPath
    -- ^ /@path@/: a t'GI.Gtk.Objects.Widget.Widget'
    -> m GType
    -- ^ __Returns:__ The object type
widgetPathGetObjectType path = liftIO $ do
    path' <- unsafeManagedPtrGetPtr path
    result <- gtk_widget_path_get_object_type path'
    let result' = GType result
    touchManagedPtr path
    return result'

#if defined(ENABLE_OVERLOADING)
data WidgetPathGetObjectTypeMethodInfo
instance (signature ~ (m GType), MonadIO m) => O.OverloadedMethod WidgetPathGetObjectTypeMethodInfo WidgetPath signature where
    overloadedMethod = widgetPathGetObjectType

instance O.OverloadedMethodInfo WidgetPathGetObjectTypeMethodInfo WidgetPath where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetPath.widgetPathGetObjectType",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetPath.html#v:widgetPathGetObjectType"
        })


#endif

-- method WidgetPath::has_parent
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "path"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "WidgetPath" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWidgetPath" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "type"
--           , argType = TBasicType TGType
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "widget type to check in parents"
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

foreign import ccall "gtk_widget_path_has_parent" gtk_widget_path_has_parent :: 
    Ptr WidgetPath ->                       -- path : TInterface (Name {namespace = "Gtk", name = "WidgetPath"})
    CGType ->                               -- type : TBasicType TGType
    IO CInt

-- | Returns 'P.True' if any of the parents of the widget represented
-- in /@path@/ is of type /@type@/, or any subtype of it.
-- 
-- /Since: 3.0/
widgetPathHasParent ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    WidgetPath
    -- ^ /@path@/: a t'GI.Gtk.Structs.WidgetPath.WidgetPath'
    -> GType
    -- ^ /@type@/: widget type to check in parents
    -> m Bool
    -- ^ __Returns:__ 'P.True' if any parent is of type /@type@/
widgetPathHasParent path type_ = liftIO $ do
    path' <- unsafeManagedPtrGetPtr path
    let type_' = gtypeToCGType type_
    result <- gtk_widget_path_has_parent path' type_'
    let result' = (/= 0) result
    touchManagedPtr path
    return result'

#if defined(ENABLE_OVERLOADING)
data WidgetPathHasParentMethodInfo
instance (signature ~ (GType -> m Bool), MonadIO m) => O.OverloadedMethod WidgetPathHasParentMethodInfo WidgetPath signature where
    overloadedMethod = widgetPathHasParent

instance O.OverloadedMethodInfo WidgetPathHasParentMethodInfo WidgetPath where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetPath.widgetPathHasParent",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetPath.html#v:widgetPathHasParent"
        })


#endif

-- method WidgetPath::is_type
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "path"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "WidgetPath" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWidgetPath" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "type"
--           , argType = TBasicType TGType
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "widget type to match"
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

foreign import ccall "gtk_widget_path_is_type" gtk_widget_path_is_type :: 
    Ptr WidgetPath ->                       -- path : TInterface (Name {namespace = "Gtk", name = "WidgetPath"})
    CGType ->                               -- type : TBasicType TGType
    IO CInt

-- | Returns 'P.True' if the widget type represented by this path
-- is /@type@/, or a subtype of it.
-- 
-- /Since: 3.0/
widgetPathIsType ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    WidgetPath
    -- ^ /@path@/: a t'GI.Gtk.Structs.WidgetPath.WidgetPath'
    -> GType
    -- ^ /@type@/: widget type to match
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the widget represented by /@path@/ is of type /@type@/
widgetPathIsType path type_ = liftIO $ do
    path' <- unsafeManagedPtrGetPtr path
    let type_' = gtypeToCGType type_
    result <- gtk_widget_path_is_type path' type_'
    let result' = (/= 0) result
    touchManagedPtr path
    return result'

#if defined(ENABLE_OVERLOADING)
data WidgetPathIsTypeMethodInfo
instance (signature ~ (GType -> m Bool), MonadIO m) => O.OverloadedMethod WidgetPathIsTypeMethodInfo WidgetPath signature where
    overloadedMethod = widgetPathIsType

instance O.OverloadedMethodInfo WidgetPathIsTypeMethodInfo WidgetPath where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetPath.widgetPathIsType",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetPath.html#v:widgetPathIsType"
        })


#endif

-- method WidgetPath::iter_add_class
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "path"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "WidgetPath" }
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
--           { argCName = "pos"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "position to modify, -1 for the path head"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "name"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a class name" , sinceVersion = Nothing }
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

foreign import ccall "gtk_widget_path_iter_add_class" gtk_widget_path_iter_add_class :: 
    Ptr WidgetPath ->                       -- path : TInterface (Name {namespace = "Gtk", name = "WidgetPath"})
    Int32 ->                                -- pos : TBasicType TInt
    CString ->                              -- name : TBasicType TUTF8
    IO ()

-- | Adds the class /@name@/ to the widget at position /@pos@/ in
-- the hierarchy defined in /@path@/. See
-- 'GI.Gtk.Objects.StyleContext.styleContextAddClass'.
-- 
-- /Since: 3.0/
widgetPathIterAddClass ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    WidgetPath
    -- ^ /@path@/: a t'GI.Gtk.Objects.Widget.Widget'
    -> Int32
    -- ^ /@pos@/: position to modify, -1 for the path head
    -> T.Text
    -- ^ /@name@/: a class name
    -> m ()
widgetPathIterAddClass path pos name = liftIO $ do
    path' <- unsafeManagedPtrGetPtr path
    name' <- textToCString name
    gtk_widget_path_iter_add_class path' pos name'
    touchManagedPtr path
    freeMem name'
    return ()

#if defined(ENABLE_OVERLOADING)
data WidgetPathIterAddClassMethodInfo
instance (signature ~ (Int32 -> T.Text -> m ()), MonadIO m) => O.OverloadedMethod WidgetPathIterAddClassMethodInfo WidgetPath signature where
    overloadedMethod = widgetPathIterAddClass

instance O.OverloadedMethodInfo WidgetPathIterAddClassMethodInfo WidgetPath where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetPath.widgetPathIterAddClass",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetPath.html#v:widgetPathIterAddClass"
        })


#endif

-- method WidgetPath::iter_add_region
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "path"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "WidgetPath" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWidgetPath" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "pos"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "position to modify, -1 for the path head"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "name"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "region name" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "flags"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "RegionFlags" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "flags affecting the region"
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

foreign import ccall "gtk_widget_path_iter_add_region" gtk_widget_path_iter_add_region :: 
    Ptr WidgetPath ->                       -- path : TInterface (Name {namespace = "Gtk", name = "WidgetPath"})
    Int32 ->                                -- pos : TBasicType TInt
    CString ->                              -- name : TBasicType TUTF8
    CUInt ->                                -- flags : TInterface (Name {namespace = "Gtk", name = "RegionFlags"})
    IO ()

{-# DEPRECATED widgetPathIterAddRegion ["(Since version 3.14)","The use of regions is deprecated."] #-}
-- | Adds the region /@name@/ to the widget at position /@pos@/ in
-- the hierarchy defined in /@path@/. See
-- 'GI.Gtk.Objects.StyleContext.styleContextAddRegion'.
-- 
-- Region names must only contain lowercase letters
-- and “-”, starting always with a lowercase letter.
-- 
-- /Since: 3.0/
widgetPathIterAddRegion ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    WidgetPath
    -- ^ /@path@/: a t'GI.Gtk.Structs.WidgetPath.WidgetPath'
    -> Int32
    -- ^ /@pos@/: position to modify, -1 for the path head
    -> T.Text
    -- ^ /@name@/: region name
    -> [Gtk.Flags.RegionFlags]
    -- ^ /@flags@/: flags affecting the region
    -> m ()
widgetPathIterAddRegion path pos name flags = liftIO $ do
    path' <- unsafeManagedPtrGetPtr path
    name' <- textToCString name
    let flags' = gflagsToWord flags
    gtk_widget_path_iter_add_region path' pos name' flags'
    touchManagedPtr path
    freeMem name'
    return ()

#if defined(ENABLE_OVERLOADING)
data WidgetPathIterAddRegionMethodInfo
instance (signature ~ (Int32 -> T.Text -> [Gtk.Flags.RegionFlags] -> m ()), MonadIO m) => O.OverloadedMethod WidgetPathIterAddRegionMethodInfo WidgetPath signature where
    overloadedMethod = widgetPathIterAddRegion

instance O.OverloadedMethodInfo WidgetPathIterAddRegionMethodInfo WidgetPath where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetPath.widgetPathIterAddRegion",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetPath.html#v:widgetPathIterAddRegion"
        })


#endif

-- method WidgetPath::iter_clear_classes
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "path"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "WidgetPath" }
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
--           { argCName = "pos"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "position to modify, -1 for the path head"
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

foreign import ccall "gtk_widget_path_iter_clear_classes" gtk_widget_path_iter_clear_classes :: 
    Ptr WidgetPath ->                       -- path : TInterface (Name {namespace = "Gtk", name = "WidgetPath"})
    Int32 ->                                -- pos : TBasicType TInt
    IO ()

-- | Removes all classes from the widget at position /@pos@/ in the
-- hierarchy defined in /@path@/.
-- 
-- /Since: 3.0/
widgetPathIterClearClasses ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    WidgetPath
    -- ^ /@path@/: a t'GI.Gtk.Objects.Widget.Widget'
    -> Int32
    -- ^ /@pos@/: position to modify, -1 for the path head
    -> m ()
widgetPathIterClearClasses path pos = liftIO $ do
    path' <- unsafeManagedPtrGetPtr path
    gtk_widget_path_iter_clear_classes path' pos
    touchManagedPtr path
    return ()

#if defined(ENABLE_OVERLOADING)
data WidgetPathIterClearClassesMethodInfo
instance (signature ~ (Int32 -> m ()), MonadIO m) => O.OverloadedMethod WidgetPathIterClearClassesMethodInfo WidgetPath signature where
    overloadedMethod = widgetPathIterClearClasses

instance O.OverloadedMethodInfo WidgetPathIterClearClassesMethodInfo WidgetPath where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetPath.widgetPathIterClearClasses",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetPath.html#v:widgetPathIterClearClasses"
        })


#endif

-- method WidgetPath::iter_clear_regions
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "path"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "WidgetPath" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWidgetPath" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "pos"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "position to modify, -1 for the path head"
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

foreign import ccall "gtk_widget_path_iter_clear_regions" gtk_widget_path_iter_clear_regions :: 
    Ptr WidgetPath ->                       -- path : TInterface (Name {namespace = "Gtk", name = "WidgetPath"})
    Int32 ->                                -- pos : TBasicType TInt
    IO ()

{-# DEPRECATED widgetPathIterClearRegions ["(Since version 3.14)","The use of regions is deprecated."] #-}
-- | Removes all regions from the widget at position /@pos@/ in the
-- hierarchy defined in /@path@/.
-- 
-- /Since: 3.0/
widgetPathIterClearRegions ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    WidgetPath
    -- ^ /@path@/: a t'GI.Gtk.Structs.WidgetPath.WidgetPath'
    -> Int32
    -- ^ /@pos@/: position to modify, -1 for the path head
    -> m ()
widgetPathIterClearRegions path pos = liftIO $ do
    path' <- unsafeManagedPtrGetPtr path
    gtk_widget_path_iter_clear_regions path' pos
    touchManagedPtr path
    return ()

#if defined(ENABLE_OVERLOADING)
data WidgetPathIterClearRegionsMethodInfo
instance (signature ~ (Int32 -> m ()), MonadIO m) => O.OverloadedMethod WidgetPathIterClearRegionsMethodInfo WidgetPath signature where
    overloadedMethod = widgetPathIterClearRegions

instance O.OverloadedMethodInfo WidgetPathIterClearRegionsMethodInfo WidgetPath where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetPath.widgetPathIterClearRegions",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetPath.html#v:widgetPathIterClearRegions"
        })


#endif

-- method WidgetPath::iter_get_name
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "path"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "WidgetPath" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWidgetPath" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "pos"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "position to get the widget name for, -1 for the path head"
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
-- returnType: Just (TBasicType TUTF8)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_widget_path_iter_get_name" gtk_widget_path_iter_get_name :: 
    Ptr WidgetPath ->                       -- path : TInterface (Name {namespace = "Gtk", name = "WidgetPath"})
    Int32 ->                                -- pos : TBasicType TInt
    IO CString

-- | Returns the name corresponding to the widget found at
-- the position /@pos@/ in the widget hierarchy defined by
-- /@path@/
widgetPathIterGetName ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    WidgetPath
    -- ^ /@path@/: a t'GI.Gtk.Structs.WidgetPath.WidgetPath'
    -> Int32
    -- ^ /@pos@/: position to get the widget name for, -1 for the path head
    -> m (Maybe T.Text)
    -- ^ __Returns:__ The widget name, or 'P.Nothing' if none was set.
widgetPathIterGetName path pos = liftIO $ do
    path' <- unsafeManagedPtrGetPtr path
    result <- gtk_widget_path_iter_get_name path' pos
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- cstringToText result'
        return result''
    touchManagedPtr path
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data WidgetPathIterGetNameMethodInfo
instance (signature ~ (Int32 -> m (Maybe T.Text)), MonadIO m) => O.OverloadedMethod WidgetPathIterGetNameMethodInfo WidgetPath signature where
    overloadedMethod = widgetPathIterGetName

instance O.OverloadedMethodInfo WidgetPathIterGetNameMethodInfo WidgetPath where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetPath.widgetPathIterGetName",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetPath.html#v:widgetPathIterGetName"
        })


#endif

-- method WidgetPath::iter_get_object_name
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "path"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "WidgetPath" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWidgetPath" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "pos"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "position to get the object name for, -1 for the path head"
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
-- returnType: Just (TBasicType TUTF8)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_widget_path_iter_get_object_name" gtk_widget_path_iter_get_object_name :: 
    Ptr WidgetPath ->                       -- path : TInterface (Name {namespace = "Gtk", name = "WidgetPath"})
    Int32 ->                                -- pos : TBasicType TInt
    IO CString

-- | Returns the object name that is at position /@pos@/ in the widget
-- hierarchy defined in /@path@/.
-- 
-- /Since: 3.20/
widgetPathIterGetObjectName ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    WidgetPath
    -- ^ /@path@/: a t'GI.Gtk.Structs.WidgetPath.WidgetPath'
    -> Int32
    -- ^ /@pos@/: position to get the object name for, -1 for the path head
    -> m (Maybe T.Text)
    -- ^ __Returns:__ the name or 'P.Nothing'
widgetPathIterGetObjectName path pos = liftIO $ do
    path' <- unsafeManagedPtrGetPtr path
    result <- gtk_widget_path_iter_get_object_name path' pos
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- cstringToText result'
        return result''
    touchManagedPtr path
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data WidgetPathIterGetObjectNameMethodInfo
instance (signature ~ (Int32 -> m (Maybe T.Text)), MonadIO m) => O.OverloadedMethod WidgetPathIterGetObjectNameMethodInfo WidgetPath signature where
    overloadedMethod = widgetPathIterGetObjectName

instance O.OverloadedMethodInfo WidgetPathIterGetObjectNameMethodInfo WidgetPath where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetPath.widgetPathIterGetObjectName",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetPath.html#v:widgetPathIterGetObjectName"
        })


#endif

-- method WidgetPath::iter_get_object_type
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "path"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "WidgetPath" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWidgetPath" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "pos"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "position to get the object type for, -1 for the path head"
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
-- returnType: Just (TBasicType TGType)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_widget_path_iter_get_object_type" gtk_widget_path_iter_get_object_type :: 
    Ptr WidgetPath ->                       -- path : TInterface (Name {namespace = "Gtk", name = "WidgetPath"})
    Int32 ->                                -- pos : TBasicType TInt
    IO CGType

-- | Returns the object t'GType' that is at position /@pos@/ in the widget
-- hierarchy defined in /@path@/.
-- 
-- /Since: 3.0/
widgetPathIterGetObjectType ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    WidgetPath
    -- ^ /@path@/: a t'GI.Gtk.Structs.WidgetPath.WidgetPath'
    -> Int32
    -- ^ /@pos@/: position to get the object type for, -1 for the path head
    -> m GType
    -- ^ __Returns:__ a widget type
widgetPathIterGetObjectType path pos = liftIO $ do
    path' <- unsafeManagedPtrGetPtr path
    result <- gtk_widget_path_iter_get_object_type path' pos
    let result' = GType result
    touchManagedPtr path
    return result'

#if defined(ENABLE_OVERLOADING)
data WidgetPathIterGetObjectTypeMethodInfo
instance (signature ~ (Int32 -> m GType), MonadIO m) => O.OverloadedMethod WidgetPathIterGetObjectTypeMethodInfo WidgetPath signature where
    overloadedMethod = widgetPathIterGetObjectType

instance O.OverloadedMethodInfo WidgetPathIterGetObjectTypeMethodInfo WidgetPath where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetPath.widgetPathIterGetObjectType",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetPath.html#v:widgetPathIterGetObjectType"
        })


#endif

-- method WidgetPath::iter_get_sibling_index
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "path"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "WidgetPath" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWidgetPath" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "pos"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "position to get the sibling index for, -1 for the path head"
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
-- returnType: Just (TBasicType TUInt)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_widget_path_iter_get_sibling_index" gtk_widget_path_iter_get_sibling_index :: 
    Ptr WidgetPath ->                       -- path : TInterface (Name {namespace = "Gtk", name = "WidgetPath"})
    Int32 ->                                -- pos : TBasicType TInt
    IO Word32

-- | Returns the index into the list of siblings for the element at /@pos@/ as
-- returned by 'GI.Gtk.Structs.WidgetPath.widgetPathIterGetSiblings'. If that function would
-- return 'P.Nothing' because the element at /@pos@/ has no siblings, this function
-- will return 0.
widgetPathIterGetSiblingIndex ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    WidgetPath
    -- ^ /@path@/: a t'GI.Gtk.Structs.WidgetPath.WidgetPath'
    -> Int32
    -- ^ /@pos@/: position to get the sibling index for, -1 for the path head
    -> m Word32
    -- ^ __Returns:__ 0 or the index into the list of siblings for the element at /@pos@/.
widgetPathIterGetSiblingIndex path pos = liftIO $ do
    path' <- unsafeManagedPtrGetPtr path
    result <- gtk_widget_path_iter_get_sibling_index path' pos
    touchManagedPtr path
    return result

#if defined(ENABLE_OVERLOADING)
data WidgetPathIterGetSiblingIndexMethodInfo
instance (signature ~ (Int32 -> m Word32), MonadIO m) => O.OverloadedMethod WidgetPathIterGetSiblingIndexMethodInfo WidgetPath signature where
    overloadedMethod = widgetPathIterGetSiblingIndex

instance O.OverloadedMethodInfo WidgetPathIterGetSiblingIndexMethodInfo WidgetPath where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetPath.widgetPathIterGetSiblingIndex",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetPath.html#v:widgetPathIterGetSiblingIndex"
        })


#endif

-- method WidgetPath::iter_get_siblings
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "path"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "WidgetPath" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWidgetPath" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "pos"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "position to get the siblings for, -1 for the path head"
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "WidgetPath" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_widget_path_iter_get_siblings" gtk_widget_path_iter_get_siblings :: 
    Ptr WidgetPath ->                       -- path : TInterface (Name {namespace = "Gtk", name = "WidgetPath"})
    Int32 ->                                -- pos : TBasicType TInt
    IO (Ptr WidgetPath)

-- | Returns the list of siblings for the element at /@pos@/. If the element
-- was not added with siblings, 'P.Nothing' is returned.
widgetPathIterGetSiblings ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    WidgetPath
    -- ^ /@path@/: a t'GI.Gtk.Structs.WidgetPath.WidgetPath'
    -> Int32
    -- ^ /@pos@/: position to get the siblings for, -1 for the path head
    -> m WidgetPath
    -- ^ __Returns:__ 'P.Nothing' or the list of siblings for the element at /@pos@/.
widgetPathIterGetSiblings path pos = liftIO $ do
    path' <- unsafeManagedPtrGetPtr path
    result <- gtk_widget_path_iter_get_siblings path' pos
    checkUnexpectedReturnNULL "widgetPathIterGetSiblings" result
    result' <- (newBoxed WidgetPath) result
    touchManagedPtr path
    return result'

#if defined(ENABLE_OVERLOADING)
data WidgetPathIterGetSiblingsMethodInfo
instance (signature ~ (Int32 -> m WidgetPath), MonadIO m) => O.OverloadedMethod WidgetPathIterGetSiblingsMethodInfo WidgetPath signature where
    overloadedMethod = widgetPathIterGetSiblings

instance O.OverloadedMethodInfo WidgetPathIterGetSiblingsMethodInfo WidgetPath where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetPath.widgetPathIterGetSiblings",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetPath.html#v:widgetPathIterGetSiblings"
        })


#endif

-- method WidgetPath::iter_get_state
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "path"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "WidgetPath" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWidgetPath" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "pos"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "position to get the state for, -1 for the path head"
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "StateFlags" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_widget_path_iter_get_state" gtk_widget_path_iter_get_state :: 
    Ptr WidgetPath ->                       -- path : TInterface (Name {namespace = "Gtk", name = "WidgetPath"})
    Int32 ->                                -- pos : TBasicType TInt
    IO CUInt

-- | Returns the state flags corresponding to the widget found at
-- the position /@pos@/ in the widget hierarchy defined by
-- /@path@/
-- 
-- /Since: 3.14/
widgetPathIterGetState ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    WidgetPath
    -- ^ /@path@/: a t'GI.Gtk.Structs.WidgetPath.WidgetPath'
    -> Int32
    -- ^ /@pos@/: position to get the state for, -1 for the path head
    -> m [Gtk.Flags.StateFlags]
    -- ^ __Returns:__ The state flags
widgetPathIterGetState path pos = liftIO $ do
    path' <- unsafeManagedPtrGetPtr path
    result <- gtk_widget_path_iter_get_state path' pos
    let result' = wordToGFlags result
    touchManagedPtr path
    return result'

#if defined(ENABLE_OVERLOADING)
data WidgetPathIterGetStateMethodInfo
instance (signature ~ (Int32 -> m [Gtk.Flags.StateFlags]), MonadIO m) => O.OverloadedMethod WidgetPathIterGetStateMethodInfo WidgetPath signature where
    overloadedMethod = widgetPathIterGetState

instance O.OverloadedMethodInfo WidgetPathIterGetStateMethodInfo WidgetPath where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetPath.widgetPathIterGetState",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetPath.html#v:widgetPathIterGetState"
        })


#endif

-- method WidgetPath::iter_has_class
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "path"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "WidgetPath" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWidgetPath" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "pos"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "position to query, -1 for the path head"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "name"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "class name" , sinceVersion = Nothing }
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

foreign import ccall "gtk_widget_path_iter_has_class" gtk_widget_path_iter_has_class :: 
    Ptr WidgetPath ->                       -- path : TInterface (Name {namespace = "Gtk", name = "WidgetPath"})
    Int32 ->                                -- pos : TBasicType TInt
    CString ->                              -- name : TBasicType TUTF8
    IO CInt

-- | Returns 'P.True' if the widget at position /@pos@/ has the class /@name@/
-- defined, 'P.False' otherwise.
-- 
-- /Since: 3.0/
widgetPathIterHasClass ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    WidgetPath
    -- ^ /@path@/: a t'GI.Gtk.Structs.WidgetPath.WidgetPath'
    -> Int32
    -- ^ /@pos@/: position to query, -1 for the path head
    -> T.Text
    -- ^ /@name@/: class name
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the class /@name@/ is defined for the widget at /@pos@/
widgetPathIterHasClass path pos name = liftIO $ do
    path' <- unsafeManagedPtrGetPtr path
    name' <- textToCString name
    result <- gtk_widget_path_iter_has_class path' pos name'
    let result' = (/= 0) result
    touchManagedPtr path
    freeMem name'
    return result'

#if defined(ENABLE_OVERLOADING)
data WidgetPathIterHasClassMethodInfo
instance (signature ~ (Int32 -> T.Text -> m Bool), MonadIO m) => O.OverloadedMethod WidgetPathIterHasClassMethodInfo WidgetPath signature where
    overloadedMethod = widgetPathIterHasClass

instance O.OverloadedMethodInfo WidgetPathIterHasClassMethodInfo WidgetPath where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetPath.widgetPathIterHasClass",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetPath.html#v:widgetPathIterHasClass"
        })


#endif

-- method WidgetPath::iter_has_name
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "path"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "WidgetPath" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWidgetPath" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "pos"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "position to query, -1 for the path head"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "name"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a widget name" , sinceVersion = Nothing }
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

foreign import ccall "gtk_widget_path_iter_has_name" gtk_widget_path_iter_has_name :: 
    Ptr WidgetPath ->                       -- path : TInterface (Name {namespace = "Gtk", name = "WidgetPath"})
    Int32 ->                                -- pos : TBasicType TInt
    CString ->                              -- name : TBasicType TUTF8
    IO CInt

-- | Returns 'P.True' if the widget at position /@pos@/ has the name /@name@/,
-- 'P.False' otherwise.
-- 
-- /Since: 3.0/
widgetPathIterHasName ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    WidgetPath
    -- ^ /@path@/: a t'GI.Gtk.Structs.WidgetPath.WidgetPath'
    -> Int32
    -- ^ /@pos@/: position to query, -1 for the path head
    -> T.Text
    -- ^ /@name@/: a widget name
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the widget at /@pos@/ has this name
widgetPathIterHasName path pos name = liftIO $ do
    path' <- unsafeManagedPtrGetPtr path
    name' <- textToCString name
    result <- gtk_widget_path_iter_has_name path' pos name'
    let result' = (/= 0) result
    touchManagedPtr path
    freeMem name'
    return result'

#if defined(ENABLE_OVERLOADING)
data WidgetPathIterHasNameMethodInfo
instance (signature ~ (Int32 -> T.Text -> m Bool), MonadIO m) => O.OverloadedMethod WidgetPathIterHasNameMethodInfo WidgetPath signature where
    overloadedMethod = widgetPathIterHasName

instance O.OverloadedMethodInfo WidgetPathIterHasNameMethodInfo WidgetPath where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetPath.widgetPathIterHasName",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetPath.html#v:widgetPathIterHasName"
        })


#endif

-- method WidgetPath::iter_has_qclass
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "path"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "WidgetPath" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWidgetPath" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "pos"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "position to query, -1 for the path head"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "qname"
--           , argType = TBasicType TUInt32
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "class name as a #GQuark"
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

foreign import ccall "gtk_widget_path_iter_has_qclass" gtk_widget_path_iter_has_qclass :: 
    Ptr WidgetPath ->                       -- path : TInterface (Name {namespace = "Gtk", name = "WidgetPath"})
    Int32 ->                                -- pos : TBasicType TInt
    Word32 ->                               -- qname : TBasicType TUInt32
    IO CInt

-- | See 'GI.Gtk.Structs.WidgetPath.widgetPathIterHasClass'. This is a version that operates
-- with GQuarks.
-- 
-- /Since: 3.0/
widgetPathIterHasQclass ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    WidgetPath
    -- ^ /@path@/: a t'GI.Gtk.Structs.WidgetPath.WidgetPath'
    -> Int32
    -- ^ /@pos@/: position to query, -1 for the path head
    -> Word32
    -- ^ /@qname@/: class name as a @/GQuark/@
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the widget at /@pos@/ has the class defined.
widgetPathIterHasQclass path pos qname = liftIO $ do
    path' <- unsafeManagedPtrGetPtr path
    result <- gtk_widget_path_iter_has_qclass path' pos qname
    let result' = (/= 0) result
    touchManagedPtr path
    return result'

#if defined(ENABLE_OVERLOADING)
data WidgetPathIterHasQclassMethodInfo
instance (signature ~ (Int32 -> Word32 -> m Bool), MonadIO m) => O.OverloadedMethod WidgetPathIterHasQclassMethodInfo WidgetPath signature where
    overloadedMethod = widgetPathIterHasQclass

instance O.OverloadedMethodInfo WidgetPathIterHasQclassMethodInfo WidgetPath where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetPath.widgetPathIterHasQclass",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetPath.html#v:widgetPathIterHasQclass"
        })


#endif

-- method WidgetPath::iter_has_qname
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "path"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "WidgetPath" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWidgetPath" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "pos"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "position to query, -1 for the path head"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "qname"
--           , argType = TBasicType TUInt32
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "widget name as a #GQuark"
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

foreign import ccall "gtk_widget_path_iter_has_qname" gtk_widget_path_iter_has_qname :: 
    Ptr WidgetPath ->                       -- path : TInterface (Name {namespace = "Gtk", name = "WidgetPath"})
    Int32 ->                                -- pos : TBasicType TInt
    Word32 ->                               -- qname : TBasicType TUInt32
    IO CInt

-- | See 'GI.Gtk.Structs.WidgetPath.widgetPathIterHasName'. This is a version
-- that operates on @/GQuarks/@.
-- 
-- /Since: 3.0/
widgetPathIterHasQname ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    WidgetPath
    -- ^ /@path@/: a t'GI.Gtk.Structs.WidgetPath.WidgetPath'
    -> Int32
    -- ^ /@pos@/: position to query, -1 for the path head
    -> Word32
    -- ^ /@qname@/: widget name as a @/GQuark/@
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the widget at /@pos@/ has this name
widgetPathIterHasQname path pos qname = liftIO $ do
    path' <- unsafeManagedPtrGetPtr path
    result <- gtk_widget_path_iter_has_qname path' pos qname
    let result' = (/= 0) result
    touchManagedPtr path
    return result'

#if defined(ENABLE_OVERLOADING)
data WidgetPathIterHasQnameMethodInfo
instance (signature ~ (Int32 -> Word32 -> m Bool), MonadIO m) => O.OverloadedMethod WidgetPathIterHasQnameMethodInfo WidgetPath signature where
    overloadedMethod = widgetPathIterHasQname

instance O.OverloadedMethodInfo WidgetPathIterHasQnameMethodInfo WidgetPath where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetPath.widgetPathIterHasQname",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetPath.html#v:widgetPathIterHasQname"
        })


#endif

-- method WidgetPath::iter_has_qregion
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "path"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "WidgetPath" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWidgetPath" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "pos"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "position to query, -1 for the path head"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "qname"
--           , argType = TBasicType TUInt32
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "region name as a #GQuark"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "flags"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "RegionFlags" }
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "return location for the region flags"
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
-- returnType: Just (TBasicType TBoolean)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_widget_path_iter_has_qregion" gtk_widget_path_iter_has_qregion :: 
    Ptr WidgetPath ->                       -- path : TInterface (Name {namespace = "Gtk", name = "WidgetPath"})
    Int32 ->                                -- pos : TBasicType TInt
    Word32 ->                               -- qname : TBasicType TUInt32
    Ptr CUInt ->                            -- flags : TInterface (Name {namespace = "Gtk", name = "RegionFlags"})
    IO CInt

{-# DEPRECATED widgetPathIterHasQregion ["(Since version 3.14)","The use of regions is deprecated."] #-}
-- | See 'GI.Gtk.Structs.WidgetPath.widgetPathIterHasRegion'. This is a version that operates
-- with GQuarks.
-- 
-- /Since: 3.0/
widgetPathIterHasQregion ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    WidgetPath
    -- ^ /@path@/: a t'GI.Gtk.Structs.WidgetPath.WidgetPath'
    -> Int32
    -- ^ /@pos@/: position to query, -1 for the path head
    -> Word32
    -- ^ /@qname@/: region name as a @/GQuark/@
    -> m ((Bool, [Gtk.Flags.RegionFlags]))
    -- ^ __Returns:__ 'P.True' if the widget at /@pos@/ has the region defined.
widgetPathIterHasQregion path pos qname = liftIO $ do
    path' <- unsafeManagedPtrGetPtr path
    flags <- allocMem :: IO (Ptr CUInt)
    result <- gtk_widget_path_iter_has_qregion path' pos qname flags
    let result' = (/= 0) result
    flags' <- peek flags
    let flags'' = wordToGFlags flags'
    touchManagedPtr path
    freeMem flags
    return (result', flags'')

#if defined(ENABLE_OVERLOADING)
data WidgetPathIterHasQregionMethodInfo
instance (signature ~ (Int32 -> Word32 -> m ((Bool, [Gtk.Flags.RegionFlags]))), MonadIO m) => O.OverloadedMethod WidgetPathIterHasQregionMethodInfo WidgetPath signature where
    overloadedMethod = widgetPathIterHasQregion

instance O.OverloadedMethodInfo WidgetPathIterHasQregionMethodInfo WidgetPath where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetPath.widgetPathIterHasQregion",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetPath.html#v:widgetPathIterHasQregion"
        })


#endif

-- method WidgetPath::iter_has_region
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "path"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "WidgetPath" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWidgetPath" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "pos"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "position to query, -1 for the path head"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "name"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "region name" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "flags"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "RegionFlags" }
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "return location for the region flags"
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
-- returnType: Just (TBasicType TBoolean)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_widget_path_iter_has_region" gtk_widget_path_iter_has_region :: 
    Ptr WidgetPath ->                       -- path : TInterface (Name {namespace = "Gtk", name = "WidgetPath"})
    Int32 ->                                -- pos : TBasicType TInt
    CString ->                              -- name : TBasicType TUTF8
    Ptr CUInt ->                            -- flags : TInterface (Name {namespace = "Gtk", name = "RegionFlags"})
    IO CInt

{-# DEPRECATED widgetPathIterHasRegion ["(Since version 3.14)","The use of regions is deprecated."] #-}
-- | Returns 'P.True' if the widget at position /@pos@/ has the class /@name@/
-- defined, 'P.False' otherwise.
-- 
-- /Since: 3.0/
widgetPathIterHasRegion ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    WidgetPath
    -- ^ /@path@/: a t'GI.Gtk.Structs.WidgetPath.WidgetPath'
    -> Int32
    -- ^ /@pos@/: position to query, -1 for the path head
    -> T.Text
    -- ^ /@name@/: region name
    -> m ((Bool, [Gtk.Flags.RegionFlags]))
    -- ^ __Returns:__ 'P.True' if the class /@name@/ is defined for the widget at /@pos@/
widgetPathIterHasRegion path pos name = liftIO $ do
    path' <- unsafeManagedPtrGetPtr path
    name' <- textToCString name
    flags <- allocMem :: IO (Ptr CUInt)
    result <- gtk_widget_path_iter_has_region path' pos name' flags
    let result' = (/= 0) result
    flags' <- peek flags
    let flags'' = wordToGFlags flags'
    touchManagedPtr path
    freeMem name'
    freeMem flags
    return (result', flags'')

#if defined(ENABLE_OVERLOADING)
data WidgetPathIterHasRegionMethodInfo
instance (signature ~ (Int32 -> T.Text -> m ((Bool, [Gtk.Flags.RegionFlags]))), MonadIO m) => O.OverloadedMethod WidgetPathIterHasRegionMethodInfo WidgetPath signature where
    overloadedMethod = widgetPathIterHasRegion

instance O.OverloadedMethodInfo WidgetPathIterHasRegionMethodInfo WidgetPath where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetPath.widgetPathIterHasRegion",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetPath.html#v:widgetPathIterHasRegion"
        })


#endif

-- method WidgetPath::iter_list_classes
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "path"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "WidgetPath" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWidgetPath" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "pos"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "position to query, -1 for the path head"
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
-- returnType: Just (TGSList (TBasicType TUTF8))
-- throws : False
-- Skip return : False

foreign import ccall "gtk_widget_path_iter_list_classes" gtk_widget_path_iter_list_classes :: 
    Ptr WidgetPath ->                       -- path : TInterface (Name {namespace = "Gtk", name = "WidgetPath"})
    Int32 ->                                -- pos : TBasicType TInt
    IO (Ptr (GSList CString))

-- | Returns a list with all the class names defined for the widget
-- at position /@pos@/ in the hierarchy defined in /@path@/.
-- 
-- /Since: 3.0/
widgetPathIterListClasses ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    WidgetPath
    -- ^ /@path@/: a t'GI.Gtk.Structs.WidgetPath.WidgetPath'
    -> Int32
    -- ^ /@pos@/: position to query, -1 for the path head
    -> m [T.Text]
    -- ^ __Returns:__ The list of
    --          classes, This is a list of strings, the t'GI.GLib.Structs.SList.SList' contents
    --          are owned by GTK+, but you should use @/g_slist_free()/@ to
    --          free the list itself.
widgetPathIterListClasses path pos = liftIO $ do
    path' <- unsafeManagedPtrGetPtr path
    result <- gtk_widget_path_iter_list_classes path' pos
    result' <- unpackGSList result
    result'' <- mapM cstringToText result'
    g_slist_free result
    touchManagedPtr path
    return result''

#if defined(ENABLE_OVERLOADING)
data WidgetPathIterListClassesMethodInfo
instance (signature ~ (Int32 -> m [T.Text]), MonadIO m) => O.OverloadedMethod WidgetPathIterListClassesMethodInfo WidgetPath signature where
    overloadedMethod = widgetPathIterListClasses

instance O.OverloadedMethodInfo WidgetPathIterListClassesMethodInfo WidgetPath where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetPath.widgetPathIterListClasses",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetPath.html#v:widgetPathIterListClasses"
        })


#endif

-- method WidgetPath::iter_list_regions
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "path"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "WidgetPath" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWidgetPath" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "pos"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "position to query, -1 for the path head"
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
-- returnType: Just (TGSList (TBasicType TUTF8))
-- throws : False
-- Skip return : False

foreign import ccall "gtk_widget_path_iter_list_regions" gtk_widget_path_iter_list_regions :: 
    Ptr WidgetPath ->                       -- path : TInterface (Name {namespace = "Gtk", name = "WidgetPath"})
    Int32 ->                                -- pos : TBasicType TInt
    IO (Ptr (GSList CString))

{-# DEPRECATED widgetPathIterListRegions ["(Since version 3.14)","The use of regions is deprecated."] #-}
-- | Returns a list with all the region names defined for the widget
-- at position /@pos@/ in the hierarchy defined in /@path@/.
-- 
-- /Since: 3.0/
widgetPathIterListRegions ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    WidgetPath
    -- ^ /@path@/: a t'GI.Gtk.Structs.WidgetPath.WidgetPath'
    -> Int32
    -- ^ /@pos@/: position to query, -1 for the path head
    -> m [T.Text]
    -- ^ __Returns:__ The list of
    --          regions, This is a list of strings, the t'GI.GLib.Structs.SList.SList' contents
    --          are owned by GTK+, but you should use @/g_slist_free()/@ to
    --          free the list itself.
widgetPathIterListRegions path pos = liftIO $ do
    path' <- unsafeManagedPtrGetPtr path
    result <- gtk_widget_path_iter_list_regions path' pos
    result' <- unpackGSList result
    result'' <- mapM cstringToText result'
    g_slist_free result
    touchManagedPtr path
    return result''

#if defined(ENABLE_OVERLOADING)
data WidgetPathIterListRegionsMethodInfo
instance (signature ~ (Int32 -> m [T.Text]), MonadIO m) => O.OverloadedMethod WidgetPathIterListRegionsMethodInfo WidgetPath signature where
    overloadedMethod = widgetPathIterListRegions

instance O.OverloadedMethodInfo WidgetPathIterListRegionsMethodInfo WidgetPath where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetPath.widgetPathIterListRegions",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetPath.html#v:widgetPathIterListRegions"
        })


#endif

-- method WidgetPath::iter_remove_class
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "path"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "WidgetPath" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWidgetPath" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "pos"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "position to modify, -1 for the path head"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "name"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "class name" , sinceVersion = Nothing }
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

foreign import ccall "gtk_widget_path_iter_remove_class" gtk_widget_path_iter_remove_class :: 
    Ptr WidgetPath ->                       -- path : TInterface (Name {namespace = "Gtk", name = "WidgetPath"})
    Int32 ->                                -- pos : TBasicType TInt
    CString ->                              -- name : TBasicType TUTF8
    IO ()

-- | Removes the class /@name@/ from the widget at position /@pos@/ in
-- the hierarchy defined in /@path@/.
-- 
-- /Since: 3.0/
widgetPathIterRemoveClass ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    WidgetPath
    -- ^ /@path@/: a t'GI.Gtk.Structs.WidgetPath.WidgetPath'
    -> Int32
    -- ^ /@pos@/: position to modify, -1 for the path head
    -> T.Text
    -- ^ /@name@/: class name
    -> m ()
widgetPathIterRemoveClass path pos name = liftIO $ do
    path' <- unsafeManagedPtrGetPtr path
    name' <- textToCString name
    gtk_widget_path_iter_remove_class path' pos name'
    touchManagedPtr path
    freeMem name'
    return ()

#if defined(ENABLE_OVERLOADING)
data WidgetPathIterRemoveClassMethodInfo
instance (signature ~ (Int32 -> T.Text -> m ()), MonadIO m) => O.OverloadedMethod WidgetPathIterRemoveClassMethodInfo WidgetPath signature where
    overloadedMethod = widgetPathIterRemoveClass

instance O.OverloadedMethodInfo WidgetPathIterRemoveClassMethodInfo WidgetPath where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetPath.widgetPathIterRemoveClass",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetPath.html#v:widgetPathIterRemoveClass"
        })


#endif

-- method WidgetPath::iter_remove_region
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "path"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "WidgetPath" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWidgetPath" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "pos"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "position to modify, -1 for the path head"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "name"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "region name" , sinceVersion = Nothing }
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

foreign import ccall "gtk_widget_path_iter_remove_region" gtk_widget_path_iter_remove_region :: 
    Ptr WidgetPath ->                       -- path : TInterface (Name {namespace = "Gtk", name = "WidgetPath"})
    Int32 ->                                -- pos : TBasicType TInt
    CString ->                              -- name : TBasicType TUTF8
    IO ()

{-# DEPRECATED widgetPathIterRemoveRegion ["(Since version 3.14)","The use of regions is deprecated."] #-}
-- | Removes the region /@name@/ from the widget at position /@pos@/ in
-- the hierarchy defined in /@path@/.
-- 
-- /Since: 3.0/
widgetPathIterRemoveRegion ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    WidgetPath
    -- ^ /@path@/: a t'GI.Gtk.Structs.WidgetPath.WidgetPath'
    -> Int32
    -- ^ /@pos@/: position to modify, -1 for the path head
    -> T.Text
    -- ^ /@name@/: region name
    -> m ()
widgetPathIterRemoveRegion path pos name = liftIO $ do
    path' <- unsafeManagedPtrGetPtr path
    name' <- textToCString name
    gtk_widget_path_iter_remove_region path' pos name'
    touchManagedPtr path
    freeMem name'
    return ()

#if defined(ENABLE_OVERLOADING)
data WidgetPathIterRemoveRegionMethodInfo
instance (signature ~ (Int32 -> T.Text -> m ()), MonadIO m) => O.OverloadedMethod WidgetPathIterRemoveRegionMethodInfo WidgetPath signature where
    overloadedMethod = widgetPathIterRemoveRegion

instance O.OverloadedMethodInfo WidgetPathIterRemoveRegionMethodInfo WidgetPath where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetPath.widgetPathIterRemoveRegion",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetPath.html#v:widgetPathIterRemoveRegion"
        })


#endif

-- method WidgetPath::iter_set_name
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "path"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "WidgetPath" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWidgetPath" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "pos"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "position to modify, -1 for the path head"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "name"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "widget name" , sinceVersion = Nothing }
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

foreign import ccall "gtk_widget_path_iter_set_name" gtk_widget_path_iter_set_name :: 
    Ptr WidgetPath ->                       -- path : TInterface (Name {namespace = "Gtk", name = "WidgetPath"})
    Int32 ->                                -- pos : TBasicType TInt
    CString ->                              -- name : TBasicType TUTF8
    IO ()

-- | Sets the widget name for the widget found at position /@pos@/
-- in the widget hierarchy defined by /@path@/.
-- 
-- /Since: 3.0/
widgetPathIterSetName ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    WidgetPath
    -- ^ /@path@/: a t'GI.Gtk.Structs.WidgetPath.WidgetPath'
    -> Int32
    -- ^ /@pos@/: position to modify, -1 for the path head
    -> T.Text
    -- ^ /@name@/: widget name
    -> m ()
widgetPathIterSetName path pos name = liftIO $ do
    path' <- unsafeManagedPtrGetPtr path
    name' <- textToCString name
    gtk_widget_path_iter_set_name path' pos name'
    touchManagedPtr path
    freeMem name'
    return ()

#if defined(ENABLE_OVERLOADING)
data WidgetPathIterSetNameMethodInfo
instance (signature ~ (Int32 -> T.Text -> m ()), MonadIO m) => O.OverloadedMethod WidgetPathIterSetNameMethodInfo WidgetPath signature where
    overloadedMethod = widgetPathIterSetName

instance O.OverloadedMethodInfo WidgetPathIterSetNameMethodInfo WidgetPath where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetPath.widgetPathIterSetName",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetPath.html#v:widgetPathIterSetName"
        })


#endif

-- method WidgetPath::iter_set_object_name
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "path"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "WidgetPath" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWidgetPath" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "pos"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "position to modify, -1 for the path head"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "name"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "object name to set or %NULL to unset"
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

foreign import ccall "gtk_widget_path_iter_set_object_name" gtk_widget_path_iter_set_object_name :: 
    Ptr WidgetPath ->                       -- path : TInterface (Name {namespace = "Gtk", name = "WidgetPath"})
    Int32 ->                                -- pos : TBasicType TInt
    CString ->                              -- name : TBasicType TUTF8
    IO ()

-- | Sets the object name for a given position in the widget hierarchy
-- defined by /@path@/.
-- 
-- When set, the object name overrides the object type when matching
-- CSS.
-- 
-- /Since: 3.20/
widgetPathIterSetObjectName ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    WidgetPath
    -- ^ /@path@/: a t'GI.Gtk.Structs.WidgetPath.WidgetPath'
    -> Int32
    -- ^ /@pos@/: position to modify, -1 for the path head
    -> Maybe (T.Text)
    -- ^ /@name@/: object name to set or 'P.Nothing' to unset
    -> m ()
widgetPathIterSetObjectName path pos name = liftIO $ do
    path' <- unsafeManagedPtrGetPtr path
    maybeName <- case name of
        Nothing -> return nullPtr
        Just jName -> do
            jName' <- textToCString jName
            return jName'
    gtk_widget_path_iter_set_object_name path' pos maybeName
    touchManagedPtr path
    freeMem maybeName
    return ()

#if defined(ENABLE_OVERLOADING)
data WidgetPathIterSetObjectNameMethodInfo
instance (signature ~ (Int32 -> Maybe (T.Text) -> m ()), MonadIO m) => O.OverloadedMethod WidgetPathIterSetObjectNameMethodInfo WidgetPath signature where
    overloadedMethod = widgetPathIterSetObjectName

instance O.OverloadedMethodInfo WidgetPathIterSetObjectNameMethodInfo WidgetPath where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetPath.widgetPathIterSetObjectName",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetPath.html#v:widgetPathIterSetObjectName"
        })


#endif

-- method WidgetPath::iter_set_object_type
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "path"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "WidgetPath" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWidgetPath" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "pos"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "position to modify, -1 for the path head"
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
--           , argType = TBasicType TGType
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "object type to set" , sinceVersion = Nothing }
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

foreign import ccall "gtk_widget_path_iter_set_object_type" gtk_widget_path_iter_set_object_type :: 
    Ptr WidgetPath ->                       -- path : TInterface (Name {namespace = "Gtk", name = "WidgetPath"})
    Int32 ->                                -- pos : TBasicType TInt
    CGType ->                               -- type : TBasicType TGType
    IO ()

-- | Sets the object type for a given position in the widget hierarchy
-- defined by /@path@/.
-- 
-- /Since: 3.0/
widgetPathIterSetObjectType ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    WidgetPath
    -- ^ /@path@/: a t'GI.Gtk.Structs.WidgetPath.WidgetPath'
    -> Int32
    -- ^ /@pos@/: position to modify, -1 for the path head
    -> GType
    -- ^ /@type@/: object type to set
    -> m ()
widgetPathIterSetObjectType path pos type_ = liftIO $ do
    path' <- unsafeManagedPtrGetPtr path
    let type_' = gtypeToCGType type_
    gtk_widget_path_iter_set_object_type path' pos type_'
    touchManagedPtr path
    return ()

#if defined(ENABLE_OVERLOADING)
data WidgetPathIterSetObjectTypeMethodInfo
instance (signature ~ (Int32 -> GType -> m ()), MonadIO m) => O.OverloadedMethod WidgetPathIterSetObjectTypeMethodInfo WidgetPath signature where
    overloadedMethod = widgetPathIterSetObjectType

instance O.OverloadedMethodInfo WidgetPathIterSetObjectTypeMethodInfo WidgetPath where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetPath.widgetPathIterSetObjectType",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetPath.html#v:widgetPathIterSetObjectType"
        })


#endif

-- method WidgetPath::iter_set_state
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "path"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "WidgetPath" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWidgetPath" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "pos"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "position to modify, -1 for the path head"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "state"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StateFlags" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "state flags" , sinceVersion = Nothing }
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

foreign import ccall "gtk_widget_path_iter_set_state" gtk_widget_path_iter_set_state :: 
    Ptr WidgetPath ->                       -- path : TInterface (Name {namespace = "Gtk", name = "WidgetPath"})
    Int32 ->                                -- pos : TBasicType TInt
    CUInt ->                                -- state : TInterface (Name {namespace = "Gtk", name = "StateFlags"})
    IO ()

-- | Sets the widget name for the widget found at position /@pos@/
-- in the widget hierarchy defined by /@path@/.
-- 
-- If you want to update just a single state flag, you need to do
-- this manually, as this function updates all state flags.
-- 
-- == Setting a flag
-- 
-- 
-- === /C code/
-- >
-- >gtk_widget_path_iter_set_state (path, pos, gtk_widget_path_iter_get_state (path, pos) | flag);
-- 
-- 
-- == Unsetting a flag
-- 
-- 
-- === /C code/
-- >
-- >gtk_widget_path_iter_set_state (path, pos, gtk_widget_path_iter_get_state (path, pos) & ~flag);
-- 
-- 
-- /Since: 3.14/
widgetPathIterSetState ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    WidgetPath
    -- ^ /@path@/: a t'GI.Gtk.Structs.WidgetPath.WidgetPath'
    -> Int32
    -- ^ /@pos@/: position to modify, -1 for the path head
    -> [Gtk.Flags.StateFlags]
    -- ^ /@state@/: state flags
    -> m ()
widgetPathIterSetState path pos state = liftIO $ do
    path' <- unsafeManagedPtrGetPtr path
    let state' = gflagsToWord state
    gtk_widget_path_iter_set_state path' pos state'
    touchManagedPtr path
    return ()

#if defined(ENABLE_OVERLOADING)
data WidgetPathIterSetStateMethodInfo
instance (signature ~ (Int32 -> [Gtk.Flags.StateFlags] -> m ()), MonadIO m) => O.OverloadedMethod WidgetPathIterSetStateMethodInfo WidgetPath signature where
    overloadedMethod = widgetPathIterSetState

instance O.OverloadedMethodInfo WidgetPathIterSetStateMethodInfo WidgetPath where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetPath.widgetPathIterSetState",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetPath.html#v:widgetPathIterSetState"
        })


#endif

-- method WidgetPath::length
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "path"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "WidgetPath" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWidgetPath" , sinceVersion = Nothing }
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

foreign import ccall "gtk_widget_path_length" gtk_widget_path_length :: 
    Ptr WidgetPath ->                       -- path : TInterface (Name {namespace = "Gtk", name = "WidgetPath"})
    IO Int32

-- | Returns the number of t'GI.Gtk.Objects.Widget.Widget' @/GTypes/@ between the represented
-- widget and its topmost container.
-- 
-- /Since: 3.0/
widgetPathLength ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    WidgetPath
    -- ^ /@path@/: a t'GI.Gtk.Structs.WidgetPath.WidgetPath'
    -> m Int32
    -- ^ __Returns:__ the number of elements in the path
widgetPathLength path = liftIO $ do
    path' <- unsafeManagedPtrGetPtr path
    result <- gtk_widget_path_length path'
    touchManagedPtr path
    return result

#if defined(ENABLE_OVERLOADING)
data WidgetPathLengthMethodInfo
instance (signature ~ (m Int32), MonadIO m) => O.OverloadedMethod WidgetPathLengthMethodInfo WidgetPath signature where
    overloadedMethod = widgetPathLength

instance O.OverloadedMethodInfo WidgetPathLengthMethodInfo WidgetPath where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetPath.widgetPathLength",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetPath.html#v:widgetPathLength"
        })


#endif

-- method WidgetPath::prepend_type
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "path"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "WidgetPath" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWidgetPath" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "type"
--           , argType = TBasicType TGType
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "widget type to prepend"
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

foreign import ccall "gtk_widget_path_prepend_type" gtk_widget_path_prepend_type :: 
    Ptr WidgetPath ->                       -- path : TInterface (Name {namespace = "Gtk", name = "WidgetPath"})
    CGType ->                               -- type : TBasicType TGType
    IO ()

-- | Prepends a widget type to the widget hierachy represented by /@path@/.
-- 
-- /Since: 3.0/
widgetPathPrependType ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    WidgetPath
    -- ^ /@path@/: a t'GI.Gtk.Structs.WidgetPath.WidgetPath'
    -> GType
    -- ^ /@type@/: widget type to prepend
    -> m ()
widgetPathPrependType path type_ = liftIO $ do
    path' <- unsafeManagedPtrGetPtr path
    let type_' = gtypeToCGType type_
    gtk_widget_path_prepend_type path' type_'
    touchManagedPtr path
    return ()

#if defined(ENABLE_OVERLOADING)
data WidgetPathPrependTypeMethodInfo
instance (signature ~ (GType -> m ()), MonadIO m) => O.OverloadedMethod WidgetPathPrependTypeMethodInfo WidgetPath signature where
    overloadedMethod = widgetPathPrependType

instance O.OverloadedMethodInfo WidgetPathPrependTypeMethodInfo WidgetPath where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetPath.widgetPathPrependType",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetPath.html#v:widgetPathPrependType"
        })


#endif

-- method WidgetPath::ref
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "path"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "WidgetPath" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWidgetPath" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "WidgetPath" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_widget_path_ref" gtk_widget_path_ref :: 
    Ptr WidgetPath ->                       -- path : TInterface (Name {namespace = "Gtk", name = "WidgetPath"})
    IO (Ptr WidgetPath)

-- | Increments the reference count on /@path@/.
-- 
-- /Since: 3.2/
widgetPathRef ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    WidgetPath
    -- ^ /@path@/: a t'GI.Gtk.Structs.WidgetPath.WidgetPath'
    -> m WidgetPath
    -- ^ __Returns:__ /@path@/ itself.
widgetPathRef path = liftIO $ do
    path' <- unsafeManagedPtrGetPtr path
    result <- gtk_widget_path_ref path'
    checkUnexpectedReturnNULL "widgetPathRef" result
    result' <- (wrapBoxed WidgetPath) result
    touchManagedPtr path
    return result'

#if defined(ENABLE_OVERLOADING)
data WidgetPathRefMethodInfo
instance (signature ~ (m WidgetPath), MonadIO m) => O.OverloadedMethod WidgetPathRefMethodInfo WidgetPath signature where
    overloadedMethod = widgetPathRef

instance O.OverloadedMethodInfo WidgetPathRefMethodInfo WidgetPath where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetPath.widgetPathRef",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetPath.html#v:widgetPathRef"
        })


#endif

-- method WidgetPath::to_string
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "path"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "WidgetPath" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the path" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TBasicType TUTF8)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_widget_path_to_string" gtk_widget_path_to_string :: 
    Ptr WidgetPath ->                       -- path : TInterface (Name {namespace = "Gtk", name = "WidgetPath"})
    IO CString

-- | Dumps the widget path into a string representation. It tries to match
-- the CSS style as closely as possible (Note that there might be paths
-- that cannot be represented in CSS).
-- 
-- The main use of this code is for debugging purposes, so that you can
-- @/g_print()/@ the path or dump it in a gdb session.
-- 
-- /Since: 3.2/
widgetPathToString ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    WidgetPath
    -- ^ /@path@/: the path
    -> m T.Text
    -- ^ __Returns:__ A new string describing /@path@/.
widgetPathToString path = liftIO $ do
    path' <- unsafeManagedPtrGetPtr path
    result <- gtk_widget_path_to_string path'
    checkUnexpectedReturnNULL "widgetPathToString" result
    result' <- cstringToText result
    freeMem result
    touchManagedPtr path
    return result'

#if defined(ENABLE_OVERLOADING)
data WidgetPathToStringMethodInfo
instance (signature ~ (m T.Text), MonadIO m) => O.OverloadedMethod WidgetPathToStringMethodInfo WidgetPath signature where
    overloadedMethod = widgetPathToString

instance O.OverloadedMethodInfo WidgetPathToStringMethodInfo WidgetPath where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetPath.widgetPathToString",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetPath.html#v:widgetPathToString"
        })


#endif

-- method WidgetPath::unref
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "path"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "WidgetPath" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWidgetPath" , sinceVersion = Nothing }
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

foreign import ccall "gtk_widget_path_unref" gtk_widget_path_unref :: 
    Ptr WidgetPath ->                       -- path : TInterface (Name {namespace = "Gtk", name = "WidgetPath"})
    IO ()

-- | Decrements the reference count on /@path@/, freeing the structure
-- if the reference count reaches 0.
-- 
-- /Since: 3.2/
widgetPathUnref ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    WidgetPath
    -- ^ /@path@/: a t'GI.Gtk.Structs.WidgetPath.WidgetPath'
    -> m ()
widgetPathUnref path = liftIO $ do
    path' <- unsafeManagedPtrGetPtr path
    gtk_widget_path_unref path'
    touchManagedPtr path
    return ()

#if defined(ENABLE_OVERLOADING)
data WidgetPathUnrefMethodInfo
instance (signature ~ (m ()), MonadIO m) => O.OverloadedMethod WidgetPathUnrefMethodInfo WidgetPath signature where
    overloadedMethod = widgetPathUnref

instance O.OverloadedMethodInfo WidgetPathUnrefMethodInfo WidgetPath where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetPath.widgetPathUnref",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetPath.html#v:widgetPathUnref"
        })


#endif

#if defined(ENABLE_OVERLOADING)
type family ResolveWidgetPathMethod (t :: Symbol) (o :: *) :: * where
    ResolveWidgetPathMethod "appendForWidget" o = WidgetPathAppendForWidgetMethodInfo
    ResolveWidgetPathMethod "appendType" o = WidgetPathAppendTypeMethodInfo
    ResolveWidgetPathMethod "appendWithSiblings" o = WidgetPathAppendWithSiblingsMethodInfo
    ResolveWidgetPathMethod "copy" o = WidgetPathCopyMethodInfo
    ResolveWidgetPathMethod "free" o = WidgetPathFreeMethodInfo
    ResolveWidgetPathMethod "hasParent" o = WidgetPathHasParentMethodInfo
    ResolveWidgetPathMethod "isType" o = WidgetPathIsTypeMethodInfo
    ResolveWidgetPathMethod "iterAddClass" o = WidgetPathIterAddClassMethodInfo
    ResolveWidgetPathMethod "iterAddRegion" o = WidgetPathIterAddRegionMethodInfo
    ResolveWidgetPathMethod "iterClearClasses" o = WidgetPathIterClearClassesMethodInfo
    ResolveWidgetPathMethod "iterClearRegions" o = WidgetPathIterClearRegionsMethodInfo
    ResolveWidgetPathMethod "iterGetName" o = WidgetPathIterGetNameMethodInfo
    ResolveWidgetPathMethod "iterGetObjectName" o = WidgetPathIterGetObjectNameMethodInfo
    ResolveWidgetPathMethod "iterGetObjectType" o = WidgetPathIterGetObjectTypeMethodInfo
    ResolveWidgetPathMethod "iterGetSiblingIndex" o = WidgetPathIterGetSiblingIndexMethodInfo
    ResolveWidgetPathMethod "iterGetSiblings" o = WidgetPathIterGetSiblingsMethodInfo
    ResolveWidgetPathMethod "iterGetState" o = WidgetPathIterGetStateMethodInfo
    ResolveWidgetPathMethod "iterHasClass" o = WidgetPathIterHasClassMethodInfo
    ResolveWidgetPathMethod "iterHasName" o = WidgetPathIterHasNameMethodInfo
    ResolveWidgetPathMethod "iterHasQclass" o = WidgetPathIterHasQclassMethodInfo
    ResolveWidgetPathMethod "iterHasQname" o = WidgetPathIterHasQnameMethodInfo
    ResolveWidgetPathMethod "iterHasQregion" o = WidgetPathIterHasQregionMethodInfo
    ResolveWidgetPathMethod "iterHasRegion" o = WidgetPathIterHasRegionMethodInfo
    ResolveWidgetPathMethod "iterListClasses" o = WidgetPathIterListClassesMethodInfo
    ResolveWidgetPathMethod "iterListRegions" o = WidgetPathIterListRegionsMethodInfo
    ResolveWidgetPathMethod "iterRemoveClass" o = WidgetPathIterRemoveClassMethodInfo
    ResolveWidgetPathMethod "iterRemoveRegion" o = WidgetPathIterRemoveRegionMethodInfo
    ResolveWidgetPathMethod "iterSetName" o = WidgetPathIterSetNameMethodInfo
    ResolveWidgetPathMethod "iterSetObjectName" o = WidgetPathIterSetObjectNameMethodInfo
    ResolveWidgetPathMethod "iterSetObjectType" o = WidgetPathIterSetObjectTypeMethodInfo
    ResolveWidgetPathMethod "iterSetState" o = WidgetPathIterSetStateMethodInfo
    ResolveWidgetPathMethod "length" o = WidgetPathLengthMethodInfo
    ResolveWidgetPathMethod "prependType" o = WidgetPathPrependTypeMethodInfo
    ResolveWidgetPathMethod "ref" o = WidgetPathRefMethodInfo
    ResolveWidgetPathMethod "toString" o = WidgetPathToStringMethodInfo
    ResolveWidgetPathMethod "unref" o = WidgetPathUnrefMethodInfo
    ResolveWidgetPathMethod "getObjectType" o = WidgetPathGetObjectTypeMethodInfo
    ResolveWidgetPathMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveWidgetPathMethod t WidgetPath, O.OverloadedMethod info WidgetPath p) => OL.IsLabel t (WidgetPath -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveWidgetPathMethod t WidgetPath, O.OverloadedMethod info WidgetPath p, R.HasField t WidgetPath p) => R.HasField t WidgetPath p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveWidgetPathMethod t WidgetPath, O.OverloadedMethodInfo info WidgetPath) => OL.IsLabel t (O.MethodProxy info WidgetPath) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif


