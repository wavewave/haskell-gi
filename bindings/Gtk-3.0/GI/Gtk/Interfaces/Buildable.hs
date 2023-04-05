{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- GtkBuildable allows objects to extend and customize their deserialization
-- from [GtkBuilder UI descriptions][BUILDER-UI].
-- The interface includes methods for setting names and properties of objects,
-- parsing custom tags and constructing child objects.
-- 
-- The GtkBuildable interface is implemented by all widgets and
-- many of the non-widget objects that are provided by GTK+. The
-- main user of this interface is t'GI.Gtk.Objects.Builder.Builder'. There should be
-- very little need for applications to call any of these functions directly.
-- 
-- An object only needs to implement this interface if it needs to extend the
-- t'GI.Gtk.Objects.Builder.Builder' format or run any extra routines at deserialization time.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Interfaces.Buildable
    ( 

-- * Exported types
    Buildable(..)                           ,
    IsBuildable                             ,
    toBuildable                             ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [addChild]("GI.Gtk.Interfaces.Buildable#g:method:addChild"), [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [constructChild]("GI.Gtk.Interfaces.Buildable#g:method:constructChild"), [customFinished]("GI.Gtk.Interfaces.Buildable#g:method:customFinished"), [customTagEnd]("GI.Gtk.Interfaces.Buildable#g:method:customTagEnd"), [customTagStart]("GI.Gtk.Interfaces.Buildable#g:method:customTagStart"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [parserFinished]("GI.Gtk.Interfaces.Buildable#g:method:parserFinished"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getData]("GI.GObject.Objects.Object#g:method:getData"), [getInternalChild]("GI.Gtk.Interfaces.Buildable#g:method:getInternalChild"), [getName]("GI.Gtk.Interfaces.Buildable#g:method:getName"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata").
-- 
-- ==== Setters
-- [setBuildableProperty]("GI.Gtk.Interfaces.Buildable#g:method:setBuildableProperty"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setName]("GI.Gtk.Interfaces.Buildable#g:method:setName"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty").

#if defined(ENABLE_OVERLOADING)
    ResolveBuildableMethod                  ,
#endif

-- ** addChild #method:addChild#

#if defined(ENABLE_OVERLOADING)
    BuildableAddChildMethodInfo             ,
#endif
    buildableAddChild                       ,


-- ** constructChild #method:constructChild#

#if defined(ENABLE_OVERLOADING)
    BuildableConstructChildMethodInfo       ,
#endif
    buildableConstructChild                 ,


-- ** customFinished #method:customFinished#

#if defined(ENABLE_OVERLOADING)
    BuildableCustomFinishedMethodInfo       ,
#endif
    buildableCustomFinished                 ,


-- ** customTagEnd #method:customTagEnd#

#if defined(ENABLE_OVERLOADING)
    BuildableCustomTagEndMethodInfo         ,
#endif
    buildableCustomTagEnd                   ,


-- ** customTagStart #method:customTagStart#

#if defined(ENABLE_OVERLOADING)
    BuildableCustomTagStartMethodInfo       ,
#endif
    buildableCustomTagStart                 ,


-- ** getInternalChild #method:getInternalChild#

#if defined(ENABLE_OVERLOADING)
    BuildableGetInternalChildMethodInfo     ,
#endif
    buildableGetInternalChild               ,


-- ** getName #method:getName#

#if defined(ENABLE_OVERLOADING)
    BuildableGetNameMethodInfo              ,
#endif
    buildableGetName                        ,


-- ** parserFinished #method:parserFinished#

#if defined(ENABLE_OVERLOADING)
    BuildableParserFinishedMethodInfo       ,
#endif
    buildableParserFinished                 ,


-- ** setBuildableProperty #method:setBuildableProperty#

#if defined(ENABLE_OVERLOADING)
    BuildableSetBuildablePropertyMethodInfo ,
#endif
    buildableSetBuildableProperty           ,


-- ** setName #method:setName#

#if defined(ENABLE_OVERLOADING)
    BuildableSetNameMethodInfo              ,
#endif
    buildableSetName                        ,




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

import qualified GI.GLib.Structs.MarkupParser as GLib.MarkupParser
import qualified GI.GObject.Objects.Object as GObject.Object
import {-# SOURCE #-} qualified GI.Gtk.Objects.Builder as Gtk.Builder

-- interface Buildable 
-- | Memory-managed wrapper type.
newtype Buildable = Buildable (SP.ManagedPtr Buildable)
    deriving (Eq)

instance SP.ManagedPtrNewtype Buildable where
    toManagedPtr (Buildable p) = p

foreign import ccall "gtk_buildable_get_type"
    c_gtk_buildable_get_type :: IO B.Types.GType

instance B.Types.TypedObject Buildable where
    glibType = c_gtk_buildable_get_type

instance B.Types.GObject Buildable

-- | Type class for types which can be safely cast to `Buildable`, for instance with `toBuildable`.
class (SP.GObject o, O.IsDescendantOf Buildable o) => IsBuildable o
instance (SP.GObject o, O.IsDescendantOf Buildable o) => IsBuildable o

instance O.HasParentTypes Buildable
type instance O.ParentTypes Buildable = '[GObject.Object.Object]

-- | Cast to `Buildable`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toBuildable :: (MIO.MonadIO m, IsBuildable o) => o -> m Buildable
toBuildable = MIO.liftIO . B.ManagedPtr.unsafeCastTo Buildable

-- | Convert 'Buildable' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe Buildable) where
    gvalueGType_ = c_gtk_buildable_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr Buildable)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr Buildable)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject Buildable ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList Buildable
type instance O.AttributeList Buildable = BuildableAttributeList
type BuildableAttributeList = ('[ ] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
#endif

#if defined(ENABLE_OVERLOADING)
type family ResolveBuildableMethod (t :: Symbol) (o :: *) :: * where
    ResolveBuildableMethod "addChild" o = BuildableAddChildMethodInfo
    ResolveBuildableMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveBuildableMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveBuildableMethod "constructChild" o = BuildableConstructChildMethodInfo
    ResolveBuildableMethod "customFinished" o = BuildableCustomFinishedMethodInfo
    ResolveBuildableMethod "customTagEnd" o = BuildableCustomTagEndMethodInfo
    ResolveBuildableMethod "customTagStart" o = BuildableCustomTagStartMethodInfo
    ResolveBuildableMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveBuildableMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveBuildableMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveBuildableMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveBuildableMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveBuildableMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveBuildableMethod "parserFinished" o = BuildableParserFinishedMethodInfo
    ResolveBuildableMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveBuildableMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveBuildableMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveBuildableMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveBuildableMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveBuildableMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveBuildableMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveBuildableMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveBuildableMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveBuildableMethod "getInternalChild" o = BuildableGetInternalChildMethodInfo
    ResolveBuildableMethod "getName" o = BuildableGetNameMethodInfo
    ResolveBuildableMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveBuildableMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveBuildableMethod "setBuildableProperty" o = BuildableSetBuildablePropertyMethodInfo
    ResolveBuildableMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveBuildableMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveBuildableMethod "setName" o = BuildableSetNameMethodInfo
    ResolveBuildableMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveBuildableMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveBuildableMethod t Buildable, O.OverloadedMethod info Buildable p) => OL.IsLabel t (Buildable -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveBuildableMethod t Buildable, O.OverloadedMethod info Buildable p, R.HasField t Buildable p) => R.HasField t Buildable p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveBuildableMethod t Buildable, O.OverloadedMethodInfo info Buildable) => OL.IsLabel t (O.MethodProxy info Buildable) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- method Buildable::add_child
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "buildable"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Buildable" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkBuildable" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "builder"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Builder" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkBuilder" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "child"
--           , argType =
--               TInterface Name { namespace = "GObject" , name = "Object" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "child to add" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "type"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "kind of child or %NULL"
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

foreign import ccall "gtk_buildable_add_child" gtk_buildable_add_child :: 
    Ptr Buildable ->                        -- buildable : TInterface (Name {namespace = "Gtk", name = "Buildable"})
    Ptr Gtk.Builder.Builder ->              -- builder : TInterface (Name {namespace = "Gtk", name = "Builder"})
    Ptr GObject.Object.Object ->            -- child : TInterface (Name {namespace = "GObject", name = "Object"})
    CString ->                              -- type : TBasicType TUTF8
    IO ()

-- | Adds a child to /@buildable@/. /@type@/ is an optional string
-- describing how the child should be added.
-- 
-- /Since: 2.12/
buildableAddChild ::
    (B.CallStack.HasCallStack, MonadIO m, IsBuildable a, Gtk.Builder.IsBuilder b, GObject.Object.IsObject c) =>
    a
    -- ^ /@buildable@/: a t'GI.Gtk.Interfaces.Buildable.Buildable'
    -> b
    -- ^ /@builder@/: a t'GI.Gtk.Objects.Builder.Builder'
    -> c
    -- ^ /@child@/: child to add
    -> Maybe (T.Text)
    -- ^ /@type@/: kind of child or 'P.Nothing'
    -> m ()
buildableAddChild buildable builder child type_ = liftIO $ do
    buildable' <- unsafeManagedPtrCastPtr buildable
    builder' <- unsafeManagedPtrCastPtr builder
    child' <- unsafeManagedPtrCastPtr child
    maybeType_ <- case type_ of
        Nothing -> return nullPtr
        Just jType_ -> do
            jType_' <- textToCString jType_
            return jType_'
    gtk_buildable_add_child buildable' builder' child' maybeType_
    touchManagedPtr buildable
    touchManagedPtr builder
    touchManagedPtr child
    freeMem maybeType_
    return ()

#if defined(ENABLE_OVERLOADING)
data BuildableAddChildMethodInfo
instance (signature ~ (b -> c -> Maybe (T.Text) -> m ()), MonadIO m, IsBuildable a, Gtk.Builder.IsBuilder b, GObject.Object.IsObject c) => O.OverloadedMethod BuildableAddChildMethodInfo a signature where
    overloadedMethod = buildableAddChild

instance O.OverloadedMethodInfo BuildableAddChildMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.Buildable.buildableAddChild",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-Buildable.html#v:buildableAddChild"
        })


#endif

-- method Buildable::construct_child
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "buildable"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Buildable" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkBuildable" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "builder"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Builder" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "#GtkBuilder used to construct this object"
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
--                 { rawDocText = Just "name of child to construct"
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
-- returnType: Just (TInterface Name { namespace = "GObject" , name = "Object" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_buildable_construct_child" gtk_buildable_construct_child :: 
    Ptr Buildable ->                        -- buildable : TInterface (Name {namespace = "Gtk", name = "Buildable"})
    Ptr Gtk.Builder.Builder ->              -- builder : TInterface (Name {namespace = "Gtk", name = "Builder"})
    CString ->                              -- name : TBasicType TUTF8
    IO (Ptr GObject.Object.Object)

-- | Constructs a child of /@buildable@/ with the name /@name@/.
-- 
-- t'GI.Gtk.Objects.Builder.Builder' calls this function if a “constructor” has been
-- specified in the UI definition.
-- 
-- /Since: 2.12/
buildableConstructChild ::
    (B.CallStack.HasCallStack, MonadIO m, IsBuildable a, Gtk.Builder.IsBuilder b) =>
    a
    -- ^ /@buildable@/: A t'GI.Gtk.Interfaces.Buildable.Buildable'
    -> b
    -- ^ /@builder@/: t'GI.Gtk.Objects.Builder.Builder' used to construct this object
    -> T.Text
    -- ^ /@name@/: name of child to construct
    -> m GObject.Object.Object
    -- ^ __Returns:__ the constructed child
buildableConstructChild buildable builder name = liftIO $ do
    buildable' <- unsafeManagedPtrCastPtr buildable
    builder' <- unsafeManagedPtrCastPtr builder
    name' <- textToCString name
    result <- gtk_buildable_construct_child buildable' builder' name'
    checkUnexpectedReturnNULL "buildableConstructChild" result
    result' <- (wrapObject GObject.Object.Object) result
    touchManagedPtr buildable
    touchManagedPtr builder
    freeMem name'
    return result'

#if defined(ENABLE_OVERLOADING)
data BuildableConstructChildMethodInfo
instance (signature ~ (b -> T.Text -> m GObject.Object.Object), MonadIO m, IsBuildable a, Gtk.Builder.IsBuilder b) => O.OverloadedMethod BuildableConstructChildMethodInfo a signature where
    overloadedMethod = buildableConstructChild

instance O.OverloadedMethodInfo BuildableConstructChildMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.Buildable.buildableConstructChild",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-Buildable.html#v:buildableConstructChild"
        })


#endif

-- method Buildable::custom_finished
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "buildable"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Buildable" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkBuildable" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "builder"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Builder" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkBuilder" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "child"
--           , argType =
--               TInterface Name { namespace = "GObject" , name = "Object" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "child object or %NULL for non-child tags"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "tagname"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the name of the tag"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
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
--                 { rawDocText = Just "user data created in custom_tag_start"
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

foreign import ccall "gtk_buildable_custom_finished" gtk_buildable_custom_finished :: 
    Ptr Buildable ->                        -- buildable : TInterface (Name {namespace = "Gtk", name = "Buildable"})
    Ptr Gtk.Builder.Builder ->              -- builder : TInterface (Name {namespace = "Gtk", name = "Builder"})
    Ptr GObject.Object.Object ->            -- child : TInterface (Name {namespace = "GObject", name = "Object"})
    CString ->                              -- tagname : TBasicType TUTF8
    Ptr () ->                               -- data : TBasicType TPtr
    IO ()

-- | This is similar to 'GI.Gtk.Interfaces.Buildable.buildableParserFinished' but is
-- called once for each custom tag handled by the /@buildable@/.
-- 
-- /Since: 2.12/
buildableCustomFinished ::
    (B.CallStack.HasCallStack, MonadIO m, IsBuildable a, Gtk.Builder.IsBuilder b, GObject.Object.IsObject c) =>
    a
    -- ^ /@buildable@/: a t'GI.Gtk.Interfaces.Buildable.Buildable'
    -> b
    -- ^ /@builder@/: a t'GI.Gtk.Objects.Builder.Builder'
    -> Maybe (c)
    -- ^ /@child@/: child object or 'P.Nothing' for non-child tags
    -> T.Text
    -- ^ /@tagname@/: the name of the tag
    -> Ptr ()
    -- ^ /@data@/: user data created in custom_tag_start
    -> m ()
buildableCustomFinished buildable builder child tagname data_ = liftIO $ do
    buildable' <- unsafeManagedPtrCastPtr buildable
    builder' <- unsafeManagedPtrCastPtr builder
    maybeChild <- case child of
        Nothing -> return nullPtr
        Just jChild -> do
            jChild' <- unsafeManagedPtrCastPtr jChild
            return jChild'
    tagname' <- textToCString tagname
    gtk_buildable_custom_finished buildable' builder' maybeChild tagname' data_
    touchManagedPtr buildable
    touchManagedPtr builder
    whenJust child touchManagedPtr
    freeMem tagname'
    return ()

#if defined(ENABLE_OVERLOADING)
data BuildableCustomFinishedMethodInfo
instance (signature ~ (b -> Maybe (c) -> T.Text -> Ptr () -> m ()), MonadIO m, IsBuildable a, Gtk.Builder.IsBuilder b, GObject.Object.IsObject c) => O.OverloadedMethod BuildableCustomFinishedMethodInfo a signature where
    overloadedMethod = buildableCustomFinished

instance O.OverloadedMethodInfo BuildableCustomFinishedMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.Buildable.buildableCustomFinished",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-Buildable.html#v:buildableCustomFinished"
        })


#endif

-- method Buildable::custom_tag_end
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "buildable"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Buildable" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkBuildable" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "builder"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Builder" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "#GtkBuilder used to construct this object"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "child"
--           , argType =
--               TInterface Name { namespace = "GObject" , name = "Object" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "child object or %NULL for non-child tags"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "tagname"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "name of tag" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
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
--                 { rawDocText =
--                     Just "user data that will be passed in to parser functions"
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

foreign import ccall "gtk_buildable_custom_tag_end" gtk_buildable_custom_tag_end :: 
    Ptr Buildable ->                        -- buildable : TInterface (Name {namespace = "Gtk", name = "Buildable"})
    Ptr Gtk.Builder.Builder ->              -- builder : TInterface (Name {namespace = "Gtk", name = "Builder"})
    Ptr GObject.Object.Object ->            -- child : TInterface (Name {namespace = "GObject", name = "Object"})
    CString ->                              -- tagname : TBasicType TUTF8
    Ptr () ->                               -- data : TBasicType TPtr
    IO ()

-- | This is called at the end of each custom element handled by
-- the buildable.
-- 
-- /Since: 2.12/
buildableCustomTagEnd ::
    (B.CallStack.HasCallStack, MonadIO m, IsBuildable a, Gtk.Builder.IsBuilder b, GObject.Object.IsObject c) =>
    a
    -- ^ /@buildable@/: A t'GI.Gtk.Interfaces.Buildable.Buildable'
    -> b
    -- ^ /@builder@/: t'GI.Gtk.Objects.Builder.Builder' used to construct this object
    -> Maybe (c)
    -- ^ /@child@/: child object or 'P.Nothing' for non-child tags
    -> T.Text
    -- ^ /@tagname@/: name of tag
    -> Ptr ()
    -- ^ /@data@/: user data that will be passed in to parser functions
    -> m ()
buildableCustomTagEnd buildable builder child tagname data_ = liftIO $ do
    buildable' <- unsafeManagedPtrCastPtr buildable
    builder' <- unsafeManagedPtrCastPtr builder
    maybeChild <- case child of
        Nothing -> return nullPtr
        Just jChild -> do
            jChild' <- unsafeManagedPtrCastPtr jChild
            return jChild'
    tagname' <- textToCString tagname
    gtk_buildable_custom_tag_end buildable' builder' maybeChild tagname' data_
    touchManagedPtr buildable
    touchManagedPtr builder
    whenJust child touchManagedPtr
    freeMem tagname'
    return ()

#if defined(ENABLE_OVERLOADING)
data BuildableCustomTagEndMethodInfo
instance (signature ~ (b -> Maybe (c) -> T.Text -> Ptr () -> m ()), MonadIO m, IsBuildable a, Gtk.Builder.IsBuilder b, GObject.Object.IsObject c) => O.OverloadedMethod BuildableCustomTagEndMethodInfo a signature where
    overloadedMethod = buildableCustomTagEnd

instance O.OverloadedMethodInfo BuildableCustomTagEndMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.Buildable.buildableCustomTagEnd",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-Buildable.html#v:buildableCustomTagEnd"
        })


#endif

-- method Buildable::custom_tag_start
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "buildable"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Buildable" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkBuildable" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "builder"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Builder" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkBuilder used to construct this object"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "child"
--           , argType =
--               TInterface Name { namespace = "GObject" , name = "Object" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "child object or %NULL for non-child tags"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "tagname"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "name of tag" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "parser"
--           , argType =
--               TInterface Name { namespace = "GLib" , name = "MarkupParser" }
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GMarkupParser to fill in"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = True
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "data"
--           , argType = TBasicType TPtr
--           , direction = DirectionOut
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "return location for user data that will be passed in\n  to parser functions"
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

foreign import ccall "gtk_buildable_custom_tag_start" gtk_buildable_custom_tag_start :: 
    Ptr Buildable ->                        -- buildable : TInterface (Name {namespace = "Gtk", name = "Buildable"})
    Ptr Gtk.Builder.Builder ->              -- builder : TInterface (Name {namespace = "Gtk", name = "Builder"})
    Ptr GObject.Object.Object ->            -- child : TInterface (Name {namespace = "GObject", name = "Object"})
    CString ->                              -- tagname : TBasicType TUTF8
    Ptr GLib.MarkupParser.MarkupParser ->   -- parser : TInterface (Name {namespace = "GLib", name = "MarkupParser"})
    Ptr (Ptr ()) ->                         -- data : TBasicType TPtr
    IO CInt

-- | This is called for each unknown element under @\<child>@.
-- 
-- /Since: 2.12/
buildableCustomTagStart ::
    (B.CallStack.HasCallStack, MonadIO m, IsBuildable a, Gtk.Builder.IsBuilder b, GObject.Object.IsObject c) =>
    a
    -- ^ /@buildable@/: a t'GI.Gtk.Interfaces.Buildable.Buildable'
    -> b
    -- ^ /@builder@/: a t'GI.Gtk.Objects.Builder.Builder' used to construct this object
    -> Maybe (c)
    -- ^ /@child@/: child object or 'P.Nothing' for non-child tags
    -> T.Text
    -- ^ /@tagname@/: name of tag
    -> m ((Bool, GLib.MarkupParser.MarkupParser, Ptr ()))
    -- ^ __Returns:__ 'P.True' if a object has a custom implementation, 'P.False'
    --          if it doesn\'t.
buildableCustomTagStart buildable builder child tagname = liftIO $ do
    buildable' <- unsafeManagedPtrCastPtr buildable
    builder' <- unsafeManagedPtrCastPtr builder
    maybeChild <- case child of
        Nothing -> return nullPtr
        Just jChild -> do
            jChild' <- unsafeManagedPtrCastPtr jChild
            return jChild'
    tagname' <- textToCString tagname
    parser <- SP.callocBytes 40 :: IO (Ptr GLib.MarkupParser.MarkupParser)
    data_ <- callocMem :: IO (Ptr (Ptr ()))
    result <- gtk_buildable_custom_tag_start buildable' builder' maybeChild tagname' parser data_
    let result' = (/= 0) result
    parser' <- (wrapPtr GLib.MarkupParser.MarkupParser) parser
    data_' <- peek data_
    touchManagedPtr buildable
    touchManagedPtr builder
    whenJust child touchManagedPtr
    freeMem tagname'
    freeMem data_
    return (result', parser', data_')

#if defined(ENABLE_OVERLOADING)
data BuildableCustomTagStartMethodInfo
instance (signature ~ (b -> Maybe (c) -> T.Text -> m ((Bool, GLib.MarkupParser.MarkupParser, Ptr ()))), MonadIO m, IsBuildable a, Gtk.Builder.IsBuilder b, GObject.Object.IsObject c) => O.OverloadedMethod BuildableCustomTagStartMethodInfo a signature where
    overloadedMethod = buildableCustomTagStart

instance O.OverloadedMethodInfo BuildableCustomTagStartMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.Buildable.buildableCustomTagStart",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-Buildable.html#v:buildableCustomTagStart"
        })


#endif

-- method Buildable::get_internal_child
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "buildable"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Buildable" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkBuildable" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "builder"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Builder" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkBuilder" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "childname"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "name of child" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "GObject" , name = "Object" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_buildable_get_internal_child" gtk_buildable_get_internal_child :: 
    Ptr Buildable ->                        -- buildable : TInterface (Name {namespace = "Gtk", name = "Buildable"})
    Ptr Gtk.Builder.Builder ->              -- builder : TInterface (Name {namespace = "Gtk", name = "Builder"})
    CString ->                              -- childname : TBasicType TUTF8
    IO (Ptr GObject.Object.Object)

-- | Get the internal child called /@childname@/ of the /@buildable@/ object.
-- 
-- /Since: 2.12/
buildableGetInternalChild ::
    (B.CallStack.HasCallStack, MonadIO m, IsBuildable a, Gtk.Builder.IsBuilder b) =>
    a
    -- ^ /@buildable@/: a t'GI.Gtk.Interfaces.Buildable.Buildable'
    -> b
    -- ^ /@builder@/: a t'GI.Gtk.Objects.Builder.Builder'
    -> T.Text
    -- ^ /@childname@/: name of child
    -> m GObject.Object.Object
    -- ^ __Returns:__ the internal child of the buildable object
buildableGetInternalChild buildable builder childname = liftIO $ do
    buildable' <- unsafeManagedPtrCastPtr buildable
    builder' <- unsafeManagedPtrCastPtr builder
    childname' <- textToCString childname
    result <- gtk_buildable_get_internal_child buildable' builder' childname'
    checkUnexpectedReturnNULL "buildableGetInternalChild" result
    result' <- (newObject GObject.Object.Object) result
    touchManagedPtr buildable
    touchManagedPtr builder
    freeMem childname'
    return result'

#if defined(ENABLE_OVERLOADING)
data BuildableGetInternalChildMethodInfo
instance (signature ~ (b -> T.Text -> m GObject.Object.Object), MonadIO m, IsBuildable a, Gtk.Builder.IsBuilder b) => O.OverloadedMethod BuildableGetInternalChildMethodInfo a signature where
    overloadedMethod = buildableGetInternalChild

instance O.OverloadedMethodInfo BuildableGetInternalChildMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.Buildable.buildableGetInternalChild",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-Buildable.html#v:buildableGetInternalChild"
        })


#endif

-- method Buildable::get_name
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "buildable"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Buildable" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkBuildable" , sinceVersion = Nothing }
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

foreign import ccall "gtk_buildable_get_name" gtk_buildable_get_name :: 
    Ptr Buildable ->                        -- buildable : TInterface (Name {namespace = "Gtk", name = "Buildable"})
    IO CString

-- | Gets the name of the /@buildable@/ object.
-- 
-- t'GI.Gtk.Objects.Builder.Builder' sets the name based on the
-- [GtkBuilder UI definition][BUILDER-UI]
-- used to construct the /@buildable@/.
-- 
-- /Since: 2.12/
buildableGetName ::
    (B.CallStack.HasCallStack, MonadIO m, IsBuildable a) =>
    a
    -- ^ /@buildable@/: a t'GI.Gtk.Interfaces.Buildable.Buildable'
    -> m T.Text
    -- ^ __Returns:__ the name set with 'GI.Gtk.Interfaces.Buildable.buildableSetName'
buildableGetName buildable = liftIO $ do
    buildable' <- unsafeManagedPtrCastPtr buildable
    result <- gtk_buildable_get_name buildable'
    checkUnexpectedReturnNULL "buildableGetName" result
    result' <- cstringToText result
    touchManagedPtr buildable
    return result'

#if defined(ENABLE_OVERLOADING)
data BuildableGetNameMethodInfo
instance (signature ~ (m T.Text), MonadIO m, IsBuildable a) => O.OverloadedMethod BuildableGetNameMethodInfo a signature where
    overloadedMethod = buildableGetName

instance O.OverloadedMethodInfo BuildableGetNameMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.Buildable.buildableGetName",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-Buildable.html#v:buildableGetName"
        })


#endif

-- method Buildable::parser_finished
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "buildable"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Buildable" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkBuildable" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "builder"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Builder" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkBuilder" , sinceVersion = Nothing }
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

foreign import ccall "gtk_buildable_parser_finished" gtk_buildable_parser_finished :: 
    Ptr Buildable ->                        -- buildable : TInterface (Name {namespace = "Gtk", name = "Buildable"})
    Ptr Gtk.Builder.Builder ->              -- builder : TInterface (Name {namespace = "Gtk", name = "Builder"})
    IO ()

-- | Called when the builder finishes the parsing of a
-- [GtkBuilder UI definition][BUILDER-UI].
-- Note that this will be called once for each time
-- 'GI.Gtk.Objects.Builder.builderAddFromFile' or 'GI.Gtk.Objects.Builder.builderAddFromString'
-- is called on a builder.
-- 
-- /Since: 2.12/
buildableParserFinished ::
    (B.CallStack.HasCallStack, MonadIO m, IsBuildable a, Gtk.Builder.IsBuilder b) =>
    a
    -- ^ /@buildable@/: a t'GI.Gtk.Interfaces.Buildable.Buildable'
    -> b
    -- ^ /@builder@/: a t'GI.Gtk.Objects.Builder.Builder'
    -> m ()
buildableParserFinished buildable builder = liftIO $ do
    buildable' <- unsafeManagedPtrCastPtr buildable
    builder' <- unsafeManagedPtrCastPtr builder
    gtk_buildable_parser_finished buildable' builder'
    touchManagedPtr buildable
    touchManagedPtr builder
    return ()

#if defined(ENABLE_OVERLOADING)
data BuildableParserFinishedMethodInfo
instance (signature ~ (b -> m ()), MonadIO m, IsBuildable a, Gtk.Builder.IsBuilder b) => O.OverloadedMethod BuildableParserFinishedMethodInfo a signature where
    overloadedMethod = buildableParserFinished

instance O.OverloadedMethodInfo BuildableParserFinishedMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.Buildable.buildableParserFinished",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-Buildable.html#v:buildableParserFinished"
        })


#endif

-- method Buildable::set_buildable_property
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "buildable"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Buildable" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkBuildable" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "builder"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Builder" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkBuilder" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "name of property" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "value"
--           , argType = TGValue
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "value of property" , sinceVersion = Nothing }
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

foreign import ccall "gtk_buildable_set_buildable_property" gtk_buildable_set_buildable_property :: 
    Ptr Buildable ->                        -- buildable : TInterface (Name {namespace = "Gtk", name = "Buildable"})
    Ptr Gtk.Builder.Builder ->              -- builder : TInterface (Name {namespace = "Gtk", name = "Builder"})
    CString ->                              -- name : TBasicType TUTF8
    Ptr GValue ->                           -- value : TGValue
    IO ()

-- | Sets the property name /@name@/ to /@value@/ on the /@buildable@/ object.
-- 
-- /Since: 2.12/
buildableSetBuildableProperty ::
    (B.CallStack.HasCallStack, MonadIO m, IsBuildable a, Gtk.Builder.IsBuilder b) =>
    a
    -- ^ /@buildable@/: a t'GI.Gtk.Interfaces.Buildable.Buildable'
    -> b
    -- ^ /@builder@/: a t'GI.Gtk.Objects.Builder.Builder'
    -> T.Text
    -- ^ /@name@/: name of property
    -> GValue
    -- ^ /@value@/: value of property
    -> m ()
buildableSetBuildableProperty buildable builder name value = liftIO $ do
    buildable' <- unsafeManagedPtrCastPtr buildable
    builder' <- unsafeManagedPtrCastPtr builder
    name' <- textToCString name
    value' <- unsafeManagedPtrGetPtr value
    gtk_buildable_set_buildable_property buildable' builder' name' value'
    touchManagedPtr buildable
    touchManagedPtr builder
    touchManagedPtr value
    freeMem name'
    return ()

#if defined(ENABLE_OVERLOADING)
data BuildableSetBuildablePropertyMethodInfo
instance (signature ~ (b -> T.Text -> GValue -> m ()), MonadIO m, IsBuildable a, Gtk.Builder.IsBuilder b) => O.OverloadedMethod BuildableSetBuildablePropertyMethodInfo a signature where
    overloadedMethod = buildableSetBuildableProperty

instance O.OverloadedMethodInfo BuildableSetBuildablePropertyMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.Buildable.buildableSetBuildableProperty",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-Buildable.html#v:buildableSetBuildableProperty"
        })


#endif

-- method Buildable::set_name
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "buildable"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Buildable" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkBuildable" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "name to set" , sinceVersion = Nothing }
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

foreign import ccall "gtk_buildable_set_name" gtk_buildable_set_name :: 
    Ptr Buildable ->                        -- buildable : TInterface (Name {namespace = "Gtk", name = "Buildable"})
    CString ->                              -- name : TBasicType TUTF8
    IO ()

-- | Sets the name of the /@buildable@/ object.
-- 
-- /Since: 2.12/
buildableSetName ::
    (B.CallStack.HasCallStack, MonadIO m, IsBuildable a) =>
    a
    -- ^ /@buildable@/: a t'GI.Gtk.Interfaces.Buildable.Buildable'
    -> T.Text
    -- ^ /@name@/: name to set
    -> m ()
buildableSetName buildable name = liftIO $ do
    buildable' <- unsafeManagedPtrCastPtr buildable
    name' <- textToCString name
    gtk_buildable_set_name buildable' name'
    touchManagedPtr buildable
    freeMem name'
    return ()

#if defined(ENABLE_OVERLOADING)
data BuildableSetNameMethodInfo
instance (signature ~ (T.Text -> m ()), MonadIO m, IsBuildable a) => O.OverloadedMethod BuildableSetNameMethodInfo a signature where
    overloadedMethod = buildableSetName

instance O.OverloadedMethodInfo BuildableSetNameMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.Buildable.buildableSetName",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-Buildable.html#v:buildableSetName"
        })


#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList Buildable = BuildableSignalList
type BuildableSignalList = ('[ '("notify", GObject.Object.ObjectNotifySignalInfo)] :: [(Symbol, *)])

#endif


