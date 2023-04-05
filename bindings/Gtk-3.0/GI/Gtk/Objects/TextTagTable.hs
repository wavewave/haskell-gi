{-# LANGUAGE ImplicitParams, RankNTypes, TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- You may wish to begin by reading the
-- [text widget conceptual overview][TextWidget]
-- which gives an overview of all the objects and
-- data types related to the text widget and how they work together.
-- 
-- = GtkTextTagTables as GtkBuildable
-- 
-- The GtkTextTagTable implementation of the GtkBuildable interface
-- supports adding tags by specifying “tag” as the “type” attribute
-- of a @\<child>@ element.
-- 
-- An example of a UI definition fragment specifying tags:
-- 
-- 
-- === /xml code/
-- >
-- ><object class="GtkTextTagTable">
-- > <child type="tag">
-- >   <object class="GtkTextTag"/>
-- > </child>
-- ></object>
-- 

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.TextTagTable
    ( 

-- * Exported types
    TextTagTable(..)                        ,
    IsTextTagTable                          ,
    toTextTagTable                          ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [add]("GI.Gtk.Objects.TextTagTable#g:method:add"), [addChild]("GI.Gtk.Interfaces.Buildable#g:method:addChild"), [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [constructChild]("GI.Gtk.Interfaces.Buildable#g:method:constructChild"), [customFinished]("GI.Gtk.Interfaces.Buildable#g:method:customFinished"), [customTagEnd]("GI.Gtk.Interfaces.Buildable#g:method:customTagEnd"), [customTagStart]("GI.Gtk.Interfaces.Buildable#g:method:customTagStart"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [foreach]("GI.Gtk.Objects.TextTagTable#g:method:foreach"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [lookup]("GI.Gtk.Objects.TextTagTable#g:method:lookup"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [parserFinished]("GI.Gtk.Interfaces.Buildable#g:method:parserFinished"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [remove]("GI.Gtk.Objects.TextTagTable#g:method:remove"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getData]("GI.GObject.Objects.Object#g:method:getData"), [getInternalChild]("GI.Gtk.Interfaces.Buildable#g:method:getInternalChild"), [getName]("GI.Gtk.Interfaces.Buildable#g:method:getName"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getSize]("GI.Gtk.Objects.TextTagTable#g:method:getSize").
-- 
-- ==== Setters
-- [setBuildableProperty]("GI.Gtk.Interfaces.Buildable#g:method:setBuildableProperty"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setName]("GI.Gtk.Interfaces.Buildable#g:method:setName"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty").

#if defined(ENABLE_OVERLOADING)
    ResolveTextTagTableMethod               ,
#endif

-- ** add #method:add#

#if defined(ENABLE_OVERLOADING)
    TextTagTableAddMethodInfo               ,
#endif
    textTagTableAdd                         ,


-- ** foreach #method:foreach#

#if defined(ENABLE_OVERLOADING)
    TextTagTableForeachMethodInfo           ,
#endif
    textTagTableForeach                     ,


-- ** getSize #method:getSize#

#if defined(ENABLE_OVERLOADING)
    TextTagTableGetSizeMethodInfo           ,
#endif
    textTagTableGetSize                     ,


-- ** lookup #method:lookup#

#if defined(ENABLE_OVERLOADING)
    TextTagTableLookupMethodInfo            ,
#endif
    textTagTableLookup                      ,


-- ** new #method:new#

    textTagTableNew                         ,


-- ** remove #method:remove#

#if defined(ENABLE_OVERLOADING)
    TextTagTableRemoveMethodInfo            ,
#endif
    textTagTableRemove                      ,




 -- * Signals


-- ** tagAdded #signal:tagAdded#

    TextTagTableTagAddedCallback            ,
#if defined(ENABLE_OVERLOADING)
    TextTagTableTagAddedSignalInfo          ,
#endif
    afterTextTagTableTagAdded               ,
    onTextTagTableTagAdded                  ,


-- ** tagChanged #signal:tagChanged#

    TextTagTableTagChangedCallback          ,
#if defined(ENABLE_OVERLOADING)
    TextTagTableTagChangedSignalInfo        ,
#endif
    afterTextTagTableTagChanged             ,
    onTextTagTableTagChanged                ,


-- ** tagRemoved #signal:tagRemoved#

    TextTagTableTagRemovedCallback          ,
#if defined(ENABLE_OVERLOADING)
    TextTagTableTagRemovedSignalInfo        ,
#endif
    afterTextTagTableTagRemoved             ,
    onTextTagTableTagRemoved                ,




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
import qualified GI.Gtk.Callbacks as Gtk.Callbacks
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.Buildable as Gtk.Buildable
import {-# SOURCE #-} qualified GI.Gtk.Objects.TextTag as Gtk.TextTag

-- | Memory-managed wrapper type.
newtype TextTagTable = TextTagTable (SP.ManagedPtr TextTagTable)
    deriving (Eq)

instance SP.ManagedPtrNewtype TextTagTable where
    toManagedPtr (TextTagTable p) = p

foreign import ccall "gtk_text_tag_table_get_type"
    c_gtk_text_tag_table_get_type :: IO B.Types.GType

instance B.Types.TypedObject TextTagTable where
    glibType = c_gtk_text_tag_table_get_type

instance B.Types.GObject TextTagTable

-- | Type class for types which can be safely cast to `TextTagTable`, for instance with `toTextTagTable`.
class (SP.GObject o, O.IsDescendantOf TextTagTable o) => IsTextTagTable o
instance (SP.GObject o, O.IsDescendantOf TextTagTable o) => IsTextTagTable o

instance O.HasParentTypes TextTagTable
type instance O.ParentTypes TextTagTable = '[GObject.Object.Object, Gtk.Buildable.Buildable]

-- | Cast to `TextTagTable`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toTextTagTable :: (MIO.MonadIO m, IsTextTagTable o) => o -> m TextTagTable
toTextTagTable = MIO.liftIO . B.ManagedPtr.unsafeCastTo TextTagTable

-- | Convert 'TextTagTable' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe TextTagTable) where
    gvalueGType_ = c_gtk_text_tag_table_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr TextTagTable)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr TextTagTable)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject TextTagTable ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveTextTagTableMethod (t :: Symbol) (o :: *) :: * where
    ResolveTextTagTableMethod "add" o = TextTagTableAddMethodInfo
    ResolveTextTagTableMethod "addChild" o = Gtk.Buildable.BuildableAddChildMethodInfo
    ResolveTextTagTableMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveTextTagTableMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveTextTagTableMethod "constructChild" o = Gtk.Buildable.BuildableConstructChildMethodInfo
    ResolveTextTagTableMethod "customFinished" o = Gtk.Buildable.BuildableCustomFinishedMethodInfo
    ResolveTextTagTableMethod "customTagEnd" o = Gtk.Buildable.BuildableCustomTagEndMethodInfo
    ResolveTextTagTableMethod "customTagStart" o = Gtk.Buildable.BuildableCustomTagStartMethodInfo
    ResolveTextTagTableMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveTextTagTableMethod "foreach" o = TextTagTableForeachMethodInfo
    ResolveTextTagTableMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveTextTagTableMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveTextTagTableMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveTextTagTableMethod "lookup" o = TextTagTableLookupMethodInfo
    ResolveTextTagTableMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveTextTagTableMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveTextTagTableMethod "parserFinished" o = Gtk.Buildable.BuildableParserFinishedMethodInfo
    ResolveTextTagTableMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveTextTagTableMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveTextTagTableMethod "remove" o = TextTagTableRemoveMethodInfo
    ResolveTextTagTableMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveTextTagTableMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveTextTagTableMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveTextTagTableMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveTextTagTableMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveTextTagTableMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveTextTagTableMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveTextTagTableMethod "getInternalChild" o = Gtk.Buildable.BuildableGetInternalChildMethodInfo
    ResolveTextTagTableMethod "getName" o = Gtk.Buildable.BuildableGetNameMethodInfo
    ResolveTextTagTableMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveTextTagTableMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveTextTagTableMethod "getSize" o = TextTagTableGetSizeMethodInfo
    ResolveTextTagTableMethod "setBuildableProperty" o = Gtk.Buildable.BuildableSetBuildablePropertyMethodInfo
    ResolveTextTagTableMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveTextTagTableMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveTextTagTableMethod "setName" o = Gtk.Buildable.BuildableSetNameMethodInfo
    ResolveTextTagTableMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveTextTagTableMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveTextTagTableMethod t TextTagTable, O.OverloadedMethod info TextTagTable p) => OL.IsLabel t (TextTagTable -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveTextTagTableMethod t TextTagTable, O.OverloadedMethod info TextTagTable p, R.HasField t TextTagTable p) => R.HasField t TextTagTable p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveTextTagTableMethod t TextTagTable, O.OverloadedMethodInfo info TextTagTable) => OL.IsLabel t (O.MethodProxy info TextTagTable) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- signal TextTagTable::tag-added
-- | /No description available in the introspection data./
type TextTagTableTagAddedCallback =
    Gtk.TextTag.TextTag
    -- ^ /@tag@/: the added tag.
    -> IO ()

type C_TextTagTableTagAddedCallback =
    Ptr TextTagTable ->                     -- object
    Ptr Gtk.TextTag.TextTag ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_TextTagTableTagAddedCallback`.
foreign import ccall "wrapper"
    mk_TextTagTableTagAddedCallback :: C_TextTagTableTagAddedCallback -> IO (FunPtr C_TextTagTableTagAddedCallback)

wrap_TextTagTableTagAddedCallback :: 
    GObject a => (a -> TextTagTableTagAddedCallback) ->
    C_TextTagTableTagAddedCallback
wrap_TextTagTableTagAddedCallback gi'cb gi'selfPtr tag _ = do
    tag' <- (newObject Gtk.TextTag.TextTag) tag
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  tag'


-- | Connect a signal handler for the [tagAdded](#signal:tagAdded) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' textTagTable #tagAdded callback
-- @
-- 
-- 
onTextTagTableTagAdded :: (IsTextTagTable a, MonadIO m) => a -> ((?self :: a) => TextTagTableTagAddedCallback) -> m SignalHandlerId
onTextTagTableTagAdded obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TextTagTableTagAddedCallback wrapped
    wrapped'' <- mk_TextTagTableTagAddedCallback wrapped'
    connectSignalFunPtr obj "tag-added" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [tagAdded](#signal:tagAdded) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' textTagTable #tagAdded callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterTextTagTableTagAdded :: (IsTextTagTable a, MonadIO m) => a -> ((?self :: a) => TextTagTableTagAddedCallback) -> m SignalHandlerId
afterTextTagTableTagAdded obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TextTagTableTagAddedCallback wrapped
    wrapped'' <- mk_TextTagTableTagAddedCallback wrapped'
    connectSignalFunPtr obj "tag-added" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data TextTagTableTagAddedSignalInfo
instance SignalInfo TextTagTableTagAddedSignalInfo where
    type HaskellCallbackType TextTagTableTagAddedSignalInfo = TextTagTableTagAddedCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_TextTagTableTagAddedCallback cb
        cb'' <- mk_TextTagTableTagAddedCallback cb'
        connectSignalFunPtr obj "tag-added" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextTagTable::tag-added"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextTagTable.html#g:signal:tagAdded"})

#endif

-- signal TextTagTable::tag-changed
-- | /No description available in the introspection data./
type TextTagTableTagChangedCallback =
    Gtk.TextTag.TextTag
    -- ^ /@tag@/: the changed tag.
    -> Bool
    -- ^ /@sizeChanged@/: whether the change affects the t'GI.Gtk.Objects.TextView.TextView' layout.
    -> IO ()

type C_TextTagTableTagChangedCallback =
    Ptr TextTagTable ->                     -- object
    Ptr Gtk.TextTag.TextTag ->
    CInt ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_TextTagTableTagChangedCallback`.
foreign import ccall "wrapper"
    mk_TextTagTableTagChangedCallback :: C_TextTagTableTagChangedCallback -> IO (FunPtr C_TextTagTableTagChangedCallback)

wrap_TextTagTableTagChangedCallback :: 
    GObject a => (a -> TextTagTableTagChangedCallback) ->
    C_TextTagTableTagChangedCallback
wrap_TextTagTableTagChangedCallback gi'cb gi'selfPtr tag sizeChanged _ = do
    tag' <- (newObject Gtk.TextTag.TextTag) tag
    let sizeChanged' = (/= 0) sizeChanged
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  tag' sizeChanged'


-- | Connect a signal handler for the [tagChanged](#signal:tagChanged) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' textTagTable #tagChanged callback
-- @
-- 
-- 
onTextTagTableTagChanged :: (IsTextTagTable a, MonadIO m) => a -> ((?self :: a) => TextTagTableTagChangedCallback) -> m SignalHandlerId
onTextTagTableTagChanged obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TextTagTableTagChangedCallback wrapped
    wrapped'' <- mk_TextTagTableTagChangedCallback wrapped'
    connectSignalFunPtr obj "tag-changed" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [tagChanged](#signal:tagChanged) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' textTagTable #tagChanged callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterTextTagTableTagChanged :: (IsTextTagTable a, MonadIO m) => a -> ((?self :: a) => TextTagTableTagChangedCallback) -> m SignalHandlerId
afterTextTagTableTagChanged obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TextTagTableTagChangedCallback wrapped
    wrapped'' <- mk_TextTagTableTagChangedCallback wrapped'
    connectSignalFunPtr obj "tag-changed" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data TextTagTableTagChangedSignalInfo
instance SignalInfo TextTagTableTagChangedSignalInfo where
    type HaskellCallbackType TextTagTableTagChangedSignalInfo = TextTagTableTagChangedCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_TextTagTableTagChangedCallback cb
        cb'' <- mk_TextTagTableTagChangedCallback cb'
        connectSignalFunPtr obj "tag-changed" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextTagTable::tag-changed"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextTagTable.html#g:signal:tagChanged"})

#endif

-- signal TextTagTable::tag-removed
-- | /No description available in the introspection data./
type TextTagTableTagRemovedCallback =
    Gtk.TextTag.TextTag
    -- ^ /@tag@/: the removed tag.
    -> IO ()

type C_TextTagTableTagRemovedCallback =
    Ptr TextTagTable ->                     -- object
    Ptr Gtk.TextTag.TextTag ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_TextTagTableTagRemovedCallback`.
foreign import ccall "wrapper"
    mk_TextTagTableTagRemovedCallback :: C_TextTagTableTagRemovedCallback -> IO (FunPtr C_TextTagTableTagRemovedCallback)

wrap_TextTagTableTagRemovedCallback :: 
    GObject a => (a -> TextTagTableTagRemovedCallback) ->
    C_TextTagTableTagRemovedCallback
wrap_TextTagTableTagRemovedCallback gi'cb gi'selfPtr tag _ = do
    tag' <- (newObject Gtk.TextTag.TextTag) tag
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  tag'


-- | Connect a signal handler for the [tagRemoved](#signal:tagRemoved) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' textTagTable #tagRemoved callback
-- @
-- 
-- 
onTextTagTableTagRemoved :: (IsTextTagTable a, MonadIO m) => a -> ((?self :: a) => TextTagTableTagRemovedCallback) -> m SignalHandlerId
onTextTagTableTagRemoved obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TextTagTableTagRemovedCallback wrapped
    wrapped'' <- mk_TextTagTableTagRemovedCallback wrapped'
    connectSignalFunPtr obj "tag-removed" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [tagRemoved](#signal:tagRemoved) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' textTagTable #tagRemoved callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterTextTagTableTagRemoved :: (IsTextTagTable a, MonadIO m) => a -> ((?self :: a) => TextTagTableTagRemovedCallback) -> m SignalHandlerId
afterTextTagTableTagRemoved obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TextTagTableTagRemovedCallback wrapped
    wrapped'' <- mk_TextTagTableTagRemovedCallback wrapped'
    connectSignalFunPtr obj "tag-removed" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data TextTagTableTagRemovedSignalInfo
instance SignalInfo TextTagTableTagRemovedSignalInfo where
    type HaskellCallbackType TextTagTableTagRemovedSignalInfo = TextTagTableTagRemovedCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_TextTagTableTagRemovedCallback cb
        cb'' <- mk_TextTagTableTagRemovedCallback cb'
        connectSignalFunPtr obj "tag-removed" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextTagTable::tag-removed"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextTagTable.html#g:signal:tagRemoved"})

#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList TextTagTable
type instance O.AttributeList TextTagTable = TextTagTableAttributeList
type TextTagTableAttributeList = ('[ ] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList TextTagTable = TextTagTableSignalList
type TextTagTableSignalList = ('[ '("notify", GObject.Object.ObjectNotifySignalInfo), '("tagAdded", TextTagTableTagAddedSignalInfo), '("tagChanged", TextTagTableTagChangedSignalInfo), '("tagRemoved", TextTagTableTagRemovedSignalInfo)] :: [(Symbol, *)])

#endif

-- method TextTagTable::new
-- method type : Constructor
-- Args: []
-- Lengths: []
-- returnType: Just
--               (TInterface Name { namespace = "Gtk" , name = "TextTagTable" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_text_tag_table_new" gtk_text_tag_table_new :: 
    IO (Ptr TextTagTable)

-- | Creates a new t'GI.Gtk.Objects.TextTagTable.TextTagTable'. The table contains no tags by
-- default.
textTagTableNew ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m TextTagTable
    -- ^ __Returns:__ a new t'GI.Gtk.Objects.TextTagTable.TextTagTable'
textTagTableNew  = liftIO $ do
    result <- gtk_text_tag_table_new
    checkUnexpectedReturnNULL "textTagTableNew" result
    result' <- (wrapObject TextTagTable) result
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method TextTagTable::add
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "table"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextTagTable" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextTagTable" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "tag"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextTag" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextTag" , sinceVersion = Nothing }
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

foreign import ccall "gtk_text_tag_table_add" gtk_text_tag_table_add :: 
    Ptr TextTagTable ->                     -- table : TInterface (Name {namespace = "Gtk", name = "TextTagTable"})
    Ptr Gtk.TextTag.TextTag ->              -- tag : TInterface (Name {namespace = "Gtk", name = "TextTag"})
    IO CInt

-- | Add a tag to the table. The tag is assigned the highest priority
-- in the table.
-- 
-- /@tag@/ must not be in a tag table already, and may not have
-- the same name as an already-added tag.
textTagTableAdd ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextTagTable a, Gtk.TextTag.IsTextTag b) =>
    a
    -- ^ /@table@/: a t'GI.Gtk.Objects.TextTagTable.TextTagTable'
    -> b
    -- ^ /@tag@/: a t'GI.Gtk.Objects.TextTag.TextTag'
    -> m Bool
    -- ^ __Returns:__ 'P.True' on success.
textTagTableAdd table tag = liftIO $ do
    table' <- unsafeManagedPtrCastPtr table
    tag' <- unsafeManagedPtrCastPtr tag
    result <- gtk_text_tag_table_add table' tag'
    let result' = (/= 0) result
    touchManagedPtr table
    touchManagedPtr tag
    return result'

#if defined(ENABLE_OVERLOADING)
data TextTagTableAddMethodInfo
instance (signature ~ (b -> m Bool), MonadIO m, IsTextTagTable a, Gtk.TextTag.IsTextTag b) => O.OverloadedMethod TextTagTableAddMethodInfo a signature where
    overloadedMethod = textTagTableAdd

instance O.OverloadedMethodInfo TextTagTableAddMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextTagTable.textTagTableAdd",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextTagTable.html#v:textTagTableAdd"
        })


#endif

-- method TextTagTable::foreach
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "table"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextTagTable" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextTagTable" , sinceVersion = Nothing }
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
--                 Name { namespace = "Gtk" , name = "TextTagTableForeach" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a function to call on each tag"
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
--                 { rawDocText = Just "user data" , sinceVersion = Nothing }
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

foreign import ccall "gtk_text_tag_table_foreach" gtk_text_tag_table_foreach :: 
    Ptr TextTagTable ->                     -- table : TInterface (Name {namespace = "Gtk", name = "TextTagTable"})
    FunPtr Gtk.Callbacks.C_TextTagTableForeach -> -- func : TInterface (Name {namespace = "Gtk", name = "TextTagTableForeach"})
    Ptr () ->                               -- data : TBasicType TPtr
    IO ()

-- | Calls /@func@/ on each tag in /@table@/, with user data /@data@/.
-- Note that the table may not be modified while iterating
-- over it (you can’t add\/remove tags).
textTagTableForeach ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextTagTable a) =>
    a
    -- ^ /@table@/: a t'GI.Gtk.Objects.TextTagTable.TextTagTable'
    -> Gtk.Callbacks.TextTagTableForeach
    -- ^ /@func@/: a function to call on each tag
    -> m ()
textTagTableForeach table func = liftIO $ do
    table' <- unsafeManagedPtrCastPtr table
    func' <- Gtk.Callbacks.mk_TextTagTableForeach (Gtk.Callbacks.wrap_TextTagTableForeach Nothing (Gtk.Callbacks.drop_closures_TextTagTableForeach func))
    let data_ = nullPtr
    gtk_text_tag_table_foreach table' func' data_
    safeFreeFunPtr $ castFunPtrToPtr func'
    touchManagedPtr table
    return ()

#if defined(ENABLE_OVERLOADING)
data TextTagTableForeachMethodInfo
instance (signature ~ (Gtk.Callbacks.TextTagTableForeach -> m ()), MonadIO m, IsTextTagTable a) => O.OverloadedMethod TextTagTableForeachMethodInfo a signature where
    overloadedMethod = textTagTableForeach

instance O.OverloadedMethodInfo TextTagTableForeachMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextTagTable.textTagTableForeach",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextTagTable.html#v:textTagTableForeach"
        })


#endif

-- method TextTagTable::get_size
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "table"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextTagTable" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextTagTable" , sinceVersion = Nothing }
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

foreign import ccall "gtk_text_tag_table_get_size" gtk_text_tag_table_get_size :: 
    Ptr TextTagTable ->                     -- table : TInterface (Name {namespace = "Gtk", name = "TextTagTable"})
    IO Int32

-- | Returns the size of the table (number of tags)
textTagTableGetSize ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextTagTable a) =>
    a
    -- ^ /@table@/: a t'GI.Gtk.Objects.TextTagTable.TextTagTable'
    -> m Int32
    -- ^ __Returns:__ number of tags in /@table@/
textTagTableGetSize table = liftIO $ do
    table' <- unsafeManagedPtrCastPtr table
    result <- gtk_text_tag_table_get_size table'
    touchManagedPtr table
    return result

#if defined(ENABLE_OVERLOADING)
data TextTagTableGetSizeMethodInfo
instance (signature ~ (m Int32), MonadIO m, IsTextTagTable a) => O.OverloadedMethod TextTagTableGetSizeMethodInfo a signature where
    overloadedMethod = textTagTableGetSize

instance O.OverloadedMethodInfo TextTagTableGetSizeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextTagTable.textTagTableGetSize",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextTagTable.html#v:textTagTableGetSize"
        })


#endif

-- method TextTagTable::lookup
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "table"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextTagTable" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextTagTable" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "name of a tag" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "TextTag" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_text_tag_table_lookup" gtk_text_tag_table_lookup :: 
    Ptr TextTagTable ->                     -- table : TInterface (Name {namespace = "Gtk", name = "TextTagTable"})
    CString ->                              -- name : TBasicType TUTF8
    IO (Ptr Gtk.TextTag.TextTag)

-- | Look up a named tag.
textTagTableLookup ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextTagTable a) =>
    a
    -- ^ /@table@/: a t'GI.Gtk.Objects.TextTagTable.TextTagTable'
    -> T.Text
    -- ^ /@name@/: name of a tag
    -> m (Maybe Gtk.TextTag.TextTag)
    -- ^ __Returns:__ The tag, or 'P.Nothing' if none by that
    -- name is in the table.
textTagTableLookup table name = liftIO $ do
    table' <- unsafeManagedPtrCastPtr table
    name' <- textToCString name
    result <- gtk_text_tag_table_lookup table' name'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (newObject Gtk.TextTag.TextTag) result'
        return result''
    touchManagedPtr table
    freeMem name'
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data TextTagTableLookupMethodInfo
instance (signature ~ (T.Text -> m (Maybe Gtk.TextTag.TextTag)), MonadIO m, IsTextTagTable a) => O.OverloadedMethod TextTagTableLookupMethodInfo a signature where
    overloadedMethod = textTagTableLookup

instance O.OverloadedMethodInfo TextTagTableLookupMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextTagTable.textTagTableLookup",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextTagTable.html#v:textTagTableLookup"
        })


#endif

-- method TextTagTable::remove
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "table"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextTagTable" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextTagTable" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "tag"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextTag" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextTag" , sinceVersion = Nothing }
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

foreign import ccall "gtk_text_tag_table_remove" gtk_text_tag_table_remove :: 
    Ptr TextTagTable ->                     -- table : TInterface (Name {namespace = "Gtk", name = "TextTagTable"})
    Ptr Gtk.TextTag.TextTag ->              -- tag : TInterface (Name {namespace = "Gtk", name = "TextTag"})
    IO ()

-- | Remove a tag from the table. If a t'GI.Gtk.Objects.TextBuffer.TextBuffer' has /@table@/ as its tag table,
-- the tag is removed from the buffer. The table’s reference to the tag is
-- removed, so the tag will end up destroyed if you don’t have a reference to
-- it.
textTagTableRemove ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextTagTable a, Gtk.TextTag.IsTextTag b) =>
    a
    -- ^ /@table@/: a t'GI.Gtk.Objects.TextTagTable.TextTagTable'
    -> b
    -- ^ /@tag@/: a t'GI.Gtk.Objects.TextTag.TextTag'
    -> m ()
textTagTableRemove table tag = liftIO $ do
    table' <- unsafeManagedPtrCastPtr table
    tag' <- unsafeManagedPtrCastPtr tag
    gtk_text_tag_table_remove table' tag'
    touchManagedPtr table
    touchManagedPtr tag
    return ()

#if defined(ENABLE_OVERLOADING)
data TextTagTableRemoveMethodInfo
instance (signature ~ (b -> m ()), MonadIO m, IsTextTagTable a, Gtk.TextTag.IsTextTag b) => O.OverloadedMethod TextTagTableRemoveMethodInfo a signature where
    overloadedMethod = textTagTableRemove

instance O.OverloadedMethodInfo TextTagTableRemoveMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextTagTable.textTagTableRemove",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextTagTable.html#v:textTagTableRemove"
        })


#endif


