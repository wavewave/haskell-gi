{-# LANGUAGE ImplicitParams, RankNTypes, TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- The t'GI.Gtk.Objects.EntryBuffer.EntryBuffer' class contains the actual text displayed in a
-- t'GI.Gtk.Objects.Entry.Entry' widget.
-- 
-- A single t'GI.Gtk.Objects.EntryBuffer.EntryBuffer' object can be shared by multiple t'GI.Gtk.Objects.Entry.Entry'
-- widgets which will then share the same text content, but not the cursor
-- position, visibility attributes, icon etc.
-- 
-- t'GI.Gtk.Objects.EntryBuffer.EntryBuffer' may be derived from. Such a derived class might allow
-- text to be stored in an alternate location, such as non-pageable memory,
-- useful in the case of important passwords. Or a derived class could
-- integrate with an application’s concept of undo\/redo.
-- 
-- /Since: 2.18/

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.EntryBuffer
    ( 

-- * Exported types
    EntryBuffer(..)                         ,
    IsEntryBuffer                           ,
    toEntryBuffer                           ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [deleteText]("GI.Gtk.Objects.EntryBuffer#g:method:deleteText"), [emitDeletedText]("GI.Gtk.Objects.EntryBuffer#g:method:emitDeletedText"), [emitInsertedText]("GI.Gtk.Objects.EntryBuffer#g:method:emitInsertedText"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [insertText]("GI.Gtk.Objects.EntryBuffer#g:method:insertText"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getBytes]("GI.Gtk.Objects.EntryBuffer#g:method:getBytes"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getLength]("GI.Gtk.Objects.EntryBuffer#g:method:getLength"), [getMaxLength]("GI.Gtk.Objects.EntryBuffer#g:method:getMaxLength"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getText]("GI.Gtk.Objects.EntryBuffer#g:method:getText").
-- 
-- ==== Setters
-- [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setMaxLength]("GI.Gtk.Objects.EntryBuffer#g:method:setMaxLength"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setText]("GI.Gtk.Objects.EntryBuffer#g:method:setText").

#if defined(ENABLE_OVERLOADING)
    ResolveEntryBufferMethod                ,
#endif

-- ** deleteText #method:deleteText#

#if defined(ENABLE_OVERLOADING)
    EntryBufferDeleteTextMethodInfo         ,
#endif
    entryBufferDeleteText                   ,


-- ** emitDeletedText #method:emitDeletedText#

#if defined(ENABLE_OVERLOADING)
    EntryBufferEmitDeletedTextMethodInfo    ,
#endif
    entryBufferEmitDeletedText              ,


-- ** emitInsertedText #method:emitInsertedText#

#if defined(ENABLE_OVERLOADING)
    EntryBufferEmitInsertedTextMethodInfo   ,
#endif
    entryBufferEmitInsertedText             ,


-- ** getBytes #method:getBytes#

#if defined(ENABLE_OVERLOADING)
    EntryBufferGetBytesMethodInfo           ,
#endif
    entryBufferGetBytes                     ,


-- ** getLength #method:getLength#

#if defined(ENABLE_OVERLOADING)
    EntryBufferGetLengthMethodInfo          ,
#endif
    entryBufferGetLength                    ,


-- ** getMaxLength #method:getMaxLength#

#if defined(ENABLE_OVERLOADING)
    EntryBufferGetMaxLengthMethodInfo       ,
#endif
    entryBufferGetMaxLength                 ,


-- ** getText #method:getText#

#if defined(ENABLE_OVERLOADING)
    EntryBufferGetTextMethodInfo            ,
#endif
    entryBufferGetText                      ,


-- ** insertText #method:insertText#

#if defined(ENABLE_OVERLOADING)
    EntryBufferInsertTextMethodInfo         ,
#endif
    entryBufferInsertText                   ,


-- ** new #method:new#

    entryBufferNew                          ,


-- ** setMaxLength #method:setMaxLength#

#if defined(ENABLE_OVERLOADING)
    EntryBufferSetMaxLengthMethodInfo       ,
#endif
    entryBufferSetMaxLength                 ,


-- ** setText #method:setText#

#if defined(ENABLE_OVERLOADING)
    EntryBufferSetTextMethodInfo            ,
#endif
    entryBufferSetText                      ,




 -- * Properties


-- ** length #attr:length#
-- | The length (in characters) of the text in buffer.
-- 
-- /Since: 2.18/

#if defined(ENABLE_OVERLOADING)
    EntryBufferLengthPropertyInfo           ,
#endif
#if defined(ENABLE_OVERLOADING)
    entryBufferLength                       ,
#endif
    getEntryBufferLength                    ,


-- ** maxLength #attr:maxLength#
-- | The maximum length (in characters) of the text in the buffer.
-- 
-- /Since: 2.18/

#if defined(ENABLE_OVERLOADING)
    EntryBufferMaxLengthPropertyInfo        ,
#endif
    constructEntryBufferMaxLength           ,
#if defined(ENABLE_OVERLOADING)
    entryBufferMaxLength                    ,
#endif
    getEntryBufferMaxLength                 ,
    setEntryBufferMaxLength                 ,


-- ** text #attr:text#
-- | The contents of the buffer.
-- 
-- /Since: 2.18/

#if defined(ENABLE_OVERLOADING)
    EntryBufferTextPropertyInfo             ,
#endif
    clearEntryBufferText                    ,
    constructEntryBufferText                ,
#if defined(ENABLE_OVERLOADING)
    entryBufferText                         ,
#endif
    getEntryBufferText                      ,
    setEntryBufferText                      ,




 -- * Signals


-- ** deletedText #signal:deletedText#

    EntryBufferDeletedTextCallback          ,
#if defined(ENABLE_OVERLOADING)
    EntryBufferDeletedTextSignalInfo        ,
#endif
    afterEntryBufferDeletedText             ,
    onEntryBufferDeletedText                ,


-- ** insertedText #signal:insertedText#

    EntryBufferInsertedTextCallback         ,
#if defined(ENABLE_OVERLOADING)
    EntryBufferInsertedTextSignalInfo       ,
#endif
    afterEntryBufferInsertedText            ,
    onEntryBufferInsertedText               ,




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

-- | Memory-managed wrapper type.
newtype EntryBuffer = EntryBuffer (SP.ManagedPtr EntryBuffer)
    deriving (Eq)

instance SP.ManagedPtrNewtype EntryBuffer where
    toManagedPtr (EntryBuffer p) = p

foreign import ccall "gtk_entry_buffer_get_type"
    c_gtk_entry_buffer_get_type :: IO B.Types.GType

instance B.Types.TypedObject EntryBuffer where
    glibType = c_gtk_entry_buffer_get_type

instance B.Types.GObject EntryBuffer

-- | Type class for types which can be safely cast to `EntryBuffer`, for instance with `toEntryBuffer`.
class (SP.GObject o, O.IsDescendantOf EntryBuffer o) => IsEntryBuffer o
instance (SP.GObject o, O.IsDescendantOf EntryBuffer o) => IsEntryBuffer o

instance O.HasParentTypes EntryBuffer
type instance O.ParentTypes EntryBuffer = '[GObject.Object.Object]

-- | Cast to `EntryBuffer`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toEntryBuffer :: (MIO.MonadIO m, IsEntryBuffer o) => o -> m EntryBuffer
toEntryBuffer = MIO.liftIO . B.ManagedPtr.unsafeCastTo EntryBuffer

-- | Convert 'EntryBuffer' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe EntryBuffer) where
    gvalueGType_ = c_gtk_entry_buffer_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr EntryBuffer)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr EntryBuffer)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject EntryBuffer ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveEntryBufferMethod (t :: Symbol) (o :: *) :: * where
    ResolveEntryBufferMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveEntryBufferMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveEntryBufferMethod "deleteText" o = EntryBufferDeleteTextMethodInfo
    ResolveEntryBufferMethod "emitDeletedText" o = EntryBufferEmitDeletedTextMethodInfo
    ResolveEntryBufferMethod "emitInsertedText" o = EntryBufferEmitInsertedTextMethodInfo
    ResolveEntryBufferMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveEntryBufferMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveEntryBufferMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveEntryBufferMethod "insertText" o = EntryBufferInsertTextMethodInfo
    ResolveEntryBufferMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveEntryBufferMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveEntryBufferMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveEntryBufferMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveEntryBufferMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveEntryBufferMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveEntryBufferMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveEntryBufferMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveEntryBufferMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveEntryBufferMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveEntryBufferMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveEntryBufferMethod "getBytes" o = EntryBufferGetBytesMethodInfo
    ResolveEntryBufferMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveEntryBufferMethod "getLength" o = EntryBufferGetLengthMethodInfo
    ResolveEntryBufferMethod "getMaxLength" o = EntryBufferGetMaxLengthMethodInfo
    ResolveEntryBufferMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveEntryBufferMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveEntryBufferMethod "getText" o = EntryBufferGetTextMethodInfo
    ResolveEntryBufferMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveEntryBufferMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveEntryBufferMethod "setMaxLength" o = EntryBufferSetMaxLengthMethodInfo
    ResolveEntryBufferMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveEntryBufferMethod "setText" o = EntryBufferSetTextMethodInfo
    ResolveEntryBufferMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveEntryBufferMethod t EntryBuffer, O.OverloadedMethod info EntryBuffer p) => OL.IsLabel t (EntryBuffer -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveEntryBufferMethod t EntryBuffer, O.OverloadedMethod info EntryBuffer p, R.HasField t EntryBuffer p) => R.HasField t EntryBuffer p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveEntryBufferMethod t EntryBuffer, O.OverloadedMethodInfo info EntryBuffer) => OL.IsLabel t (O.MethodProxy info EntryBuffer) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- signal EntryBuffer::deleted-text
-- | This signal is emitted after text is deleted from the buffer.
-- 
-- /Since: 2.18/
type EntryBufferDeletedTextCallback =
    Word32
    -- ^ /@position@/: the position the text was deleted at.
    -> Word32
    -- ^ /@nChars@/: The number of characters that were deleted.
    -> IO ()

type C_EntryBufferDeletedTextCallback =
    Ptr EntryBuffer ->                      -- object
    Word32 ->
    Word32 ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_EntryBufferDeletedTextCallback`.
foreign import ccall "wrapper"
    mk_EntryBufferDeletedTextCallback :: C_EntryBufferDeletedTextCallback -> IO (FunPtr C_EntryBufferDeletedTextCallback)

wrap_EntryBufferDeletedTextCallback :: 
    GObject a => (a -> EntryBufferDeletedTextCallback) ->
    C_EntryBufferDeletedTextCallback
wrap_EntryBufferDeletedTextCallback gi'cb gi'selfPtr position nChars _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  position nChars


-- | Connect a signal handler for the [deletedText](#signal:deletedText) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' entryBuffer #deletedText callback
-- @
-- 
-- 
onEntryBufferDeletedText :: (IsEntryBuffer a, MonadIO m) => a -> ((?self :: a) => EntryBufferDeletedTextCallback) -> m SignalHandlerId
onEntryBufferDeletedText obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_EntryBufferDeletedTextCallback wrapped
    wrapped'' <- mk_EntryBufferDeletedTextCallback wrapped'
    connectSignalFunPtr obj "deleted-text" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [deletedText](#signal:deletedText) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' entryBuffer #deletedText callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterEntryBufferDeletedText :: (IsEntryBuffer a, MonadIO m) => a -> ((?self :: a) => EntryBufferDeletedTextCallback) -> m SignalHandlerId
afterEntryBufferDeletedText obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_EntryBufferDeletedTextCallback wrapped
    wrapped'' <- mk_EntryBufferDeletedTextCallback wrapped'
    connectSignalFunPtr obj "deleted-text" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data EntryBufferDeletedTextSignalInfo
instance SignalInfo EntryBufferDeletedTextSignalInfo where
    type HaskellCallbackType EntryBufferDeletedTextSignalInfo = EntryBufferDeletedTextCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_EntryBufferDeletedTextCallback cb
        cb'' <- mk_EntryBufferDeletedTextCallback cb'
        connectSignalFunPtr obj "deleted-text" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.EntryBuffer::deleted-text"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-EntryBuffer.html#g:signal:deletedText"})

#endif

-- signal EntryBuffer::inserted-text
-- | This signal is emitted after text is inserted into the buffer.
-- 
-- /Since: 2.18/
type EntryBufferInsertedTextCallback =
    Word32
    -- ^ /@position@/: the position the text was inserted at.
    -> T.Text
    -- ^ /@chars@/: The text that was inserted.
    -> Word32
    -- ^ /@nChars@/: The number of characters that were inserted.
    -> IO ()

type C_EntryBufferInsertedTextCallback =
    Ptr EntryBuffer ->                      -- object
    Word32 ->
    CString ->
    Word32 ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_EntryBufferInsertedTextCallback`.
foreign import ccall "wrapper"
    mk_EntryBufferInsertedTextCallback :: C_EntryBufferInsertedTextCallback -> IO (FunPtr C_EntryBufferInsertedTextCallback)

wrap_EntryBufferInsertedTextCallback :: 
    GObject a => (a -> EntryBufferInsertedTextCallback) ->
    C_EntryBufferInsertedTextCallback
wrap_EntryBufferInsertedTextCallback gi'cb gi'selfPtr position chars nChars _ = do
    chars' <- cstringToText chars
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  position chars' nChars


-- | Connect a signal handler for the [insertedText](#signal:insertedText) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' entryBuffer #insertedText callback
-- @
-- 
-- 
onEntryBufferInsertedText :: (IsEntryBuffer a, MonadIO m) => a -> ((?self :: a) => EntryBufferInsertedTextCallback) -> m SignalHandlerId
onEntryBufferInsertedText obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_EntryBufferInsertedTextCallback wrapped
    wrapped'' <- mk_EntryBufferInsertedTextCallback wrapped'
    connectSignalFunPtr obj "inserted-text" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [insertedText](#signal:insertedText) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' entryBuffer #insertedText callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterEntryBufferInsertedText :: (IsEntryBuffer a, MonadIO m) => a -> ((?self :: a) => EntryBufferInsertedTextCallback) -> m SignalHandlerId
afterEntryBufferInsertedText obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_EntryBufferInsertedTextCallback wrapped
    wrapped'' <- mk_EntryBufferInsertedTextCallback wrapped'
    connectSignalFunPtr obj "inserted-text" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data EntryBufferInsertedTextSignalInfo
instance SignalInfo EntryBufferInsertedTextSignalInfo where
    type HaskellCallbackType EntryBufferInsertedTextSignalInfo = EntryBufferInsertedTextCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_EntryBufferInsertedTextCallback cb
        cb'' <- mk_EntryBufferInsertedTextCallback cb'
        connectSignalFunPtr obj "inserted-text" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.EntryBuffer::inserted-text"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-EntryBuffer.html#g:signal:insertedText"})

#endif

-- VVV Prop "length"
   -- Type: TBasicType TUInt
   -- Flags: [PropertyReadable]
   -- Nullable: (Just False,Nothing)

-- | Get the value of the “@length@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' entryBuffer #length
-- @
getEntryBufferLength :: (MonadIO m, IsEntryBuffer o) => o -> m Word32
getEntryBufferLength obj = MIO.liftIO $ B.Properties.getObjectPropertyUInt32 obj "length"

#if defined(ENABLE_OVERLOADING)
data EntryBufferLengthPropertyInfo
instance AttrInfo EntryBufferLengthPropertyInfo where
    type AttrAllowedOps EntryBufferLengthPropertyInfo = '[ 'AttrGet]
    type AttrBaseTypeConstraint EntryBufferLengthPropertyInfo = IsEntryBuffer
    type AttrSetTypeConstraint EntryBufferLengthPropertyInfo = (~) ()
    type AttrTransferTypeConstraint EntryBufferLengthPropertyInfo = (~) ()
    type AttrTransferType EntryBufferLengthPropertyInfo = ()
    type AttrGetType EntryBufferLengthPropertyInfo = Word32
    type AttrLabel EntryBufferLengthPropertyInfo = "length"
    type AttrOrigin EntryBufferLengthPropertyInfo = EntryBuffer
    attrGet = getEntryBufferLength
    attrSet = undefined
    attrTransfer _ = undefined
    attrConstruct = undefined
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.EntryBuffer.length"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-EntryBuffer.html#g:attr:length"
        })
#endif

-- VVV Prop "max-length"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@max-length@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' entryBuffer #maxLength
-- @
getEntryBufferMaxLength :: (MonadIO m, IsEntryBuffer o) => o -> m Int32
getEntryBufferMaxLength obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "max-length"

-- | Set the value of the “@max-length@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' entryBuffer [ #maxLength 'Data.GI.Base.Attributes.:=' value ]
-- @
setEntryBufferMaxLength :: (MonadIO m, IsEntryBuffer o) => o -> Int32 -> m ()
setEntryBufferMaxLength obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyInt32 obj "max-length" val

-- | Construct a `GValueConstruct` with valid value for the “@max-length@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructEntryBufferMaxLength :: (IsEntryBuffer o, MIO.MonadIO m) => Int32 -> m (GValueConstruct o)
constructEntryBufferMaxLength val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyInt32 "max-length" val

#if defined(ENABLE_OVERLOADING)
data EntryBufferMaxLengthPropertyInfo
instance AttrInfo EntryBufferMaxLengthPropertyInfo where
    type AttrAllowedOps EntryBufferMaxLengthPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint EntryBufferMaxLengthPropertyInfo = IsEntryBuffer
    type AttrSetTypeConstraint EntryBufferMaxLengthPropertyInfo = (~) Int32
    type AttrTransferTypeConstraint EntryBufferMaxLengthPropertyInfo = (~) Int32
    type AttrTransferType EntryBufferMaxLengthPropertyInfo = Int32
    type AttrGetType EntryBufferMaxLengthPropertyInfo = Int32
    type AttrLabel EntryBufferMaxLengthPropertyInfo = "max-length"
    type AttrOrigin EntryBufferMaxLengthPropertyInfo = EntryBuffer
    attrGet = getEntryBufferMaxLength
    attrSet = setEntryBufferMaxLength
    attrTransfer _ v = do
        return v
    attrConstruct = constructEntryBufferMaxLength
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.EntryBuffer.maxLength"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-EntryBuffer.html#g:attr:maxLength"
        })
#endif

-- VVV Prop "text"
   -- Type: TBasicType TUTF8
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Nothing)

-- | Get the value of the “@text@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' entryBuffer #text
-- @
getEntryBufferText :: (MonadIO m, IsEntryBuffer o) => o -> m T.Text
getEntryBufferText obj = MIO.liftIO $ checkUnexpectedNothing "getEntryBufferText" $ B.Properties.getObjectPropertyString obj "text"

-- | Set the value of the “@text@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' entryBuffer [ #text 'Data.GI.Base.Attributes.:=' value ]
-- @
setEntryBufferText :: (MonadIO m, IsEntryBuffer o) => o -> T.Text -> m ()
setEntryBufferText obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "text" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@text@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructEntryBufferText :: (IsEntryBuffer o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructEntryBufferText val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "text" (P.Just val)

-- | Set the value of the “@text@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #text
-- @
clearEntryBufferText :: (MonadIO m, IsEntryBuffer o) => o -> m ()
clearEntryBufferText obj = liftIO $ B.Properties.setObjectPropertyString obj "text" (Nothing :: Maybe T.Text)

#if defined(ENABLE_OVERLOADING)
data EntryBufferTextPropertyInfo
instance AttrInfo EntryBufferTextPropertyInfo where
    type AttrAllowedOps EntryBufferTextPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint EntryBufferTextPropertyInfo = IsEntryBuffer
    type AttrSetTypeConstraint EntryBufferTextPropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint EntryBufferTextPropertyInfo = (~) T.Text
    type AttrTransferType EntryBufferTextPropertyInfo = T.Text
    type AttrGetType EntryBufferTextPropertyInfo = T.Text
    type AttrLabel EntryBufferTextPropertyInfo = "text"
    type AttrOrigin EntryBufferTextPropertyInfo = EntryBuffer
    attrGet = getEntryBufferText
    attrSet = setEntryBufferText
    attrTransfer _ v = do
        return v
    attrConstruct = constructEntryBufferText
    attrClear = clearEntryBufferText
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.EntryBuffer.text"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-EntryBuffer.html#g:attr:text"
        })
#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList EntryBuffer
type instance O.AttributeList EntryBuffer = EntryBufferAttributeList
type EntryBufferAttributeList = ('[ '("length", EntryBufferLengthPropertyInfo), '("maxLength", EntryBufferMaxLengthPropertyInfo), '("text", EntryBufferTextPropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
entryBufferLength :: AttrLabelProxy "length"
entryBufferLength = AttrLabelProxy

entryBufferMaxLength :: AttrLabelProxy "maxLength"
entryBufferMaxLength = AttrLabelProxy

entryBufferText :: AttrLabelProxy "text"
entryBufferText = AttrLabelProxy

#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList EntryBuffer = EntryBufferSignalList
type EntryBufferSignalList = ('[ '("deletedText", EntryBufferDeletedTextSignalInfo), '("insertedText", EntryBufferInsertedTextSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo)] :: [(Symbol, *)])

#endif

-- method EntryBuffer::new
-- method type : Constructor
-- Args: [ Arg
--           { argCName = "initial_chars"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "initial buffer text, or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "n_initial_chars"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "number of characters in @initial_chars, or -1"
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "EntryBuffer" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_entry_buffer_new" gtk_entry_buffer_new :: 
    CString ->                              -- initial_chars : TBasicType TUTF8
    Int32 ->                                -- n_initial_chars : TBasicType TInt
    IO (Ptr EntryBuffer)

-- | Create a new GtkEntryBuffer object.
-- 
-- Optionally, specify initial text to set in the buffer.
-- 
-- /Since: 2.18/
entryBufferNew ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    Maybe (T.Text)
    -- ^ /@initialChars@/: initial buffer text, or 'P.Nothing'
    -> Int32
    -- ^ /@nInitialChars@/: number of characters in /@initialChars@/, or -1
    -> m EntryBuffer
    -- ^ __Returns:__ A new GtkEntryBuffer object.
entryBufferNew initialChars nInitialChars = liftIO $ do
    maybeInitialChars <- case initialChars of
        Nothing -> return nullPtr
        Just jInitialChars -> do
            jInitialChars' <- textToCString jInitialChars
            return jInitialChars'
    result <- gtk_entry_buffer_new maybeInitialChars nInitialChars
    checkUnexpectedReturnNULL "entryBufferNew" result
    result' <- (wrapObject EntryBuffer) result
    freeMem maybeInitialChars
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method EntryBuffer::delete_text
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "buffer"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "EntryBuffer" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEntryBuffer" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "position"
--           , argType = TBasicType TUInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "position at which to delete text"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "n_chars"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "number of characters to delete"
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

foreign import ccall "gtk_entry_buffer_delete_text" gtk_entry_buffer_delete_text :: 
    Ptr EntryBuffer ->                      -- buffer : TInterface (Name {namespace = "Gtk", name = "EntryBuffer"})
    Word32 ->                               -- position : TBasicType TUInt
    Int32 ->                                -- n_chars : TBasicType TInt
    IO Word32

-- | Deletes a sequence of characters from the buffer. /@nChars@/ characters are
-- deleted starting at /@position@/. If /@nChars@/ is negative, then all characters
-- until the end of the text are deleted.
-- 
-- If /@position@/ or /@nChars@/ are out of bounds, then they are coerced to sane
-- values.
-- 
-- Note that the positions are specified in characters, not bytes.
-- 
-- /Since: 2.18/
entryBufferDeleteText ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntryBuffer a) =>
    a
    -- ^ /@buffer@/: a t'GI.Gtk.Objects.EntryBuffer.EntryBuffer'
    -> Word32
    -- ^ /@position@/: position at which to delete text
    -> Int32
    -- ^ /@nChars@/: number of characters to delete
    -> m Word32
    -- ^ __Returns:__ The number of characters deleted.
entryBufferDeleteText buffer position nChars = liftIO $ do
    buffer' <- unsafeManagedPtrCastPtr buffer
    result <- gtk_entry_buffer_delete_text buffer' position nChars
    touchManagedPtr buffer
    return result

#if defined(ENABLE_OVERLOADING)
data EntryBufferDeleteTextMethodInfo
instance (signature ~ (Word32 -> Int32 -> m Word32), MonadIO m, IsEntryBuffer a) => O.OverloadedMethod EntryBufferDeleteTextMethodInfo a signature where
    overloadedMethod = entryBufferDeleteText

instance O.OverloadedMethodInfo EntryBufferDeleteTextMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.EntryBuffer.entryBufferDeleteText",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-EntryBuffer.html#v:entryBufferDeleteText"
        })


#endif

-- method EntryBuffer::emit_deleted_text
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "buffer"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "EntryBuffer" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEntryBuffer" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "position"
--           , argType = TBasicType TUInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "position at which text was deleted"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "n_chars"
--           , argType = TBasicType TUInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "number of characters deleted"
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

foreign import ccall "gtk_entry_buffer_emit_deleted_text" gtk_entry_buffer_emit_deleted_text :: 
    Ptr EntryBuffer ->                      -- buffer : TInterface (Name {namespace = "Gtk", name = "EntryBuffer"})
    Word32 ->                               -- position : TBasicType TUInt
    Word32 ->                               -- n_chars : TBasicType TUInt
    IO ()

-- | Used when subclassing t'GI.Gtk.Objects.EntryBuffer.EntryBuffer'
-- 
-- /Since: 2.18/
entryBufferEmitDeletedText ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntryBuffer a) =>
    a
    -- ^ /@buffer@/: a t'GI.Gtk.Objects.EntryBuffer.EntryBuffer'
    -> Word32
    -- ^ /@position@/: position at which text was deleted
    -> Word32
    -- ^ /@nChars@/: number of characters deleted
    -> m ()
entryBufferEmitDeletedText buffer position nChars = liftIO $ do
    buffer' <- unsafeManagedPtrCastPtr buffer
    gtk_entry_buffer_emit_deleted_text buffer' position nChars
    touchManagedPtr buffer
    return ()

#if defined(ENABLE_OVERLOADING)
data EntryBufferEmitDeletedTextMethodInfo
instance (signature ~ (Word32 -> Word32 -> m ()), MonadIO m, IsEntryBuffer a) => O.OverloadedMethod EntryBufferEmitDeletedTextMethodInfo a signature where
    overloadedMethod = entryBufferEmitDeletedText

instance O.OverloadedMethodInfo EntryBufferEmitDeletedTextMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.EntryBuffer.entryBufferEmitDeletedText",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-EntryBuffer.html#v:entryBufferEmitDeletedText"
        })


#endif

-- method EntryBuffer::emit_inserted_text
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "buffer"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "EntryBuffer" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEntryBuffer" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "position"
--           , argType = TBasicType TUInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "position at which text was inserted"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "chars"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "text that was inserted"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "n_chars"
--           , argType = TBasicType TUInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "number of characters inserted"
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

foreign import ccall "gtk_entry_buffer_emit_inserted_text" gtk_entry_buffer_emit_inserted_text :: 
    Ptr EntryBuffer ->                      -- buffer : TInterface (Name {namespace = "Gtk", name = "EntryBuffer"})
    Word32 ->                               -- position : TBasicType TUInt
    CString ->                              -- chars : TBasicType TUTF8
    Word32 ->                               -- n_chars : TBasicType TUInt
    IO ()

-- | Used when subclassing t'GI.Gtk.Objects.EntryBuffer.EntryBuffer'
-- 
-- /Since: 2.18/
entryBufferEmitInsertedText ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntryBuffer a) =>
    a
    -- ^ /@buffer@/: a t'GI.Gtk.Objects.EntryBuffer.EntryBuffer'
    -> Word32
    -- ^ /@position@/: position at which text was inserted
    -> T.Text
    -- ^ /@chars@/: text that was inserted
    -> Word32
    -- ^ /@nChars@/: number of characters inserted
    -> m ()
entryBufferEmitInsertedText buffer position chars nChars = liftIO $ do
    buffer' <- unsafeManagedPtrCastPtr buffer
    chars' <- textToCString chars
    gtk_entry_buffer_emit_inserted_text buffer' position chars' nChars
    touchManagedPtr buffer
    freeMem chars'
    return ()

#if defined(ENABLE_OVERLOADING)
data EntryBufferEmitInsertedTextMethodInfo
instance (signature ~ (Word32 -> T.Text -> Word32 -> m ()), MonadIO m, IsEntryBuffer a) => O.OverloadedMethod EntryBufferEmitInsertedTextMethodInfo a signature where
    overloadedMethod = entryBufferEmitInsertedText

instance O.OverloadedMethodInfo EntryBufferEmitInsertedTextMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.EntryBuffer.entryBufferEmitInsertedText",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-EntryBuffer.html#v:entryBufferEmitInsertedText"
        })


#endif

-- method EntryBuffer::get_bytes
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "buffer"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "EntryBuffer" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEntryBuffer" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TBasicType TUInt64)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_entry_buffer_get_bytes" gtk_entry_buffer_get_bytes :: 
    Ptr EntryBuffer ->                      -- buffer : TInterface (Name {namespace = "Gtk", name = "EntryBuffer"})
    IO Word64

-- | Retrieves the length in bytes of the buffer.
-- See 'GI.Gtk.Objects.EntryBuffer.entryBufferGetLength'.
-- 
-- /Since: 2.18/
entryBufferGetBytes ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntryBuffer a) =>
    a
    -- ^ /@buffer@/: a t'GI.Gtk.Objects.EntryBuffer.EntryBuffer'
    -> m Word64
    -- ^ __Returns:__ The byte length of the buffer.
entryBufferGetBytes buffer = liftIO $ do
    buffer' <- unsafeManagedPtrCastPtr buffer
    result <- gtk_entry_buffer_get_bytes buffer'
    touchManagedPtr buffer
    return result

#if defined(ENABLE_OVERLOADING)
data EntryBufferGetBytesMethodInfo
instance (signature ~ (m Word64), MonadIO m, IsEntryBuffer a) => O.OverloadedMethod EntryBufferGetBytesMethodInfo a signature where
    overloadedMethod = entryBufferGetBytes

instance O.OverloadedMethodInfo EntryBufferGetBytesMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.EntryBuffer.entryBufferGetBytes",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-EntryBuffer.html#v:entryBufferGetBytes"
        })


#endif

-- method EntryBuffer::get_length
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "buffer"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "EntryBuffer" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEntryBuffer" , sinceVersion = Nothing }
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

foreign import ccall "gtk_entry_buffer_get_length" gtk_entry_buffer_get_length :: 
    Ptr EntryBuffer ->                      -- buffer : TInterface (Name {namespace = "Gtk", name = "EntryBuffer"})
    IO Word32

-- | Retrieves the length in characters of the buffer.
-- 
-- /Since: 2.18/
entryBufferGetLength ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntryBuffer a) =>
    a
    -- ^ /@buffer@/: a t'GI.Gtk.Objects.EntryBuffer.EntryBuffer'
    -> m Word32
    -- ^ __Returns:__ The number of characters in the buffer.
entryBufferGetLength buffer = liftIO $ do
    buffer' <- unsafeManagedPtrCastPtr buffer
    result <- gtk_entry_buffer_get_length buffer'
    touchManagedPtr buffer
    return result

#if defined(ENABLE_OVERLOADING)
data EntryBufferGetLengthMethodInfo
instance (signature ~ (m Word32), MonadIO m, IsEntryBuffer a) => O.OverloadedMethod EntryBufferGetLengthMethodInfo a signature where
    overloadedMethod = entryBufferGetLength

instance O.OverloadedMethodInfo EntryBufferGetLengthMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.EntryBuffer.entryBufferGetLength",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-EntryBuffer.html#v:entryBufferGetLength"
        })


#endif

-- method EntryBuffer::get_max_length
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "buffer"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "EntryBuffer" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEntryBuffer" , sinceVersion = Nothing }
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

foreign import ccall "gtk_entry_buffer_get_max_length" gtk_entry_buffer_get_max_length :: 
    Ptr EntryBuffer ->                      -- buffer : TInterface (Name {namespace = "Gtk", name = "EntryBuffer"})
    IO Int32

-- | Retrieves the maximum allowed length of the text in
-- /@buffer@/. See 'GI.Gtk.Objects.EntryBuffer.entryBufferSetMaxLength'.
-- 
-- /Since: 2.18/
entryBufferGetMaxLength ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntryBuffer a) =>
    a
    -- ^ /@buffer@/: a t'GI.Gtk.Objects.EntryBuffer.EntryBuffer'
    -> m Int32
    -- ^ __Returns:__ the maximum allowed number of characters
    --               in t'GI.Gtk.Objects.EntryBuffer.EntryBuffer', or 0 if there is no maximum.
entryBufferGetMaxLength buffer = liftIO $ do
    buffer' <- unsafeManagedPtrCastPtr buffer
    result <- gtk_entry_buffer_get_max_length buffer'
    touchManagedPtr buffer
    return result

#if defined(ENABLE_OVERLOADING)
data EntryBufferGetMaxLengthMethodInfo
instance (signature ~ (m Int32), MonadIO m, IsEntryBuffer a) => O.OverloadedMethod EntryBufferGetMaxLengthMethodInfo a signature where
    overloadedMethod = entryBufferGetMaxLength

instance O.OverloadedMethodInfo EntryBufferGetMaxLengthMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.EntryBuffer.entryBufferGetMaxLength",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-EntryBuffer.html#v:entryBufferGetMaxLength"
        })


#endif

-- method EntryBuffer::get_text
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "buffer"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "EntryBuffer" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEntryBuffer" , sinceVersion = Nothing }
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

foreign import ccall "gtk_entry_buffer_get_text" gtk_entry_buffer_get_text :: 
    Ptr EntryBuffer ->                      -- buffer : TInterface (Name {namespace = "Gtk", name = "EntryBuffer"})
    IO CString

-- | Retrieves the contents of the buffer.
-- 
-- The memory pointer returned by this call will not change
-- unless this object emits a signal, or is finalized.
-- 
-- /Since: 2.18/
entryBufferGetText ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntryBuffer a) =>
    a
    -- ^ /@buffer@/: a t'GI.Gtk.Objects.EntryBuffer.EntryBuffer'
    -> m T.Text
    -- ^ __Returns:__ a pointer to the contents of the widget as a
    --      string. This string points to internally allocated
    --      storage in the buffer and must not be freed, modified or
    --      stored.
entryBufferGetText buffer = liftIO $ do
    buffer' <- unsafeManagedPtrCastPtr buffer
    result <- gtk_entry_buffer_get_text buffer'
    checkUnexpectedReturnNULL "entryBufferGetText" result
    result' <- cstringToText result
    touchManagedPtr buffer
    return result'

#if defined(ENABLE_OVERLOADING)
data EntryBufferGetTextMethodInfo
instance (signature ~ (m T.Text), MonadIO m, IsEntryBuffer a) => O.OverloadedMethod EntryBufferGetTextMethodInfo a signature where
    overloadedMethod = entryBufferGetText

instance O.OverloadedMethodInfo EntryBufferGetTextMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.EntryBuffer.entryBufferGetText",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-EntryBuffer.html#v:entryBufferGetText"
        })


#endif

-- method EntryBuffer::insert_text
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "buffer"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "EntryBuffer" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEntryBuffer" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "position"
--           , argType = TBasicType TUInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the position at which to insert text."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "chars"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the text to insert into the buffer."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "n_chars"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the length of the text in characters, or -1"
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

foreign import ccall "gtk_entry_buffer_insert_text" gtk_entry_buffer_insert_text :: 
    Ptr EntryBuffer ->                      -- buffer : TInterface (Name {namespace = "Gtk", name = "EntryBuffer"})
    Word32 ->                               -- position : TBasicType TUInt
    CString ->                              -- chars : TBasicType TUTF8
    Int32 ->                                -- n_chars : TBasicType TInt
    IO Word32

-- | Inserts /@nChars@/ characters of /@chars@/ into the contents of the
-- buffer, at position /@position@/.
-- 
-- If /@nChars@/ is negative, then characters from chars will be inserted
-- until a null-terminator is found. If /@position@/ or /@nChars@/ are out of
-- bounds, or the maximum buffer text length is exceeded, then they are
-- coerced to sane values.
-- 
-- Note that the position and length are in characters, not in bytes.
-- 
-- /Since: 2.18/
entryBufferInsertText ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntryBuffer a) =>
    a
    -- ^ /@buffer@/: a t'GI.Gtk.Objects.EntryBuffer.EntryBuffer'
    -> Word32
    -- ^ /@position@/: the position at which to insert text.
    -> T.Text
    -- ^ /@chars@/: the text to insert into the buffer.
    -> Int32
    -- ^ /@nChars@/: the length of the text in characters, or -1
    -> m Word32
    -- ^ __Returns:__ The number of characters actually inserted.
entryBufferInsertText buffer position chars nChars = liftIO $ do
    buffer' <- unsafeManagedPtrCastPtr buffer
    chars' <- textToCString chars
    result <- gtk_entry_buffer_insert_text buffer' position chars' nChars
    touchManagedPtr buffer
    freeMem chars'
    return result

#if defined(ENABLE_OVERLOADING)
data EntryBufferInsertTextMethodInfo
instance (signature ~ (Word32 -> T.Text -> Int32 -> m Word32), MonadIO m, IsEntryBuffer a) => O.OverloadedMethod EntryBufferInsertTextMethodInfo a signature where
    overloadedMethod = entryBufferInsertText

instance O.OverloadedMethodInfo EntryBufferInsertTextMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.EntryBuffer.entryBufferInsertText",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-EntryBuffer.html#v:entryBufferInsertText"
        })


#endif

-- method EntryBuffer::set_max_length
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "buffer"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "EntryBuffer" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEntryBuffer" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "max_length"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the maximum length of the entry buffer, or 0 for no maximum.\n  (other than the maximum length of entries.) The value passed in will\n  be clamped to the range 0-65536."
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

foreign import ccall "gtk_entry_buffer_set_max_length" gtk_entry_buffer_set_max_length :: 
    Ptr EntryBuffer ->                      -- buffer : TInterface (Name {namespace = "Gtk", name = "EntryBuffer"})
    Int32 ->                                -- max_length : TBasicType TInt
    IO ()

-- | Sets the maximum allowed length of the contents of the buffer. If
-- the current contents are longer than the given length, then they
-- will be truncated to fit.
-- 
-- /Since: 2.18/
entryBufferSetMaxLength ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntryBuffer a) =>
    a
    -- ^ /@buffer@/: a t'GI.Gtk.Objects.EntryBuffer.EntryBuffer'
    -> Int32
    -- ^ /@maxLength@/: the maximum length of the entry buffer, or 0 for no maximum.
    --   (other than the maximum length of entries.) The value passed in will
    --   be clamped to the range 0-65536.
    -> m ()
entryBufferSetMaxLength buffer maxLength = liftIO $ do
    buffer' <- unsafeManagedPtrCastPtr buffer
    gtk_entry_buffer_set_max_length buffer' maxLength
    touchManagedPtr buffer
    return ()

#if defined(ENABLE_OVERLOADING)
data EntryBufferSetMaxLengthMethodInfo
instance (signature ~ (Int32 -> m ()), MonadIO m, IsEntryBuffer a) => O.OverloadedMethod EntryBufferSetMaxLengthMethodInfo a signature where
    overloadedMethod = entryBufferSetMaxLength

instance O.OverloadedMethodInfo EntryBufferSetMaxLengthMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.EntryBuffer.entryBufferSetMaxLength",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-EntryBuffer.html#v:entryBufferSetMaxLength"
        })


#endif

-- method EntryBuffer::set_text
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "buffer"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "EntryBuffer" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEntryBuffer" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "chars"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the new text" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "n_chars"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the number of characters in @text, or -1"
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

foreign import ccall "gtk_entry_buffer_set_text" gtk_entry_buffer_set_text :: 
    Ptr EntryBuffer ->                      -- buffer : TInterface (Name {namespace = "Gtk", name = "EntryBuffer"})
    CString ->                              -- chars : TBasicType TUTF8
    Int32 ->                                -- n_chars : TBasicType TInt
    IO ()

-- | Sets the text in the buffer.
-- 
-- This is roughly equivalent to calling 'GI.Gtk.Objects.EntryBuffer.entryBufferDeleteText'
-- and 'GI.Gtk.Objects.EntryBuffer.entryBufferInsertText'.
-- 
-- Note that /@nChars@/ is in characters, not in bytes.
-- 
-- /Since: 2.18/
entryBufferSetText ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntryBuffer a) =>
    a
    -- ^ /@buffer@/: a t'GI.Gtk.Objects.EntryBuffer.EntryBuffer'
    -> T.Text
    -- ^ /@chars@/: the new text
    -> Int32
    -- ^ /@nChars@/: the number of characters in /@text@/, or -1
    -> m ()
entryBufferSetText buffer chars nChars = liftIO $ do
    buffer' <- unsafeManagedPtrCastPtr buffer
    chars' <- textToCString chars
    gtk_entry_buffer_set_text buffer' chars' nChars
    touchManagedPtr buffer
    freeMem chars'
    return ()

#if defined(ENABLE_OVERLOADING)
data EntryBufferSetTextMethodInfo
instance (signature ~ (T.Text -> Int32 -> m ()), MonadIO m, IsEntryBuffer a) => O.OverloadedMethod EntryBufferSetTextMethodInfo a signature where
    overloadedMethod = entryBufferSetText

instance O.OverloadedMethodInfo EntryBufferSetTextMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.EntryBuffer.entryBufferSetText",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-EntryBuffer.html#v:entryBufferSetText"
        })


#endif


