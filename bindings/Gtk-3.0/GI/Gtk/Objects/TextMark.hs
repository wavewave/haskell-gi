{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- You may wish to begin by reading the
-- [text widget conceptual overview][TextWidget]
-- which gives an overview of all the objects and data
-- types related to the text widget and how they work together.
-- 
-- A t'GI.Gtk.Objects.TextMark.TextMark' is like a bookmark in a text buffer; it preserves a position in
-- the text. You can convert the mark to an iterator using
-- 'GI.Gtk.Objects.TextBuffer.textBufferGetIterAtMark'. Unlike iterators, marks remain valid across
-- buffer mutations, because their behavior is defined when text is inserted or
-- deleted. When text containing a mark is deleted, the mark remains in the
-- position originally occupied by the deleted text. When text is inserted at a
-- mark, a mark with “left gravity” will be moved to the
-- beginning of the newly-inserted text, and a mark with “right
-- gravity” will be moved to the end.
-- 
-- Note that “left” and “right” here refer to logical direction (left
-- is the toward the start of the buffer); in some languages such as
-- Hebrew the logically-leftmost text is not actually on the left when
-- displayed.
-- 
-- Marks are reference counted, but the reference count only controls the validity
-- of the memory; marks can be deleted from the buffer at any time with
-- 'GI.Gtk.Objects.TextBuffer.textBufferDeleteMark'. Once deleted from the buffer, a mark is
-- essentially useless.
-- 
-- Marks optionally have names; these can be convenient to avoid passing the
-- t'GI.Gtk.Objects.TextMark.TextMark' object around.
-- 
-- Marks are typically created using the 'GI.Gtk.Objects.TextBuffer.textBufferCreateMark' function.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.TextMark
    ( 

-- * Exported types
    TextMark(..)                            ,
    IsTextMark                              ,
    toTextMark                              ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getBuffer]("GI.Gtk.Objects.TextMark#g:method:getBuffer"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getDeleted]("GI.Gtk.Objects.TextMark#g:method:getDeleted"), [getLeftGravity]("GI.Gtk.Objects.TextMark#g:method:getLeftGravity"), [getName]("GI.Gtk.Objects.TextMark#g:method:getName"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getVisible]("GI.Gtk.Objects.TextMark#g:method:getVisible").
-- 
-- ==== Setters
-- [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setVisible]("GI.Gtk.Objects.TextMark#g:method:setVisible").

#if defined(ENABLE_OVERLOADING)
    ResolveTextMarkMethod                   ,
#endif

-- ** getBuffer #method:getBuffer#

#if defined(ENABLE_OVERLOADING)
    TextMarkGetBufferMethodInfo             ,
#endif
    textMarkGetBuffer                       ,


-- ** getDeleted #method:getDeleted#

#if defined(ENABLE_OVERLOADING)
    TextMarkGetDeletedMethodInfo            ,
#endif
    textMarkGetDeleted                      ,


-- ** getLeftGravity #method:getLeftGravity#

#if defined(ENABLE_OVERLOADING)
    TextMarkGetLeftGravityMethodInfo        ,
#endif
    textMarkGetLeftGravity                  ,


-- ** getName #method:getName#

#if defined(ENABLE_OVERLOADING)
    TextMarkGetNameMethodInfo               ,
#endif
    textMarkGetName                         ,


-- ** getVisible #method:getVisible#

#if defined(ENABLE_OVERLOADING)
    TextMarkGetVisibleMethodInfo            ,
#endif
    textMarkGetVisible                      ,


-- ** new #method:new#

    textMarkNew                             ,


-- ** setVisible #method:setVisible#

#if defined(ENABLE_OVERLOADING)
    TextMarkSetVisibleMethodInfo            ,
#endif
    textMarkSetVisible                      ,




 -- * Properties


-- ** leftGravity #attr:leftGravity#
-- | Whether the mark has left gravity. When text is inserted at the mark’s
-- current location, if the mark has left gravity it will be moved
-- to the left of the newly-inserted text, otherwise to the right.

#if defined(ENABLE_OVERLOADING)
    TextMarkLeftGravityPropertyInfo         ,
#endif
    constructTextMarkLeftGravity            ,
    getTextMarkLeftGravity                  ,
#if defined(ENABLE_OVERLOADING)
    textMarkLeftGravity                     ,
#endif


-- ** name #attr:name#
-- | The name of the mark or 'P.Nothing' if the mark is anonymous.

#if defined(ENABLE_OVERLOADING)
    TextMarkNamePropertyInfo                ,
#endif
    constructTextMarkName                   ,
    getTextMarkName                         ,
#if defined(ENABLE_OVERLOADING)
    textMarkName                            ,
#endif




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
import {-# SOURCE #-} qualified GI.Gtk.Objects.TextBuffer as Gtk.TextBuffer

-- | Memory-managed wrapper type.
newtype TextMark = TextMark (SP.ManagedPtr TextMark)
    deriving (Eq)

instance SP.ManagedPtrNewtype TextMark where
    toManagedPtr (TextMark p) = p

foreign import ccall "gtk_text_mark_get_type"
    c_gtk_text_mark_get_type :: IO B.Types.GType

instance B.Types.TypedObject TextMark where
    glibType = c_gtk_text_mark_get_type

instance B.Types.GObject TextMark

-- | Type class for types which can be safely cast to `TextMark`, for instance with `toTextMark`.
class (SP.GObject o, O.IsDescendantOf TextMark o) => IsTextMark o
instance (SP.GObject o, O.IsDescendantOf TextMark o) => IsTextMark o

instance O.HasParentTypes TextMark
type instance O.ParentTypes TextMark = '[GObject.Object.Object]

-- | Cast to `TextMark`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toTextMark :: (MIO.MonadIO m, IsTextMark o) => o -> m TextMark
toTextMark = MIO.liftIO . B.ManagedPtr.unsafeCastTo TextMark

-- | Convert 'TextMark' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe TextMark) where
    gvalueGType_ = c_gtk_text_mark_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr TextMark)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr TextMark)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject TextMark ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveTextMarkMethod (t :: Symbol) (o :: *) :: * where
    ResolveTextMarkMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveTextMarkMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveTextMarkMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveTextMarkMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveTextMarkMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveTextMarkMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveTextMarkMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveTextMarkMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveTextMarkMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveTextMarkMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveTextMarkMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveTextMarkMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveTextMarkMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveTextMarkMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveTextMarkMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveTextMarkMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveTextMarkMethod "getBuffer" o = TextMarkGetBufferMethodInfo
    ResolveTextMarkMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveTextMarkMethod "getDeleted" o = TextMarkGetDeletedMethodInfo
    ResolveTextMarkMethod "getLeftGravity" o = TextMarkGetLeftGravityMethodInfo
    ResolveTextMarkMethod "getName" o = TextMarkGetNameMethodInfo
    ResolveTextMarkMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveTextMarkMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveTextMarkMethod "getVisible" o = TextMarkGetVisibleMethodInfo
    ResolveTextMarkMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveTextMarkMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveTextMarkMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveTextMarkMethod "setVisible" o = TextMarkSetVisibleMethodInfo
    ResolveTextMarkMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveTextMarkMethod t TextMark, O.OverloadedMethod info TextMark p) => OL.IsLabel t (TextMark -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveTextMarkMethod t TextMark, O.OverloadedMethod info TextMark p, R.HasField t TextMark p) => R.HasField t TextMark p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveTextMarkMethod t TextMark, O.OverloadedMethodInfo info TextMark) => OL.IsLabel t (O.MethodProxy info TextMark) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- VVV Prop "left-gravity"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable,PropertyConstructOnly]
   -- Nullable: (Just False,Nothing)

-- | Get the value of the “@left-gravity@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textMark #leftGravity
-- @
getTextMarkLeftGravity :: (MonadIO m, IsTextMark o) => o -> m Bool
getTextMarkLeftGravity obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "left-gravity"

-- | Construct a `GValueConstruct` with valid value for the “@left-gravity@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextMarkLeftGravity :: (IsTextMark o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructTextMarkLeftGravity val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "left-gravity" val

#if defined(ENABLE_OVERLOADING)
data TextMarkLeftGravityPropertyInfo
instance AttrInfo TextMarkLeftGravityPropertyInfo where
    type AttrAllowedOps TextMarkLeftGravityPropertyInfo = '[ 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TextMarkLeftGravityPropertyInfo = IsTextMark
    type AttrSetTypeConstraint TextMarkLeftGravityPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint TextMarkLeftGravityPropertyInfo = (~) Bool
    type AttrTransferType TextMarkLeftGravityPropertyInfo = Bool
    type AttrGetType TextMarkLeftGravityPropertyInfo = Bool
    type AttrLabel TextMarkLeftGravityPropertyInfo = "left-gravity"
    type AttrOrigin TextMarkLeftGravityPropertyInfo = TextMark
    attrGet = getTextMarkLeftGravity
    attrSet = undefined
    attrTransfer _ v = do
        return v
    attrConstruct = constructTextMarkLeftGravity
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextMark.leftGravity"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextMark.html#g:attr:leftGravity"
        })
#endif

-- VVV Prop "name"
   -- Type: TBasicType TUTF8
   -- Flags: [PropertyReadable,PropertyWritable,PropertyConstructOnly]
   -- Nullable: (Just True,Nothing)

-- | Get the value of the “@name@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textMark #name
-- @
getTextMarkName :: (MonadIO m, IsTextMark o) => o -> m (Maybe T.Text)
getTextMarkName obj = MIO.liftIO $ B.Properties.getObjectPropertyString obj "name"

-- | Construct a `GValueConstruct` with valid value for the “@name@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextMarkName :: (IsTextMark o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructTextMarkName val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "name" (P.Just val)

#if defined(ENABLE_OVERLOADING)
data TextMarkNamePropertyInfo
instance AttrInfo TextMarkNamePropertyInfo where
    type AttrAllowedOps TextMarkNamePropertyInfo = '[ 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint TextMarkNamePropertyInfo = IsTextMark
    type AttrSetTypeConstraint TextMarkNamePropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint TextMarkNamePropertyInfo = (~) T.Text
    type AttrTransferType TextMarkNamePropertyInfo = T.Text
    type AttrGetType TextMarkNamePropertyInfo = (Maybe T.Text)
    type AttrLabel TextMarkNamePropertyInfo = "name"
    type AttrOrigin TextMarkNamePropertyInfo = TextMark
    attrGet = getTextMarkName
    attrSet = undefined
    attrTransfer _ v = do
        return v
    attrConstruct = constructTextMarkName
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextMark.name"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextMark.html#g:attr:name"
        })
#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList TextMark
type instance O.AttributeList TextMark = TextMarkAttributeList
type TextMarkAttributeList = ('[ '("leftGravity", TextMarkLeftGravityPropertyInfo), '("name", TextMarkNamePropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
textMarkLeftGravity :: AttrLabelProxy "leftGravity"
textMarkLeftGravity = AttrLabelProxy

textMarkName :: AttrLabelProxy "name"
textMarkName = AttrLabelProxy

#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList TextMark = TextMarkSignalList
type TextMarkSignalList = ('[ '("notify", GObject.Object.ObjectNotifySignalInfo)] :: [(Symbol, *)])

#endif

-- method TextMark::new
-- method type : Constructor
-- Args: [ Arg
--           { argCName = "name"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "mark name or %NULL" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "left_gravity"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "whether the mark should have left gravity"
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "TextMark" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_text_mark_new" gtk_text_mark_new :: 
    CString ->                              -- name : TBasicType TUTF8
    CInt ->                                 -- left_gravity : TBasicType TBoolean
    IO (Ptr TextMark)

-- | Creates a text mark. Add it to a buffer using 'GI.Gtk.Objects.TextBuffer.textBufferAddMark'.
-- If /@name@/ is 'P.Nothing', the mark is anonymous; otherwise, the mark can be
-- retrieved by name using 'GI.Gtk.Objects.TextBuffer.textBufferGetMark'. If a mark has left
-- gravity, and text is inserted at the mark’s current location, the mark
-- will be moved to the left of the newly-inserted text. If the mark has
-- right gravity (/@leftGravity@/ = 'P.False'), the mark will end up on the
-- right of newly-inserted text. The standard left-to-right cursor is a
-- mark with right gravity (when you type, the cursor stays on the right
-- side of the text you’re typing).
-- 
-- /Since: 2.12/
textMarkNew ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    Maybe (T.Text)
    -- ^ /@name@/: mark name or 'P.Nothing'
    -> Bool
    -- ^ /@leftGravity@/: whether the mark should have left gravity
    -> m TextMark
    -- ^ __Returns:__ new t'GI.Gtk.Objects.TextMark.TextMark'
textMarkNew name leftGravity = liftIO $ do
    maybeName <- case name of
        Nothing -> return nullPtr
        Just jName -> do
            jName' <- textToCString jName
            return jName'
    let leftGravity' = (fromIntegral . fromEnum) leftGravity
    result <- gtk_text_mark_new maybeName leftGravity'
    checkUnexpectedReturnNULL "textMarkNew" result
    result' <- (wrapObject TextMark) result
    freeMem maybeName
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method TextMark::get_buffer
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "mark"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextMark" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextMark" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "TextBuffer" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_text_mark_get_buffer" gtk_text_mark_get_buffer :: 
    Ptr TextMark ->                         -- mark : TInterface (Name {namespace = "Gtk", name = "TextMark"})
    IO (Ptr Gtk.TextBuffer.TextBuffer)

-- | Gets the buffer this mark is located inside,
-- or 'P.Nothing' if the mark is deleted.
textMarkGetBuffer ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextMark a) =>
    a
    -- ^ /@mark@/: a t'GI.Gtk.Objects.TextMark.TextMark'
    -> m Gtk.TextBuffer.TextBuffer
    -- ^ __Returns:__ the mark’s t'GI.Gtk.Objects.TextBuffer.TextBuffer'
textMarkGetBuffer mark = liftIO $ do
    mark' <- unsafeManagedPtrCastPtr mark
    result <- gtk_text_mark_get_buffer mark'
    checkUnexpectedReturnNULL "textMarkGetBuffer" result
    result' <- (newObject Gtk.TextBuffer.TextBuffer) result
    touchManagedPtr mark
    return result'

#if defined(ENABLE_OVERLOADING)
data TextMarkGetBufferMethodInfo
instance (signature ~ (m Gtk.TextBuffer.TextBuffer), MonadIO m, IsTextMark a) => O.OverloadedMethod TextMarkGetBufferMethodInfo a signature where
    overloadedMethod = textMarkGetBuffer

instance O.OverloadedMethodInfo TextMarkGetBufferMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextMark.textMarkGetBuffer",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextMark.html#v:textMarkGetBuffer"
        })


#endif

-- method TextMark::get_deleted
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "mark"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextMark" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextMark" , sinceVersion = Nothing }
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

foreign import ccall "gtk_text_mark_get_deleted" gtk_text_mark_get_deleted :: 
    Ptr TextMark ->                         -- mark : TInterface (Name {namespace = "Gtk", name = "TextMark"})
    IO CInt

-- | Returns 'P.True' if the mark has been removed from its buffer
-- with 'GI.Gtk.Objects.TextBuffer.textBufferDeleteMark'. See 'GI.Gtk.Objects.TextBuffer.textBufferAddMark'
-- for a way to add it to a buffer again.
textMarkGetDeleted ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextMark a) =>
    a
    -- ^ /@mark@/: a t'GI.Gtk.Objects.TextMark.TextMark'
    -> m Bool
    -- ^ __Returns:__ whether the mark is deleted
textMarkGetDeleted mark = liftIO $ do
    mark' <- unsafeManagedPtrCastPtr mark
    result <- gtk_text_mark_get_deleted mark'
    let result' = (/= 0) result
    touchManagedPtr mark
    return result'

#if defined(ENABLE_OVERLOADING)
data TextMarkGetDeletedMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsTextMark a) => O.OverloadedMethod TextMarkGetDeletedMethodInfo a signature where
    overloadedMethod = textMarkGetDeleted

instance O.OverloadedMethodInfo TextMarkGetDeletedMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextMark.textMarkGetDeleted",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextMark.html#v:textMarkGetDeleted"
        })


#endif

-- method TextMark::get_left_gravity
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "mark"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextMark" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextMark" , sinceVersion = Nothing }
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

foreign import ccall "gtk_text_mark_get_left_gravity" gtk_text_mark_get_left_gravity :: 
    Ptr TextMark ->                         -- mark : TInterface (Name {namespace = "Gtk", name = "TextMark"})
    IO CInt

-- | Determines whether the mark has left gravity.
textMarkGetLeftGravity ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextMark a) =>
    a
    -- ^ /@mark@/: a t'GI.Gtk.Objects.TextMark.TextMark'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the mark has left gravity, 'P.False' otherwise
textMarkGetLeftGravity mark = liftIO $ do
    mark' <- unsafeManagedPtrCastPtr mark
    result <- gtk_text_mark_get_left_gravity mark'
    let result' = (/= 0) result
    touchManagedPtr mark
    return result'

#if defined(ENABLE_OVERLOADING)
data TextMarkGetLeftGravityMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsTextMark a) => O.OverloadedMethod TextMarkGetLeftGravityMethodInfo a signature where
    overloadedMethod = textMarkGetLeftGravity

instance O.OverloadedMethodInfo TextMarkGetLeftGravityMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextMark.textMarkGetLeftGravity",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextMark.html#v:textMarkGetLeftGravity"
        })


#endif

-- method TextMark::get_name
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "mark"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextMark" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextMark" , sinceVersion = Nothing }
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

foreign import ccall "gtk_text_mark_get_name" gtk_text_mark_get_name :: 
    Ptr TextMark ->                         -- mark : TInterface (Name {namespace = "Gtk", name = "TextMark"})
    IO CString

-- | Returns the mark name; returns NULL for anonymous marks.
textMarkGetName ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextMark a) =>
    a
    -- ^ /@mark@/: a t'GI.Gtk.Objects.TextMark.TextMark'
    -> m (Maybe T.Text)
    -- ^ __Returns:__ mark name
textMarkGetName mark = liftIO $ do
    mark' <- unsafeManagedPtrCastPtr mark
    result <- gtk_text_mark_get_name mark'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- cstringToText result'
        return result''
    touchManagedPtr mark
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data TextMarkGetNameMethodInfo
instance (signature ~ (m (Maybe T.Text)), MonadIO m, IsTextMark a) => O.OverloadedMethod TextMarkGetNameMethodInfo a signature where
    overloadedMethod = textMarkGetName

instance O.OverloadedMethodInfo TextMarkGetNameMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextMark.textMarkGetName",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextMark.html#v:textMarkGetName"
        })


#endif

-- method TextMark::get_visible
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "mark"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextMark" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextMark" , sinceVersion = Nothing }
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

foreign import ccall "gtk_text_mark_get_visible" gtk_text_mark_get_visible :: 
    Ptr TextMark ->                         -- mark : TInterface (Name {namespace = "Gtk", name = "TextMark"})
    IO CInt

-- | Returns 'P.True' if the mark is visible (i.e. a cursor is displayed
-- for it).
textMarkGetVisible ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextMark a) =>
    a
    -- ^ /@mark@/: a t'GI.Gtk.Objects.TextMark.TextMark'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if visible
textMarkGetVisible mark = liftIO $ do
    mark' <- unsafeManagedPtrCastPtr mark
    result <- gtk_text_mark_get_visible mark'
    let result' = (/= 0) result
    touchManagedPtr mark
    return result'

#if defined(ENABLE_OVERLOADING)
data TextMarkGetVisibleMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsTextMark a) => O.OverloadedMethod TextMarkGetVisibleMethodInfo a signature where
    overloadedMethod = textMarkGetVisible

instance O.OverloadedMethodInfo TextMarkGetVisibleMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextMark.textMarkGetVisible",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextMark.html#v:textMarkGetVisible"
        })


#endif

-- method TextMark::set_visible
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "mark"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextMark" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextMark" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "visibility of mark" , sinceVersion = Nothing }
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

foreign import ccall "gtk_text_mark_set_visible" gtk_text_mark_set_visible :: 
    Ptr TextMark ->                         -- mark : TInterface (Name {namespace = "Gtk", name = "TextMark"})
    CInt ->                                 -- setting : TBasicType TBoolean
    IO ()

-- | Sets the visibility of /@mark@/; the insertion point is normally
-- visible, i.e. you can see it as a vertical bar. Also, the text
-- widget uses a visible mark to indicate where a drop will occur when
-- dragging-and-dropping text. Most other marks are not visible.
-- Marks are not visible by default.
textMarkSetVisible ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextMark a) =>
    a
    -- ^ /@mark@/: a t'GI.Gtk.Objects.TextMark.TextMark'
    -> Bool
    -- ^ /@setting@/: visibility of mark
    -> m ()
textMarkSetVisible mark setting = liftIO $ do
    mark' <- unsafeManagedPtrCastPtr mark
    let setting' = (fromIntegral . fromEnum) setting
    gtk_text_mark_set_visible mark' setting'
    touchManagedPtr mark
    return ()

#if defined(ENABLE_OVERLOADING)
data TextMarkSetVisibleMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsTextMark a) => O.OverloadedMethod TextMarkSetVisibleMethodInfo a signature where
    overloadedMethod = textMarkSetVisible

instance O.OverloadedMethodInfo TextMarkSetVisibleMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextMark.textMarkSetVisible",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextMark.html#v:textMarkSetVisible"
        })


#endif


