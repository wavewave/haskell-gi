{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- /No description available in the introspection data./

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Structs.TextAppearance
    ( 

-- * Exported types
    TextAppearance(..)                      ,
    newZeroTextAppearance                   ,


 -- * Methods

#if defined(ENABLE_OVERLOADING)
    ResolveTextAppearanceMethod             ,
#endif



 -- * Properties


-- ** bgColor #attr:bgColor#
-- | Background t'GI.Gdk.Structs.Color.Color'.

    getTextAppearanceBgColor                ,
#if defined(ENABLE_OVERLOADING)
    textAppearance_bgColor                  ,
#endif


-- ** drawBg #attr:drawBg#
-- | Whether to use background-related values; this is
--   irrelevant for the values struct when in a tag, but is used for
--   the composite values struct; it’s true if any of the tags being
--   composited had background stuff set.

    getTextAppearanceDrawBg                 ,
    setTextAppearanceDrawBg                 ,
#if defined(ENABLE_OVERLOADING)
    textAppearance_drawBg                   ,
#endif


-- ** fgColor #attr:fgColor#
-- | Foreground t'GI.Gdk.Structs.Color.Color'.

    getTextAppearanceFgColor                ,
#if defined(ENABLE_OVERLOADING)
    textAppearance_fgColor                  ,
#endif


-- ** insideSelection #attr:insideSelection#
-- | This are only used when we are actually laying
--   out and rendering a paragraph; not when a t'GI.Gtk.Structs.TextAppearance.TextAppearance' is
--   part of a t'GI.Gtk.Structs.TextAttributes.TextAttributes'.

    getTextAppearanceInsideSelection        ,
    setTextAppearanceInsideSelection        ,
#if defined(ENABLE_OVERLOADING)
    textAppearance_insideSelection          ,
#endif


-- ** isText #attr:isText#
-- | This are only used when we are actually laying
--   out and rendering a paragraph; not when a t'GI.Gtk.Structs.TextAppearance.TextAppearance' is
--   part of a t'GI.Gtk.Structs.TextAttributes.TextAttributes'.

    getTextAppearanceIsText                 ,
    setTextAppearanceIsText                 ,
#if defined(ENABLE_OVERLOADING)
    textAppearance_isText                   ,
#endif


-- ** rise #attr:rise#
-- | Super\/subscript rise, can be negative.

    getTextAppearanceRise                   ,
    setTextAppearanceRise                   ,
#if defined(ENABLE_OVERLOADING)
    textAppearance_rise                     ,
#endif


-- ** strikethrough #attr:strikethrough#
-- | Strikethrough style

    getTextAppearanceStrikethrough          ,
    setTextAppearanceStrikethrough          ,
#if defined(ENABLE_OVERLOADING)
    textAppearance_strikethrough            ,
#endif


-- ** underline #attr:underline#
-- | t'GI.Pango.Enums.Underline'

    getTextAppearanceUnderline              ,
    setTextAppearanceUnderline              ,
#if defined(ENABLE_OVERLOADING)
    textAppearance_underline                ,
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

import qualified GI.Gdk.Structs.Color as Gdk.Color

-- | Memory-managed wrapper type.
newtype TextAppearance = TextAppearance (SP.ManagedPtr TextAppearance)
    deriving (Eq)

instance SP.ManagedPtrNewtype TextAppearance where
    toManagedPtr (TextAppearance p) = p

instance BoxedPtr TextAppearance where
    boxedPtrCopy = \p -> B.ManagedPtr.withManagedPtr p (copyBytes 48 >=> B.ManagedPtr.wrapPtr TextAppearance)
    boxedPtrFree = \x -> SP.withManagedPtr x SP.freeMem
instance CallocPtr TextAppearance where
    boxedPtrCalloc = callocBytes 48


-- | Construct a `TextAppearance` struct initialized to zero.
newZeroTextAppearance :: MonadIO m => m TextAppearance
newZeroTextAppearance = liftIO $ boxedPtrCalloc >>= wrapPtr TextAppearance

instance tag ~ 'AttrSet => Constructible TextAppearance tag where
    new _ attrs = do
        o <- newZeroTextAppearance
        GI.Attributes.set o attrs
        return o


-- | Get the value of the “@bg_color@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textAppearance #bgColor
-- @
getTextAppearanceBgColor :: MonadIO m => TextAppearance -> m Gdk.Color.Color
getTextAppearanceBgColor s = liftIO $ withManagedPtr s $ \ptr -> do
    let val = ptr `plusPtr` 0 :: (Ptr Gdk.Color.Color)
    val' <- (newBoxed Gdk.Color.Color) val
    return val'

#if defined(ENABLE_OVERLOADING)
data TextAppearanceBgColorFieldInfo
instance AttrInfo TextAppearanceBgColorFieldInfo where
    type AttrBaseTypeConstraint TextAppearanceBgColorFieldInfo = (~) TextAppearance
    type AttrAllowedOps TextAppearanceBgColorFieldInfo = '[ 'AttrGet]
    type AttrSetTypeConstraint TextAppearanceBgColorFieldInfo = (~) (Ptr Gdk.Color.Color)
    type AttrTransferTypeConstraint TextAppearanceBgColorFieldInfo = (~)(Ptr Gdk.Color.Color)
    type AttrTransferType TextAppearanceBgColorFieldInfo = (Ptr Gdk.Color.Color)
    type AttrGetType TextAppearanceBgColorFieldInfo = Gdk.Color.Color
    type AttrLabel TextAppearanceBgColorFieldInfo = "bg_color"
    type AttrOrigin TextAppearanceBgColorFieldInfo = TextAppearance
    attrGet = getTextAppearanceBgColor
    attrSet = undefined
    attrConstruct = undefined
    attrClear = undefined
    attrTransfer = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextAppearance.bgColor"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextAppearance.html#g:attr:bgColor"
        })

textAppearance_bgColor :: AttrLabelProxy "bgColor"
textAppearance_bgColor = AttrLabelProxy

#endif


-- | Get the value of the “@fg_color@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textAppearance #fgColor
-- @
getTextAppearanceFgColor :: MonadIO m => TextAppearance -> m Gdk.Color.Color
getTextAppearanceFgColor s = liftIO $ withManagedPtr s $ \ptr -> do
    let val = ptr `plusPtr` 12 :: (Ptr Gdk.Color.Color)
    val' <- (newBoxed Gdk.Color.Color) val
    return val'

#if defined(ENABLE_OVERLOADING)
data TextAppearanceFgColorFieldInfo
instance AttrInfo TextAppearanceFgColorFieldInfo where
    type AttrBaseTypeConstraint TextAppearanceFgColorFieldInfo = (~) TextAppearance
    type AttrAllowedOps TextAppearanceFgColorFieldInfo = '[ 'AttrGet]
    type AttrSetTypeConstraint TextAppearanceFgColorFieldInfo = (~) (Ptr Gdk.Color.Color)
    type AttrTransferTypeConstraint TextAppearanceFgColorFieldInfo = (~)(Ptr Gdk.Color.Color)
    type AttrTransferType TextAppearanceFgColorFieldInfo = (Ptr Gdk.Color.Color)
    type AttrGetType TextAppearanceFgColorFieldInfo = Gdk.Color.Color
    type AttrLabel TextAppearanceFgColorFieldInfo = "fg_color"
    type AttrOrigin TextAppearanceFgColorFieldInfo = TextAppearance
    attrGet = getTextAppearanceFgColor
    attrSet = undefined
    attrConstruct = undefined
    attrClear = undefined
    attrTransfer = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextAppearance.fgColor"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextAppearance.html#g:attr:fgColor"
        })

textAppearance_fgColor :: AttrLabelProxy "fgColor"
textAppearance_fgColor = AttrLabelProxy

#endif


-- | Get the value of the “@rise@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textAppearance #rise
-- @
getTextAppearanceRise :: MonadIO m => TextAppearance -> m Int32
getTextAppearanceRise s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 24) :: IO Int32
    return val

-- | Set the value of the “@rise@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textAppearance [ #rise 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextAppearanceRise :: MonadIO m => TextAppearance -> Int32 -> m ()
setTextAppearanceRise s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 24) (val :: Int32)

#if defined(ENABLE_OVERLOADING)
data TextAppearanceRiseFieldInfo
instance AttrInfo TextAppearanceRiseFieldInfo where
    type AttrBaseTypeConstraint TextAppearanceRiseFieldInfo = (~) TextAppearance
    type AttrAllowedOps TextAppearanceRiseFieldInfo = '[ 'AttrSet, 'AttrGet]
    type AttrSetTypeConstraint TextAppearanceRiseFieldInfo = (~) Int32
    type AttrTransferTypeConstraint TextAppearanceRiseFieldInfo = (~)Int32
    type AttrTransferType TextAppearanceRiseFieldInfo = Int32
    type AttrGetType TextAppearanceRiseFieldInfo = Int32
    type AttrLabel TextAppearanceRiseFieldInfo = "rise"
    type AttrOrigin TextAppearanceRiseFieldInfo = TextAppearance
    attrGet = getTextAppearanceRise
    attrSet = setTextAppearanceRise
    attrConstruct = undefined
    attrClear = undefined
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextAppearance.rise"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextAppearance.html#g:attr:rise"
        })

textAppearance_rise :: AttrLabelProxy "rise"
textAppearance_rise = AttrLabelProxy

#endif


-- | Get the value of the “@underline@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textAppearance #underline
-- @
getTextAppearanceUnderline :: MonadIO m => TextAppearance -> m Word32
getTextAppearanceUnderline s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 28) :: IO Word32
    return val

-- | Set the value of the “@underline@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textAppearance [ #underline 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextAppearanceUnderline :: MonadIO m => TextAppearance -> Word32 -> m ()
setTextAppearanceUnderline s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 28) (val :: Word32)

#if defined(ENABLE_OVERLOADING)
data TextAppearanceUnderlineFieldInfo
instance AttrInfo TextAppearanceUnderlineFieldInfo where
    type AttrBaseTypeConstraint TextAppearanceUnderlineFieldInfo = (~) TextAppearance
    type AttrAllowedOps TextAppearanceUnderlineFieldInfo = '[ 'AttrSet, 'AttrGet]
    type AttrSetTypeConstraint TextAppearanceUnderlineFieldInfo = (~) Word32
    type AttrTransferTypeConstraint TextAppearanceUnderlineFieldInfo = (~)Word32
    type AttrTransferType TextAppearanceUnderlineFieldInfo = Word32
    type AttrGetType TextAppearanceUnderlineFieldInfo = Word32
    type AttrLabel TextAppearanceUnderlineFieldInfo = "underline"
    type AttrOrigin TextAppearanceUnderlineFieldInfo = TextAppearance
    attrGet = getTextAppearanceUnderline
    attrSet = setTextAppearanceUnderline
    attrConstruct = undefined
    attrClear = undefined
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextAppearance.underline"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextAppearance.html#g:attr:underline"
        })

textAppearance_underline :: AttrLabelProxy "underline"
textAppearance_underline = AttrLabelProxy

#endif


-- | Get the value of the “@strikethrough@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textAppearance #strikethrough
-- @
getTextAppearanceStrikethrough :: MonadIO m => TextAppearance -> m Word32
getTextAppearanceStrikethrough s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 32) :: IO Word32
    return val

-- | Set the value of the “@strikethrough@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textAppearance [ #strikethrough 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextAppearanceStrikethrough :: MonadIO m => TextAppearance -> Word32 -> m ()
setTextAppearanceStrikethrough s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 32) (val :: Word32)

#if defined(ENABLE_OVERLOADING)
data TextAppearanceStrikethroughFieldInfo
instance AttrInfo TextAppearanceStrikethroughFieldInfo where
    type AttrBaseTypeConstraint TextAppearanceStrikethroughFieldInfo = (~) TextAppearance
    type AttrAllowedOps TextAppearanceStrikethroughFieldInfo = '[ 'AttrSet, 'AttrGet]
    type AttrSetTypeConstraint TextAppearanceStrikethroughFieldInfo = (~) Word32
    type AttrTransferTypeConstraint TextAppearanceStrikethroughFieldInfo = (~)Word32
    type AttrTransferType TextAppearanceStrikethroughFieldInfo = Word32
    type AttrGetType TextAppearanceStrikethroughFieldInfo = Word32
    type AttrLabel TextAppearanceStrikethroughFieldInfo = "strikethrough"
    type AttrOrigin TextAppearanceStrikethroughFieldInfo = TextAppearance
    attrGet = getTextAppearanceStrikethrough
    attrSet = setTextAppearanceStrikethrough
    attrConstruct = undefined
    attrClear = undefined
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextAppearance.strikethrough"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextAppearance.html#g:attr:strikethrough"
        })

textAppearance_strikethrough :: AttrLabelProxy "strikethrough"
textAppearance_strikethrough = AttrLabelProxy

#endif


-- | Get the value of the “@draw_bg@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textAppearance #drawBg
-- @
getTextAppearanceDrawBg :: MonadIO m => TextAppearance -> m Word32
getTextAppearanceDrawBg s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 36) :: IO Word32
    return val

-- | Set the value of the “@draw_bg@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textAppearance [ #drawBg 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextAppearanceDrawBg :: MonadIO m => TextAppearance -> Word32 -> m ()
setTextAppearanceDrawBg s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 36) (val :: Word32)

#if defined(ENABLE_OVERLOADING)
data TextAppearanceDrawBgFieldInfo
instance AttrInfo TextAppearanceDrawBgFieldInfo where
    type AttrBaseTypeConstraint TextAppearanceDrawBgFieldInfo = (~) TextAppearance
    type AttrAllowedOps TextAppearanceDrawBgFieldInfo = '[ 'AttrSet, 'AttrGet]
    type AttrSetTypeConstraint TextAppearanceDrawBgFieldInfo = (~) Word32
    type AttrTransferTypeConstraint TextAppearanceDrawBgFieldInfo = (~)Word32
    type AttrTransferType TextAppearanceDrawBgFieldInfo = Word32
    type AttrGetType TextAppearanceDrawBgFieldInfo = Word32
    type AttrLabel TextAppearanceDrawBgFieldInfo = "draw_bg"
    type AttrOrigin TextAppearanceDrawBgFieldInfo = TextAppearance
    attrGet = getTextAppearanceDrawBg
    attrSet = setTextAppearanceDrawBg
    attrConstruct = undefined
    attrClear = undefined
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextAppearance.drawBg"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextAppearance.html#g:attr:drawBg"
        })

textAppearance_drawBg :: AttrLabelProxy "drawBg"
textAppearance_drawBg = AttrLabelProxy

#endif


-- | Get the value of the “@inside_selection@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textAppearance #insideSelection
-- @
getTextAppearanceInsideSelection :: MonadIO m => TextAppearance -> m Word32
getTextAppearanceInsideSelection s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 40) :: IO Word32
    return val

-- | Set the value of the “@inside_selection@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textAppearance [ #insideSelection 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextAppearanceInsideSelection :: MonadIO m => TextAppearance -> Word32 -> m ()
setTextAppearanceInsideSelection s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 40) (val :: Word32)

#if defined(ENABLE_OVERLOADING)
data TextAppearanceInsideSelectionFieldInfo
instance AttrInfo TextAppearanceInsideSelectionFieldInfo where
    type AttrBaseTypeConstraint TextAppearanceInsideSelectionFieldInfo = (~) TextAppearance
    type AttrAllowedOps TextAppearanceInsideSelectionFieldInfo = '[ 'AttrSet, 'AttrGet]
    type AttrSetTypeConstraint TextAppearanceInsideSelectionFieldInfo = (~) Word32
    type AttrTransferTypeConstraint TextAppearanceInsideSelectionFieldInfo = (~)Word32
    type AttrTransferType TextAppearanceInsideSelectionFieldInfo = Word32
    type AttrGetType TextAppearanceInsideSelectionFieldInfo = Word32
    type AttrLabel TextAppearanceInsideSelectionFieldInfo = "inside_selection"
    type AttrOrigin TextAppearanceInsideSelectionFieldInfo = TextAppearance
    attrGet = getTextAppearanceInsideSelection
    attrSet = setTextAppearanceInsideSelection
    attrConstruct = undefined
    attrClear = undefined
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextAppearance.insideSelection"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextAppearance.html#g:attr:insideSelection"
        })

textAppearance_insideSelection :: AttrLabelProxy "insideSelection"
textAppearance_insideSelection = AttrLabelProxy

#endif


-- | Get the value of the “@is_text@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textAppearance #isText
-- @
getTextAppearanceIsText :: MonadIO m => TextAppearance -> m Word32
getTextAppearanceIsText s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 44) :: IO Word32
    return val

-- | Set the value of the “@is_text@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textAppearance [ #isText 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextAppearanceIsText :: MonadIO m => TextAppearance -> Word32 -> m ()
setTextAppearanceIsText s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 44) (val :: Word32)

#if defined(ENABLE_OVERLOADING)
data TextAppearanceIsTextFieldInfo
instance AttrInfo TextAppearanceIsTextFieldInfo where
    type AttrBaseTypeConstraint TextAppearanceIsTextFieldInfo = (~) TextAppearance
    type AttrAllowedOps TextAppearanceIsTextFieldInfo = '[ 'AttrSet, 'AttrGet]
    type AttrSetTypeConstraint TextAppearanceIsTextFieldInfo = (~) Word32
    type AttrTransferTypeConstraint TextAppearanceIsTextFieldInfo = (~)Word32
    type AttrTransferType TextAppearanceIsTextFieldInfo = Word32
    type AttrGetType TextAppearanceIsTextFieldInfo = Word32
    type AttrLabel TextAppearanceIsTextFieldInfo = "is_text"
    type AttrOrigin TextAppearanceIsTextFieldInfo = TextAppearance
    attrGet = getTextAppearanceIsText
    attrSet = setTextAppearanceIsText
    attrConstruct = undefined
    attrClear = undefined
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextAppearance.isText"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextAppearance.html#g:attr:isText"
        })

textAppearance_isText :: AttrLabelProxy "isText"
textAppearance_isText = AttrLabelProxy

#endif



#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList TextAppearance
type instance O.AttributeList TextAppearance = TextAppearanceAttributeList
type TextAppearanceAttributeList = ('[ '("bgColor", TextAppearanceBgColorFieldInfo), '("fgColor", TextAppearanceFgColorFieldInfo), '("rise", TextAppearanceRiseFieldInfo), '("underline", TextAppearanceUnderlineFieldInfo), '("strikethrough", TextAppearanceStrikethroughFieldInfo), '("drawBg", TextAppearanceDrawBgFieldInfo), '("insideSelection", TextAppearanceInsideSelectionFieldInfo), '("isText", TextAppearanceIsTextFieldInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
type family ResolveTextAppearanceMethod (t :: Symbol) (o :: *) :: * where
    ResolveTextAppearanceMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveTextAppearanceMethod t TextAppearance, O.OverloadedMethod info TextAppearance p) => OL.IsLabel t (TextAppearance -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveTextAppearanceMethod t TextAppearance, O.OverloadedMethod info TextAppearance p, R.HasField t TextAppearance p) => R.HasField t TextAppearance p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveTextAppearanceMethod t TextAppearance, O.OverloadedMethodInfo info TextAppearance) => OL.IsLabel t (O.MethodProxy info TextAppearance) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif


