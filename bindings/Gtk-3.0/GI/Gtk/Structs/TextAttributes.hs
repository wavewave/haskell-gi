{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- Using t'GI.Gtk.Structs.TextAttributes.TextAttributes' directly should rarely be necessary.
-- It’s primarily useful with 'GI.Gtk.Structs.TextIter.textIterGetAttributes'.
-- As with most GTK+ structs, the fields in this struct should only
-- be read, never modified directly.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Structs.TextAttributes
    ( 

-- * Exported types
    TextAttributes(..)                      ,
    newZeroTextAttributes                   ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [copy]("GI.Gtk.Structs.TextAttributes#g:method:copy"), [copyValues]("GI.Gtk.Structs.TextAttributes#g:method:copyValues"), [ref]("GI.Gtk.Structs.TextAttributes#g:method:ref"), [unref]("GI.Gtk.Structs.TextAttributes#g:method:unref").
-- 
-- ==== Getters
-- /None/.
-- 
-- ==== Setters
-- /None/.

#if defined(ENABLE_OVERLOADING)
    ResolveTextAttributesMethod             ,
#endif

-- ** copy #method:copy#

#if defined(ENABLE_OVERLOADING)
    TextAttributesCopyMethodInfo            ,
#endif
    textAttributesCopy                      ,


-- ** copyValues #method:copyValues#

#if defined(ENABLE_OVERLOADING)
    TextAttributesCopyValuesMethodInfo      ,
#endif
    textAttributesCopyValues                ,


-- ** new #method:new#

    textAttributesNew                       ,


-- ** ref #method:ref#

#if defined(ENABLE_OVERLOADING)
    TextAttributesRefMethodInfo             ,
#endif
    textAttributesRef                       ,


-- ** unref #method:unref#

#if defined(ENABLE_OVERLOADING)
    TextAttributesUnrefMethodInfo           ,
#endif
    textAttributesUnref                     ,




 -- * Properties


-- ** appearance #attr:appearance#
-- | t'GI.Gtk.Structs.TextAppearance.TextAppearance' for text.

    getTextAttributesAppearance             ,
#if defined(ENABLE_OVERLOADING)
    textAttributes_appearance               ,
#endif


-- ** bgFullHeight #attr:bgFullHeight#
-- | Background is fit to full line height rather than
--    baseline +\/- ascent\/descent (font height).

    getTextAttributesBgFullHeight           ,
    setTextAttributesBgFullHeight           ,
#if defined(ENABLE_OVERLOADING)
    textAttributes_bgFullHeight             ,
#endif


-- ** direction #attr:direction#
-- | t'GI.Gtk.Enums.TextDirection' for text.

    getTextAttributesDirection              ,
    setTextAttributesDirection              ,
#if defined(ENABLE_OVERLOADING)
    textAttributes_direction                ,
#endif


-- ** editable #attr:editable#
-- | Can edit this text.

    getTextAttributesEditable               ,
    setTextAttributesEditable               ,
#if defined(ENABLE_OVERLOADING)
    textAttributes_editable                 ,
#endif


-- ** font #attr:font#
-- | t'GI.Pango.Structs.FontDescription.FontDescription' for text.

    clearTextAttributesFont                 ,
    getTextAttributesFont                   ,
    setTextAttributesFont                   ,
#if defined(ENABLE_OVERLOADING)
    textAttributes_font                     ,
#endif


-- ** fontScale #attr:fontScale#
-- | Font scale factor.

    getTextAttributesFontScale              ,
    setTextAttributesFontScale              ,
#if defined(ENABLE_OVERLOADING)
    textAttributes_fontScale                ,
#endif


-- ** indent #attr:indent#
-- | Amount to indent the paragraph, in pixels.

    getTextAttributesIndent                 ,
    setTextAttributesIndent                 ,
#if defined(ENABLE_OVERLOADING)
    textAttributes_indent                   ,
#endif


-- ** invisible #attr:invisible#
-- | Hide the text.

    getTextAttributesInvisible              ,
    setTextAttributesInvisible              ,
#if defined(ENABLE_OVERLOADING)
    textAttributes_invisible                ,
#endif


-- ** justification #attr:justification#
-- | t'GI.Gtk.Enums.Justification' for text.

    getTextAttributesJustification          ,
    setTextAttributesJustification          ,
#if defined(ENABLE_OVERLOADING)
    textAttributes_justification            ,
#endif


-- ** language #attr:language#
-- | t'GI.Pango.Structs.Language.Language' for text.

    clearTextAttributesLanguage             ,
    getTextAttributesLanguage               ,
    setTextAttributesLanguage               ,
#if defined(ENABLE_OVERLOADING)
    textAttributes_language                 ,
#endif


-- ** leftMargin #attr:leftMargin#
-- | Width of the left margin in pixels.

    getTextAttributesLeftMargin             ,
    setTextAttributesLeftMargin             ,
#if defined(ENABLE_OVERLOADING)
    textAttributes_leftMargin               ,
#endif


-- ** letterSpacing #attr:letterSpacing#
-- | Extra space to insert between graphemes, in Pango units

    getTextAttributesLetterSpacing          ,
    setTextAttributesLetterSpacing          ,
#if defined(ENABLE_OVERLOADING)
    textAttributes_letterSpacing            ,
#endif


-- ** noFallback #attr:noFallback#
-- | Whether to disable font fallback.

    getTextAttributesNoFallback             ,
    setTextAttributesNoFallback             ,
#if defined(ENABLE_OVERLOADING)
    textAttributes_noFallback               ,
#endif


-- ** pixelsAboveLines #attr:pixelsAboveLines#
-- | Pixels of blank space above paragraphs.

    getTextAttributesPixelsAboveLines       ,
    setTextAttributesPixelsAboveLines       ,
#if defined(ENABLE_OVERLOADING)
    textAttributes_pixelsAboveLines         ,
#endif


-- ** pixelsBelowLines #attr:pixelsBelowLines#
-- | Pixels of blank space below paragraphs.

    getTextAttributesPixelsBelowLines       ,
    setTextAttributesPixelsBelowLines       ,
#if defined(ENABLE_OVERLOADING)
    textAttributes_pixelsBelowLines         ,
#endif


-- ** pixelsInsideWrap #attr:pixelsInsideWrap#
-- | Pixels of blank space between wrapped lines in
--   a paragraph.

    getTextAttributesPixelsInsideWrap       ,
    setTextAttributesPixelsInsideWrap       ,
#if defined(ENABLE_OVERLOADING)
    textAttributes_pixelsInsideWrap         ,
#endif


-- ** rightMargin #attr:rightMargin#
-- | Width of the right margin in pixels.

    getTextAttributesRightMargin            ,
    setTextAttributesRightMargin            ,
#if defined(ENABLE_OVERLOADING)
    textAttributes_rightMargin              ,
#endif


-- ** tabs #attr:tabs#
-- | Custom t'GI.Pango.Structs.TabArray.TabArray' for this text.

    clearTextAttributesTabs                 ,
    getTextAttributesTabs                   ,
    setTextAttributesTabs                   ,
#if defined(ENABLE_OVERLOADING)
    textAttributes_tabs                     ,
#endif


-- ** wrapMode #attr:wrapMode#
-- | t'GI.Gtk.Enums.WrapMode' for text.

    getTextAttributesWrapMode               ,
    setTextAttributesWrapMode               ,
#if defined(ENABLE_OVERLOADING)
    textAttributes_wrapMode                 ,
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

import {-# SOURCE #-} qualified GI.Gtk.Enums as Gtk.Enums
import {-# SOURCE #-} qualified GI.Gtk.Structs.TextAppearance as Gtk.TextAppearance
import qualified GI.Pango.Structs.FontDescription as Pango.FontDescription
import qualified GI.Pango.Structs.Language as Pango.Language
import qualified GI.Pango.Structs.TabArray as Pango.TabArray

-- | Memory-managed wrapper type.
newtype TextAttributes = TextAttributes (SP.ManagedPtr TextAttributes)
    deriving (Eq)

instance SP.ManagedPtrNewtype TextAttributes where
    toManagedPtr (TextAttributes p) = p

foreign import ccall "gtk_text_attributes_get_type" c_gtk_text_attributes_get_type :: 
    IO GType

type instance O.ParentTypes TextAttributes = '[]
instance O.HasParentTypes TextAttributes

instance B.Types.TypedObject TextAttributes where
    glibType = c_gtk_text_attributes_get_type

instance B.Types.GBoxed TextAttributes

-- | Convert 'TextAttributes' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe TextAttributes) where
    gvalueGType_ = c_gtk_text_attributes_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_boxed gv (FP.nullPtr :: FP.Ptr TextAttributes)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_boxed gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_boxed gv :: IO (Ptr TextAttributes)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newBoxed TextAttributes ptr
        else return P.Nothing
        
    

-- | Construct a `TextAttributes` struct initialized to zero.
newZeroTextAttributes :: MonadIO m => m TextAttributes
newZeroTextAttributes = liftIO $ callocBoxedBytes 168 >>= wrapBoxed TextAttributes

instance tag ~ 'AttrSet => Constructible TextAttributes tag where
    new _ attrs = do
        o <- newZeroTextAttributes
        GI.Attributes.set o attrs
        return o


-- | Get the value of the “@appearance@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textAttributes #appearance
-- @
getTextAttributesAppearance :: MonadIO m => TextAttributes -> m Gtk.TextAppearance.TextAppearance
getTextAttributesAppearance s = liftIO $ withManagedPtr s $ \ptr -> do
    let val = ptr `plusPtr` 4 :: (Ptr Gtk.TextAppearance.TextAppearance)
    val' <- (newPtr Gtk.TextAppearance.TextAppearance) val
    return val'

#if defined(ENABLE_OVERLOADING)
data TextAttributesAppearanceFieldInfo
instance AttrInfo TextAttributesAppearanceFieldInfo where
    type AttrBaseTypeConstraint TextAttributesAppearanceFieldInfo = (~) TextAttributes
    type AttrAllowedOps TextAttributesAppearanceFieldInfo = '[ 'AttrGet]
    type AttrSetTypeConstraint TextAttributesAppearanceFieldInfo = (~) (Ptr Gtk.TextAppearance.TextAppearance)
    type AttrTransferTypeConstraint TextAttributesAppearanceFieldInfo = (~)(Ptr Gtk.TextAppearance.TextAppearance)
    type AttrTransferType TextAttributesAppearanceFieldInfo = (Ptr Gtk.TextAppearance.TextAppearance)
    type AttrGetType TextAttributesAppearanceFieldInfo = Gtk.TextAppearance.TextAppearance
    type AttrLabel TextAttributesAppearanceFieldInfo = "appearance"
    type AttrOrigin TextAttributesAppearanceFieldInfo = TextAttributes
    attrGet = getTextAttributesAppearance
    attrSet = undefined
    attrConstruct = undefined
    attrClear = undefined
    attrTransfer = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextAttributes.appearance"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextAttributes.html#g:attr:appearance"
        })

textAttributes_appearance :: AttrLabelProxy "appearance"
textAttributes_appearance = AttrLabelProxy

#endif


-- | Get the value of the “@justification@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textAttributes #justification
-- @
getTextAttributesJustification :: MonadIO m => TextAttributes -> m Gtk.Enums.Justification
getTextAttributesJustification s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 52) :: IO CUInt
    let val' = (toEnum . fromIntegral) val
    return val'

-- | Set the value of the “@justification@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textAttributes [ #justification 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextAttributesJustification :: MonadIO m => TextAttributes -> Gtk.Enums.Justification -> m ()
setTextAttributesJustification s val = liftIO $ withManagedPtr s $ \ptr -> do
    let val' = (fromIntegral . fromEnum) val
    poke (ptr `plusPtr` 52) (val' :: CUInt)

#if defined(ENABLE_OVERLOADING)
data TextAttributesJustificationFieldInfo
instance AttrInfo TextAttributesJustificationFieldInfo where
    type AttrBaseTypeConstraint TextAttributesJustificationFieldInfo = (~) TextAttributes
    type AttrAllowedOps TextAttributesJustificationFieldInfo = '[ 'AttrSet, 'AttrGet]
    type AttrSetTypeConstraint TextAttributesJustificationFieldInfo = (~) Gtk.Enums.Justification
    type AttrTransferTypeConstraint TextAttributesJustificationFieldInfo = (~)Gtk.Enums.Justification
    type AttrTransferType TextAttributesJustificationFieldInfo = Gtk.Enums.Justification
    type AttrGetType TextAttributesJustificationFieldInfo = Gtk.Enums.Justification
    type AttrLabel TextAttributesJustificationFieldInfo = "justification"
    type AttrOrigin TextAttributesJustificationFieldInfo = TextAttributes
    attrGet = getTextAttributesJustification
    attrSet = setTextAttributesJustification
    attrConstruct = undefined
    attrClear = undefined
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextAttributes.justification"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextAttributes.html#g:attr:justification"
        })

textAttributes_justification :: AttrLabelProxy "justification"
textAttributes_justification = AttrLabelProxy

#endif


-- | Get the value of the “@direction@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textAttributes #direction
-- @
getTextAttributesDirection :: MonadIO m => TextAttributes -> m Gtk.Enums.TextDirection
getTextAttributesDirection s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 56) :: IO CUInt
    let val' = (toEnum . fromIntegral) val
    return val'

-- | Set the value of the “@direction@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textAttributes [ #direction 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextAttributesDirection :: MonadIO m => TextAttributes -> Gtk.Enums.TextDirection -> m ()
setTextAttributesDirection s val = liftIO $ withManagedPtr s $ \ptr -> do
    let val' = (fromIntegral . fromEnum) val
    poke (ptr `plusPtr` 56) (val' :: CUInt)

#if defined(ENABLE_OVERLOADING)
data TextAttributesDirectionFieldInfo
instance AttrInfo TextAttributesDirectionFieldInfo where
    type AttrBaseTypeConstraint TextAttributesDirectionFieldInfo = (~) TextAttributes
    type AttrAllowedOps TextAttributesDirectionFieldInfo = '[ 'AttrSet, 'AttrGet]
    type AttrSetTypeConstraint TextAttributesDirectionFieldInfo = (~) Gtk.Enums.TextDirection
    type AttrTransferTypeConstraint TextAttributesDirectionFieldInfo = (~)Gtk.Enums.TextDirection
    type AttrTransferType TextAttributesDirectionFieldInfo = Gtk.Enums.TextDirection
    type AttrGetType TextAttributesDirectionFieldInfo = Gtk.Enums.TextDirection
    type AttrLabel TextAttributesDirectionFieldInfo = "direction"
    type AttrOrigin TextAttributesDirectionFieldInfo = TextAttributes
    attrGet = getTextAttributesDirection
    attrSet = setTextAttributesDirection
    attrConstruct = undefined
    attrClear = undefined
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextAttributes.direction"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextAttributes.html#g:attr:direction"
        })

textAttributes_direction :: AttrLabelProxy "direction"
textAttributes_direction = AttrLabelProxy

#endif


-- | Get the value of the “@font@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textAttributes #font
-- @
getTextAttributesFont :: MonadIO m => TextAttributes -> m (Maybe Pango.FontDescription.FontDescription)
getTextAttributesFont s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 64) :: IO (Ptr Pango.FontDescription.FontDescription)
    result <- SP.convertIfNonNull val $ \val' -> do
        val'' <- (newBoxed Pango.FontDescription.FontDescription) val'
        return val''
    return result

-- | Set the value of the “@font@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textAttributes [ #font 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextAttributesFont :: MonadIO m => TextAttributes -> Ptr Pango.FontDescription.FontDescription -> m ()
setTextAttributesFont s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 64) (val :: Ptr Pango.FontDescription.FontDescription)

-- | Set the value of the “@font@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #font
-- @
clearTextAttributesFont :: MonadIO m => TextAttributes -> m ()
clearTextAttributesFont s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 64) (FP.nullPtr :: Ptr Pango.FontDescription.FontDescription)

#if defined(ENABLE_OVERLOADING)
data TextAttributesFontFieldInfo
instance AttrInfo TextAttributesFontFieldInfo where
    type AttrBaseTypeConstraint TextAttributesFontFieldInfo = (~) TextAttributes
    type AttrAllowedOps TextAttributesFontFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint TextAttributesFontFieldInfo = (~) (Ptr Pango.FontDescription.FontDescription)
    type AttrTransferTypeConstraint TextAttributesFontFieldInfo = (~)(Ptr Pango.FontDescription.FontDescription)
    type AttrTransferType TextAttributesFontFieldInfo = (Ptr Pango.FontDescription.FontDescription)
    type AttrGetType TextAttributesFontFieldInfo = Maybe Pango.FontDescription.FontDescription
    type AttrLabel TextAttributesFontFieldInfo = "font"
    type AttrOrigin TextAttributesFontFieldInfo = TextAttributes
    attrGet = getTextAttributesFont
    attrSet = setTextAttributesFont
    attrConstruct = undefined
    attrClear = clearTextAttributesFont
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextAttributes.font"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextAttributes.html#g:attr:font"
        })

textAttributes_font :: AttrLabelProxy "font"
textAttributes_font = AttrLabelProxy

#endif


-- | Get the value of the “@font_scale@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textAttributes #fontScale
-- @
getTextAttributesFontScale :: MonadIO m => TextAttributes -> m Double
getTextAttributesFontScale s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 72) :: IO CDouble
    let val' = realToFrac val
    return val'

-- | Set the value of the “@font_scale@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textAttributes [ #fontScale 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextAttributesFontScale :: MonadIO m => TextAttributes -> Double -> m ()
setTextAttributesFontScale s val = liftIO $ withManagedPtr s $ \ptr -> do
    let val' = realToFrac val
    poke (ptr `plusPtr` 72) (val' :: CDouble)

#if defined(ENABLE_OVERLOADING)
data TextAttributesFontScaleFieldInfo
instance AttrInfo TextAttributesFontScaleFieldInfo where
    type AttrBaseTypeConstraint TextAttributesFontScaleFieldInfo = (~) TextAttributes
    type AttrAllowedOps TextAttributesFontScaleFieldInfo = '[ 'AttrSet, 'AttrGet]
    type AttrSetTypeConstraint TextAttributesFontScaleFieldInfo = (~) Double
    type AttrTransferTypeConstraint TextAttributesFontScaleFieldInfo = (~)Double
    type AttrTransferType TextAttributesFontScaleFieldInfo = Double
    type AttrGetType TextAttributesFontScaleFieldInfo = Double
    type AttrLabel TextAttributesFontScaleFieldInfo = "font_scale"
    type AttrOrigin TextAttributesFontScaleFieldInfo = TextAttributes
    attrGet = getTextAttributesFontScale
    attrSet = setTextAttributesFontScale
    attrConstruct = undefined
    attrClear = undefined
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextAttributes.fontScale"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextAttributes.html#g:attr:fontScale"
        })

textAttributes_fontScale :: AttrLabelProxy "fontScale"
textAttributes_fontScale = AttrLabelProxy

#endif


-- | Get the value of the “@left_margin@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textAttributes #leftMargin
-- @
getTextAttributesLeftMargin :: MonadIO m => TextAttributes -> m Int32
getTextAttributesLeftMargin s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 80) :: IO Int32
    return val

-- | Set the value of the “@left_margin@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textAttributes [ #leftMargin 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextAttributesLeftMargin :: MonadIO m => TextAttributes -> Int32 -> m ()
setTextAttributesLeftMargin s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 80) (val :: Int32)

#if defined(ENABLE_OVERLOADING)
data TextAttributesLeftMarginFieldInfo
instance AttrInfo TextAttributesLeftMarginFieldInfo where
    type AttrBaseTypeConstraint TextAttributesLeftMarginFieldInfo = (~) TextAttributes
    type AttrAllowedOps TextAttributesLeftMarginFieldInfo = '[ 'AttrSet, 'AttrGet]
    type AttrSetTypeConstraint TextAttributesLeftMarginFieldInfo = (~) Int32
    type AttrTransferTypeConstraint TextAttributesLeftMarginFieldInfo = (~)Int32
    type AttrTransferType TextAttributesLeftMarginFieldInfo = Int32
    type AttrGetType TextAttributesLeftMarginFieldInfo = Int32
    type AttrLabel TextAttributesLeftMarginFieldInfo = "left_margin"
    type AttrOrigin TextAttributesLeftMarginFieldInfo = TextAttributes
    attrGet = getTextAttributesLeftMargin
    attrSet = setTextAttributesLeftMargin
    attrConstruct = undefined
    attrClear = undefined
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextAttributes.leftMargin"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextAttributes.html#g:attr:leftMargin"
        })

textAttributes_leftMargin :: AttrLabelProxy "leftMargin"
textAttributes_leftMargin = AttrLabelProxy

#endif


-- | Get the value of the “@right_margin@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textAttributes #rightMargin
-- @
getTextAttributesRightMargin :: MonadIO m => TextAttributes -> m Int32
getTextAttributesRightMargin s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 84) :: IO Int32
    return val

-- | Set the value of the “@right_margin@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textAttributes [ #rightMargin 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextAttributesRightMargin :: MonadIO m => TextAttributes -> Int32 -> m ()
setTextAttributesRightMargin s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 84) (val :: Int32)

#if defined(ENABLE_OVERLOADING)
data TextAttributesRightMarginFieldInfo
instance AttrInfo TextAttributesRightMarginFieldInfo where
    type AttrBaseTypeConstraint TextAttributesRightMarginFieldInfo = (~) TextAttributes
    type AttrAllowedOps TextAttributesRightMarginFieldInfo = '[ 'AttrSet, 'AttrGet]
    type AttrSetTypeConstraint TextAttributesRightMarginFieldInfo = (~) Int32
    type AttrTransferTypeConstraint TextAttributesRightMarginFieldInfo = (~)Int32
    type AttrTransferType TextAttributesRightMarginFieldInfo = Int32
    type AttrGetType TextAttributesRightMarginFieldInfo = Int32
    type AttrLabel TextAttributesRightMarginFieldInfo = "right_margin"
    type AttrOrigin TextAttributesRightMarginFieldInfo = TextAttributes
    attrGet = getTextAttributesRightMargin
    attrSet = setTextAttributesRightMargin
    attrConstruct = undefined
    attrClear = undefined
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextAttributes.rightMargin"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextAttributes.html#g:attr:rightMargin"
        })

textAttributes_rightMargin :: AttrLabelProxy "rightMargin"
textAttributes_rightMargin = AttrLabelProxy

#endif


-- | Get the value of the “@indent@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textAttributes #indent
-- @
getTextAttributesIndent :: MonadIO m => TextAttributes -> m Int32
getTextAttributesIndent s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 88) :: IO Int32
    return val

-- | Set the value of the “@indent@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textAttributes [ #indent 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextAttributesIndent :: MonadIO m => TextAttributes -> Int32 -> m ()
setTextAttributesIndent s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 88) (val :: Int32)

#if defined(ENABLE_OVERLOADING)
data TextAttributesIndentFieldInfo
instance AttrInfo TextAttributesIndentFieldInfo where
    type AttrBaseTypeConstraint TextAttributesIndentFieldInfo = (~) TextAttributes
    type AttrAllowedOps TextAttributesIndentFieldInfo = '[ 'AttrSet, 'AttrGet]
    type AttrSetTypeConstraint TextAttributesIndentFieldInfo = (~) Int32
    type AttrTransferTypeConstraint TextAttributesIndentFieldInfo = (~)Int32
    type AttrTransferType TextAttributesIndentFieldInfo = Int32
    type AttrGetType TextAttributesIndentFieldInfo = Int32
    type AttrLabel TextAttributesIndentFieldInfo = "indent"
    type AttrOrigin TextAttributesIndentFieldInfo = TextAttributes
    attrGet = getTextAttributesIndent
    attrSet = setTextAttributesIndent
    attrConstruct = undefined
    attrClear = undefined
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextAttributes.indent"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextAttributes.html#g:attr:indent"
        })

textAttributes_indent :: AttrLabelProxy "indent"
textAttributes_indent = AttrLabelProxy

#endif


-- | Get the value of the “@pixels_above_lines@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textAttributes #pixelsAboveLines
-- @
getTextAttributesPixelsAboveLines :: MonadIO m => TextAttributes -> m Int32
getTextAttributesPixelsAboveLines s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 92) :: IO Int32
    return val

-- | Set the value of the “@pixels_above_lines@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textAttributes [ #pixelsAboveLines 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextAttributesPixelsAboveLines :: MonadIO m => TextAttributes -> Int32 -> m ()
setTextAttributesPixelsAboveLines s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 92) (val :: Int32)

#if defined(ENABLE_OVERLOADING)
data TextAttributesPixelsAboveLinesFieldInfo
instance AttrInfo TextAttributesPixelsAboveLinesFieldInfo where
    type AttrBaseTypeConstraint TextAttributesPixelsAboveLinesFieldInfo = (~) TextAttributes
    type AttrAllowedOps TextAttributesPixelsAboveLinesFieldInfo = '[ 'AttrSet, 'AttrGet]
    type AttrSetTypeConstraint TextAttributesPixelsAboveLinesFieldInfo = (~) Int32
    type AttrTransferTypeConstraint TextAttributesPixelsAboveLinesFieldInfo = (~)Int32
    type AttrTransferType TextAttributesPixelsAboveLinesFieldInfo = Int32
    type AttrGetType TextAttributesPixelsAboveLinesFieldInfo = Int32
    type AttrLabel TextAttributesPixelsAboveLinesFieldInfo = "pixels_above_lines"
    type AttrOrigin TextAttributesPixelsAboveLinesFieldInfo = TextAttributes
    attrGet = getTextAttributesPixelsAboveLines
    attrSet = setTextAttributesPixelsAboveLines
    attrConstruct = undefined
    attrClear = undefined
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextAttributes.pixelsAboveLines"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextAttributes.html#g:attr:pixelsAboveLines"
        })

textAttributes_pixelsAboveLines :: AttrLabelProxy "pixelsAboveLines"
textAttributes_pixelsAboveLines = AttrLabelProxy

#endif


-- | Get the value of the “@pixels_below_lines@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textAttributes #pixelsBelowLines
-- @
getTextAttributesPixelsBelowLines :: MonadIO m => TextAttributes -> m Int32
getTextAttributesPixelsBelowLines s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 96) :: IO Int32
    return val

-- | Set the value of the “@pixels_below_lines@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textAttributes [ #pixelsBelowLines 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextAttributesPixelsBelowLines :: MonadIO m => TextAttributes -> Int32 -> m ()
setTextAttributesPixelsBelowLines s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 96) (val :: Int32)

#if defined(ENABLE_OVERLOADING)
data TextAttributesPixelsBelowLinesFieldInfo
instance AttrInfo TextAttributesPixelsBelowLinesFieldInfo where
    type AttrBaseTypeConstraint TextAttributesPixelsBelowLinesFieldInfo = (~) TextAttributes
    type AttrAllowedOps TextAttributesPixelsBelowLinesFieldInfo = '[ 'AttrSet, 'AttrGet]
    type AttrSetTypeConstraint TextAttributesPixelsBelowLinesFieldInfo = (~) Int32
    type AttrTransferTypeConstraint TextAttributesPixelsBelowLinesFieldInfo = (~)Int32
    type AttrTransferType TextAttributesPixelsBelowLinesFieldInfo = Int32
    type AttrGetType TextAttributesPixelsBelowLinesFieldInfo = Int32
    type AttrLabel TextAttributesPixelsBelowLinesFieldInfo = "pixels_below_lines"
    type AttrOrigin TextAttributesPixelsBelowLinesFieldInfo = TextAttributes
    attrGet = getTextAttributesPixelsBelowLines
    attrSet = setTextAttributesPixelsBelowLines
    attrConstruct = undefined
    attrClear = undefined
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextAttributes.pixelsBelowLines"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextAttributes.html#g:attr:pixelsBelowLines"
        })

textAttributes_pixelsBelowLines :: AttrLabelProxy "pixelsBelowLines"
textAttributes_pixelsBelowLines = AttrLabelProxy

#endif


-- | Get the value of the “@pixels_inside_wrap@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textAttributes #pixelsInsideWrap
-- @
getTextAttributesPixelsInsideWrap :: MonadIO m => TextAttributes -> m Int32
getTextAttributesPixelsInsideWrap s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 100) :: IO Int32
    return val

-- | Set the value of the “@pixels_inside_wrap@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textAttributes [ #pixelsInsideWrap 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextAttributesPixelsInsideWrap :: MonadIO m => TextAttributes -> Int32 -> m ()
setTextAttributesPixelsInsideWrap s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 100) (val :: Int32)

#if defined(ENABLE_OVERLOADING)
data TextAttributesPixelsInsideWrapFieldInfo
instance AttrInfo TextAttributesPixelsInsideWrapFieldInfo where
    type AttrBaseTypeConstraint TextAttributesPixelsInsideWrapFieldInfo = (~) TextAttributes
    type AttrAllowedOps TextAttributesPixelsInsideWrapFieldInfo = '[ 'AttrSet, 'AttrGet]
    type AttrSetTypeConstraint TextAttributesPixelsInsideWrapFieldInfo = (~) Int32
    type AttrTransferTypeConstraint TextAttributesPixelsInsideWrapFieldInfo = (~)Int32
    type AttrTransferType TextAttributesPixelsInsideWrapFieldInfo = Int32
    type AttrGetType TextAttributesPixelsInsideWrapFieldInfo = Int32
    type AttrLabel TextAttributesPixelsInsideWrapFieldInfo = "pixels_inside_wrap"
    type AttrOrigin TextAttributesPixelsInsideWrapFieldInfo = TextAttributes
    attrGet = getTextAttributesPixelsInsideWrap
    attrSet = setTextAttributesPixelsInsideWrap
    attrConstruct = undefined
    attrClear = undefined
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextAttributes.pixelsInsideWrap"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextAttributes.html#g:attr:pixelsInsideWrap"
        })

textAttributes_pixelsInsideWrap :: AttrLabelProxy "pixelsInsideWrap"
textAttributes_pixelsInsideWrap = AttrLabelProxy

#endif


-- | Get the value of the “@tabs@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textAttributes #tabs
-- @
getTextAttributesTabs :: MonadIO m => TextAttributes -> m (Maybe Pango.TabArray.TabArray)
getTextAttributesTabs s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 104) :: IO (Ptr Pango.TabArray.TabArray)
    result <- SP.convertIfNonNull val $ \val' -> do
        val'' <- (newBoxed Pango.TabArray.TabArray) val'
        return val''
    return result

-- | Set the value of the “@tabs@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textAttributes [ #tabs 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextAttributesTabs :: MonadIO m => TextAttributes -> Ptr Pango.TabArray.TabArray -> m ()
setTextAttributesTabs s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 104) (val :: Ptr Pango.TabArray.TabArray)

-- | Set the value of the “@tabs@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #tabs
-- @
clearTextAttributesTabs :: MonadIO m => TextAttributes -> m ()
clearTextAttributesTabs s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 104) (FP.nullPtr :: Ptr Pango.TabArray.TabArray)

#if defined(ENABLE_OVERLOADING)
data TextAttributesTabsFieldInfo
instance AttrInfo TextAttributesTabsFieldInfo where
    type AttrBaseTypeConstraint TextAttributesTabsFieldInfo = (~) TextAttributes
    type AttrAllowedOps TextAttributesTabsFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint TextAttributesTabsFieldInfo = (~) (Ptr Pango.TabArray.TabArray)
    type AttrTransferTypeConstraint TextAttributesTabsFieldInfo = (~)(Ptr Pango.TabArray.TabArray)
    type AttrTransferType TextAttributesTabsFieldInfo = (Ptr Pango.TabArray.TabArray)
    type AttrGetType TextAttributesTabsFieldInfo = Maybe Pango.TabArray.TabArray
    type AttrLabel TextAttributesTabsFieldInfo = "tabs"
    type AttrOrigin TextAttributesTabsFieldInfo = TextAttributes
    attrGet = getTextAttributesTabs
    attrSet = setTextAttributesTabs
    attrConstruct = undefined
    attrClear = clearTextAttributesTabs
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextAttributes.tabs"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextAttributes.html#g:attr:tabs"
        })

textAttributes_tabs :: AttrLabelProxy "tabs"
textAttributes_tabs = AttrLabelProxy

#endif


-- | Get the value of the “@wrap_mode@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textAttributes #wrapMode
-- @
getTextAttributesWrapMode :: MonadIO m => TextAttributes -> m Gtk.Enums.WrapMode
getTextAttributesWrapMode s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 112) :: IO CUInt
    let val' = (toEnum . fromIntegral) val
    return val'

-- | Set the value of the “@wrap_mode@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textAttributes [ #wrapMode 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextAttributesWrapMode :: MonadIO m => TextAttributes -> Gtk.Enums.WrapMode -> m ()
setTextAttributesWrapMode s val = liftIO $ withManagedPtr s $ \ptr -> do
    let val' = (fromIntegral . fromEnum) val
    poke (ptr `plusPtr` 112) (val' :: CUInt)

#if defined(ENABLE_OVERLOADING)
data TextAttributesWrapModeFieldInfo
instance AttrInfo TextAttributesWrapModeFieldInfo where
    type AttrBaseTypeConstraint TextAttributesWrapModeFieldInfo = (~) TextAttributes
    type AttrAllowedOps TextAttributesWrapModeFieldInfo = '[ 'AttrSet, 'AttrGet]
    type AttrSetTypeConstraint TextAttributesWrapModeFieldInfo = (~) Gtk.Enums.WrapMode
    type AttrTransferTypeConstraint TextAttributesWrapModeFieldInfo = (~)Gtk.Enums.WrapMode
    type AttrTransferType TextAttributesWrapModeFieldInfo = Gtk.Enums.WrapMode
    type AttrGetType TextAttributesWrapModeFieldInfo = Gtk.Enums.WrapMode
    type AttrLabel TextAttributesWrapModeFieldInfo = "wrap_mode"
    type AttrOrigin TextAttributesWrapModeFieldInfo = TextAttributes
    attrGet = getTextAttributesWrapMode
    attrSet = setTextAttributesWrapMode
    attrConstruct = undefined
    attrClear = undefined
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextAttributes.wrapMode"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextAttributes.html#g:attr:wrapMode"
        })

textAttributes_wrapMode :: AttrLabelProxy "wrapMode"
textAttributes_wrapMode = AttrLabelProxy

#endif


-- | Get the value of the “@language@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textAttributes #language
-- @
getTextAttributesLanguage :: MonadIO m => TextAttributes -> m (Maybe Pango.Language.Language)
getTextAttributesLanguage s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 120) :: IO (Ptr Pango.Language.Language)
    result <- SP.convertIfNonNull val $ \val' -> do
        val'' <- (newBoxed Pango.Language.Language) val'
        return val''
    return result

-- | Set the value of the “@language@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textAttributes [ #language 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextAttributesLanguage :: MonadIO m => TextAttributes -> Ptr Pango.Language.Language -> m ()
setTextAttributesLanguage s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 120) (val :: Ptr Pango.Language.Language)

-- | Set the value of the “@language@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #language
-- @
clearTextAttributesLanguage :: MonadIO m => TextAttributes -> m ()
clearTextAttributesLanguage s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 120) (FP.nullPtr :: Ptr Pango.Language.Language)

#if defined(ENABLE_OVERLOADING)
data TextAttributesLanguageFieldInfo
instance AttrInfo TextAttributesLanguageFieldInfo where
    type AttrBaseTypeConstraint TextAttributesLanguageFieldInfo = (~) TextAttributes
    type AttrAllowedOps TextAttributesLanguageFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint TextAttributesLanguageFieldInfo = (~) (Ptr Pango.Language.Language)
    type AttrTransferTypeConstraint TextAttributesLanguageFieldInfo = (~)(Ptr Pango.Language.Language)
    type AttrTransferType TextAttributesLanguageFieldInfo = (Ptr Pango.Language.Language)
    type AttrGetType TextAttributesLanguageFieldInfo = Maybe Pango.Language.Language
    type AttrLabel TextAttributesLanguageFieldInfo = "language"
    type AttrOrigin TextAttributesLanguageFieldInfo = TextAttributes
    attrGet = getTextAttributesLanguage
    attrSet = setTextAttributesLanguage
    attrConstruct = undefined
    attrClear = clearTextAttributesLanguage
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextAttributes.language"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextAttributes.html#g:attr:language"
        })

textAttributes_language :: AttrLabelProxy "language"
textAttributes_language = AttrLabelProxy

#endif


-- | Get the value of the “@invisible@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textAttributes #invisible
-- @
getTextAttributesInvisible :: MonadIO m => TextAttributes -> m Word32
getTextAttributesInvisible s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 136) :: IO Word32
    return val

-- | Set the value of the “@invisible@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textAttributes [ #invisible 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextAttributesInvisible :: MonadIO m => TextAttributes -> Word32 -> m ()
setTextAttributesInvisible s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 136) (val :: Word32)

#if defined(ENABLE_OVERLOADING)
data TextAttributesInvisibleFieldInfo
instance AttrInfo TextAttributesInvisibleFieldInfo where
    type AttrBaseTypeConstraint TextAttributesInvisibleFieldInfo = (~) TextAttributes
    type AttrAllowedOps TextAttributesInvisibleFieldInfo = '[ 'AttrSet, 'AttrGet]
    type AttrSetTypeConstraint TextAttributesInvisibleFieldInfo = (~) Word32
    type AttrTransferTypeConstraint TextAttributesInvisibleFieldInfo = (~)Word32
    type AttrTransferType TextAttributesInvisibleFieldInfo = Word32
    type AttrGetType TextAttributesInvisibleFieldInfo = Word32
    type AttrLabel TextAttributesInvisibleFieldInfo = "invisible"
    type AttrOrigin TextAttributesInvisibleFieldInfo = TextAttributes
    attrGet = getTextAttributesInvisible
    attrSet = setTextAttributesInvisible
    attrConstruct = undefined
    attrClear = undefined
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextAttributes.invisible"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextAttributes.html#g:attr:invisible"
        })

textAttributes_invisible :: AttrLabelProxy "invisible"
textAttributes_invisible = AttrLabelProxy

#endif


-- | Get the value of the “@bg_full_height@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textAttributes #bgFullHeight
-- @
getTextAttributesBgFullHeight :: MonadIO m => TextAttributes -> m Word32
getTextAttributesBgFullHeight s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 140) :: IO Word32
    return val

-- | Set the value of the “@bg_full_height@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textAttributes [ #bgFullHeight 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextAttributesBgFullHeight :: MonadIO m => TextAttributes -> Word32 -> m ()
setTextAttributesBgFullHeight s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 140) (val :: Word32)

#if defined(ENABLE_OVERLOADING)
data TextAttributesBgFullHeightFieldInfo
instance AttrInfo TextAttributesBgFullHeightFieldInfo where
    type AttrBaseTypeConstraint TextAttributesBgFullHeightFieldInfo = (~) TextAttributes
    type AttrAllowedOps TextAttributesBgFullHeightFieldInfo = '[ 'AttrSet, 'AttrGet]
    type AttrSetTypeConstraint TextAttributesBgFullHeightFieldInfo = (~) Word32
    type AttrTransferTypeConstraint TextAttributesBgFullHeightFieldInfo = (~)Word32
    type AttrTransferType TextAttributesBgFullHeightFieldInfo = Word32
    type AttrGetType TextAttributesBgFullHeightFieldInfo = Word32
    type AttrLabel TextAttributesBgFullHeightFieldInfo = "bg_full_height"
    type AttrOrigin TextAttributesBgFullHeightFieldInfo = TextAttributes
    attrGet = getTextAttributesBgFullHeight
    attrSet = setTextAttributesBgFullHeight
    attrConstruct = undefined
    attrClear = undefined
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextAttributes.bgFullHeight"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextAttributes.html#g:attr:bgFullHeight"
        })

textAttributes_bgFullHeight :: AttrLabelProxy "bgFullHeight"
textAttributes_bgFullHeight = AttrLabelProxy

#endif


-- | Get the value of the “@editable@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textAttributes #editable
-- @
getTextAttributesEditable :: MonadIO m => TextAttributes -> m Word32
getTextAttributesEditable s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 144) :: IO Word32
    return val

-- | Set the value of the “@editable@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textAttributes [ #editable 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextAttributesEditable :: MonadIO m => TextAttributes -> Word32 -> m ()
setTextAttributesEditable s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 144) (val :: Word32)

#if defined(ENABLE_OVERLOADING)
data TextAttributesEditableFieldInfo
instance AttrInfo TextAttributesEditableFieldInfo where
    type AttrBaseTypeConstraint TextAttributesEditableFieldInfo = (~) TextAttributes
    type AttrAllowedOps TextAttributesEditableFieldInfo = '[ 'AttrSet, 'AttrGet]
    type AttrSetTypeConstraint TextAttributesEditableFieldInfo = (~) Word32
    type AttrTransferTypeConstraint TextAttributesEditableFieldInfo = (~)Word32
    type AttrTransferType TextAttributesEditableFieldInfo = Word32
    type AttrGetType TextAttributesEditableFieldInfo = Word32
    type AttrLabel TextAttributesEditableFieldInfo = "editable"
    type AttrOrigin TextAttributesEditableFieldInfo = TextAttributes
    attrGet = getTextAttributesEditable
    attrSet = setTextAttributesEditable
    attrConstruct = undefined
    attrClear = undefined
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextAttributes.editable"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextAttributes.html#g:attr:editable"
        })

textAttributes_editable :: AttrLabelProxy "editable"
textAttributes_editable = AttrLabelProxy

#endif


-- | Get the value of the “@no_fallback@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textAttributes #noFallback
-- @
getTextAttributesNoFallback :: MonadIO m => TextAttributes -> m Word32
getTextAttributesNoFallback s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 148) :: IO Word32
    return val

-- | Set the value of the “@no_fallback@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textAttributes [ #noFallback 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextAttributesNoFallback :: MonadIO m => TextAttributes -> Word32 -> m ()
setTextAttributesNoFallback s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 148) (val :: Word32)

#if defined(ENABLE_OVERLOADING)
data TextAttributesNoFallbackFieldInfo
instance AttrInfo TextAttributesNoFallbackFieldInfo where
    type AttrBaseTypeConstraint TextAttributesNoFallbackFieldInfo = (~) TextAttributes
    type AttrAllowedOps TextAttributesNoFallbackFieldInfo = '[ 'AttrSet, 'AttrGet]
    type AttrSetTypeConstraint TextAttributesNoFallbackFieldInfo = (~) Word32
    type AttrTransferTypeConstraint TextAttributesNoFallbackFieldInfo = (~)Word32
    type AttrTransferType TextAttributesNoFallbackFieldInfo = Word32
    type AttrGetType TextAttributesNoFallbackFieldInfo = Word32
    type AttrLabel TextAttributesNoFallbackFieldInfo = "no_fallback"
    type AttrOrigin TextAttributesNoFallbackFieldInfo = TextAttributes
    attrGet = getTextAttributesNoFallback
    attrSet = setTextAttributesNoFallback
    attrConstruct = undefined
    attrClear = undefined
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextAttributes.noFallback"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextAttributes.html#g:attr:noFallback"
        })

textAttributes_noFallback :: AttrLabelProxy "noFallback"
textAttributes_noFallback = AttrLabelProxy

#endif


-- | Get the value of the “@letter_spacing@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textAttributes #letterSpacing
-- @
getTextAttributesLetterSpacing :: MonadIO m => TextAttributes -> m Int32
getTextAttributesLetterSpacing s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 160) :: IO Int32
    return val

-- | Set the value of the “@letter_spacing@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textAttributes [ #letterSpacing 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextAttributesLetterSpacing :: MonadIO m => TextAttributes -> Int32 -> m ()
setTextAttributesLetterSpacing s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 160) (val :: Int32)

#if defined(ENABLE_OVERLOADING)
data TextAttributesLetterSpacingFieldInfo
instance AttrInfo TextAttributesLetterSpacingFieldInfo where
    type AttrBaseTypeConstraint TextAttributesLetterSpacingFieldInfo = (~) TextAttributes
    type AttrAllowedOps TextAttributesLetterSpacingFieldInfo = '[ 'AttrSet, 'AttrGet]
    type AttrSetTypeConstraint TextAttributesLetterSpacingFieldInfo = (~) Int32
    type AttrTransferTypeConstraint TextAttributesLetterSpacingFieldInfo = (~)Int32
    type AttrTransferType TextAttributesLetterSpacingFieldInfo = Int32
    type AttrGetType TextAttributesLetterSpacingFieldInfo = Int32
    type AttrLabel TextAttributesLetterSpacingFieldInfo = "letter_spacing"
    type AttrOrigin TextAttributesLetterSpacingFieldInfo = TextAttributes
    attrGet = getTextAttributesLetterSpacing
    attrSet = setTextAttributesLetterSpacing
    attrConstruct = undefined
    attrClear = undefined
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextAttributes.letterSpacing"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextAttributes.html#g:attr:letterSpacing"
        })

textAttributes_letterSpacing :: AttrLabelProxy "letterSpacing"
textAttributes_letterSpacing = AttrLabelProxy

#endif



#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList TextAttributes
type instance O.AttributeList TextAttributes = TextAttributesAttributeList
type TextAttributesAttributeList = ('[ '("appearance", TextAttributesAppearanceFieldInfo), '("justification", TextAttributesJustificationFieldInfo), '("direction", TextAttributesDirectionFieldInfo), '("font", TextAttributesFontFieldInfo), '("fontScale", TextAttributesFontScaleFieldInfo), '("leftMargin", TextAttributesLeftMarginFieldInfo), '("rightMargin", TextAttributesRightMarginFieldInfo), '("indent", TextAttributesIndentFieldInfo), '("pixelsAboveLines", TextAttributesPixelsAboveLinesFieldInfo), '("pixelsBelowLines", TextAttributesPixelsBelowLinesFieldInfo), '("pixelsInsideWrap", TextAttributesPixelsInsideWrapFieldInfo), '("tabs", TextAttributesTabsFieldInfo), '("wrapMode", TextAttributesWrapModeFieldInfo), '("language", TextAttributesLanguageFieldInfo), '("invisible", TextAttributesInvisibleFieldInfo), '("bgFullHeight", TextAttributesBgFullHeightFieldInfo), '("editable", TextAttributesEditableFieldInfo), '("noFallback", TextAttributesNoFallbackFieldInfo), '("letterSpacing", TextAttributesLetterSpacingFieldInfo)] :: [(Symbol, *)])
#endif

-- method TextAttributes::new
-- method type : Constructor
-- Args: []
-- Lengths: []
-- returnType: Just
--               (TInterface Name { namespace = "Gtk" , name = "TextAttributes" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_text_attributes_new" gtk_text_attributes_new :: 
    IO (Ptr TextAttributes)

-- | Creates a t'GI.Gtk.Structs.TextAttributes.TextAttributes', which describes
-- a set of properties on some text.
textAttributesNew ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m TextAttributes
    -- ^ __Returns:__ a new t'GI.Gtk.Structs.TextAttributes.TextAttributes',
    --     free with 'GI.Gtk.Structs.TextAttributes.textAttributesUnref'.
textAttributesNew  = liftIO $ do
    result <- gtk_text_attributes_new
    checkUnexpectedReturnNULL "textAttributesNew" result
    result' <- (wrapBoxed TextAttributes) result
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method TextAttributes::copy
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "src"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextAttributes" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextAttributes to be copied"
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
--               (TInterface Name { namespace = "Gtk" , name = "TextAttributes" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_text_attributes_copy" gtk_text_attributes_copy :: 
    Ptr TextAttributes ->                   -- src : TInterface (Name {namespace = "Gtk", name = "TextAttributes"})
    IO (Ptr TextAttributes)

-- | Copies /@src@/ and returns a new t'GI.Gtk.Structs.TextAttributes.TextAttributes'.
textAttributesCopy ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TextAttributes
    -- ^ /@src@/: a t'GI.Gtk.Structs.TextAttributes.TextAttributes' to be copied
    -> m TextAttributes
    -- ^ __Returns:__ a copy of /@src@/,
    --     free with 'GI.Gtk.Structs.TextAttributes.textAttributesUnref'
textAttributesCopy src = liftIO $ do
    src' <- unsafeManagedPtrGetPtr src
    result <- gtk_text_attributes_copy src'
    checkUnexpectedReturnNULL "textAttributesCopy" result
    result' <- (wrapBoxed TextAttributes) result
    touchManagedPtr src
    return result'

#if defined(ENABLE_OVERLOADING)
data TextAttributesCopyMethodInfo
instance (signature ~ (m TextAttributes), MonadIO m) => O.OverloadedMethod TextAttributesCopyMethodInfo TextAttributes signature where
    overloadedMethod = textAttributesCopy

instance O.OverloadedMethodInfo TextAttributesCopyMethodInfo TextAttributes where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextAttributes.textAttributesCopy",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextAttributes.html#v:textAttributesCopy"
        })


#endif

-- method TextAttributes::copy_values
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "src"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextAttributes" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextAttributes"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "dest"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextAttributes" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "another #GtkTextAttributes"
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

foreign import ccall "gtk_text_attributes_copy_values" gtk_text_attributes_copy_values :: 
    Ptr TextAttributes ->                   -- src : TInterface (Name {namespace = "Gtk", name = "TextAttributes"})
    Ptr TextAttributes ->                   -- dest : TInterface (Name {namespace = "Gtk", name = "TextAttributes"})
    IO ()

-- | Copies the values from /@src@/ to /@dest@/ so that /@dest@/ has
-- the same values as /@src@/. Frees existing values in /@dest@/.
textAttributesCopyValues ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TextAttributes
    -- ^ /@src@/: a t'GI.Gtk.Structs.TextAttributes.TextAttributes'
    -> TextAttributes
    -- ^ /@dest@/: another t'GI.Gtk.Structs.TextAttributes.TextAttributes'
    -> m ()
textAttributesCopyValues src dest = liftIO $ do
    src' <- unsafeManagedPtrGetPtr src
    dest' <- unsafeManagedPtrGetPtr dest
    gtk_text_attributes_copy_values src' dest'
    touchManagedPtr src
    touchManagedPtr dest
    return ()

#if defined(ENABLE_OVERLOADING)
data TextAttributesCopyValuesMethodInfo
instance (signature ~ (TextAttributes -> m ()), MonadIO m) => O.OverloadedMethod TextAttributesCopyValuesMethodInfo TextAttributes signature where
    overloadedMethod = textAttributesCopyValues

instance O.OverloadedMethodInfo TextAttributesCopyValuesMethodInfo TextAttributes where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextAttributes.textAttributesCopyValues",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextAttributes.html#v:textAttributesCopyValues"
        })


#endif

-- method TextAttributes::ref
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "values"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextAttributes" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextAttributes"
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
--               (TInterface Name { namespace = "Gtk" , name = "TextAttributes" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_text_attributes_ref" gtk_text_attributes_ref :: 
    Ptr TextAttributes ->                   -- values : TInterface (Name {namespace = "Gtk", name = "TextAttributes"})
    IO (Ptr TextAttributes)

-- | Increments the reference count on /@values@/.
textAttributesRef ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TextAttributes
    -- ^ /@values@/: a t'GI.Gtk.Structs.TextAttributes.TextAttributes'
    -> m TextAttributes
    -- ^ __Returns:__ the t'GI.Gtk.Structs.TextAttributes.TextAttributes' that were passed in
textAttributesRef values = liftIO $ do
    values' <- unsafeManagedPtrGetPtr values
    result <- gtk_text_attributes_ref values'
    checkUnexpectedReturnNULL "textAttributesRef" result
    result' <- (wrapBoxed TextAttributes) result
    touchManagedPtr values
    return result'

#if defined(ENABLE_OVERLOADING)
data TextAttributesRefMethodInfo
instance (signature ~ (m TextAttributes), MonadIO m) => O.OverloadedMethod TextAttributesRefMethodInfo TextAttributes signature where
    overloadedMethod = textAttributesRef

instance O.OverloadedMethodInfo TextAttributesRefMethodInfo TextAttributes where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextAttributes.textAttributesRef",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextAttributes.html#v:textAttributesRef"
        })


#endif

-- method TextAttributes::unref
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "values"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextAttributes" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextAttributes"
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

foreign import ccall "gtk_text_attributes_unref" gtk_text_attributes_unref :: 
    Ptr TextAttributes ->                   -- values : TInterface (Name {namespace = "Gtk", name = "TextAttributes"})
    IO ()

-- | Decrements the reference count on /@values@/, freeing the structure
-- if the reference count reaches 0.
textAttributesUnref ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TextAttributes
    -- ^ /@values@/: a t'GI.Gtk.Structs.TextAttributes.TextAttributes'
    -> m ()
textAttributesUnref values = liftIO $ do
    values' <- unsafeManagedPtrGetPtr values
    gtk_text_attributes_unref values'
    touchManagedPtr values
    return ()

#if defined(ENABLE_OVERLOADING)
data TextAttributesUnrefMethodInfo
instance (signature ~ (m ()), MonadIO m) => O.OverloadedMethod TextAttributesUnrefMethodInfo TextAttributes signature where
    overloadedMethod = textAttributesUnref

instance O.OverloadedMethodInfo TextAttributesUnrefMethodInfo TextAttributes where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextAttributes.textAttributesUnref",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextAttributes.html#v:textAttributesUnref"
        })


#endif

#if defined(ENABLE_OVERLOADING)
type family ResolveTextAttributesMethod (t :: Symbol) (o :: *) :: * where
    ResolveTextAttributesMethod "copy" o = TextAttributesCopyMethodInfo
    ResolveTextAttributesMethod "copyValues" o = TextAttributesCopyValuesMethodInfo
    ResolveTextAttributesMethod "ref" o = TextAttributesRefMethodInfo
    ResolveTextAttributesMethod "unref" o = TextAttributesUnrefMethodInfo
    ResolveTextAttributesMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveTextAttributesMethod t TextAttributes, O.OverloadedMethod info TextAttributes p) => OL.IsLabel t (TextAttributes -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveTextAttributesMethod t TextAttributes, O.OverloadedMethod info TextAttributes p, R.HasField t TextAttributes p) => R.HasField t TextAttributes p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveTextAttributesMethod t TextAttributes, O.OverloadedMethodInfo info TextAttributes) => OL.IsLabel t (O.MethodProxy info TextAttributes) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif


