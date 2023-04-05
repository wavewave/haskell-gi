{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- Defines a part of a CSS document. Because sections are nested into
-- one another, you can use 'GI.Gtk.Structs.CssSection.cssSectionGetParent' to get the
-- containing region.
-- 
-- /Since: 3.2/

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Structs.CssSection
    ( 

-- * Exported types
    CssSection(..)                          ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [ref]("GI.Gtk.Structs.CssSection#g:method:ref"), [unref]("GI.Gtk.Structs.CssSection#g:method:unref").
-- 
-- ==== Getters
-- [getEndLine]("GI.Gtk.Structs.CssSection#g:method:getEndLine"), [getEndPosition]("GI.Gtk.Structs.CssSection#g:method:getEndPosition"), [getFile]("GI.Gtk.Structs.CssSection#g:method:getFile"), [getParent]("GI.Gtk.Structs.CssSection#g:method:getParent"), [getSectionType]("GI.Gtk.Structs.CssSection#g:method:getSectionType"), [getStartLine]("GI.Gtk.Structs.CssSection#g:method:getStartLine"), [getStartPosition]("GI.Gtk.Structs.CssSection#g:method:getStartPosition").
-- 
-- ==== Setters
-- /None/.

#if defined(ENABLE_OVERLOADING)
    ResolveCssSectionMethod                 ,
#endif

-- ** getEndLine #method:getEndLine#

#if defined(ENABLE_OVERLOADING)
    CssSectionGetEndLineMethodInfo          ,
#endif
    cssSectionGetEndLine                    ,


-- ** getEndPosition #method:getEndPosition#

#if defined(ENABLE_OVERLOADING)
    CssSectionGetEndPositionMethodInfo      ,
#endif
    cssSectionGetEndPosition                ,


-- ** getFile #method:getFile#

#if defined(ENABLE_OVERLOADING)
    CssSectionGetFileMethodInfo             ,
#endif
    cssSectionGetFile                       ,


-- ** getParent #method:getParent#

#if defined(ENABLE_OVERLOADING)
    CssSectionGetParentMethodInfo           ,
#endif
    cssSectionGetParent                     ,


-- ** getSectionType #method:getSectionType#

#if defined(ENABLE_OVERLOADING)
    CssSectionGetSectionTypeMethodInfo      ,
#endif
    cssSectionGetSectionType                ,


-- ** getStartLine #method:getStartLine#

#if defined(ENABLE_OVERLOADING)
    CssSectionGetStartLineMethodInfo        ,
#endif
    cssSectionGetStartLine                  ,


-- ** getStartPosition #method:getStartPosition#

#if defined(ENABLE_OVERLOADING)
    CssSectionGetStartPositionMethodInfo    ,
#endif
    cssSectionGetStartPosition              ,


-- ** ref #method:ref#

#if defined(ENABLE_OVERLOADING)
    CssSectionRefMethodInfo                 ,
#endif
    cssSectionRef                           ,


-- ** unref #method:unref#

#if defined(ENABLE_OVERLOADING)
    CssSectionUnrefMethodInfo               ,
#endif
    cssSectionUnref                         ,




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

import qualified GI.Gio.Interfaces.File as Gio.File
import {-# SOURCE #-} qualified GI.Gtk.Enums as Gtk.Enums

-- | Memory-managed wrapper type.
newtype CssSection = CssSection (SP.ManagedPtr CssSection)
    deriving (Eq)

instance SP.ManagedPtrNewtype CssSection where
    toManagedPtr (CssSection p) = p

foreign import ccall "gtk_css_section_get_type" c_gtk_css_section_get_type :: 
    IO GType

type instance O.ParentTypes CssSection = '[]
instance O.HasParentTypes CssSection

instance B.Types.TypedObject CssSection where
    glibType = c_gtk_css_section_get_type

instance B.Types.GBoxed CssSection

-- | Convert 'CssSection' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe CssSection) where
    gvalueGType_ = c_gtk_css_section_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_boxed gv (FP.nullPtr :: FP.Ptr CssSection)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_boxed gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_boxed gv :: IO (Ptr CssSection)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newBoxed CssSection ptr
        else return P.Nothing
        
    


#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList CssSection
type instance O.AttributeList CssSection = CssSectionAttributeList
type CssSectionAttributeList = ('[ ] :: [(Symbol, *)])
#endif

-- method CssSection::get_end_line
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "section"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CssSection" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the section" , sinceVersion = Nothing }
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

foreign import ccall "gtk_css_section_get_end_line" gtk_css_section_get_end_line :: 
    Ptr CssSection ->                       -- section : TInterface (Name {namespace = "Gtk", name = "CssSection"})
    IO Word32

-- | Returns the line in the CSS document where this section end.
-- The line number is 0-indexed, so the first line of the document
-- will return 0.
-- This value may change in future invocations of this function if
-- /@section@/ is not yet parsed completely. This will for example
-- happen in the GtkCssProvider[parsingError](#g:signal:parsingError) signal.
-- The end position and line may be identical to the start
-- position and line for sections which failed to parse anything
-- successfully.
-- 
-- /Since: 3.2/
cssSectionGetEndLine ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    CssSection
    -- ^ /@section@/: the section
    -> m Word32
    -- ^ __Returns:__ the line number
cssSectionGetEndLine section = liftIO $ do
    section' <- unsafeManagedPtrGetPtr section
    result <- gtk_css_section_get_end_line section'
    touchManagedPtr section
    return result

#if defined(ENABLE_OVERLOADING)
data CssSectionGetEndLineMethodInfo
instance (signature ~ (m Word32), MonadIO m) => O.OverloadedMethod CssSectionGetEndLineMethodInfo CssSection signature where
    overloadedMethod = cssSectionGetEndLine

instance O.OverloadedMethodInfo CssSectionGetEndLineMethodInfo CssSection where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.CssSection.cssSectionGetEndLine",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-CssSection.html#v:cssSectionGetEndLine"
        })


#endif

-- method CssSection::get_end_position
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "section"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CssSection" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the section" , sinceVersion = Nothing }
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

foreign import ccall "gtk_css_section_get_end_position" gtk_css_section_get_end_position :: 
    Ptr CssSection ->                       -- section : TInterface (Name {namespace = "Gtk", name = "CssSection"})
    IO Word32

-- | Returns the offset in bytes from the start of the current line
-- returned via 'GI.Gtk.Structs.CssSection.cssSectionGetEndLine'.
-- This value may change in future invocations of this function if
-- /@section@/ is not yet parsed completely. This will for example
-- happen in the GtkCssProvider[parsingError](#g:signal:parsingError) signal.
-- The end position and line may be identical to the start
-- position and line for sections which failed to parse anything
-- successfully.
-- 
-- /Since: 3.2/
cssSectionGetEndPosition ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    CssSection
    -- ^ /@section@/: the section
    -> m Word32
    -- ^ __Returns:__ the offset in bytes from the start of the line.
cssSectionGetEndPosition section = liftIO $ do
    section' <- unsafeManagedPtrGetPtr section
    result <- gtk_css_section_get_end_position section'
    touchManagedPtr section
    return result

#if defined(ENABLE_OVERLOADING)
data CssSectionGetEndPositionMethodInfo
instance (signature ~ (m Word32), MonadIO m) => O.OverloadedMethod CssSectionGetEndPositionMethodInfo CssSection signature where
    overloadedMethod = cssSectionGetEndPosition

instance O.OverloadedMethodInfo CssSectionGetEndPositionMethodInfo CssSection where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.CssSection.cssSectionGetEndPosition",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-CssSection.html#v:cssSectionGetEndPosition"
        })


#endif

-- method CssSection::get_file
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "section"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CssSection" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the section" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gio" , name = "File" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_css_section_get_file" gtk_css_section_get_file :: 
    Ptr CssSection ->                       -- section : TInterface (Name {namespace = "Gtk", name = "CssSection"})
    IO (Ptr Gio.File.File)

-- | Gets the file that /@section@/ was parsed from. If no such file exists,
-- for example because the CSS was loaded via
-- /@gtkCssProviderLoadFromData@/(), then 'P.Nothing' is returned.
-- 
-- /Since: 3.2/
cssSectionGetFile ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    CssSection
    -- ^ /@section@/: the section
    -> m Gio.File.File
    -- ^ __Returns:__ the t'GI.Gio.Interfaces.File.File' that /@section@/ was parsed from
    --     or 'P.Nothing' if /@section@/ was parsed from other data
cssSectionGetFile section = liftIO $ do
    section' <- unsafeManagedPtrGetPtr section
    result <- gtk_css_section_get_file section'
    checkUnexpectedReturnNULL "cssSectionGetFile" result
    result' <- (newObject Gio.File.File) result
    touchManagedPtr section
    return result'

#if defined(ENABLE_OVERLOADING)
data CssSectionGetFileMethodInfo
instance (signature ~ (m Gio.File.File), MonadIO m) => O.OverloadedMethod CssSectionGetFileMethodInfo CssSection signature where
    overloadedMethod = cssSectionGetFile

instance O.OverloadedMethodInfo CssSectionGetFileMethodInfo CssSection where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.CssSection.cssSectionGetFile",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-CssSection.html#v:cssSectionGetFile"
        })


#endif

-- method CssSection::get_parent
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "section"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CssSection" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the section" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "CssSection" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_css_section_get_parent" gtk_css_section_get_parent :: 
    Ptr CssSection ->                       -- section : TInterface (Name {namespace = "Gtk", name = "CssSection"})
    IO (Ptr CssSection)

-- | Gets the parent section for the given /@section@/. The parent section is
-- the section that contains this /@section@/. A special case are sections of
-- type @/GTK_CSS_SECTION_DOCUMENT/@. Their parent will either be 'P.Nothing'
-- if they are the original CSS document that was loaded by
-- 'GI.Gtk.Objects.CssProvider.cssProviderLoadFromFile' or a section of type
-- @/GTK_CSS_SECTION_IMPORT/@ if it was loaded with an import rule from
-- a different file.
-- 
-- /Since: 3.2/
cssSectionGetParent ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    CssSection
    -- ^ /@section@/: the section
    -> m (Maybe CssSection)
    -- ^ __Returns:__ the parent section or 'P.Nothing' if none
cssSectionGetParent section = liftIO $ do
    section' <- unsafeManagedPtrGetPtr section
    result <- gtk_css_section_get_parent section'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (newBoxed CssSection) result'
        return result''
    touchManagedPtr section
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data CssSectionGetParentMethodInfo
instance (signature ~ (m (Maybe CssSection)), MonadIO m) => O.OverloadedMethod CssSectionGetParentMethodInfo CssSection signature where
    overloadedMethod = cssSectionGetParent

instance O.OverloadedMethodInfo CssSectionGetParentMethodInfo CssSection where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.CssSection.cssSectionGetParent",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-CssSection.html#v:cssSectionGetParent"
        })


#endif

-- method CssSection::get_section_type
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "section"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CssSection" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the section" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just
--               (TInterface Name { namespace = "Gtk" , name = "CssSectionType" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_css_section_get_section_type" gtk_css_section_get_section_type :: 
    Ptr CssSection ->                       -- section : TInterface (Name {namespace = "Gtk", name = "CssSection"})
    IO CUInt

-- | Gets the type of information that /@section@/ describes.
-- 
-- /Since: 3.2/
cssSectionGetSectionType ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    CssSection
    -- ^ /@section@/: the section
    -> m Gtk.Enums.CssSectionType
    -- ^ __Returns:__ the type of /@section@/
cssSectionGetSectionType section = liftIO $ do
    section' <- unsafeManagedPtrGetPtr section
    result <- gtk_css_section_get_section_type section'
    let result' = (toEnum . fromIntegral) result
    touchManagedPtr section
    return result'

#if defined(ENABLE_OVERLOADING)
data CssSectionGetSectionTypeMethodInfo
instance (signature ~ (m Gtk.Enums.CssSectionType), MonadIO m) => O.OverloadedMethod CssSectionGetSectionTypeMethodInfo CssSection signature where
    overloadedMethod = cssSectionGetSectionType

instance O.OverloadedMethodInfo CssSectionGetSectionTypeMethodInfo CssSection where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.CssSection.cssSectionGetSectionType",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-CssSection.html#v:cssSectionGetSectionType"
        })


#endif

-- method CssSection::get_start_line
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "section"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CssSection" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the section" , sinceVersion = Nothing }
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

foreign import ccall "gtk_css_section_get_start_line" gtk_css_section_get_start_line :: 
    Ptr CssSection ->                       -- section : TInterface (Name {namespace = "Gtk", name = "CssSection"})
    IO Word32

-- | Returns the line in the CSS document where this section starts.
-- The line number is 0-indexed, so the first line of the document
-- will return 0.
-- 
-- /Since: 3.2/
cssSectionGetStartLine ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    CssSection
    -- ^ /@section@/: the section
    -> m Word32
    -- ^ __Returns:__ the line number
cssSectionGetStartLine section = liftIO $ do
    section' <- unsafeManagedPtrGetPtr section
    result <- gtk_css_section_get_start_line section'
    touchManagedPtr section
    return result

#if defined(ENABLE_OVERLOADING)
data CssSectionGetStartLineMethodInfo
instance (signature ~ (m Word32), MonadIO m) => O.OverloadedMethod CssSectionGetStartLineMethodInfo CssSection signature where
    overloadedMethod = cssSectionGetStartLine

instance O.OverloadedMethodInfo CssSectionGetStartLineMethodInfo CssSection where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.CssSection.cssSectionGetStartLine",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-CssSection.html#v:cssSectionGetStartLine"
        })


#endif

-- method CssSection::get_start_position
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "section"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CssSection" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the section" , sinceVersion = Nothing }
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

foreign import ccall "gtk_css_section_get_start_position" gtk_css_section_get_start_position :: 
    Ptr CssSection ->                       -- section : TInterface (Name {namespace = "Gtk", name = "CssSection"})
    IO Word32

-- | Returns the offset in bytes from the start of the current line
-- returned via 'GI.Gtk.Structs.CssSection.cssSectionGetStartLine'.
-- 
-- /Since: 3.2/
cssSectionGetStartPosition ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    CssSection
    -- ^ /@section@/: the section
    -> m Word32
    -- ^ __Returns:__ the offset in bytes from the start of the line.
cssSectionGetStartPosition section = liftIO $ do
    section' <- unsafeManagedPtrGetPtr section
    result <- gtk_css_section_get_start_position section'
    touchManagedPtr section
    return result

#if defined(ENABLE_OVERLOADING)
data CssSectionGetStartPositionMethodInfo
instance (signature ~ (m Word32), MonadIO m) => O.OverloadedMethod CssSectionGetStartPositionMethodInfo CssSection signature where
    overloadedMethod = cssSectionGetStartPosition

instance O.OverloadedMethodInfo CssSectionGetStartPositionMethodInfo CssSection where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.CssSection.cssSectionGetStartPosition",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-CssSection.html#v:cssSectionGetStartPosition"
        })


#endif

-- method CssSection::ref
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "section"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CssSection" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCssSection" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "CssSection" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_css_section_ref" gtk_css_section_ref :: 
    Ptr CssSection ->                       -- section : TInterface (Name {namespace = "Gtk", name = "CssSection"})
    IO (Ptr CssSection)

-- | Increments the reference count on /@section@/.
-- 
-- /Since: 3.2/
cssSectionRef ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    CssSection
    -- ^ /@section@/: a t'GI.Gtk.Structs.CssSection.CssSection'
    -> m CssSection
    -- ^ __Returns:__ /@section@/ itself.
cssSectionRef section = liftIO $ do
    section' <- unsafeManagedPtrGetPtr section
    result <- gtk_css_section_ref section'
    checkUnexpectedReturnNULL "cssSectionRef" result
    result' <- (wrapBoxed CssSection) result
    touchManagedPtr section
    return result'

#if defined(ENABLE_OVERLOADING)
data CssSectionRefMethodInfo
instance (signature ~ (m CssSection), MonadIO m) => O.OverloadedMethod CssSectionRefMethodInfo CssSection signature where
    overloadedMethod = cssSectionRef

instance O.OverloadedMethodInfo CssSectionRefMethodInfo CssSection where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.CssSection.cssSectionRef",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-CssSection.html#v:cssSectionRef"
        })


#endif

-- method CssSection::unref
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "section"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CssSection" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCssSection" , sinceVersion = Nothing }
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

foreign import ccall "gtk_css_section_unref" gtk_css_section_unref :: 
    Ptr CssSection ->                       -- section : TInterface (Name {namespace = "Gtk", name = "CssSection"})
    IO ()

-- | Decrements the reference count on /@section@/, freeing the
-- structure if the reference count reaches 0.
-- 
-- /Since: 3.2/
cssSectionUnref ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    CssSection
    -- ^ /@section@/: a t'GI.Gtk.Structs.CssSection.CssSection'
    -> m ()
cssSectionUnref section = liftIO $ do
    section' <- unsafeManagedPtrGetPtr section
    gtk_css_section_unref section'
    touchManagedPtr section
    return ()

#if defined(ENABLE_OVERLOADING)
data CssSectionUnrefMethodInfo
instance (signature ~ (m ()), MonadIO m) => O.OverloadedMethod CssSectionUnrefMethodInfo CssSection signature where
    overloadedMethod = cssSectionUnref

instance O.OverloadedMethodInfo CssSectionUnrefMethodInfo CssSection where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.CssSection.cssSectionUnref",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-CssSection.html#v:cssSectionUnref"
        })


#endif

#if defined(ENABLE_OVERLOADING)
type family ResolveCssSectionMethod (t :: Symbol) (o :: *) :: * where
    ResolveCssSectionMethod "ref" o = CssSectionRefMethodInfo
    ResolveCssSectionMethod "unref" o = CssSectionUnrefMethodInfo
    ResolveCssSectionMethod "getEndLine" o = CssSectionGetEndLineMethodInfo
    ResolveCssSectionMethod "getEndPosition" o = CssSectionGetEndPositionMethodInfo
    ResolveCssSectionMethod "getFile" o = CssSectionGetFileMethodInfo
    ResolveCssSectionMethod "getParent" o = CssSectionGetParentMethodInfo
    ResolveCssSectionMethod "getSectionType" o = CssSectionGetSectionTypeMethodInfo
    ResolveCssSectionMethod "getStartLine" o = CssSectionGetStartLineMethodInfo
    ResolveCssSectionMethod "getStartPosition" o = CssSectionGetStartPositionMethodInfo
    ResolveCssSectionMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveCssSectionMethod t CssSection, O.OverloadedMethod info CssSection p) => OL.IsLabel t (CssSection -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveCssSectionMethod t CssSection, O.OverloadedMethod info CssSection p, R.HasField t CssSection p) => R.HasField t CssSection p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveCssSectionMethod t CssSection, O.OverloadedMethodInfo info CssSection) => OL.IsLabel t (O.MethodProxy info CssSection) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif


