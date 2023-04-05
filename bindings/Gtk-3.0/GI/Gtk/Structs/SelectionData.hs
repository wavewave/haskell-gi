{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- /No description available in the introspection data./

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Structs.SelectionData
    ( 

-- * Exported types
    SelectionData(..)                       ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [copy]("GI.Gtk.Structs.SelectionData#g:method:copy"), [free]("GI.Gtk.Structs.SelectionData#g:method:free"), [set]("GI.Gtk.Structs.SelectionData#g:method:set"), [targetsIncludeImage]("GI.Gtk.Structs.SelectionData#g:method:targetsIncludeImage"), [targetsIncludeRichText]("GI.Gtk.Structs.SelectionData#g:method:targetsIncludeRichText"), [targetsIncludeText]("GI.Gtk.Structs.SelectionData#g:method:targetsIncludeText"), [targetsIncludeUri]("GI.Gtk.Structs.SelectionData#g:method:targetsIncludeUri").
-- 
-- ==== Getters
-- [getDataType]("GI.Gtk.Structs.SelectionData#g:method:getDataType"), [getData]("GI.Gtk.Structs.SelectionData#g:method:getData"), [getDisplay]("GI.Gtk.Structs.SelectionData#g:method:getDisplay"), [getFormat]("GI.Gtk.Structs.SelectionData#g:method:getFormat"), [getLength]("GI.Gtk.Structs.SelectionData#g:method:getLength"), [getPixbuf]("GI.Gtk.Structs.SelectionData#g:method:getPixbuf"), [getSelection]("GI.Gtk.Structs.SelectionData#g:method:getSelection"), [getTarget]("GI.Gtk.Structs.SelectionData#g:method:getTarget"), [getTargets]("GI.Gtk.Structs.SelectionData#g:method:getTargets"), [getText]("GI.Gtk.Structs.SelectionData#g:method:getText"), [getUris]("GI.Gtk.Structs.SelectionData#g:method:getUris").
-- 
-- ==== Setters
-- [setPixbuf]("GI.Gtk.Structs.SelectionData#g:method:setPixbuf"), [setText]("GI.Gtk.Structs.SelectionData#g:method:setText"), [setUris]("GI.Gtk.Structs.SelectionData#g:method:setUris").

#if defined(ENABLE_OVERLOADING)
    ResolveSelectionDataMethod              ,
#endif

-- ** copy #method:copy#

#if defined(ENABLE_OVERLOADING)
    SelectionDataCopyMethodInfo             ,
#endif
    selectionDataCopy                       ,


-- ** free #method:free#

#if defined(ENABLE_OVERLOADING)
    SelectionDataFreeMethodInfo             ,
#endif
    selectionDataFree                       ,


-- ** getData #method:getData#

#if defined(ENABLE_OVERLOADING)
    SelectionDataGetDataMethodInfo          ,
#endif
    selectionDataGetData                    ,


-- ** getDataType #method:getDataType#

#if defined(ENABLE_OVERLOADING)
    SelectionDataGetDataTypeMethodInfo      ,
#endif
    selectionDataGetDataType                ,


-- ** getDisplay #method:getDisplay#

#if defined(ENABLE_OVERLOADING)
    SelectionDataGetDisplayMethodInfo       ,
#endif
    selectionDataGetDisplay                 ,


-- ** getFormat #method:getFormat#

#if defined(ENABLE_OVERLOADING)
    SelectionDataGetFormatMethodInfo        ,
#endif
    selectionDataGetFormat                  ,


-- ** getLength #method:getLength#

#if defined(ENABLE_OVERLOADING)
    SelectionDataGetLengthMethodInfo        ,
#endif
    selectionDataGetLength                  ,


-- ** getPixbuf #method:getPixbuf#

#if defined(ENABLE_OVERLOADING)
    SelectionDataGetPixbufMethodInfo        ,
#endif
    selectionDataGetPixbuf                  ,


-- ** getSelection #method:getSelection#

#if defined(ENABLE_OVERLOADING)
    SelectionDataGetSelectionMethodInfo     ,
#endif
    selectionDataGetSelection               ,


-- ** getTarget #method:getTarget#

#if defined(ENABLE_OVERLOADING)
    SelectionDataGetTargetMethodInfo        ,
#endif
    selectionDataGetTarget                  ,


-- ** getTargets #method:getTargets#

#if defined(ENABLE_OVERLOADING)
    SelectionDataGetTargetsMethodInfo       ,
#endif
    selectionDataGetTargets                 ,


-- ** getText #method:getText#

#if defined(ENABLE_OVERLOADING)
    SelectionDataGetTextMethodInfo          ,
#endif
    selectionDataGetText                    ,


-- ** getUris #method:getUris#

#if defined(ENABLE_OVERLOADING)
    SelectionDataGetUrisMethodInfo          ,
#endif
    selectionDataGetUris                    ,


-- ** set #method:set#

#if defined(ENABLE_OVERLOADING)
    SelectionDataSetMethodInfo              ,
#endif
    selectionDataSet                        ,


-- ** setPixbuf #method:setPixbuf#

#if defined(ENABLE_OVERLOADING)
    SelectionDataSetPixbufMethodInfo        ,
#endif
    selectionDataSetPixbuf                  ,


-- ** setText #method:setText#

#if defined(ENABLE_OVERLOADING)
    SelectionDataSetTextMethodInfo          ,
#endif
    selectionDataSetText                    ,


-- ** setUris #method:setUris#

#if defined(ENABLE_OVERLOADING)
    SelectionDataSetUrisMethodInfo          ,
#endif
    selectionDataSetUris                    ,


-- ** targetsIncludeImage #method:targetsIncludeImage#

#if defined(ENABLE_OVERLOADING)
    SelectionDataTargetsIncludeImageMethodInfo,
#endif
    selectionDataTargetsIncludeImage        ,


-- ** targetsIncludeRichText #method:targetsIncludeRichText#

#if defined(ENABLE_OVERLOADING)
    SelectionDataTargetsIncludeRichTextMethodInfo,
#endif
    selectionDataTargetsIncludeRichText     ,


-- ** targetsIncludeText #method:targetsIncludeText#

#if defined(ENABLE_OVERLOADING)
    SelectionDataTargetsIncludeTextMethodInfo,
#endif
    selectionDataTargetsIncludeText         ,


-- ** targetsIncludeUri #method:targetsIncludeUri#

#if defined(ENABLE_OVERLOADING)
    SelectionDataTargetsIncludeUriMethodInfo,
#endif
    selectionDataTargetsIncludeUri          ,




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

import qualified GI.Gdk.Objects.Display as Gdk.Display
import qualified GI.Gdk.Structs.Atom as Gdk.Atom
import qualified GI.GdkPixbuf.Objects.Pixbuf as GdkPixbuf.Pixbuf
import {-# SOURCE #-} qualified GI.Gtk.Objects.TextBuffer as Gtk.TextBuffer

-- | Memory-managed wrapper type.
newtype SelectionData = SelectionData (SP.ManagedPtr SelectionData)
    deriving (Eq)

instance SP.ManagedPtrNewtype SelectionData where
    toManagedPtr (SelectionData p) = p

foreign import ccall "gtk_selection_data_get_type" c_gtk_selection_data_get_type :: 
    IO GType

type instance O.ParentTypes SelectionData = '[]
instance O.HasParentTypes SelectionData

instance B.Types.TypedObject SelectionData where
    glibType = c_gtk_selection_data_get_type

instance B.Types.GBoxed SelectionData

-- | Convert 'SelectionData' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe SelectionData) where
    gvalueGType_ = c_gtk_selection_data_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_boxed gv (FP.nullPtr :: FP.Ptr SelectionData)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_boxed gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_boxed gv :: IO (Ptr SelectionData)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newBoxed SelectionData ptr
        else return P.Nothing
        
    


#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList SelectionData
type instance O.AttributeList SelectionData = SelectionDataAttributeList
type SelectionDataAttributeList = ('[ ] :: [(Symbol, *)])
#endif

-- method SelectionData::copy
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "data"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "SelectionData" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a pointer to a #GtkSelectionData-struct."
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
--               (TInterface Name { namespace = "Gtk" , name = "SelectionData" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_selection_data_copy" gtk_selection_data_copy :: 
    Ptr SelectionData ->                    -- data : TInterface (Name {namespace = "Gtk", name = "SelectionData"})
    IO (Ptr SelectionData)

-- | Makes a copy of a t'GI.Gtk.Structs.SelectionData.SelectionData'-struct and its data.
selectionDataCopy ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    SelectionData
    -- ^ /@data@/: a pointer to a t'GI.Gtk.Structs.SelectionData.SelectionData'-struct.
    -> m SelectionData
    -- ^ __Returns:__ a pointer to a copy of /@data@/.
selectionDataCopy data_ = liftIO $ do
    data_' <- unsafeManagedPtrGetPtr data_
    result <- gtk_selection_data_copy data_'
    checkUnexpectedReturnNULL "selectionDataCopy" result
    result' <- (wrapBoxed SelectionData) result
    touchManagedPtr data_
    return result'

#if defined(ENABLE_OVERLOADING)
data SelectionDataCopyMethodInfo
instance (signature ~ (m SelectionData), MonadIO m) => O.OverloadedMethod SelectionDataCopyMethodInfo SelectionData signature where
    overloadedMethod = selectionDataCopy

instance O.OverloadedMethodInfo SelectionDataCopyMethodInfo SelectionData where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.SelectionData.selectionDataCopy",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-SelectionData.html#v:selectionDataCopy"
        })


#endif

-- method SelectionData::free
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "data"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "SelectionData" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a pointer to a #GtkSelectionData-struct."
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

foreign import ccall "gtk_selection_data_free" gtk_selection_data_free :: 
    Ptr SelectionData ->                    -- data : TInterface (Name {namespace = "Gtk", name = "SelectionData"})
    IO ()

-- | Frees a t'GI.Gtk.Structs.SelectionData.SelectionData'-struct returned from
-- 'GI.Gtk.Structs.SelectionData.selectionDataCopy'.
selectionDataFree ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    SelectionData
    -- ^ /@data@/: a pointer to a t'GI.Gtk.Structs.SelectionData.SelectionData'-struct.
    -> m ()
selectionDataFree data_ = liftIO $ do
    data_' <- unsafeManagedPtrGetPtr data_
    gtk_selection_data_free data_'
    touchManagedPtr data_
    return ()

#if defined(ENABLE_OVERLOADING)
data SelectionDataFreeMethodInfo
instance (signature ~ (m ()), MonadIO m) => O.OverloadedMethod SelectionDataFreeMethodInfo SelectionData signature where
    overloadedMethod = selectionDataFree

instance O.OverloadedMethodInfo SelectionDataFreeMethodInfo SelectionData where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.SelectionData.selectionDataFree",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-SelectionData.html#v:selectionDataFree"
        })


#endif

-- method SelectionData::get_data_type
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "selection_data"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "SelectionData" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a pointer to a #GtkSelectionData-struct."
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
-- returnType: Just (TInterface Name { namespace = "Gdk" , name = "Atom" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_selection_data_get_data_type" gtk_selection_data_get_data_type :: 
    Ptr SelectionData ->                    -- selection_data : TInterface (Name {namespace = "Gtk", name = "SelectionData"})
    IO (Ptr Gdk.Atom.Atom)

-- | Retrieves the data type of the selection.
-- 
-- /Since: 2.14/
selectionDataGetDataType ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    SelectionData
    -- ^ /@selectionData@/: a pointer to a t'GI.Gtk.Structs.SelectionData.SelectionData'-struct.
    -> m Gdk.Atom.Atom
    -- ^ __Returns:__ the data type of the selection.
selectionDataGetDataType selectionData = liftIO $ do
    selectionData' <- unsafeManagedPtrGetPtr selectionData
    result <- gtk_selection_data_get_data_type selectionData'
    checkUnexpectedReturnNULL "selectionDataGetDataType" result
    result' <- (newPtr Gdk.Atom.Atom) result
    touchManagedPtr selectionData
    return result'

#if defined(ENABLE_OVERLOADING)
data SelectionDataGetDataTypeMethodInfo
instance (signature ~ (m Gdk.Atom.Atom), MonadIO m) => O.OverloadedMethod SelectionDataGetDataTypeMethodInfo SelectionData signature where
    overloadedMethod = selectionDataGetDataType

instance O.OverloadedMethodInfo SelectionDataGetDataTypeMethodInfo SelectionData where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.SelectionData.selectionDataGetDataType",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-SelectionData.html#v:selectionDataGetDataType"
        })


#endif

-- method SelectionData::get_data
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "selection_data"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "SelectionData" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a pointer to a #GtkSelectionData-struct."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "length"
--           , argType = TBasicType TInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "return location for length of the data segment"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       ]
-- Lengths: [ Arg
--              { argCName = "length"
--              , argType = TBasicType TInt
--              , direction = DirectionOut
--              , mayBeNull = False
--              , argDoc =
--                  Documentation
--                    { rawDocText =
--                        Just "return location for length of the data segment"
--                    , sinceVersion = Nothing
--                    }
--              , argScope = ScopeTypeInvalid
--              , argClosure = -1
--              , argDestroy = -1
--              , argCallerAllocates = False
--              , transfer = TransferEverything
--              }
--          ]
-- returnType: Just (TCArray False (-1) 1 (TBasicType TUInt8))
-- throws : False
-- Skip return : False

foreign import ccall "gtk_selection_data_get_data_with_length" gtk_selection_data_get_data_with_length :: 
    Ptr SelectionData ->                    -- selection_data : TInterface (Name {namespace = "Gtk", name = "SelectionData"})
    Ptr Int32 ->                            -- length : TBasicType TInt
    IO (Ptr Word8)

-- | Retrieves the raw data of the selection along with its length.
-- 
-- /Since: 3.0/
selectionDataGetData ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    SelectionData
    -- ^ /@selectionData@/: a pointer to a t'GI.Gtk.Structs.SelectionData.SelectionData'-struct.
    -> m ByteString
    -- ^ __Returns:__ the raw data of the selection
selectionDataGetData selectionData = liftIO $ do
    selectionData' <- unsafeManagedPtrGetPtr selectionData
    length_ <- allocMem :: IO (Ptr Int32)
    result <- gtk_selection_data_get_data_with_length selectionData' length_
    length_' <- peek length_
    checkUnexpectedReturnNULL "selectionDataGetData" result
    result' <- (unpackByteStringWithLength length_') result
    touchManagedPtr selectionData
    freeMem length_
    return result'

#if defined(ENABLE_OVERLOADING)
data SelectionDataGetDataMethodInfo
instance (signature ~ (m ByteString), MonadIO m) => O.OverloadedMethod SelectionDataGetDataMethodInfo SelectionData signature where
    overloadedMethod = selectionDataGetData

instance O.OverloadedMethodInfo SelectionDataGetDataMethodInfo SelectionData where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.SelectionData.selectionDataGetData",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-SelectionData.html#v:selectionDataGetData"
        })


#endif

-- method SelectionData::get_display
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "selection_data"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "SelectionData" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a pointer to a #GtkSelectionData-struct."
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
-- returnType: Just (TInterface Name { namespace = "Gdk" , name = "Display" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_selection_data_get_display" gtk_selection_data_get_display :: 
    Ptr SelectionData ->                    -- selection_data : TInterface (Name {namespace = "Gtk", name = "SelectionData"})
    IO (Ptr Gdk.Display.Display)

-- | Retrieves the display of the selection.
-- 
-- /Since: 2.14/
selectionDataGetDisplay ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    SelectionData
    -- ^ /@selectionData@/: a pointer to a t'GI.Gtk.Structs.SelectionData.SelectionData'-struct.
    -> m Gdk.Display.Display
    -- ^ __Returns:__ the display of the selection.
selectionDataGetDisplay selectionData = liftIO $ do
    selectionData' <- unsafeManagedPtrGetPtr selectionData
    result <- gtk_selection_data_get_display selectionData'
    checkUnexpectedReturnNULL "selectionDataGetDisplay" result
    result' <- (newObject Gdk.Display.Display) result
    touchManagedPtr selectionData
    return result'

#if defined(ENABLE_OVERLOADING)
data SelectionDataGetDisplayMethodInfo
instance (signature ~ (m Gdk.Display.Display), MonadIO m) => O.OverloadedMethod SelectionDataGetDisplayMethodInfo SelectionData signature where
    overloadedMethod = selectionDataGetDisplay

instance O.OverloadedMethodInfo SelectionDataGetDisplayMethodInfo SelectionData where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.SelectionData.selectionDataGetDisplay",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-SelectionData.html#v:selectionDataGetDisplay"
        })


#endif

-- method SelectionData::get_format
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "selection_data"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "SelectionData" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a pointer to a #GtkSelectionData-struct."
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

foreign import ccall "gtk_selection_data_get_format" gtk_selection_data_get_format :: 
    Ptr SelectionData ->                    -- selection_data : TInterface (Name {namespace = "Gtk", name = "SelectionData"})
    IO Int32

-- | Retrieves the format of the selection.
-- 
-- /Since: 2.14/
selectionDataGetFormat ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    SelectionData
    -- ^ /@selectionData@/: a pointer to a t'GI.Gtk.Structs.SelectionData.SelectionData'-struct.
    -> m Int32
    -- ^ __Returns:__ the format of the selection.
selectionDataGetFormat selectionData = liftIO $ do
    selectionData' <- unsafeManagedPtrGetPtr selectionData
    result <- gtk_selection_data_get_format selectionData'
    touchManagedPtr selectionData
    return result

#if defined(ENABLE_OVERLOADING)
data SelectionDataGetFormatMethodInfo
instance (signature ~ (m Int32), MonadIO m) => O.OverloadedMethod SelectionDataGetFormatMethodInfo SelectionData signature where
    overloadedMethod = selectionDataGetFormat

instance O.OverloadedMethodInfo SelectionDataGetFormatMethodInfo SelectionData where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.SelectionData.selectionDataGetFormat",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-SelectionData.html#v:selectionDataGetFormat"
        })


#endif

-- method SelectionData::get_length
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "selection_data"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "SelectionData" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a pointer to a #GtkSelectionData-struct."
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

foreign import ccall "gtk_selection_data_get_length" gtk_selection_data_get_length :: 
    Ptr SelectionData ->                    -- selection_data : TInterface (Name {namespace = "Gtk", name = "SelectionData"})
    IO Int32

-- | Retrieves the length of the raw data of the selection.
-- 
-- /Since: 2.14/
selectionDataGetLength ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    SelectionData
    -- ^ /@selectionData@/: a pointer to a t'GI.Gtk.Structs.SelectionData.SelectionData'-struct.
    -> m Int32
    -- ^ __Returns:__ the length of the data of the selection.
selectionDataGetLength selectionData = liftIO $ do
    selectionData' <- unsafeManagedPtrGetPtr selectionData
    result <- gtk_selection_data_get_length selectionData'
    touchManagedPtr selectionData
    return result

#if defined(ENABLE_OVERLOADING)
data SelectionDataGetLengthMethodInfo
instance (signature ~ (m Int32), MonadIO m) => O.OverloadedMethod SelectionDataGetLengthMethodInfo SelectionData signature where
    overloadedMethod = selectionDataGetLength

instance O.OverloadedMethodInfo SelectionDataGetLengthMethodInfo SelectionData where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.SelectionData.selectionDataGetLength",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-SelectionData.html#v:selectionDataGetLength"
        })


#endif

-- method SelectionData::get_pixbuf
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "selection_data"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "SelectionData" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkSelectionData"
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
--               (TInterface Name { namespace = "GdkPixbuf" , name = "Pixbuf" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_selection_data_get_pixbuf" gtk_selection_data_get_pixbuf :: 
    Ptr SelectionData ->                    -- selection_data : TInterface (Name {namespace = "Gtk", name = "SelectionData"})
    IO (Ptr GdkPixbuf.Pixbuf.Pixbuf)

-- | Gets the contents of the selection data as a t'GI.GdkPixbuf.Objects.Pixbuf.Pixbuf'.
-- 
-- /Since: 2.6/
selectionDataGetPixbuf ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    SelectionData
    -- ^ /@selectionData@/: a t'GI.Gtk.Structs.SelectionData.SelectionData'
    -> m (Maybe GdkPixbuf.Pixbuf.Pixbuf)
    -- ^ __Returns:__ if the selection data
    --   contained a recognized image type and it could be converted to a
    --   t'GI.GdkPixbuf.Objects.Pixbuf.Pixbuf', a newly allocated pixbuf is returned, otherwise
    --   'P.Nothing'.  If the result is non-'P.Nothing' it must be freed with
    --   'GI.GObject.Objects.Object.objectUnref'.
selectionDataGetPixbuf selectionData = liftIO $ do
    selectionData' <- unsafeManagedPtrGetPtr selectionData
    result <- gtk_selection_data_get_pixbuf selectionData'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (wrapObject GdkPixbuf.Pixbuf.Pixbuf) result'
        return result''
    touchManagedPtr selectionData
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data SelectionDataGetPixbufMethodInfo
instance (signature ~ (m (Maybe GdkPixbuf.Pixbuf.Pixbuf)), MonadIO m) => O.OverloadedMethod SelectionDataGetPixbufMethodInfo SelectionData signature where
    overloadedMethod = selectionDataGetPixbuf

instance O.OverloadedMethodInfo SelectionDataGetPixbufMethodInfo SelectionData where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.SelectionData.selectionDataGetPixbuf",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-SelectionData.html#v:selectionDataGetPixbuf"
        })


#endif

-- method SelectionData::get_selection
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "selection_data"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "SelectionData" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a pointer to a #GtkSelectionData-struct."
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
-- returnType: Just (TInterface Name { namespace = "Gdk" , name = "Atom" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_selection_data_get_selection" gtk_selection_data_get_selection :: 
    Ptr SelectionData ->                    -- selection_data : TInterface (Name {namespace = "Gtk", name = "SelectionData"})
    IO (Ptr Gdk.Atom.Atom)

-- | Retrieves the selection t'GI.Gdk.Structs.Atom.Atom' of the selection data.
-- 
-- /Since: 2.16/
selectionDataGetSelection ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    SelectionData
    -- ^ /@selectionData@/: a pointer to a t'GI.Gtk.Structs.SelectionData.SelectionData'-struct.
    -> m Gdk.Atom.Atom
    -- ^ __Returns:__ the selection t'GI.Gdk.Structs.Atom.Atom' of the selection data.
selectionDataGetSelection selectionData = liftIO $ do
    selectionData' <- unsafeManagedPtrGetPtr selectionData
    result <- gtk_selection_data_get_selection selectionData'
    checkUnexpectedReturnNULL "selectionDataGetSelection" result
    result' <- (newPtr Gdk.Atom.Atom) result
    touchManagedPtr selectionData
    return result'

#if defined(ENABLE_OVERLOADING)
data SelectionDataGetSelectionMethodInfo
instance (signature ~ (m Gdk.Atom.Atom), MonadIO m) => O.OverloadedMethod SelectionDataGetSelectionMethodInfo SelectionData signature where
    overloadedMethod = selectionDataGetSelection

instance O.OverloadedMethodInfo SelectionDataGetSelectionMethodInfo SelectionData where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.SelectionData.selectionDataGetSelection",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-SelectionData.html#v:selectionDataGetSelection"
        })


#endif

-- method SelectionData::get_target
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "selection_data"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "SelectionData" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a pointer to a #GtkSelectionData-struct."
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
-- returnType: Just (TInterface Name { namespace = "Gdk" , name = "Atom" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_selection_data_get_target" gtk_selection_data_get_target :: 
    Ptr SelectionData ->                    -- selection_data : TInterface (Name {namespace = "Gtk", name = "SelectionData"})
    IO (Ptr Gdk.Atom.Atom)

-- | Retrieves the target of the selection.
-- 
-- /Since: 2.14/
selectionDataGetTarget ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    SelectionData
    -- ^ /@selectionData@/: a pointer to a t'GI.Gtk.Structs.SelectionData.SelectionData'-struct.
    -> m Gdk.Atom.Atom
    -- ^ __Returns:__ the target of the selection.
selectionDataGetTarget selectionData = liftIO $ do
    selectionData' <- unsafeManagedPtrGetPtr selectionData
    result <- gtk_selection_data_get_target selectionData'
    checkUnexpectedReturnNULL "selectionDataGetTarget" result
    result' <- (newPtr Gdk.Atom.Atom) result
    touchManagedPtr selectionData
    return result'

#if defined(ENABLE_OVERLOADING)
data SelectionDataGetTargetMethodInfo
instance (signature ~ (m Gdk.Atom.Atom), MonadIO m) => O.OverloadedMethod SelectionDataGetTargetMethodInfo SelectionData signature where
    overloadedMethod = selectionDataGetTarget

instance O.OverloadedMethodInfo SelectionDataGetTargetMethodInfo SelectionData where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.SelectionData.selectionDataGetTarget",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-SelectionData.html#v:selectionDataGetTarget"
        })


#endif

-- method SelectionData::get_targets
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "selection_data"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "SelectionData" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkSelectionData object"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "targets"
--           , argType =
--               TCArray
--                 False
--                 (-1)
--                 2
--                 (TInterface Name { namespace = "Gdk" , name = "Atom" })
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "\n          location to store an array of targets. The result stored\n          here must be freed with g_free()."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferContainer
--           }
--       , Arg
--           { argCName = "n_atoms"
--           , argType = TBasicType TInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "location to store number of items in @targets."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       ]
-- Lengths: [ Arg
--              { argCName = "n_atoms"
--              , argType = TBasicType TInt
--              , direction = DirectionOut
--              , mayBeNull = False
--              , argDoc =
--                  Documentation
--                    { rawDocText =
--                        Just "location to store number of items in @targets."
--                    , sinceVersion = Nothing
--                    }
--              , argScope = ScopeTypeInvalid
--              , argClosure = -1
--              , argDestroy = -1
--              , argCallerAllocates = False
--              , transfer = TransferEverything
--              }
--          ]
-- returnType: Just (TBasicType TBoolean)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_selection_data_get_targets" gtk_selection_data_get_targets :: 
    Ptr SelectionData ->                    -- selection_data : TInterface (Name {namespace = "Gtk", name = "SelectionData"})
    Ptr (Ptr (Ptr Gdk.Atom.Atom)) ->        -- targets : TCArray False (-1) 2 (TInterface (Name {namespace = "Gdk", name = "Atom"}))
    Ptr Int32 ->                            -- n_atoms : TBasicType TInt
    IO CInt

-- | Gets the contents of /@selectionData@/ as an array of targets.
-- This can be used to interpret the results of getting
-- the standard TARGETS target that is always supplied for
-- any selection.
selectionDataGetTargets ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    SelectionData
    -- ^ /@selectionData@/: a t'GI.Gtk.Structs.SelectionData.SelectionData' object
    -> m ((Bool, [Gdk.Atom.Atom]))
    -- ^ __Returns:__ 'P.True' if /@selectionData@/ contains a valid
    --    array of targets, otherwise 'P.False'.
selectionDataGetTargets selectionData = liftIO $ do
    selectionData' <- unsafeManagedPtrGetPtr selectionData
    targets <- callocMem :: IO (Ptr (Ptr (Ptr Gdk.Atom.Atom)))
    nAtoms <- allocMem :: IO (Ptr Int32)
    result <- gtk_selection_data_get_targets selectionData' targets nAtoms
    nAtoms' <- peek nAtoms
    let result' = (/= 0) result
    targets' <- peek targets
    targets'' <- (unpackPtrArrayWithLength nAtoms') targets'
    targets''' <- mapM (newPtr Gdk.Atom.Atom) targets''
    freeMem targets'
    touchManagedPtr selectionData
    freeMem targets
    freeMem nAtoms
    return (result', targets''')

#if defined(ENABLE_OVERLOADING)
data SelectionDataGetTargetsMethodInfo
instance (signature ~ (m ((Bool, [Gdk.Atom.Atom]))), MonadIO m) => O.OverloadedMethod SelectionDataGetTargetsMethodInfo SelectionData signature where
    overloadedMethod = selectionDataGetTargets

instance O.OverloadedMethodInfo SelectionDataGetTargetsMethodInfo SelectionData where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.SelectionData.selectionDataGetTargets",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-SelectionData.html#v:selectionDataGetTargets"
        })


#endif

-- method SelectionData::get_text
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "selection_data"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "SelectionData" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkSelectionData"
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

foreign import ccall "gtk_selection_data_get_text" gtk_selection_data_get_text :: 
    Ptr SelectionData ->                    -- selection_data : TInterface (Name {namespace = "Gtk", name = "SelectionData"})
    IO CString

-- | Gets the contents of the selection data as a UTF-8 string.
selectionDataGetText ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    SelectionData
    -- ^ /@selectionData@/: a t'GI.Gtk.Structs.SelectionData.SelectionData'
    -> m (Maybe T.Text)
    -- ^ __Returns:__ if the selection data contained a
    --   recognized text type and it could be converted to UTF-8, a newly
    --   allocated string containing the converted text, otherwise 'P.Nothing'.
    --   If the result is non-'P.Nothing' it must be freed with 'GI.GLib.Functions.free'.
selectionDataGetText selectionData = liftIO $ do
    selectionData' <- unsafeManagedPtrGetPtr selectionData
    result <- gtk_selection_data_get_text selectionData'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- cstringToText result'
        freeMem result'
        return result''
    touchManagedPtr selectionData
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data SelectionDataGetTextMethodInfo
instance (signature ~ (m (Maybe T.Text)), MonadIO m) => O.OverloadedMethod SelectionDataGetTextMethodInfo SelectionData signature where
    overloadedMethod = selectionDataGetText

instance O.OverloadedMethodInfo SelectionDataGetTextMethodInfo SelectionData where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.SelectionData.selectionDataGetText",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-SelectionData.html#v:selectionDataGetText"
        })


#endif

-- method SelectionData::get_uris
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "selection_data"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "SelectionData" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkSelectionData"
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
-- returnType: Just (TCArray True (-1) (-1) (TBasicType TUTF8))
-- throws : False
-- Skip return : False

foreign import ccall "gtk_selection_data_get_uris" gtk_selection_data_get_uris :: 
    Ptr SelectionData ->                    -- selection_data : TInterface (Name {namespace = "Gtk", name = "SelectionData"})
    IO (Ptr CString)

-- | Gets the contents of the selection data as array of URIs.
-- 
-- /Since: 2.6/
selectionDataGetUris ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    SelectionData
    -- ^ /@selectionData@/: a t'GI.Gtk.Structs.SelectionData.SelectionData'
    -> m [T.Text]
    -- ^ __Returns:__ if
    --   the selection data contains a list of
    --   URIs, a newly allocated 'P.Nothing'-terminated string array
    --   containing the URIs, otherwise 'P.Nothing'. If the result is
    --   non-'P.Nothing' it must be freed with 'GI.GLib.Functions.strfreev'.
selectionDataGetUris selectionData = liftIO $ do
    selectionData' <- unsafeManagedPtrGetPtr selectionData
    result <- gtk_selection_data_get_uris selectionData'
    checkUnexpectedReturnNULL "selectionDataGetUris" result
    result' <- unpackZeroTerminatedUTF8CArray result
    mapZeroTerminatedCArray freeMem result
    freeMem result
    touchManagedPtr selectionData
    return result'

#if defined(ENABLE_OVERLOADING)
data SelectionDataGetUrisMethodInfo
instance (signature ~ (m [T.Text]), MonadIO m) => O.OverloadedMethod SelectionDataGetUrisMethodInfo SelectionData signature where
    overloadedMethod = selectionDataGetUris

instance O.OverloadedMethodInfo SelectionDataGetUrisMethodInfo SelectionData where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.SelectionData.selectionDataGetUris",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-SelectionData.html#v:selectionDataGetUris"
        })


#endif

-- method SelectionData::set
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "selection_data"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "SelectionData" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a pointer to a #GtkSelectionData-struct."
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
--           , argType = TInterface Name { namespace = "Gdk" , name = "Atom" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the type of selection data"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "format"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "format (number of bits in a unit)"
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
--           , argType = TCArray False (-1) 4 (TBasicType TUInt8)
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "pointer to the data (will be copied)"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "length"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "length of the data" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: [ Arg
--              { argCName = "length"
--              , argType = TBasicType TInt
--              , direction = DirectionIn
--              , mayBeNull = False
--              , argDoc =
--                  Documentation
--                    { rawDocText = Just "length of the data" , sinceVersion = Nothing }
--              , argScope = ScopeTypeInvalid
--              , argClosure = -1
--              , argDestroy = -1
--              , argCallerAllocates = False
--              , transfer = TransferNothing
--              }
--          ]
-- returnType: Nothing
-- throws : False
-- Skip return : False

foreign import ccall "gtk_selection_data_set" gtk_selection_data_set :: 
    Ptr SelectionData ->                    -- selection_data : TInterface (Name {namespace = "Gtk", name = "SelectionData"})
    Ptr Gdk.Atom.Atom ->                    -- type : TInterface (Name {namespace = "Gdk", name = "Atom"})
    Int32 ->                                -- format : TBasicType TInt
    Ptr Word8 ->                            -- data : TCArray False (-1) 4 (TBasicType TUInt8)
    Int32 ->                                -- length : TBasicType TInt
    IO ()

-- | Stores new data into a t'GI.Gtk.Structs.SelectionData.SelectionData' object. Should
-- only be called from a selection handler callback.
-- Zero-terminates the stored data.
selectionDataSet ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    SelectionData
    -- ^ /@selectionData@/: a pointer to a t'GI.Gtk.Structs.SelectionData.SelectionData'-struct.
    -> Gdk.Atom.Atom
    -- ^ /@type@/: the type of selection data
    -> Int32
    -- ^ /@format@/: format (number of bits in a unit)
    -> ByteString
    -- ^ /@data@/: pointer to the data (will be copied)
    -> m ()
selectionDataSet selectionData type_ format data_ = liftIO $ do
    let length_ = fromIntegral $ B.length data_
    selectionData' <- unsafeManagedPtrGetPtr selectionData
    type_' <- unsafeManagedPtrGetPtr type_
    data_' <- packByteString data_
    gtk_selection_data_set selectionData' type_' format data_' length_
    touchManagedPtr selectionData
    touchManagedPtr type_
    freeMem data_'
    return ()

#if defined(ENABLE_OVERLOADING)
data SelectionDataSetMethodInfo
instance (signature ~ (Gdk.Atom.Atom -> Int32 -> ByteString -> m ()), MonadIO m) => O.OverloadedMethod SelectionDataSetMethodInfo SelectionData signature where
    overloadedMethod = selectionDataSet

instance O.OverloadedMethodInfo SelectionDataSetMethodInfo SelectionData where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.SelectionData.selectionDataSet",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-SelectionData.html#v:selectionDataSet"
        })


#endif

-- method SelectionData::set_pixbuf
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "selection_data"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "SelectionData" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkSelectionData"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "pixbuf"
--           , argType =
--               TInterface Name { namespace = "GdkPixbuf" , name = "Pixbuf" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GdkPixbuf" , sinceVersion = Nothing }
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

foreign import ccall "gtk_selection_data_set_pixbuf" gtk_selection_data_set_pixbuf :: 
    Ptr SelectionData ->                    -- selection_data : TInterface (Name {namespace = "Gtk", name = "SelectionData"})
    Ptr GdkPixbuf.Pixbuf.Pixbuf ->          -- pixbuf : TInterface (Name {namespace = "GdkPixbuf", name = "Pixbuf"})
    IO CInt

-- | Sets the contents of the selection from a t'GI.GdkPixbuf.Objects.Pixbuf.Pixbuf'
-- The pixbuf is converted to the form determined by
-- /@selectionData@/->target.
-- 
-- /Since: 2.6/
selectionDataSetPixbuf ::
    (B.CallStack.HasCallStack, MonadIO m, GdkPixbuf.Pixbuf.IsPixbuf a) =>
    SelectionData
    -- ^ /@selectionData@/: a t'GI.Gtk.Structs.SelectionData.SelectionData'
    -> a
    -- ^ /@pixbuf@/: a t'GI.GdkPixbuf.Objects.Pixbuf.Pixbuf'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the selection was successfully set,
    --   otherwise 'P.False'.
selectionDataSetPixbuf selectionData pixbuf = liftIO $ do
    selectionData' <- unsafeManagedPtrGetPtr selectionData
    pixbuf' <- unsafeManagedPtrCastPtr pixbuf
    result <- gtk_selection_data_set_pixbuf selectionData' pixbuf'
    let result' = (/= 0) result
    touchManagedPtr selectionData
    touchManagedPtr pixbuf
    return result'

#if defined(ENABLE_OVERLOADING)
data SelectionDataSetPixbufMethodInfo
instance (signature ~ (a -> m Bool), MonadIO m, GdkPixbuf.Pixbuf.IsPixbuf a) => O.OverloadedMethod SelectionDataSetPixbufMethodInfo SelectionData signature where
    overloadedMethod = selectionDataSetPixbuf

instance O.OverloadedMethodInfo SelectionDataSetPixbufMethodInfo SelectionData where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.SelectionData.selectionDataSetPixbuf",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-SelectionData.html#v:selectionDataSetPixbuf"
        })


#endif

-- method SelectionData::set_text
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "selection_data"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "SelectionData" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkSelectionData"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "str"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a UTF-8 string" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "len"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "the length of @str, or -1 if @str is nul-terminated."
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

foreign import ccall "gtk_selection_data_set_text" gtk_selection_data_set_text :: 
    Ptr SelectionData ->                    -- selection_data : TInterface (Name {namespace = "Gtk", name = "SelectionData"})
    CString ->                              -- str : TBasicType TUTF8
    Int32 ->                                -- len : TBasicType TInt
    IO CInt

-- | Sets the contents of the selection from a UTF-8 encoded string.
-- The string is converted to the form determined by
-- /@selectionData@/->target.
selectionDataSetText ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    SelectionData
    -- ^ /@selectionData@/: a t'GI.Gtk.Structs.SelectionData.SelectionData'
    -> T.Text
    -- ^ /@str@/: a UTF-8 string
    -> Int32
    -- ^ /@len@/: the length of /@str@/, or -1 if /@str@/ is nul-terminated.
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the selection was successfully set,
    --   otherwise 'P.False'.
selectionDataSetText selectionData str len = liftIO $ do
    selectionData' <- unsafeManagedPtrGetPtr selectionData
    str' <- textToCString str
    result <- gtk_selection_data_set_text selectionData' str' len
    let result' = (/= 0) result
    touchManagedPtr selectionData
    freeMem str'
    return result'

#if defined(ENABLE_OVERLOADING)
data SelectionDataSetTextMethodInfo
instance (signature ~ (T.Text -> Int32 -> m Bool), MonadIO m) => O.OverloadedMethod SelectionDataSetTextMethodInfo SelectionData signature where
    overloadedMethod = selectionDataSetText

instance O.OverloadedMethodInfo SelectionDataSetTextMethodInfo SelectionData where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.SelectionData.selectionDataSetText",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-SelectionData.html#v:selectionDataSetText"
        })


#endif

-- method SelectionData::set_uris
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "selection_data"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "SelectionData" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkSelectionData"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "uris"
--           , argType = TCArray True (-1) (-1) (TBasicType TUTF8)
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "a %NULL-terminated array of\n    strings holding URIs"
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

foreign import ccall "gtk_selection_data_set_uris" gtk_selection_data_set_uris :: 
    Ptr SelectionData ->                    -- selection_data : TInterface (Name {namespace = "Gtk", name = "SelectionData"})
    Ptr CString ->                          -- uris : TCArray True (-1) (-1) (TBasicType TUTF8)
    IO CInt

-- | Sets the contents of the selection from a list of URIs.
-- The string is converted to the form determined by
-- /@selectionData@/->target.
-- 
-- /Since: 2.6/
selectionDataSetUris ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    SelectionData
    -- ^ /@selectionData@/: a t'GI.Gtk.Structs.SelectionData.SelectionData'
    -> [T.Text]
    -- ^ /@uris@/: a 'P.Nothing'-terminated array of
    --     strings holding URIs
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the selection was successfully set,
    --   otherwise 'P.False'.
selectionDataSetUris selectionData uris = liftIO $ do
    selectionData' <- unsafeManagedPtrGetPtr selectionData
    uris' <- packZeroTerminatedUTF8CArray uris
    result <- gtk_selection_data_set_uris selectionData' uris'
    let result' = (/= 0) result
    touchManagedPtr selectionData
    mapZeroTerminatedCArray freeMem uris'
    freeMem uris'
    return result'

#if defined(ENABLE_OVERLOADING)
data SelectionDataSetUrisMethodInfo
instance (signature ~ ([T.Text] -> m Bool), MonadIO m) => O.OverloadedMethod SelectionDataSetUrisMethodInfo SelectionData signature where
    overloadedMethod = selectionDataSetUris

instance O.OverloadedMethodInfo SelectionDataSetUrisMethodInfo SelectionData where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.SelectionData.selectionDataSetUris",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-SelectionData.html#v:selectionDataSetUris"
        })


#endif

-- method SelectionData::targets_include_image
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "selection_data"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "SelectionData" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkSelectionData object"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "writable"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "whether to accept only targets for which GTK+ knows\n  how to convert a pixbuf into the format"
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

foreign import ccall "gtk_selection_data_targets_include_image" gtk_selection_data_targets_include_image :: 
    Ptr SelectionData ->                    -- selection_data : TInterface (Name {namespace = "Gtk", name = "SelectionData"})
    CInt ->                                 -- writable : TBasicType TBoolean
    IO CInt

-- | Given a t'GI.Gtk.Structs.SelectionData.SelectionData' object holding a list of targets,
-- determines if any of the targets in /@targets@/ can be used to
-- provide a t'GI.GdkPixbuf.Objects.Pixbuf.Pixbuf'.
-- 
-- /Since: 2.6/
selectionDataTargetsIncludeImage ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    SelectionData
    -- ^ /@selectionData@/: a t'GI.Gtk.Structs.SelectionData.SelectionData' object
    -> Bool
    -- ^ /@writable@/: whether to accept only targets for which GTK+ knows
    --   how to convert a pixbuf into the format
    -> m Bool
    -- ^ __Returns:__ 'P.True' if /@selectionData@/ holds a list of targets,
    --   and a suitable target for images is included, otherwise 'P.False'.
selectionDataTargetsIncludeImage selectionData writable = liftIO $ do
    selectionData' <- unsafeManagedPtrGetPtr selectionData
    let writable' = (fromIntegral . fromEnum) writable
    result <- gtk_selection_data_targets_include_image selectionData' writable'
    let result' = (/= 0) result
    touchManagedPtr selectionData
    return result'

#if defined(ENABLE_OVERLOADING)
data SelectionDataTargetsIncludeImageMethodInfo
instance (signature ~ (Bool -> m Bool), MonadIO m) => O.OverloadedMethod SelectionDataTargetsIncludeImageMethodInfo SelectionData signature where
    overloadedMethod = selectionDataTargetsIncludeImage

instance O.OverloadedMethodInfo SelectionDataTargetsIncludeImageMethodInfo SelectionData where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.SelectionData.selectionDataTargetsIncludeImage",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-SelectionData.html#v:selectionDataTargetsIncludeImage"
        })


#endif

-- method SelectionData::targets_include_rich_text
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "selection_data"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "SelectionData" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkSelectionData object"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "buffer"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextBuffer" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextBuffer" , sinceVersion = Nothing }
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

foreign import ccall "gtk_selection_data_targets_include_rich_text" gtk_selection_data_targets_include_rich_text :: 
    Ptr SelectionData ->                    -- selection_data : TInterface (Name {namespace = "Gtk", name = "SelectionData"})
    Ptr Gtk.TextBuffer.TextBuffer ->        -- buffer : TInterface (Name {namespace = "Gtk", name = "TextBuffer"})
    IO CInt

-- | Given a t'GI.Gtk.Structs.SelectionData.SelectionData' object holding a list of targets,
-- determines if any of the targets in /@targets@/ can be used to
-- provide rich text.
-- 
-- /Since: 2.10/
selectionDataTargetsIncludeRichText ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.TextBuffer.IsTextBuffer a) =>
    SelectionData
    -- ^ /@selectionData@/: a t'GI.Gtk.Structs.SelectionData.SelectionData' object
    -> a
    -- ^ /@buffer@/: a t'GI.Gtk.Objects.TextBuffer.TextBuffer'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if /@selectionData@/ holds a list of targets,
    --               and a suitable target for rich text is included,
    --               otherwise 'P.False'.
selectionDataTargetsIncludeRichText selectionData buffer = liftIO $ do
    selectionData' <- unsafeManagedPtrGetPtr selectionData
    buffer' <- unsafeManagedPtrCastPtr buffer
    result <- gtk_selection_data_targets_include_rich_text selectionData' buffer'
    let result' = (/= 0) result
    touchManagedPtr selectionData
    touchManagedPtr buffer
    return result'

#if defined(ENABLE_OVERLOADING)
data SelectionDataTargetsIncludeRichTextMethodInfo
instance (signature ~ (a -> m Bool), MonadIO m, Gtk.TextBuffer.IsTextBuffer a) => O.OverloadedMethod SelectionDataTargetsIncludeRichTextMethodInfo SelectionData signature where
    overloadedMethod = selectionDataTargetsIncludeRichText

instance O.OverloadedMethodInfo SelectionDataTargetsIncludeRichTextMethodInfo SelectionData where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.SelectionData.selectionDataTargetsIncludeRichText",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-SelectionData.html#v:selectionDataTargetsIncludeRichText"
        })


#endif

-- method SelectionData::targets_include_text
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "selection_data"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "SelectionData" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkSelectionData object"
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

foreign import ccall "gtk_selection_data_targets_include_text" gtk_selection_data_targets_include_text :: 
    Ptr SelectionData ->                    -- selection_data : TInterface (Name {namespace = "Gtk", name = "SelectionData"})
    IO CInt

-- | Given a t'GI.Gtk.Structs.SelectionData.SelectionData' object holding a list of targets,
-- determines if any of the targets in /@targets@/ can be used to
-- provide text.
selectionDataTargetsIncludeText ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    SelectionData
    -- ^ /@selectionData@/: a t'GI.Gtk.Structs.SelectionData.SelectionData' object
    -> m Bool
    -- ^ __Returns:__ 'P.True' if /@selectionData@/ holds a list of targets,
    --   and a suitable target for text is included, otherwise 'P.False'.
selectionDataTargetsIncludeText selectionData = liftIO $ do
    selectionData' <- unsafeManagedPtrGetPtr selectionData
    result <- gtk_selection_data_targets_include_text selectionData'
    let result' = (/= 0) result
    touchManagedPtr selectionData
    return result'

#if defined(ENABLE_OVERLOADING)
data SelectionDataTargetsIncludeTextMethodInfo
instance (signature ~ (m Bool), MonadIO m) => O.OverloadedMethod SelectionDataTargetsIncludeTextMethodInfo SelectionData signature where
    overloadedMethod = selectionDataTargetsIncludeText

instance O.OverloadedMethodInfo SelectionDataTargetsIncludeTextMethodInfo SelectionData where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.SelectionData.selectionDataTargetsIncludeText",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-SelectionData.html#v:selectionDataTargetsIncludeText"
        })


#endif

-- method SelectionData::targets_include_uri
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "selection_data"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "SelectionData" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkSelectionData object"
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

foreign import ccall "gtk_selection_data_targets_include_uri" gtk_selection_data_targets_include_uri :: 
    Ptr SelectionData ->                    -- selection_data : TInterface (Name {namespace = "Gtk", name = "SelectionData"})
    IO CInt

-- | Given a t'GI.Gtk.Structs.SelectionData.SelectionData' object holding a list of targets,
-- determines if any of the targets in /@targets@/ can be used to
-- provide a list or URIs.
-- 
-- /Since: 2.10/
selectionDataTargetsIncludeUri ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    SelectionData
    -- ^ /@selectionData@/: a t'GI.Gtk.Structs.SelectionData.SelectionData' object
    -> m Bool
    -- ^ __Returns:__ 'P.True' if /@selectionData@/ holds a list of targets,
    --   and a suitable target for URI lists is included, otherwise 'P.False'.
selectionDataTargetsIncludeUri selectionData = liftIO $ do
    selectionData' <- unsafeManagedPtrGetPtr selectionData
    result <- gtk_selection_data_targets_include_uri selectionData'
    let result' = (/= 0) result
    touchManagedPtr selectionData
    return result'

#if defined(ENABLE_OVERLOADING)
data SelectionDataTargetsIncludeUriMethodInfo
instance (signature ~ (m Bool), MonadIO m) => O.OverloadedMethod SelectionDataTargetsIncludeUriMethodInfo SelectionData signature where
    overloadedMethod = selectionDataTargetsIncludeUri

instance O.OverloadedMethodInfo SelectionDataTargetsIncludeUriMethodInfo SelectionData where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.SelectionData.selectionDataTargetsIncludeUri",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-SelectionData.html#v:selectionDataTargetsIncludeUri"
        })


#endif

#if defined(ENABLE_OVERLOADING)
type family ResolveSelectionDataMethod (t :: Symbol) (o :: *) :: * where
    ResolveSelectionDataMethod "copy" o = SelectionDataCopyMethodInfo
    ResolveSelectionDataMethod "free" o = SelectionDataFreeMethodInfo
    ResolveSelectionDataMethod "set" o = SelectionDataSetMethodInfo
    ResolveSelectionDataMethod "targetsIncludeImage" o = SelectionDataTargetsIncludeImageMethodInfo
    ResolveSelectionDataMethod "targetsIncludeRichText" o = SelectionDataTargetsIncludeRichTextMethodInfo
    ResolveSelectionDataMethod "targetsIncludeText" o = SelectionDataTargetsIncludeTextMethodInfo
    ResolveSelectionDataMethod "targetsIncludeUri" o = SelectionDataTargetsIncludeUriMethodInfo
    ResolveSelectionDataMethod "getDataType" o = SelectionDataGetDataTypeMethodInfo
    ResolveSelectionDataMethod "getData" o = SelectionDataGetDataMethodInfo
    ResolveSelectionDataMethod "getDisplay" o = SelectionDataGetDisplayMethodInfo
    ResolveSelectionDataMethod "getFormat" o = SelectionDataGetFormatMethodInfo
    ResolveSelectionDataMethod "getLength" o = SelectionDataGetLengthMethodInfo
    ResolveSelectionDataMethod "getPixbuf" o = SelectionDataGetPixbufMethodInfo
    ResolveSelectionDataMethod "getSelection" o = SelectionDataGetSelectionMethodInfo
    ResolveSelectionDataMethod "getTarget" o = SelectionDataGetTargetMethodInfo
    ResolveSelectionDataMethod "getTargets" o = SelectionDataGetTargetsMethodInfo
    ResolveSelectionDataMethod "getText" o = SelectionDataGetTextMethodInfo
    ResolveSelectionDataMethod "getUris" o = SelectionDataGetUrisMethodInfo
    ResolveSelectionDataMethod "setPixbuf" o = SelectionDataSetPixbufMethodInfo
    ResolveSelectionDataMethod "setText" o = SelectionDataSetTextMethodInfo
    ResolveSelectionDataMethod "setUris" o = SelectionDataSetUrisMethodInfo
    ResolveSelectionDataMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveSelectionDataMethod t SelectionData, O.OverloadedMethod info SelectionData p) => OL.IsLabel t (SelectionData -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveSelectionDataMethod t SelectionData, O.OverloadedMethod info SelectionData p, R.HasField t SelectionData p) => R.HasField t SelectionData p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveSelectionDataMethod t SelectionData, O.OverloadedMethodInfo info SelectionData) => OL.IsLabel t (O.MethodProxy info SelectionData) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif


