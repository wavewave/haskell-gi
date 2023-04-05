{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- A GtkPrintSettings object represents the settings of a print dialog in
-- a system-independent way. The main use for this object is that once
-- you’ve printed you can get a settings object that represents the settings
-- the user chose, and the next time you print you can pass that object in so
-- that the user doesn’t have to re-set all his settings.
-- 
-- Its also possible to enumerate the settings so that you can easily save
-- the settings for the next time your app runs, or even store them in a
-- document. The predefined keys try to use shared values as much as possible
-- so that moving such a document between systems still works.
-- 
-- Printing support was added in GTK+ 2.10.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.PrintSettings
    ( 

-- * Exported types
    PrintSettings(..)                       ,
    IsPrintSettings                         ,
    toPrintSettings                         ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [copy]("GI.Gtk.Objects.PrintSettings#g:method:copy"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [foreach]("GI.Gtk.Objects.PrintSettings#g:method:foreach"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [get]("GI.Gtk.Objects.PrintSettings#g:method:get"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [hasKey]("GI.Gtk.Objects.PrintSettings#g:method:hasKey"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [loadFile]("GI.Gtk.Objects.PrintSettings#g:method:loadFile"), [loadKeyFile]("GI.Gtk.Objects.PrintSettings#g:method:loadKeyFile"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [set]("GI.Gtk.Objects.PrintSettings#g:method:set"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [toFile]("GI.Gtk.Objects.PrintSettings#g:method:toFile"), [toGvariant]("GI.Gtk.Objects.PrintSettings#g:method:toGvariant"), [toKeyFile]("GI.Gtk.Objects.PrintSettings#g:method:toKeyFile"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [unset]("GI.Gtk.Objects.PrintSettings#g:method:unset"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getBool]("GI.Gtk.Objects.PrintSettings#g:method:getBool"), [getCollate]("GI.Gtk.Objects.PrintSettings#g:method:getCollate"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getDefaultSource]("GI.Gtk.Objects.PrintSettings#g:method:getDefaultSource"), [getDither]("GI.Gtk.Objects.PrintSettings#g:method:getDither"), [getDouble]("GI.Gtk.Objects.PrintSettings#g:method:getDouble"), [getDoubleWithDefault]("GI.Gtk.Objects.PrintSettings#g:method:getDoubleWithDefault"), [getDuplex]("GI.Gtk.Objects.PrintSettings#g:method:getDuplex"), [getFinishings]("GI.Gtk.Objects.PrintSettings#g:method:getFinishings"), [getInt]("GI.Gtk.Objects.PrintSettings#g:method:getInt"), [getIntWithDefault]("GI.Gtk.Objects.PrintSettings#g:method:getIntWithDefault"), [getLength]("GI.Gtk.Objects.PrintSettings#g:method:getLength"), [getMediaType]("GI.Gtk.Objects.PrintSettings#g:method:getMediaType"), [getNCopies]("GI.Gtk.Objects.PrintSettings#g:method:getNCopies"), [getNumberUp]("GI.Gtk.Objects.PrintSettings#g:method:getNumberUp"), [getNumberUpLayout]("GI.Gtk.Objects.PrintSettings#g:method:getNumberUpLayout"), [getOrientation]("GI.Gtk.Objects.PrintSettings#g:method:getOrientation"), [getOutputBin]("GI.Gtk.Objects.PrintSettings#g:method:getOutputBin"), [getPageRanges]("GI.Gtk.Objects.PrintSettings#g:method:getPageRanges"), [getPageSet]("GI.Gtk.Objects.PrintSettings#g:method:getPageSet"), [getPaperHeight]("GI.Gtk.Objects.PrintSettings#g:method:getPaperHeight"), [getPaperSize]("GI.Gtk.Objects.PrintSettings#g:method:getPaperSize"), [getPaperWidth]("GI.Gtk.Objects.PrintSettings#g:method:getPaperWidth"), [getPrintPages]("GI.Gtk.Objects.PrintSettings#g:method:getPrintPages"), [getPrinter]("GI.Gtk.Objects.PrintSettings#g:method:getPrinter"), [getPrinterLpi]("GI.Gtk.Objects.PrintSettings#g:method:getPrinterLpi"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getQuality]("GI.Gtk.Objects.PrintSettings#g:method:getQuality"), [getResolution]("GI.Gtk.Objects.PrintSettings#g:method:getResolution"), [getResolutionX]("GI.Gtk.Objects.PrintSettings#g:method:getResolutionX"), [getResolutionY]("GI.Gtk.Objects.PrintSettings#g:method:getResolutionY"), [getReverse]("GI.Gtk.Objects.PrintSettings#g:method:getReverse"), [getScale]("GI.Gtk.Objects.PrintSettings#g:method:getScale"), [getUseColor]("GI.Gtk.Objects.PrintSettings#g:method:getUseColor").
-- 
-- ==== Setters
-- [setBool]("GI.Gtk.Objects.PrintSettings#g:method:setBool"), [setCollate]("GI.Gtk.Objects.PrintSettings#g:method:setCollate"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setDefaultSource]("GI.Gtk.Objects.PrintSettings#g:method:setDefaultSource"), [setDither]("GI.Gtk.Objects.PrintSettings#g:method:setDither"), [setDouble]("GI.Gtk.Objects.PrintSettings#g:method:setDouble"), [setDuplex]("GI.Gtk.Objects.PrintSettings#g:method:setDuplex"), [setFinishings]("GI.Gtk.Objects.PrintSettings#g:method:setFinishings"), [setInt]("GI.Gtk.Objects.PrintSettings#g:method:setInt"), [setLength]("GI.Gtk.Objects.PrintSettings#g:method:setLength"), [setMediaType]("GI.Gtk.Objects.PrintSettings#g:method:setMediaType"), [setNCopies]("GI.Gtk.Objects.PrintSettings#g:method:setNCopies"), [setNumberUp]("GI.Gtk.Objects.PrintSettings#g:method:setNumberUp"), [setNumberUpLayout]("GI.Gtk.Objects.PrintSettings#g:method:setNumberUpLayout"), [setOrientation]("GI.Gtk.Objects.PrintSettings#g:method:setOrientation"), [setOutputBin]("GI.Gtk.Objects.PrintSettings#g:method:setOutputBin"), [setPageRanges]("GI.Gtk.Objects.PrintSettings#g:method:setPageRanges"), [setPageSet]("GI.Gtk.Objects.PrintSettings#g:method:setPageSet"), [setPaperHeight]("GI.Gtk.Objects.PrintSettings#g:method:setPaperHeight"), [setPaperSize]("GI.Gtk.Objects.PrintSettings#g:method:setPaperSize"), [setPaperWidth]("GI.Gtk.Objects.PrintSettings#g:method:setPaperWidth"), [setPrintPages]("GI.Gtk.Objects.PrintSettings#g:method:setPrintPages"), [setPrinter]("GI.Gtk.Objects.PrintSettings#g:method:setPrinter"), [setPrinterLpi]("GI.Gtk.Objects.PrintSettings#g:method:setPrinterLpi"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setQuality]("GI.Gtk.Objects.PrintSettings#g:method:setQuality"), [setResolution]("GI.Gtk.Objects.PrintSettings#g:method:setResolution"), [setResolutionXy]("GI.Gtk.Objects.PrintSettings#g:method:setResolutionXy"), [setReverse]("GI.Gtk.Objects.PrintSettings#g:method:setReverse"), [setScale]("GI.Gtk.Objects.PrintSettings#g:method:setScale"), [setUseColor]("GI.Gtk.Objects.PrintSettings#g:method:setUseColor").

#if defined(ENABLE_OVERLOADING)
    ResolvePrintSettingsMethod              ,
#endif

-- ** copy #method:copy#

#if defined(ENABLE_OVERLOADING)
    PrintSettingsCopyMethodInfo             ,
#endif
    printSettingsCopy                       ,


-- ** foreach #method:foreach#

#if defined(ENABLE_OVERLOADING)
    PrintSettingsForeachMethodInfo          ,
#endif
    printSettingsForeach                    ,


-- ** get #method:get#

#if defined(ENABLE_OVERLOADING)
    PrintSettingsGetMethodInfo              ,
#endif
    printSettingsGet                        ,


-- ** getBool #method:getBool#

#if defined(ENABLE_OVERLOADING)
    PrintSettingsGetBoolMethodInfo          ,
#endif
    printSettingsGetBool                    ,


-- ** getCollate #method:getCollate#

#if defined(ENABLE_OVERLOADING)
    PrintSettingsGetCollateMethodInfo       ,
#endif
    printSettingsGetCollate                 ,


-- ** getDefaultSource #method:getDefaultSource#

#if defined(ENABLE_OVERLOADING)
    PrintSettingsGetDefaultSourceMethodInfo ,
#endif
    printSettingsGetDefaultSource           ,


-- ** getDither #method:getDither#

#if defined(ENABLE_OVERLOADING)
    PrintSettingsGetDitherMethodInfo        ,
#endif
    printSettingsGetDither                  ,


-- ** getDouble #method:getDouble#

#if defined(ENABLE_OVERLOADING)
    PrintSettingsGetDoubleMethodInfo        ,
#endif
    printSettingsGetDouble                  ,


-- ** getDoubleWithDefault #method:getDoubleWithDefault#

#if defined(ENABLE_OVERLOADING)
    PrintSettingsGetDoubleWithDefaultMethodInfo,
#endif
    printSettingsGetDoubleWithDefault       ,


-- ** getDuplex #method:getDuplex#

#if defined(ENABLE_OVERLOADING)
    PrintSettingsGetDuplexMethodInfo        ,
#endif
    printSettingsGetDuplex                  ,


-- ** getFinishings #method:getFinishings#

#if defined(ENABLE_OVERLOADING)
    PrintSettingsGetFinishingsMethodInfo    ,
#endif
    printSettingsGetFinishings              ,


-- ** getInt #method:getInt#

#if defined(ENABLE_OVERLOADING)
    PrintSettingsGetIntMethodInfo           ,
#endif
    printSettingsGetInt                     ,


-- ** getIntWithDefault #method:getIntWithDefault#

#if defined(ENABLE_OVERLOADING)
    PrintSettingsGetIntWithDefaultMethodInfo,
#endif
    printSettingsGetIntWithDefault          ,


-- ** getLength #method:getLength#

#if defined(ENABLE_OVERLOADING)
    PrintSettingsGetLengthMethodInfo        ,
#endif
    printSettingsGetLength                  ,


-- ** getMediaType #method:getMediaType#

#if defined(ENABLE_OVERLOADING)
    PrintSettingsGetMediaTypeMethodInfo     ,
#endif
    printSettingsGetMediaType               ,


-- ** getNCopies #method:getNCopies#

#if defined(ENABLE_OVERLOADING)
    PrintSettingsGetNCopiesMethodInfo       ,
#endif
    printSettingsGetNCopies                 ,


-- ** getNumberUp #method:getNumberUp#

#if defined(ENABLE_OVERLOADING)
    PrintSettingsGetNumberUpMethodInfo      ,
#endif
    printSettingsGetNumberUp                ,


-- ** getNumberUpLayout #method:getNumberUpLayout#

#if defined(ENABLE_OVERLOADING)
    PrintSettingsGetNumberUpLayoutMethodInfo,
#endif
    printSettingsGetNumberUpLayout          ,


-- ** getOrientation #method:getOrientation#

#if defined(ENABLE_OVERLOADING)
    PrintSettingsGetOrientationMethodInfo   ,
#endif
    printSettingsGetOrientation             ,


-- ** getOutputBin #method:getOutputBin#

#if defined(ENABLE_OVERLOADING)
    PrintSettingsGetOutputBinMethodInfo     ,
#endif
    printSettingsGetOutputBin               ,


-- ** getPageRanges #method:getPageRanges#

#if defined(ENABLE_OVERLOADING)
    PrintSettingsGetPageRangesMethodInfo    ,
#endif
    printSettingsGetPageRanges              ,


-- ** getPageSet #method:getPageSet#

#if defined(ENABLE_OVERLOADING)
    PrintSettingsGetPageSetMethodInfo       ,
#endif
    printSettingsGetPageSet                 ,


-- ** getPaperHeight #method:getPaperHeight#

#if defined(ENABLE_OVERLOADING)
    PrintSettingsGetPaperHeightMethodInfo   ,
#endif
    printSettingsGetPaperHeight             ,


-- ** getPaperSize #method:getPaperSize#

#if defined(ENABLE_OVERLOADING)
    PrintSettingsGetPaperSizeMethodInfo     ,
#endif
    printSettingsGetPaperSize               ,


-- ** getPaperWidth #method:getPaperWidth#

#if defined(ENABLE_OVERLOADING)
    PrintSettingsGetPaperWidthMethodInfo    ,
#endif
    printSettingsGetPaperWidth              ,


-- ** getPrintPages #method:getPrintPages#

#if defined(ENABLE_OVERLOADING)
    PrintSettingsGetPrintPagesMethodInfo    ,
#endif
    printSettingsGetPrintPages              ,


-- ** getPrinter #method:getPrinter#

#if defined(ENABLE_OVERLOADING)
    PrintSettingsGetPrinterMethodInfo       ,
#endif
    printSettingsGetPrinter                 ,


-- ** getPrinterLpi #method:getPrinterLpi#

#if defined(ENABLE_OVERLOADING)
    PrintSettingsGetPrinterLpiMethodInfo    ,
#endif
    printSettingsGetPrinterLpi              ,


-- ** getQuality #method:getQuality#

#if defined(ENABLE_OVERLOADING)
    PrintSettingsGetQualityMethodInfo       ,
#endif
    printSettingsGetQuality                 ,


-- ** getResolution #method:getResolution#

#if defined(ENABLE_OVERLOADING)
    PrintSettingsGetResolutionMethodInfo    ,
#endif
    printSettingsGetResolution              ,


-- ** getResolutionX #method:getResolutionX#

#if defined(ENABLE_OVERLOADING)
    PrintSettingsGetResolutionXMethodInfo   ,
#endif
    printSettingsGetResolutionX             ,


-- ** getResolutionY #method:getResolutionY#

#if defined(ENABLE_OVERLOADING)
    PrintSettingsGetResolutionYMethodInfo   ,
#endif
    printSettingsGetResolutionY             ,


-- ** getReverse #method:getReverse#

#if defined(ENABLE_OVERLOADING)
    PrintSettingsGetReverseMethodInfo       ,
#endif
    printSettingsGetReverse                 ,


-- ** getScale #method:getScale#

#if defined(ENABLE_OVERLOADING)
    PrintSettingsGetScaleMethodInfo         ,
#endif
    printSettingsGetScale                   ,


-- ** getUseColor #method:getUseColor#

#if defined(ENABLE_OVERLOADING)
    PrintSettingsGetUseColorMethodInfo      ,
#endif
    printSettingsGetUseColor                ,


-- ** hasKey #method:hasKey#

#if defined(ENABLE_OVERLOADING)
    PrintSettingsHasKeyMethodInfo           ,
#endif
    printSettingsHasKey                     ,


-- ** loadFile #method:loadFile#

#if defined(ENABLE_OVERLOADING)
    PrintSettingsLoadFileMethodInfo         ,
#endif
    printSettingsLoadFile                   ,


-- ** loadKeyFile #method:loadKeyFile#

#if defined(ENABLE_OVERLOADING)
    PrintSettingsLoadKeyFileMethodInfo      ,
#endif
    printSettingsLoadKeyFile                ,


-- ** new #method:new#

    printSettingsNew                        ,


-- ** newFromFile #method:newFromFile#

    printSettingsNewFromFile                ,


-- ** newFromGvariant #method:newFromGvariant#

    printSettingsNewFromGvariant            ,


-- ** newFromKeyFile #method:newFromKeyFile#

    printSettingsNewFromKeyFile             ,


-- ** set #method:set#

#if defined(ENABLE_OVERLOADING)
    PrintSettingsSetMethodInfo              ,
#endif
    printSettingsSet                        ,


-- ** setBool #method:setBool#

#if defined(ENABLE_OVERLOADING)
    PrintSettingsSetBoolMethodInfo          ,
#endif
    printSettingsSetBool                    ,


-- ** setCollate #method:setCollate#

#if defined(ENABLE_OVERLOADING)
    PrintSettingsSetCollateMethodInfo       ,
#endif
    printSettingsSetCollate                 ,


-- ** setDefaultSource #method:setDefaultSource#

#if defined(ENABLE_OVERLOADING)
    PrintSettingsSetDefaultSourceMethodInfo ,
#endif
    printSettingsSetDefaultSource           ,


-- ** setDither #method:setDither#

#if defined(ENABLE_OVERLOADING)
    PrintSettingsSetDitherMethodInfo        ,
#endif
    printSettingsSetDither                  ,


-- ** setDouble #method:setDouble#

#if defined(ENABLE_OVERLOADING)
    PrintSettingsSetDoubleMethodInfo        ,
#endif
    printSettingsSetDouble                  ,


-- ** setDuplex #method:setDuplex#

#if defined(ENABLE_OVERLOADING)
    PrintSettingsSetDuplexMethodInfo        ,
#endif
    printSettingsSetDuplex                  ,


-- ** setFinishings #method:setFinishings#

#if defined(ENABLE_OVERLOADING)
    PrintSettingsSetFinishingsMethodInfo    ,
#endif
    printSettingsSetFinishings              ,


-- ** setInt #method:setInt#

#if defined(ENABLE_OVERLOADING)
    PrintSettingsSetIntMethodInfo           ,
#endif
    printSettingsSetInt                     ,


-- ** setLength #method:setLength#

#if defined(ENABLE_OVERLOADING)
    PrintSettingsSetLengthMethodInfo        ,
#endif
    printSettingsSetLength                  ,


-- ** setMediaType #method:setMediaType#

#if defined(ENABLE_OVERLOADING)
    PrintSettingsSetMediaTypeMethodInfo     ,
#endif
    printSettingsSetMediaType               ,


-- ** setNCopies #method:setNCopies#

#if defined(ENABLE_OVERLOADING)
    PrintSettingsSetNCopiesMethodInfo       ,
#endif
    printSettingsSetNCopies                 ,


-- ** setNumberUp #method:setNumberUp#

#if defined(ENABLE_OVERLOADING)
    PrintSettingsSetNumberUpMethodInfo      ,
#endif
    printSettingsSetNumberUp                ,


-- ** setNumberUpLayout #method:setNumberUpLayout#

#if defined(ENABLE_OVERLOADING)
    PrintSettingsSetNumberUpLayoutMethodInfo,
#endif
    printSettingsSetNumberUpLayout          ,


-- ** setOrientation #method:setOrientation#

#if defined(ENABLE_OVERLOADING)
    PrintSettingsSetOrientationMethodInfo   ,
#endif
    printSettingsSetOrientation             ,


-- ** setOutputBin #method:setOutputBin#

#if defined(ENABLE_OVERLOADING)
    PrintSettingsSetOutputBinMethodInfo     ,
#endif
    printSettingsSetOutputBin               ,


-- ** setPageRanges #method:setPageRanges#

#if defined(ENABLE_OVERLOADING)
    PrintSettingsSetPageRangesMethodInfo    ,
#endif
    printSettingsSetPageRanges              ,


-- ** setPageSet #method:setPageSet#

#if defined(ENABLE_OVERLOADING)
    PrintSettingsSetPageSetMethodInfo       ,
#endif
    printSettingsSetPageSet                 ,


-- ** setPaperHeight #method:setPaperHeight#

#if defined(ENABLE_OVERLOADING)
    PrintSettingsSetPaperHeightMethodInfo   ,
#endif
    printSettingsSetPaperHeight             ,


-- ** setPaperSize #method:setPaperSize#

#if defined(ENABLE_OVERLOADING)
    PrintSettingsSetPaperSizeMethodInfo     ,
#endif
    printSettingsSetPaperSize               ,


-- ** setPaperWidth #method:setPaperWidth#

#if defined(ENABLE_OVERLOADING)
    PrintSettingsSetPaperWidthMethodInfo    ,
#endif
    printSettingsSetPaperWidth              ,


-- ** setPrintPages #method:setPrintPages#

#if defined(ENABLE_OVERLOADING)
    PrintSettingsSetPrintPagesMethodInfo    ,
#endif
    printSettingsSetPrintPages              ,


-- ** setPrinter #method:setPrinter#

#if defined(ENABLE_OVERLOADING)
    PrintSettingsSetPrinterMethodInfo       ,
#endif
    printSettingsSetPrinter                 ,


-- ** setPrinterLpi #method:setPrinterLpi#

#if defined(ENABLE_OVERLOADING)
    PrintSettingsSetPrinterLpiMethodInfo    ,
#endif
    printSettingsSetPrinterLpi              ,


-- ** setQuality #method:setQuality#

#if defined(ENABLE_OVERLOADING)
    PrintSettingsSetQualityMethodInfo       ,
#endif
    printSettingsSetQuality                 ,


-- ** setResolution #method:setResolution#

#if defined(ENABLE_OVERLOADING)
    PrintSettingsSetResolutionMethodInfo    ,
#endif
    printSettingsSetResolution              ,


-- ** setResolutionXy #method:setResolutionXy#

#if defined(ENABLE_OVERLOADING)
    PrintSettingsSetResolutionXyMethodInfo  ,
#endif
    printSettingsSetResolutionXy            ,


-- ** setReverse #method:setReverse#

#if defined(ENABLE_OVERLOADING)
    PrintSettingsSetReverseMethodInfo       ,
#endif
    printSettingsSetReverse                 ,


-- ** setScale #method:setScale#

#if defined(ENABLE_OVERLOADING)
    PrintSettingsSetScaleMethodInfo         ,
#endif
    printSettingsSetScale                   ,


-- ** setUseColor #method:setUseColor#

#if defined(ENABLE_OVERLOADING)
    PrintSettingsSetUseColorMethodInfo      ,
#endif
    printSettingsSetUseColor                ,


-- ** toFile #method:toFile#

#if defined(ENABLE_OVERLOADING)
    PrintSettingsToFileMethodInfo           ,
#endif
    printSettingsToFile                     ,


-- ** toGvariant #method:toGvariant#

#if defined(ENABLE_OVERLOADING)
    PrintSettingsToGvariantMethodInfo       ,
#endif
    printSettingsToGvariant                 ,


-- ** toKeyFile #method:toKeyFile#

#if defined(ENABLE_OVERLOADING)
    PrintSettingsToKeyFileMethodInfo        ,
#endif
    printSettingsToKeyFile                  ,


-- ** unset #method:unset#

#if defined(ENABLE_OVERLOADING)
    PrintSettingsUnsetMethodInfo            ,
#endif
    printSettingsUnset                      ,




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

import qualified GI.GLib.Structs.KeyFile as GLib.KeyFile
import qualified GI.GObject.Objects.Object as GObject.Object
import qualified GI.Gtk.Callbacks as Gtk.Callbacks
import {-# SOURCE #-} qualified GI.Gtk.Enums as Gtk.Enums
import {-# SOURCE #-} qualified GI.Gtk.Structs.PageRange as Gtk.PageRange
import {-# SOURCE #-} qualified GI.Gtk.Structs.PaperSize as Gtk.PaperSize

-- | Memory-managed wrapper type.
newtype PrintSettings = PrintSettings (SP.ManagedPtr PrintSettings)
    deriving (Eq)

instance SP.ManagedPtrNewtype PrintSettings where
    toManagedPtr (PrintSettings p) = p

foreign import ccall "gtk_print_settings_get_type"
    c_gtk_print_settings_get_type :: IO B.Types.GType

instance B.Types.TypedObject PrintSettings where
    glibType = c_gtk_print_settings_get_type

instance B.Types.GObject PrintSettings

-- | Type class for types which can be safely cast to `PrintSettings`, for instance with `toPrintSettings`.
class (SP.GObject o, O.IsDescendantOf PrintSettings o) => IsPrintSettings o
instance (SP.GObject o, O.IsDescendantOf PrintSettings o) => IsPrintSettings o

instance O.HasParentTypes PrintSettings
type instance O.ParentTypes PrintSettings = '[GObject.Object.Object]

-- | Cast to `PrintSettings`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toPrintSettings :: (MIO.MonadIO m, IsPrintSettings o) => o -> m PrintSettings
toPrintSettings = MIO.liftIO . B.ManagedPtr.unsafeCastTo PrintSettings

-- | Convert 'PrintSettings' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe PrintSettings) where
    gvalueGType_ = c_gtk_print_settings_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr PrintSettings)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr PrintSettings)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject PrintSettings ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolvePrintSettingsMethod (t :: Symbol) (o :: *) :: * where
    ResolvePrintSettingsMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolvePrintSettingsMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolvePrintSettingsMethod "copy" o = PrintSettingsCopyMethodInfo
    ResolvePrintSettingsMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolvePrintSettingsMethod "foreach" o = PrintSettingsForeachMethodInfo
    ResolvePrintSettingsMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolvePrintSettingsMethod "get" o = PrintSettingsGetMethodInfo
    ResolvePrintSettingsMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolvePrintSettingsMethod "hasKey" o = PrintSettingsHasKeyMethodInfo
    ResolvePrintSettingsMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolvePrintSettingsMethod "loadFile" o = PrintSettingsLoadFileMethodInfo
    ResolvePrintSettingsMethod "loadKeyFile" o = PrintSettingsLoadKeyFileMethodInfo
    ResolvePrintSettingsMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolvePrintSettingsMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolvePrintSettingsMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolvePrintSettingsMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolvePrintSettingsMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolvePrintSettingsMethod "set" o = PrintSettingsSetMethodInfo
    ResolvePrintSettingsMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolvePrintSettingsMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolvePrintSettingsMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolvePrintSettingsMethod "toFile" o = PrintSettingsToFileMethodInfo
    ResolvePrintSettingsMethod "toGvariant" o = PrintSettingsToGvariantMethodInfo
    ResolvePrintSettingsMethod "toKeyFile" o = PrintSettingsToKeyFileMethodInfo
    ResolvePrintSettingsMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolvePrintSettingsMethod "unset" o = PrintSettingsUnsetMethodInfo
    ResolvePrintSettingsMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolvePrintSettingsMethod "getBool" o = PrintSettingsGetBoolMethodInfo
    ResolvePrintSettingsMethod "getCollate" o = PrintSettingsGetCollateMethodInfo
    ResolvePrintSettingsMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolvePrintSettingsMethod "getDefaultSource" o = PrintSettingsGetDefaultSourceMethodInfo
    ResolvePrintSettingsMethod "getDither" o = PrintSettingsGetDitherMethodInfo
    ResolvePrintSettingsMethod "getDouble" o = PrintSettingsGetDoubleMethodInfo
    ResolvePrintSettingsMethod "getDoubleWithDefault" o = PrintSettingsGetDoubleWithDefaultMethodInfo
    ResolvePrintSettingsMethod "getDuplex" o = PrintSettingsGetDuplexMethodInfo
    ResolvePrintSettingsMethod "getFinishings" o = PrintSettingsGetFinishingsMethodInfo
    ResolvePrintSettingsMethod "getInt" o = PrintSettingsGetIntMethodInfo
    ResolvePrintSettingsMethod "getIntWithDefault" o = PrintSettingsGetIntWithDefaultMethodInfo
    ResolvePrintSettingsMethod "getLength" o = PrintSettingsGetLengthMethodInfo
    ResolvePrintSettingsMethod "getMediaType" o = PrintSettingsGetMediaTypeMethodInfo
    ResolvePrintSettingsMethod "getNCopies" o = PrintSettingsGetNCopiesMethodInfo
    ResolvePrintSettingsMethod "getNumberUp" o = PrintSettingsGetNumberUpMethodInfo
    ResolvePrintSettingsMethod "getNumberUpLayout" o = PrintSettingsGetNumberUpLayoutMethodInfo
    ResolvePrintSettingsMethod "getOrientation" o = PrintSettingsGetOrientationMethodInfo
    ResolvePrintSettingsMethod "getOutputBin" o = PrintSettingsGetOutputBinMethodInfo
    ResolvePrintSettingsMethod "getPageRanges" o = PrintSettingsGetPageRangesMethodInfo
    ResolvePrintSettingsMethod "getPageSet" o = PrintSettingsGetPageSetMethodInfo
    ResolvePrintSettingsMethod "getPaperHeight" o = PrintSettingsGetPaperHeightMethodInfo
    ResolvePrintSettingsMethod "getPaperSize" o = PrintSettingsGetPaperSizeMethodInfo
    ResolvePrintSettingsMethod "getPaperWidth" o = PrintSettingsGetPaperWidthMethodInfo
    ResolvePrintSettingsMethod "getPrintPages" o = PrintSettingsGetPrintPagesMethodInfo
    ResolvePrintSettingsMethod "getPrinter" o = PrintSettingsGetPrinterMethodInfo
    ResolvePrintSettingsMethod "getPrinterLpi" o = PrintSettingsGetPrinterLpiMethodInfo
    ResolvePrintSettingsMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolvePrintSettingsMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolvePrintSettingsMethod "getQuality" o = PrintSettingsGetQualityMethodInfo
    ResolvePrintSettingsMethod "getResolution" o = PrintSettingsGetResolutionMethodInfo
    ResolvePrintSettingsMethod "getResolutionX" o = PrintSettingsGetResolutionXMethodInfo
    ResolvePrintSettingsMethod "getResolutionY" o = PrintSettingsGetResolutionYMethodInfo
    ResolvePrintSettingsMethod "getReverse" o = PrintSettingsGetReverseMethodInfo
    ResolvePrintSettingsMethod "getScale" o = PrintSettingsGetScaleMethodInfo
    ResolvePrintSettingsMethod "getUseColor" o = PrintSettingsGetUseColorMethodInfo
    ResolvePrintSettingsMethod "setBool" o = PrintSettingsSetBoolMethodInfo
    ResolvePrintSettingsMethod "setCollate" o = PrintSettingsSetCollateMethodInfo
    ResolvePrintSettingsMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolvePrintSettingsMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolvePrintSettingsMethod "setDefaultSource" o = PrintSettingsSetDefaultSourceMethodInfo
    ResolvePrintSettingsMethod "setDither" o = PrintSettingsSetDitherMethodInfo
    ResolvePrintSettingsMethod "setDouble" o = PrintSettingsSetDoubleMethodInfo
    ResolvePrintSettingsMethod "setDuplex" o = PrintSettingsSetDuplexMethodInfo
    ResolvePrintSettingsMethod "setFinishings" o = PrintSettingsSetFinishingsMethodInfo
    ResolvePrintSettingsMethod "setInt" o = PrintSettingsSetIntMethodInfo
    ResolvePrintSettingsMethod "setLength" o = PrintSettingsSetLengthMethodInfo
    ResolvePrintSettingsMethod "setMediaType" o = PrintSettingsSetMediaTypeMethodInfo
    ResolvePrintSettingsMethod "setNCopies" o = PrintSettingsSetNCopiesMethodInfo
    ResolvePrintSettingsMethod "setNumberUp" o = PrintSettingsSetNumberUpMethodInfo
    ResolvePrintSettingsMethod "setNumberUpLayout" o = PrintSettingsSetNumberUpLayoutMethodInfo
    ResolvePrintSettingsMethod "setOrientation" o = PrintSettingsSetOrientationMethodInfo
    ResolvePrintSettingsMethod "setOutputBin" o = PrintSettingsSetOutputBinMethodInfo
    ResolvePrintSettingsMethod "setPageRanges" o = PrintSettingsSetPageRangesMethodInfo
    ResolvePrintSettingsMethod "setPageSet" o = PrintSettingsSetPageSetMethodInfo
    ResolvePrintSettingsMethod "setPaperHeight" o = PrintSettingsSetPaperHeightMethodInfo
    ResolvePrintSettingsMethod "setPaperSize" o = PrintSettingsSetPaperSizeMethodInfo
    ResolvePrintSettingsMethod "setPaperWidth" o = PrintSettingsSetPaperWidthMethodInfo
    ResolvePrintSettingsMethod "setPrintPages" o = PrintSettingsSetPrintPagesMethodInfo
    ResolvePrintSettingsMethod "setPrinter" o = PrintSettingsSetPrinterMethodInfo
    ResolvePrintSettingsMethod "setPrinterLpi" o = PrintSettingsSetPrinterLpiMethodInfo
    ResolvePrintSettingsMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolvePrintSettingsMethod "setQuality" o = PrintSettingsSetQualityMethodInfo
    ResolvePrintSettingsMethod "setResolution" o = PrintSettingsSetResolutionMethodInfo
    ResolvePrintSettingsMethod "setResolutionXy" o = PrintSettingsSetResolutionXyMethodInfo
    ResolvePrintSettingsMethod "setReverse" o = PrintSettingsSetReverseMethodInfo
    ResolvePrintSettingsMethod "setScale" o = PrintSettingsSetScaleMethodInfo
    ResolvePrintSettingsMethod "setUseColor" o = PrintSettingsSetUseColorMethodInfo
    ResolvePrintSettingsMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolvePrintSettingsMethod t PrintSettings, O.OverloadedMethod info PrintSettings p) => OL.IsLabel t (PrintSettings -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolvePrintSettingsMethod t PrintSettings, O.OverloadedMethod info PrintSettings p, R.HasField t PrintSettings p) => R.HasField t PrintSettings p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolvePrintSettingsMethod t PrintSettings, O.OverloadedMethodInfo info PrintSettings) => OL.IsLabel t (O.MethodProxy info PrintSettings) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList PrintSettings
type instance O.AttributeList PrintSettings = PrintSettingsAttributeList
type PrintSettingsAttributeList = ('[ ] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList PrintSettings = PrintSettingsSignalList
type PrintSettingsSignalList = ('[ '("notify", GObject.Object.ObjectNotifySignalInfo)] :: [(Symbol, *)])

#endif

-- method PrintSettings::new
-- method type : Constructor
-- Args: []
-- Lengths: []
-- returnType: Just
--               (TInterface Name { namespace = "Gtk" , name = "PrintSettings" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_print_settings_new" gtk_print_settings_new :: 
    IO (Ptr PrintSettings)

-- | Creates a new t'GI.Gtk.Objects.PrintSettings.PrintSettings' object.
-- 
-- /Since: 2.10/
printSettingsNew ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m PrintSettings
    -- ^ __Returns:__ a new t'GI.Gtk.Objects.PrintSettings.PrintSettings' object
printSettingsNew  = liftIO $ do
    result <- gtk_print_settings_new
    checkUnexpectedReturnNULL "printSettingsNew" result
    result' <- (wrapObject PrintSettings) result
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method PrintSettings::new_from_file
-- method type : Constructor
-- Args: [ Arg
--           { argCName = "file_name"
--           , argType = TBasicType TFileName
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the filename to read the settings from"
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
--               (TInterface Name { namespace = "Gtk" , name = "PrintSettings" })
-- throws : True
-- Skip return : False

foreign import ccall "gtk_print_settings_new_from_file" gtk_print_settings_new_from_file :: 
    CString ->                              -- file_name : TBasicType TFileName
    Ptr (Ptr GError) ->                     -- error
    IO (Ptr PrintSettings)

-- | Reads the print settings from /@fileName@/. Returns a new t'GI.Gtk.Objects.PrintSettings.PrintSettings'
-- object with the restored settings, or 'P.Nothing' if an error occurred. If the
-- file could not be loaded then error is set to either a t'GI.GLib.Enums.FileError' or
-- t'GI.GLib.Enums.KeyFileError'.  See 'GI.Gtk.Objects.PrintSettings.printSettingsToFile'.
-- 
-- /Since: 2.12/
printSettingsNewFromFile ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    [Char]
    -- ^ /@fileName@/: the filename to read the settings from
    -> m PrintSettings
    -- ^ __Returns:__ the restored t'GI.Gtk.Objects.PrintSettings.PrintSettings' /(Can throw 'Data.GI.Base.GError.GError')/
printSettingsNewFromFile fileName = liftIO $ do
    fileName' <- stringToCString fileName
    onException (do
        result <- propagateGError $ gtk_print_settings_new_from_file fileName'
        checkUnexpectedReturnNULL "printSettingsNewFromFile" result
        result' <- (wrapObject PrintSettings) result
        freeMem fileName'
        return result'
     ) (do
        freeMem fileName'
     )

#if defined(ENABLE_OVERLOADING)
#endif

-- method PrintSettings::new_from_gvariant
-- method type : Constructor
-- Args: [ Arg
--           { argCName = "variant"
--           , argType = TVariant
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "an a{sv} #GVariant" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just
--               (TInterface Name { namespace = "Gtk" , name = "PrintSettings" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_print_settings_new_from_gvariant" gtk_print_settings_new_from_gvariant :: 
    Ptr GVariant ->                         -- variant : TVariant
    IO (Ptr PrintSettings)

-- | Deserialize print settings from an a{sv} variant in
-- the format produced by 'GI.Gtk.Objects.PrintSettings.printSettingsToGvariant'.
-- 
-- /Since: 3.22/
printSettingsNewFromGvariant ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    GVariant
    -- ^ /@variant@/: an a{sv} t'GVariant'
    -> m PrintSettings
    -- ^ __Returns:__ a new t'GI.Gtk.Objects.PrintSettings.PrintSettings' object
printSettingsNewFromGvariant variant = liftIO $ do
    variant' <- unsafeManagedPtrGetPtr variant
    result <- gtk_print_settings_new_from_gvariant variant'
    checkUnexpectedReturnNULL "printSettingsNewFromGvariant" result
    result' <- (wrapObject PrintSettings) result
    touchManagedPtr variant
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method PrintSettings::new_from_key_file
-- method type : Constructor
-- Args: [ Arg
--           { argCName = "key_file"
--           , argType =
--               TInterface Name { namespace = "GLib" , name = "KeyFile" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the #GKeyFile to retrieve the settings from"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "group_name"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the name of the group to use, or %NULL to use\n    the default \8220Print Settings\8221"
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
--               (TInterface Name { namespace = "Gtk" , name = "PrintSettings" })
-- throws : True
-- Skip return : False

foreign import ccall "gtk_print_settings_new_from_key_file" gtk_print_settings_new_from_key_file :: 
    Ptr GLib.KeyFile.KeyFile ->             -- key_file : TInterface (Name {namespace = "GLib", name = "KeyFile"})
    CString ->                              -- group_name : TBasicType TUTF8
    Ptr (Ptr GError) ->                     -- error
    IO (Ptr PrintSettings)

-- | Reads the print settings from the group /@groupName@/ in /@keyFile@/.  Returns a
-- new t'GI.Gtk.Objects.PrintSettings.PrintSettings' object with the restored settings, or 'P.Nothing' if an
-- error occurred. If the file could not be loaded then error is set to either
-- a t'GI.GLib.Enums.FileError' or t'GI.GLib.Enums.KeyFileError'.
-- 
-- /Since: 2.12/
printSettingsNewFromKeyFile ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    GLib.KeyFile.KeyFile
    -- ^ /@keyFile@/: the t'GI.GLib.Structs.KeyFile.KeyFile' to retrieve the settings from
    -> Maybe (T.Text)
    -- ^ /@groupName@/: the name of the group to use, or 'P.Nothing' to use
    --     the default “Print Settings”
    -> m PrintSettings
    -- ^ __Returns:__ the restored t'GI.Gtk.Objects.PrintSettings.PrintSettings' /(Can throw 'Data.GI.Base.GError.GError')/
printSettingsNewFromKeyFile keyFile groupName = liftIO $ do
    keyFile' <- unsafeManagedPtrGetPtr keyFile
    maybeGroupName <- case groupName of
        Nothing -> return nullPtr
        Just jGroupName -> do
            jGroupName' <- textToCString jGroupName
            return jGroupName'
    onException (do
        result <- propagateGError $ gtk_print_settings_new_from_key_file keyFile' maybeGroupName
        checkUnexpectedReturnNULL "printSettingsNewFromKeyFile" result
        result' <- (wrapObject PrintSettings) result
        touchManagedPtr keyFile
        freeMem maybeGroupName
        return result'
     ) (do
        freeMem maybeGroupName
     )

#if defined(ENABLE_OVERLOADING)
#endif

-- method PrintSettings::copy
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "other"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintSettings" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintSettings"
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
--               (TInterface Name { namespace = "Gtk" , name = "PrintSettings" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_print_settings_copy" gtk_print_settings_copy :: 
    Ptr PrintSettings ->                    -- other : TInterface (Name {namespace = "Gtk", name = "PrintSettings"})
    IO (Ptr PrintSettings)

-- | Copies a t'GI.Gtk.Objects.PrintSettings.PrintSettings' object.
-- 
-- /Since: 2.10/
printSettingsCopy ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintSettings a) =>
    a
    -- ^ /@other@/: a t'GI.Gtk.Objects.PrintSettings.PrintSettings'
    -> m PrintSettings
    -- ^ __Returns:__ a newly allocated copy of /@other@/
printSettingsCopy other = liftIO $ do
    other' <- unsafeManagedPtrCastPtr other
    result <- gtk_print_settings_copy other'
    checkUnexpectedReturnNULL "printSettingsCopy" result
    result' <- (wrapObject PrintSettings) result
    touchManagedPtr other
    return result'

#if defined(ENABLE_OVERLOADING)
data PrintSettingsCopyMethodInfo
instance (signature ~ (m PrintSettings), MonadIO m, IsPrintSettings a) => O.OverloadedMethod PrintSettingsCopyMethodInfo a signature where
    overloadedMethod = printSettingsCopy

instance O.OverloadedMethodInfo PrintSettingsCopyMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintSettings.printSettingsCopy",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintSettings.html#v:printSettingsCopy"
        })


#endif

-- method PrintSettings::foreach
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "settings"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintSettings" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintSettings"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "func"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintSettingsFunc" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the function to call"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeCall
--           , argClosure = 2
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "user_data"
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
--       ]
-- Lengths: []
-- returnType: Nothing
-- throws : False
-- Skip return : False

foreign import ccall "gtk_print_settings_foreach" gtk_print_settings_foreach :: 
    Ptr PrintSettings ->                    -- settings : TInterface (Name {namespace = "Gtk", name = "PrintSettings"})
    FunPtr Gtk.Callbacks.C_PrintSettingsFunc -> -- func : TInterface (Name {namespace = "Gtk", name = "PrintSettingsFunc"})
    Ptr () ->                               -- user_data : TBasicType TPtr
    IO ()

-- | Calls /@func@/ for each key-value pair of /@settings@/.
-- 
-- /Since: 2.10/
printSettingsForeach ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintSettings a) =>
    a
    -- ^ /@settings@/: a t'GI.Gtk.Objects.PrintSettings.PrintSettings'
    -> Gtk.Callbacks.PrintSettingsFunc
    -- ^ /@func@/: the function to call
    -> m ()
printSettingsForeach settings func = liftIO $ do
    settings' <- unsafeManagedPtrCastPtr settings
    func' <- Gtk.Callbacks.mk_PrintSettingsFunc (Gtk.Callbacks.wrap_PrintSettingsFunc Nothing (Gtk.Callbacks.drop_closures_PrintSettingsFunc func))
    let userData = nullPtr
    gtk_print_settings_foreach settings' func' userData
    safeFreeFunPtr $ castFunPtrToPtr func'
    touchManagedPtr settings
    return ()

#if defined(ENABLE_OVERLOADING)
data PrintSettingsForeachMethodInfo
instance (signature ~ (Gtk.Callbacks.PrintSettingsFunc -> m ()), MonadIO m, IsPrintSettings a) => O.OverloadedMethod PrintSettingsForeachMethodInfo a signature where
    overloadedMethod = printSettingsForeach

instance O.OverloadedMethodInfo PrintSettingsForeachMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintSettings.printSettingsForeach",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintSettings.html#v:printSettingsForeach"
        })


#endif

-- method PrintSettings::get
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "settings"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintSettings" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintSettings"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "key"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a key" , sinceVersion = Nothing }
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

foreign import ccall "gtk_print_settings_get" gtk_print_settings_get :: 
    Ptr PrintSettings ->                    -- settings : TInterface (Name {namespace = "Gtk", name = "PrintSettings"})
    CString ->                              -- key : TBasicType TUTF8
    IO CString

-- | Looks up the string value associated with /@key@/.
-- 
-- /Since: 2.10/
printSettingsGet ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintSettings a) =>
    a
    -- ^ /@settings@/: a t'GI.Gtk.Objects.PrintSettings.PrintSettings'
    -> T.Text
    -- ^ /@key@/: a key
    -> m T.Text
    -- ^ __Returns:__ the string value for /@key@/
printSettingsGet settings key = liftIO $ do
    settings' <- unsafeManagedPtrCastPtr settings
    key' <- textToCString key
    result <- gtk_print_settings_get settings' key'
    checkUnexpectedReturnNULL "printSettingsGet" result
    result' <- cstringToText result
    touchManagedPtr settings
    freeMem key'
    return result'

#if defined(ENABLE_OVERLOADING)
data PrintSettingsGetMethodInfo
instance (signature ~ (T.Text -> m T.Text), MonadIO m, IsPrintSettings a) => O.OverloadedMethod PrintSettingsGetMethodInfo a signature where
    overloadedMethod = printSettingsGet

instance O.OverloadedMethodInfo PrintSettingsGetMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintSettings.printSettingsGet",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintSettings.html#v:printSettingsGet"
        })


#endif

-- method PrintSettings::get_bool
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "settings"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintSettings" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintSettings"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "key"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a key" , sinceVersion = Nothing }
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

foreign import ccall "gtk_print_settings_get_bool" gtk_print_settings_get_bool :: 
    Ptr PrintSettings ->                    -- settings : TInterface (Name {namespace = "Gtk", name = "PrintSettings"})
    CString ->                              -- key : TBasicType TUTF8
    IO CInt

-- | Returns the boolean represented by the value
-- that is associated with /@key@/.
-- 
-- The string “true” represents 'P.True', any other
-- string 'P.False'.
-- 
-- /Since: 2.10/
printSettingsGetBool ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintSettings a) =>
    a
    -- ^ /@settings@/: a t'GI.Gtk.Objects.PrintSettings.PrintSettings'
    -> T.Text
    -- ^ /@key@/: a key
    -> m Bool
    -- ^ __Returns:__ 'P.True', if /@key@/ maps to a true value.
printSettingsGetBool settings key = liftIO $ do
    settings' <- unsafeManagedPtrCastPtr settings
    key' <- textToCString key
    result <- gtk_print_settings_get_bool settings' key'
    let result' = (/= 0) result
    touchManagedPtr settings
    freeMem key'
    return result'

#if defined(ENABLE_OVERLOADING)
data PrintSettingsGetBoolMethodInfo
instance (signature ~ (T.Text -> m Bool), MonadIO m, IsPrintSettings a) => O.OverloadedMethod PrintSettingsGetBoolMethodInfo a signature where
    overloadedMethod = printSettingsGetBool

instance O.OverloadedMethodInfo PrintSettingsGetBoolMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintSettings.printSettingsGetBool",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintSettings.html#v:printSettingsGetBool"
        })


#endif

-- method PrintSettings::get_collate
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "settings"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintSettings" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintSettings"
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

foreign import ccall "gtk_print_settings_get_collate" gtk_print_settings_get_collate :: 
    Ptr PrintSettings ->                    -- settings : TInterface (Name {namespace = "Gtk", name = "PrintSettings"})
    IO CInt

-- | Gets the value of 'GI.Gtk.Constants.PRINT_SETTINGS_COLLATE'.
-- 
-- /Since: 2.10/
printSettingsGetCollate ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintSettings a) =>
    a
    -- ^ /@settings@/: a t'GI.Gtk.Objects.PrintSettings.PrintSettings'
    -> m Bool
    -- ^ __Returns:__ whether to collate the printed pages
printSettingsGetCollate settings = liftIO $ do
    settings' <- unsafeManagedPtrCastPtr settings
    result <- gtk_print_settings_get_collate settings'
    let result' = (/= 0) result
    touchManagedPtr settings
    return result'

#if defined(ENABLE_OVERLOADING)
data PrintSettingsGetCollateMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsPrintSettings a) => O.OverloadedMethod PrintSettingsGetCollateMethodInfo a signature where
    overloadedMethod = printSettingsGetCollate

instance O.OverloadedMethodInfo PrintSettingsGetCollateMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintSettings.printSettingsGetCollate",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintSettings.html#v:printSettingsGetCollate"
        })


#endif

-- method PrintSettings::get_default_source
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "settings"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintSettings" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintSettings"
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

foreign import ccall "gtk_print_settings_get_default_source" gtk_print_settings_get_default_source :: 
    Ptr PrintSettings ->                    -- settings : TInterface (Name {namespace = "Gtk", name = "PrintSettings"})
    IO CString

-- | Gets the value of 'GI.Gtk.Constants.PRINT_SETTINGS_DEFAULT_SOURCE'.
-- 
-- /Since: 2.10/
printSettingsGetDefaultSource ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintSettings a) =>
    a
    -- ^ /@settings@/: a t'GI.Gtk.Objects.PrintSettings.PrintSettings'
    -> m T.Text
    -- ^ __Returns:__ the default source
printSettingsGetDefaultSource settings = liftIO $ do
    settings' <- unsafeManagedPtrCastPtr settings
    result <- gtk_print_settings_get_default_source settings'
    checkUnexpectedReturnNULL "printSettingsGetDefaultSource" result
    result' <- cstringToText result
    touchManagedPtr settings
    return result'

#if defined(ENABLE_OVERLOADING)
data PrintSettingsGetDefaultSourceMethodInfo
instance (signature ~ (m T.Text), MonadIO m, IsPrintSettings a) => O.OverloadedMethod PrintSettingsGetDefaultSourceMethodInfo a signature where
    overloadedMethod = printSettingsGetDefaultSource

instance O.OverloadedMethodInfo PrintSettingsGetDefaultSourceMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintSettings.printSettingsGetDefaultSource",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintSettings.html#v:printSettingsGetDefaultSource"
        })


#endif

-- method PrintSettings::get_dither
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "settings"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintSettings" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintSettings"
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

foreign import ccall "gtk_print_settings_get_dither" gtk_print_settings_get_dither :: 
    Ptr PrintSettings ->                    -- settings : TInterface (Name {namespace = "Gtk", name = "PrintSettings"})
    IO CString

-- | Gets the value of 'GI.Gtk.Constants.PRINT_SETTINGS_DITHER'.
-- 
-- /Since: 2.10/
printSettingsGetDither ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintSettings a) =>
    a
    -- ^ /@settings@/: a t'GI.Gtk.Objects.PrintSettings.PrintSettings'
    -> m T.Text
    -- ^ __Returns:__ the dithering that is used
printSettingsGetDither settings = liftIO $ do
    settings' <- unsafeManagedPtrCastPtr settings
    result <- gtk_print_settings_get_dither settings'
    checkUnexpectedReturnNULL "printSettingsGetDither" result
    result' <- cstringToText result
    touchManagedPtr settings
    return result'

#if defined(ENABLE_OVERLOADING)
data PrintSettingsGetDitherMethodInfo
instance (signature ~ (m T.Text), MonadIO m, IsPrintSettings a) => O.OverloadedMethod PrintSettingsGetDitherMethodInfo a signature where
    overloadedMethod = printSettingsGetDither

instance O.OverloadedMethodInfo PrintSettingsGetDitherMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintSettings.printSettingsGetDither",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintSettings.html#v:printSettingsGetDither"
        })


#endif

-- method PrintSettings::get_double
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "settings"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintSettings" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintSettings"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "key"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a key" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TBasicType TDouble)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_print_settings_get_double" gtk_print_settings_get_double :: 
    Ptr PrintSettings ->                    -- settings : TInterface (Name {namespace = "Gtk", name = "PrintSettings"})
    CString ->                              -- key : TBasicType TUTF8
    IO CDouble

-- | Returns the double value associated with /@key@/, or 0.
-- 
-- /Since: 2.10/
printSettingsGetDouble ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintSettings a) =>
    a
    -- ^ /@settings@/: a t'GI.Gtk.Objects.PrintSettings.PrintSettings'
    -> T.Text
    -- ^ /@key@/: a key
    -> m Double
    -- ^ __Returns:__ the double value of /@key@/
printSettingsGetDouble settings key = liftIO $ do
    settings' <- unsafeManagedPtrCastPtr settings
    key' <- textToCString key
    result <- gtk_print_settings_get_double settings' key'
    let result' = realToFrac result
    touchManagedPtr settings
    freeMem key'
    return result'

#if defined(ENABLE_OVERLOADING)
data PrintSettingsGetDoubleMethodInfo
instance (signature ~ (T.Text -> m Double), MonadIO m, IsPrintSettings a) => O.OverloadedMethod PrintSettingsGetDoubleMethodInfo a signature where
    overloadedMethod = printSettingsGetDouble

instance O.OverloadedMethodInfo PrintSettingsGetDoubleMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintSettings.printSettingsGetDouble",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintSettings.html#v:printSettingsGetDouble"
        })


#endif

-- method PrintSettings::get_double_with_default
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "settings"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintSettings" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintSettings"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "key"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a key" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "def"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the default value" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TBasicType TDouble)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_print_settings_get_double_with_default" gtk_print_settings_get_double_with_default :: 
    Ptr PrintSettings ->                    -- settings : TInterface (Name {namespace = "Gtk", name = "PrintSettings"})
    CString ->                              -- key : TBasicType TUTF8
    CDouble ->                              -- def : TBasicType TDouble
    IO CDouble

-- | Returns the floating point number represented by
-- the value that is associated with /@key@/, or /@defaultVal@/
-- if the value does not represent a floating point number.
-- 
-- Floating point numbers are parsed with 'GI.GLib.Functions.asciiStrtod'.
-- 
-- /Since: 2.10/
printSettingsGetDoubleWithDefault ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintSettings a) =>
    a
    -- ^ /@settings@/: a t'GI.Gtk.Objects.PrintSettings.PrintSettings'
    -> T.Text
    -- ^ /@key@/: a key
    -> Double
    -- ^ /@def@/: the default value
    -> m Double
    -- ^ __Returns:__ the floating point number associated with /@key@/
printSettingsGetDoubleWithDefault settings key def = liftIO $ do
    settings' <- unsafeManagedPtrCastPtr settings
    key' <- textToCString key
    let def' = realToFrac def
    result <- gtk_print_settings_get_double_with_default settings' key' def'
    let result' = realToFrac result
    touchManagedPtr settings
    freeMem key'
    return result'

#if defined(ENABLE_OVERLOADING)
data PrintSettingsGetDoubleWithDefaultMethodInfo
instance (signature ~ (T.Text -> Double -> m Double), MonadIO m, IsPrintSettings a) => O.OverloadedMethod PrintSettingsGetDoubleWithDefaultMethodInfo a signature where
    overloadedMethod = printSettingsGetDoubleWithDefault

instance O.OverloadedMethodInfo PrintSettingsGetDoubleWithDefaultMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintSettings.printSettingsGetDoubleWithDefault",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintSettings.html#v:printSettingsGetDoubleWithDefault"
        })


#endif

-- method PrintSettings::get_duplex
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "settings"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintSettings" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintSettings"
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "PrintDuplex" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_print_settings_get_duplex" gtk_print_settings_get_duplex :: 
    Ptr PrintSettings ->                    -- settings : TInterface (Name {namespace = "Gtk", name = "PrintSettings"})
    IO CUInt

-- | Gets the value of 'GI.Gtk.Constants.PRINT_SETTINGS_DUPLEX'.
-- 
-- /Since: 2.10/
printSettingsGetDuplex ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintSettings a) =>
    a
    -- ^ /@settings@/: a t'GI.Gtk.Objects.PrintSettings.PrintSettings'
    -> m Gtk.Enums.PrintDuplex
    -- ^ __Returns:__ whether to print the output in duplex.
printSettingsGetDuplex settings = liftIO $ do
    settings' <- unsafeManagedPtrCastPtr settings
    result <- gtk_print_settings_get_duplex settings'
    let result' = (toEnum . fromIntegral) result
    touchManagedPtr settings
    return result'

#if defined(ENABLE_OVERLOADING)
data PrintSettingsGetDuplexMethodInfo
instance (signature ~ (m Gtk.Enums.PrintDuplex), MonadIO m, IsPrintSettings a) => O.OverloadedMethod PrintSettingsGetDuplexMethodInfo a signature where
    overloadedMethod = printSettingsGetDuplex

instance O.OverloadedMethodInfo PrintSettingsGetDuplexMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintSettings.printSettingsGetDuplex",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintSettings.html#v:printSettingsGetDuplex"
        })


#endif

-- method PrintSettings::get_finishings
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "settings"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintSettings" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintSettings"
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

foreign import ccall "gtk_print_settings_get_finishings" gtk_print_settings_get_finishings :: 
    Ptr PrintSettings ->                    -- settings : TInterface (Name {namespace = "Gtk", name = "PrintSettings"})
    IO CString

-- | Gets the value of 'GI.Gtk.Constants.PRINT_SETTINGS_FINISHINGS'.
-- 
-- /Since: 2.10/
printSettingsGetFinishings ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintSettings a) =>
    a
    -- ^ /@settings@/: a t'GI.Gtk.Objects.PrintSettings.PrintSettings'
    -> m T.Text
    -- ^ __Returns:__ the finishings
printSettingsGetFinishings settings = liftIO $ do
    settings' <- unsafeManagedPtrCastPtr settings
    result <- gtk_print_settings_get_finishings settings'
    checkUnexpectedReturnNULL "printSettingsGetFinishings" result
    result' <- cstringToText result
    touchManagedPtr settings
    return result'

#if defined(ENABLE_OVERLOADING)
data PrintSettingsGetFinishingsMethodInfo
instance (signature ~ (m T.Text), MonadIO m, IsPrintSettings a) => O.OverloadedMethod PrintSettingsGetFinishingsMethodInfo a signature where
    overloadedMethod = printSettingsGetFinishings

instance O.OverloadedMethodInfo PrintSettingsGetFinishingsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintSettings.printSettingsGetFinishings",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintSettings.html#v:printSettingsGetFinishings"
        })


#endif

-- method PrintSettings::get_int
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "settings"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintSettings" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintSettings"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "key"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a key" , sinceVersion = Nothing }
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

foreign import ccall "gtk_print_settings_get_int" gtk_print_settings_get_int :: 
    Ptr PrintSettings ->                    -- settings : TInterface (Name {namespace = "Gtk", name = "PrintSettings"})
    CString ->                              -- key : TBasicType TUTF8
    IO Int32

-- | Returns the integer value of /@key@/, or 0.
-- 
-- /Since: 2.10/
printSettingsGetInt ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintSettings a) =>
    a
    -- ^ /@settings@/: a t'GI.Gtk.Objects.PrintSettings.PrintSettings'
    -> T.Text
    -- ^ /@key@/: a key
    -> m Int32
    -- ^ __Returns:__ the integer value of /@key@/
printSettingsGetInt settings key = liftIO $ do
    settings' <- unsafeManagedPtrCastPtr settings
    key' <- textToCString key
    result <- gtk_print_settings_get_int settings' key'
    touchManagedPtr settings
    freeMem key'
    return result

#if defined(ENABLE_OVERLOADING)
data PrintSettingsGetIntMethodInfo
instance (signature ~ (T.Text -> m Int32), MonadIO m, IsPrintSettings a) => O.OverloadedMethod PrintSettingsGetIntMethodInfo a signature where
    overloadedMethod = printSettingsGetInt

instance O.OverloadedMethodInfo PrintSettingsGetIntMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintSettings.printSettingsGetInt",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintSettings.html#v:printSettingsGetInt"
        })


#endif

-- method PrintSettings::get_int_with_default
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "settings"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintSettings" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintSettings"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "key"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a key" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "def"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the default value" , sinceVersion = Nothing }
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

foreign import ccall "gtk_print_settings_get_int_with_default" gtk_print_settings_get_int_with_default :: 
    Ptr PrintSettings ->                    -- settings : TInterface (Name {namespace = "Gtk", name = "PrintSettings"})
    CString ->                              -- key : TBasicType TUTF8
    Int32 ->                                -- def : TBasicType TInt
    IO Int32

-- | Returns the value of /@key@/, interpreted as
-- an integer, or the default value.
-- 
-- /Since: 2.10/
printSettingsGetIntWithDefault ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintSettings a) =>
    a
    -- ^ /@settings@/: a t'GI.Gtk.Objects.PrintSettings.PrintSettings'
    -> T.Text
    -- ^ /@key@/: a key
    -> Int32
    -- ^ /@def@/: the default value
    -> m Int32
    -- ^ __Returns:__ the integer value of /@key@/
printSettingsGetIntWithDefault settings key def = liftIO $ do
    settings' <- unsafeManagedPtrCastPtr settings
    key' <- textToCString key
    result <- gtk_print_settings_get_int_with_default settings' key' def
    touchManagedPtr settings
    freeMem key'
    return result

#if defined(ENABLE_OVERLOADING)
data PrintSettingsGetIntWithDefaultMethodInfo
instance (signature ~ (T.Text -> Int32 -> m Int32), MonadIO m, IsPrintSettings a) => O.OverloadedMethod PrintSettingsGetIntWithDefaultMethodInfo a signature where
    overloadedMethod = printSettingsGetIntWithDefault

instance O.OverloadedMethodInfo PrintSettingsGetIntWithDefaultMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintSettings.printSettingsGetIntWithDefault",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintSettings.html#v:printSettingsGetIntWithDefault"
        })


#endif

-- method PrintSettings::get_length
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "settings"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintSettings" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintSettings"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "key"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a key" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "unit"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Unit" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the unit of the return value"
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
-- returnType: Just (TBasicType TDouble)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_print_settings_get_length" gtk_print_settings_get_length :: 
    Ptr PrintSettings ->                    -- settings : TInterface (Name {namespace = "Gtk", name = "PrintSettings"})
    CString ->                              -- key : TBasicType TUTF8
    CUInt ->                                -- unit : TInterface (Name {namespace = "Gtk", name = "Unit"})
    IO CDouble

-- | Returns the value associated with /@key@/, interpreted
-- as a length. The returned value is converted to /@units@/.
-- 
-- /Since: 2.10/
printSettingsGetLength ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintSettings a) =>
    a
    -- ^ /@settings@/: a t'GI.Gtk.Objects.PrintSettings.PrintSettings'
    -> T.Text
    -- ^ /@key@/: a key
    -> Gtk.Enums.Unit
    -- ^ /@unit@/: the unit of the return value
    -> m Double
    -- ^ __Returns:__ the length value of /@key@/, converted to /@unit@/
printSettingsGetLength settings key unit = liftIO $ do
    settings' <- unsafeManagedPtrCastPtr settings
    key' <- textToCString key
    let unit' = (fromIntegral . fromEnum) unit
    result <- gtk_print_settings_get_length settings' key' unit'
    let result' = realToFrac result
    touchManagedPtr settings
    freeMem key'
    return result'

#if defined(ENABLE_OVERLOADING)
data PrintSettingsGetLengthMethodInfo
instance (signature ~ (T.Text -> Gtk.Enums.Unit -> m Double), MonadIO m, IsPrintSettings a) => O.OverloadedMethod PrintSettingsGetLengthMethodInfo a signature where
    overloadedMethod = printSettingsGetLength

instance O.OverloadedMethodInfo PrintSettingsGetLengthMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintSettings.printSettingsGetLength",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintSettings.html#v:printSettingsGetLength"
        })


#endif

-- method PrintSettings::get_media_type
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "settings"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintSettings" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintSettings"
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

foreign import ccall "gtk_print_settings_get_media_type" gtk_print_settings_get_media_type :: 
    Ptr PrintSettings ->                    -- settings : TInterface (Name {namespace = "Gtk", name = "PrintSettings"})
    IO CString

-- | Gets the value of 'GI.Gtk.Constants.PRINT_SETTINGS_MEDIA_TYPE'.
-- 
-- The set of media types is defined in PWG 5101.1-2002 PWG.
-- 
-- /Since: 2.10/
printSettingsGetMediaType ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintSettings a) =>
    a
    -- ^ /@settings@/: a t'GI.Gtk.Objects.PrintSettings.PrintSettings'
    -> m T.Text
    -- ^ __Returns:__ the media type
printSettingsGetMediaType settings = liftIO $ do
    settings' <- unsafeManagedPtrCastPtr settings
    result <- gtk_print_settings_get_media_type settings'
    checkUnexpectedReturnNULL "printSettingsGetMediaType" result
    result' <- cstringToText result
    touchManagedPtr settings
    return result'

#if defined(ENABLE_OVERLOADING)
data PrintSettingsGetMediaTypeMethodInfo
instance (signature ~ (m T.Text), MonadIO m, IsPrintSettings a) => O.OverloadedMethod PrintSettingsGetMediaTypeMethodInfo a signature where
    overloadedMethod = printSettingsGetMediaType

instance O.OverloadedMethodInfo PrintSettingsGetMediaTypeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintSettings.printSettingsGetMediaType",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintSettings.html#v:printSettingsGetMediaType"
        })


#endif

-- method PrintSettings::get_n_copies
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "settings"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintSettings" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintSettings"
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

foreign import ccall "gtk_print_settings_get_n_copies" gtk_print_settings_get_n_copies :: 
    Ptr PrintSettings ->                    -- settings : TInterface (Name {namespace = "Gtk", name = "PrintSettings"})
    IO Int32

-- | Gets the value of 'GI.Gtk.Constants.PRINT_SETTINGS_N_COPIES'.
-- 
-- /Since: 2.10/
printSettingsGetNCopies ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintSettings a) =>
    a
    -- ^ /@settings@/: a t'GI.Gtk.Objects.PrintSettings.PrintSettings'
    -> m Int32
    -- ^ __Returns:__ the number of copies to print
printSettingsGetNCopies settings = liftIO $ do
    settings' <- unsafeManagedPtrCastPtr settings
    result <- gtk_print_settings_get_n_copies settings'
    touchManagedPtr settings
    return result

#if defined(ENABLE_OVERLOADING)
data PrintSettingsGetNCopiesMethodInfo
instance (signature ~ (m Int32), MonadIO m, IsPrintSettings a) => O.OverloadedMethod PrintSettingsGetNCopiesMethodInfo a signature where
    overloadedMethod = printSettingsGetNCopies

instance O.OverloadedMethodInfo PrintSettingsGetNCopiesMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintSettings.printSettingsGetNCopies",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintSettings.html#v:printSettingsGetNCopies"
        })


#endif

-- method PrintSettings::get_number_up
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "settings"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintSettings" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintSettings"
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

foreign import ccall "gtk_print_settings_get_number_up" gtk_print_settings_get_number_up :: 
    Ptr PrintSettings ->                    -- settings : TInterface (Name {namespace = "Gtk", name = "PrintSettings"})
    IO Int32

-- | Gets the value of 'GI.Gtk.Constants.PRINT_SETTINGS_NUMBER_UP'.
-- 
-- /Since: 2.10/
printSettingsGetNumberUp ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintSettings a) =>
    a
    -- ^ /@settings@/: a t'GI.Gtk.Objects.PrintSettings.PrintSettings'
    -> m Int32
    -- ^ __Returns:__ the number of pages per sheet
printSettingsGetNumberUp settings = liftIO $ do
    settings' <- unsafeManagedPtrCastPtr settings
    result <- gtk_print_settings_get_number_up settings'
    touchManagedPtr settings
    return result

#if defined(ENABLE_OVERLOADING)
data PrintSettingsGetNumberUpMethodInfo
instance (signature ~ (m Int32), MonadIO m, IsPrintSettings a) => O.OverloadedMethod PrintSettingsGetNumberUpMethodInfo a signature where
    overloadedMethod = printSettingsGetNumberUp

instance O.OverloadedMethodInfo PrintSettingsGetNumberUpMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintSettings.printSettingsGetNumberUp",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintSettings.html#v:printSettingsGetNumberUp"
        })


#endif

-- method PrintSettings::get_number_up_layout
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "settings"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintSettings" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintSettings"
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
--               (TInterface Name { namespace = "Gtk" , name = "NumberUpLayout" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_print_settings_get_number_up_layout" gtk_print_settings_get_number_up_layout :: 
    Ptr PrintSettings ->                    -- settings : TInterface (Name {namespace = "Gtk", name = "PrintSettings"})
    IO CUInt

-- | Gets the value of 'GI.Gtk.Constants.PRINT_SETTINGS_NUMBER_UP_LAYOUT'.
-- 
-- /Since: 2.14/
printSettingsGetNumberUpLayout ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintSettings a) =>
    a
    -- ^ /@settings@/: a t'GI.Gtk.Objects.PrintSettings.PrintSettings'
    -> m Gtk.Enums.NumberUpLayout
    -- ^ __Returns:__ layout of page in number-up mode
printSettingsGetNumberUpLayout settings = liftIO $ do
    settings' <- unsafeManagedPtrCastPtr settings
    result <- gtk_print_settings_get_number_up_layout settings'
    let result' = (toEnum . fromIntegral) result
    touchManagedPtr settings
    return result'

#if defined(ENABLE_OVERLOADING)
data PrintSettingsGetNumberUpLayoutMethodInfo
instance (signature ~ (m Gtk.Enums.NumberUpLayout), MonadIO m, IsPrintSettings a) => O.OverloadedMethod PrintSettingsGetNumberUpLayoutMethodInfo a signature where
    overloadedMethod = printSettingsGetNumberUpLayout

instance O.OverloadedMethodInfo PrintSettingsGetNumberUpLayoutMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintSettings.printSettingsGetNumberUpLayout",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintSettings.html#v:printSettingsGetNumberUpLayout"
        })


#endif

-- method PrintSettings::get_orientation
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "settings"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintSettings" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintSettings"
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
--               (TInterface Name { namespace = "Gtk" , name = "PageOrientation" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_print_settings_get_orientation" gtk_print_settings_get_orientation :: 
    Ptr PrintSettings ->                    -- settings : TInterface (Name {namespace = "Gtk", name = "PrintSettings"})
    IO CUInt

-- | Get the value of 'GI.Gtk.Constants.PRINT_SETTINGS_ORIENTATION',
-- converted to a t'GI.Gtk.Enums.PageOrientation'.
-- 
-- /Since: 2.10/
printSettingsGetOrientation ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintSettings a) =>
    a
    -- ^ /@settings@/: a t'GI.Gtk.Objects.PrintSettings.PrintSettings'
    -> m Gtk.Enums.PageOrientation
    -- ^ __Returns:__ the orientation
printSettingsGetOrientation settings = liftIO $ do
    settings' <- unsafeManagedPtrCastPtr settings
    result <- gtk_print_settings_get_orientation settings'
    let result' = (toEnum . fromIntegral) result
    touchManagedPtr settings
    return result'

#if defined(ENABLE_OVERLOADING)
data PrintSettingsGetOrientationMethodInfo
instance (signature ~ (m Gtk.Enums.PageOrientation), MonadIO m, IsPrintSettings a) => O.OverloadedMethod PrintSettingsGetOrientationMethodInfo a signature where
    overloadedMethod = printSettingsGetOrientation

instance O.OverloadedMethodInfo PrintSettingsGetOrientationMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintSettings.printSettingsGetOrientation",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintSettings.html#v:printSettingsGetOrientation"
        })


#endif

-- method PrintSettings::get_output_bin
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "settings"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintSettings" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintSettings"
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

foreign import ccall "gtk_print_settings_get_output_bin" gtk_print_settings_get_output_bin :: 
    Ptr PrintSettings ->                    -- settings : TInterface (Name {namespace = "Gtk", name = "PrintSettings"})
    IO CString

-- | Gets the value of 'GI.Gtk.Constants.PRINT_SETTINGS_OUTPUT_BIN'.
-- 
-- /Since: 2.10/
printSettingsGetOutputBin ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintSettings a) =>
    a
    -- ^ /@settings@/: a t'GI.Gtk.Objects.PrintSettings.PrintSettings'
    -> m T.Text
    -- ^ __Returns:__ the output bin
printSettingsGetOutputBin settings = liftIO $ do
    settings' <- unsafeManagedPtrCastPtr settings
    result <- gtk_print_settings_get_output_bin settings'
    checkUnexpectedReturnNULL "printSettingsGetOutputBin" result
    result' <- cstringToText result
    touchManagedPtr settings
    return result'

#if defined(ENABLE_OVERLOADING)
data PrintSettingsGetOutputBinMethodInfo
instance (signature ~ (m T.Text), MonadIO m, IsPrintSettings a) => O.OverloadedMethod PrintSettingsGetOutputBinMethodInfo a signature where
    overloadedMethod = printSettingsGetOutputBin

instance O.OverloadedMethodInfo PrintSettingsGetOutputBinMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintSettings.printSettingsGetOutputBin",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintSettings.html#v:printSettingsGetOutputBin"
        })


#endif

-- method PrintSettings::get_page_ranges
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "settings"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintSettings" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintSettings"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "num_ranges"
--           , argType = TBasicType TInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "return location for the length of the returned array"
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
--              { argCName = "num_ranges"
--              , argType = TBasicType TInt
--              , direction = DirectionOut
--              , mayBeNull = False
--              , argDoc =
--                  Documentation
--                    { rawDocText =
--                        Just "return location for the length of the returned array"
--                    , sinceVersion = Nothing
--                    }
--              , argScope = ScopeTypeInvalid
--              , argClosure = -1
--              , argDestroy = -1
--              , argCallerAllocates = False
--              , transfer = TransferEverything
--              }
--          ]
-- returnType: Just
--               (TCArray
--                  False
--                  (-1)
--                  1
--                  (TInterface Name { namespace = "Gtk" , name = "PageRange" }))
-- throws : False
-- Skip return : False

foreign import ccall "gtk_print_settings_get_page_ranges" gtk_print_settings_get_page_ranges :: 
    Ptr PrintSettings ->                    -- settings : TInterface (Name {namespace = "Gtk", name = "PrintSettings"})
    Ptr Int32 ->                            -- num_ranges : TBasicType TInt
    IO (Ptr Gtk.PageRange.PageRange)

-- | Gets the value of 'GI.Gtk.Constants.PRINT_SETTINGS_PAGE_RANGES'.
-- 
-- /Since: 2.10/
printSettingsGetPageRanges ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintSettings a) =>
    a
    -- ^ /@settings@/: a t'GI.Gtk.Objects.PrintSettings.PrintSettings'
    -> m [Gtk.PageRange.PageRange]
    -- ^ __Returns:__ an array
    --     of @/GtkPageRanges/@.  Use 'GI.GLib.Functions.free' to free the array when
    --     it is no longer needed.
printSettingsGetPageRanges settings = liftIO $ do
    settings' <- unsafeManagedPtrCastPtr settings
    numRanges <- allocMem :: IO (Ptr Int32)
    result <- gtk_print_settings_get_page_ranges settings' numRanges
    numRanges' <- peek numRanges
    checkUnexpectedReturnNULL "printSettingsGetPageRanges" result
    result' <- (unpackBlockArrayWithLength 8 numRanges') result
    result'' <- mapM (wrapPtr Gtk.PageRange.PageRange) result'
    freeMem result
    touchManagedPtr settings
    freeMem numRanges
    return result''

#if defined(ENABLE_OVERLOADING)
data PrintSettingsGetPageRangesMethodInfo
instance (signature ~ (m [Gtk.PageRange.PageRange]), MonadIO m, IsPrintSettings a) => O.OverloadedMethod PrintSettingsGetPageRangesMethodInfo a signature where
    overloadedMethod = printSettingsGetPageRanges

instance O.OverloadedMethodInfo PrintSettingsGetPageRangesMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintSettings.printSettingsGetPageRanges",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintSettings.html#v:printSettingsGetPageRanges"
        })


#endif

-- method PrintSettings::get_page_set
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "settings"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintSettings" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintSettings"
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "PageSet" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_print_settings_get_page_set" gtk_print_settings_get_page_set :: 
    Ptr PrintSettings ->                    -- settings : TInterface (Name {namespace = "Gtk", name = "PrintSettings"})
    IO CUInt

-- | Gets the value of 'GI.Gtk.Constants.PRINT_SETTINGS_PAGE_SET'.
-- 
-- /Since: 2.10/
printSettingsGetPageSet ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintSettings a) =>
    a
    -- ^ /@settings@/: a t'GI.Gtk.Objects.PrintSettings.PrintSettings'
    -> m Gtk.Enums.PageSet
    -- ^ __Returns:__ the set of pages to print
printSettingsGetPageSet settings = liftIO $ do
    settings' <- unsafeManagedPtrCastPtr settings
    result <- gtk_print_settings_get_page_set settings'
    let result' = (toEnum . fromIntegral) result
    touchManagedPtr settings
    return result'

#if defined(ENABLE_OVERLOADING)
data PrintSettingsGetPageSetMethodInfo
instance (signature ~ (m Gtk.Enums.PageSet), MonadIO m, IsPrintSettings a) => O.OverloadedMethod PrintSettingsGetPageSetMethodInfo a signature where
    overloadedMethod = printSettingsGetPageSet

instance O.OverloadedMethodInfo PrintSettingsGetPageSetMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintSettings.printSettingsGetPageSet",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintSettings.html#v:printSettingsGetPageSet"
        })


#endif

-- method PrintSettings::get_paper_height
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "settings"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintSettings" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintSettings"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "unit"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Unit" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the unit for the return value"
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
-- returnType: Just (TBasicType TDouble)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_print_settings_get_paper_height" gtk_print_settings_get_paper_height :: 
    Ptr PrintSettings ->                    -- settings : TInterface (Name {namespace = "Gtk", name = "PrintSettings"})
    CUInt ->                                -- unit : TInterface (Name {namespace = "Gtk", name = "Unit"})
    IO CDouble

-- | Gets the value of 'GI.Gtk.Constants.PRINT_SETTINGS_PAPER_HEIGHT',
-- converted to /@unit@/.
-- 
-- /Since: 2.10/
printSettingsGetPaperHeight ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintSettings a) =>
    a
    -- ^ /@settings@/: a t'GI.Gtk.Objects.PrintSettings.PrintSettings'
    -> Gtk.Enums.Unit
    -- ^ /@unit@/: the unit for the return value
    -> m Double
    -- ^ __Returns:__ the paper height, in units of /@unit@/
printSettingsGetPaperHeight settings unit = liftIO $ do
    settings' <- unsafeManagedPtrCastPtr settings
    let unit' = (fromIntegral . fromEnum) unit
    result <- gtk_print_settings_get_paper_height settings' unit'
    let result' = realToFrac result
    touchManagedPtr settings
    return result'

#if defined(ENABLE_OVERLOADING)
data PrintSettingsGetPaperHeightMethodInfo
instance (signature ~ (Gtk.Enums.Unit -> m Double), MonadIO m, IsPrintSettings a) => O.OverloadedMethod PrintSettingsGetPaperHeightMethodInfo a signature where
    overloadedMethod = printSettingsGetPaperHeight

instance O.OverloadedMethodInfo PrintSettingsGetPaperHeightMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintSettings.printSettingsGetPaperHeight",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintSettings.html#v:printSettingsGetPaperHeight"
        })


#endif

-- method PrintSettings::get_paper_size
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "settings"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintSettings" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintSettings"
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "PaperSize" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_print_settings_get_paper_size" gtk_print_settings_get_paper_size :: 
    Ptr PrintSettings ->                    -- settings : TInterface (Name {namespace = "Gtk", name = "PrintSettings"})
    IO (Ptr Gtk.PaperSize.PaperSize)

-- | Gets the value of 'GI.Gtk.Constants.PRINT_SETTINGS_PAPER_FORMAT',
-- converted to a t'GI.Gtk.Structs.PaperSize.PaperSize'.
-- 
-- /Since: 2.10/
printSettingsGetPaperSize ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintSettings a) =>
    a
    -- ^ /@settings@/: a t'GI.Gtk.Objects.PrintSettings.PrintSettings'
    -> m Gtk.PaperSize.PaperSize
    -- ^ __Returns:__ the paper size
printSettingsGetPaperSize settings = liftIO $ do
    settings' <- unsafeManagedPtrCastPtr settings
    result <- gtk_print_settings_get_paper_size settings'
    checkUnexpectedReturnNULL "printSettingsGetPaperSize" result
    result' <- (wrapBoxed Gtk.PaperSize.PaperSize) result
    touchManagedPtr settings
    return result'

#if defined(ENABLE_OVERLOADING)
data PrintSettingsGetPaperSizeMethodInfo
instance (signature ~ (m Gtk.PaperSize.PaperSize), MonadIO m, IsPrintSettings a) => O.OverloadedMethod PrintSettingsGetPaperSizeMethodInfo a signature where
    overloadedMethod = printSettingsGetPaperSize

instance O.OverloadedMethodInfo PrintSettingsGetPaperSizeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintSettings.printSettingsGetPaperSize",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintSettings.html#v:printSettingsGetPaperSize"
        })


#endif

-- method PrintSettings::get_paper_width
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "settings"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintSettings" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintSettings"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "unit"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Unit" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the unit for the return value"
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
-- returnType: Just (TBasicType TDouble)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_print_settings_get_paper_width" gtk_print_settings_get_paper_width :: 
    Ptr PrintSettings ->                    -- settings : TInterface (Name {namespace = "Gtk", name = "PrintSettings"})
    CUInt ->                                -- unit : TInterface (Name {namespace = "Gtk", name = "Unit"})
    IO CDouble

-- | Gets the value of 'GI.Gtk.Constants.PRINT_SETTINGS_PAPER_WIDTH',
-- converted to /@unit@/.
-- 
-- /Since: 2.10/
printSettingsGetPaperWidth ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintSettings a) =>
    a
    -- ^ /@settings@/: a t'GI.Gtk.Objects.PrintSettings.PrintSettings'
    -> Gtk.Enums.Unit
    -- ^ /@unit@/: the unit for the return value
    -> m Double
    -- ^ __Returns:__ the paper width, in units of /@unit@/
printSettingsGetPaperWidth settings unit = liftIO $ do
    settings' <- unsafeManagedPtrCastPtr settings
    let unit' = (fromIntegral . fromEnum) unit
    result <- gtk_print_settings_get_paper_width settings' unit'
    let result' = realToFrac result
    touchManagedPtr settings
    return result'

#if defined(ENABLE_OVERLOADING)
data PrintSettingsGetPaperWidthMethodInfo
instance (signature ~ (Gtk.Enums.Unit -> m Double), MonadIO m, IsPrintSettings a) => O.OverloadedMethod PrintSettingsGetPaperWidthMethodInfo a signature where
    overloadedMethod = printSettingsGetPaperWidth

instance O.OverloadedMethodInfo PrintSettingsGetPaperWidthMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintSettings.printSettingsGetPaperWidth",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintSettings.html#v:printSettingsGetPaperWidth"
        })


#endif

-- method PrintSettings::get_print_pages
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "settings"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintSettings" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintSettings"
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "PrintPages" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_print_settings_get_print_pages" gtk_print_settings_get_print_pages :: 
    Ptr PrintSettings ->                    -- settings : TInterface (Name {namespace = "Gtk", name = "PrintSettings"})
    IO CUInt

-- | Gets the value of 'GI.Gtk.Constants.PRINT_SETTINGS_PRINT_PAGES'.
-- 
-- /Since: 2.10/
printSettingsGetPrintPages ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintSettings a) =>
    a
    -- ^ /@settings@/: a t'GI.Gtk.Objects.PrintSettings.PrintSettings'
    -> m Gtk.Enums.PrintPages
    -- ^ __Returns:__ which pages to print
printSettingsGetPrintPages settings = liftIO $ do
    settings' <- unsafeManagedPtrCastPtr settings
    result <- gtk_print_settings_get_print_pages settings'
    let result' = (toEnum . fromIntegral) result
    touchManagedPtr settings
    return result'

#if defined(ENABLE_OVERLOADING)
data PrintSettingsGetPrintPagesMethodInfo
instance (signature ~ (m Gtk.Enums.PrintPages), MonadIO m, IsPrintSettings a) => O.OverloadedMethod PrintSettingsGetPrintPagesMethodInfo a signature where
    overloadedMethod = printSettingsGetPrintPages

instance O.OverloadedMethodInfo PrintSettingsGetPrintPagesMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintSettings.printSettingsGetPrintPages",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintSettings.html#v:printSettingsGetPrintPages"
        })


#endif

-- method PrintSettings::get_printer
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "settings"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintSettings" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintSettings"
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

foreign import ccall "gtk_print_settings_get_printer" gtk_print_settings_get_printer :: 
    Ptr PrintSettings ->                    -- settings : TInterface (Name {namespace = "Gtk", name = "PrintSettings"})
    IO CString

-- | Convenience function to obtain the value of
-- 'GI.Gtk.Constants.PRINT_SETTINGS_PRINTER'.
-- 
-- /Since: 2.10/
printSettingsGetPrinter ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintSettings a) =>
    a
    -- ^ /@settings@/: a t'GI.Gtk.Objects.PrintSettings.PrintSettings'
    -> m T.Text
    -- ^ __Returns:__ the printer name
printSettingsGetPrinter settings = liftIO $ do
    settings' <- unsafeManagedPtrCastPtr settings
    result <- gtk_print_settings_get_printer settings'
    checkUnexpectedReturnNULL "printSettingsGetPrinter" result
    result' <- cstringToText result
    touchManagedPtr settings
    return result'

#if defined(ENABLE_OVERLOADING)
data PrintSettingsGetPrinterMethodInfo
instance (signature ~ (m T.Text), MonadIO m, IsPrintSettings a) => O.OverloadedMethod PrintSettingsGetPrinterMethodInfo a signature where
    overloadedMethod = printSettingsGetPrinter

instance O.OverloadedMethodInfo PrintSettingsGetPrinterMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintSettings.printSettingsGetPrinter",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintSettings.html#v:printSettingsGetPrinter"
        })


#endif

-- method PrintSettings::get_printer_lpi
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "settings"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintSettings" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintSettings"
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
-- returnType: Just (TBasicType TDouble)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_print_settings_get_printer_lpi" gtk_print_settings_get_printer_lpi :: 
    Ptr PrintSettings ->                    -- settings : TInterface (Name {namespace = "Gtk", name = "PrintSettings"})
    IO CDouble

-- | Gets the value of 'GI.Gtk.Constants.PRINT_SETTINGS_PRINTER_LPI'.
-- 
-- /Since: 2.16/
printSettingsGetPrinterLpi ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintSettings a) =>
    a
    -- ^ /@settings@/: a t'GI.Gtk.Objects.PrintSettings.PrintSettings'
    -> m Double
    -- ^ __Returns:__ the resolution in lpi (lines per inch)
printSettingsGetPrinterLpi settings = liftIO $ do
    settings' <- unsafeManagedPtrCastPtr settings
    result <- gtk_print_settings_get_printer_lpi settings'
    let result' = realToFrac result
    touchManagedPtr settings
    return result'

#if defined(ENABLE_OVERLOADING)
data PrintSettingsGetPrinterLpiMethodInfo
instance (signature ~ (m Double), MonadIO m, IsPrintSettings a) => O.OverloadedMethod PrintSettingsGetPrinterLpiMethodInfo a signature where
    overloadedMethod = printSettingsGetPrinterLpi

instance O.OverloadedMethodInfo PrintSettingsGetPrinterLpiMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintSettings.printSettingsGetPrinterLpi",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintSettings.html#v:printSettingsGetPrinterLpi"
        })


#endif

-- method PrintSettings::get_quality
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "settings"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintSettings" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintSettings"
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
--               (TInterface Name { namespace = "Gtk" , name = "PrintQuality" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_print_settings_get_quality" gtk_print_settings_get_quality :: 
    Ptr PrintSettings ->                    -- settings : TInterface (Name {namespace = "Gtk", name = "PrintSettings"})
    IO CUInt

-- | Gets the value of 'GI.Gtk.Constants.PRINT_SETTINGS_QUALITY'.
-- 
-- /Since: 2.10/
printSettingsGetQuality ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintSettings a) =>
    a
    -- ^ /@settings@/: a t'GI.Gtk.Objects.PrintSettings.PrintSettings'
    -> m Gtk.Enums.PrintQuality
    -- ^ __Returns:__ the print quality
printSettingsGetQuality settings = liftIO $ do
    settings' <- unsafeManagedPtrCastPtr settings
    result <- gtk_print_settings_get_quality settings'
    let result' = (toEnum . fromIntegral) result
    touchManagedPtr settings
    return result'

#if defined(ENABLE_OVERLOADING)
data PrintSettingsGetQualityMethodInfo
instance (signature ~ (m Gtk.Enums.PrintQuality), MonadIO m, IsPrintSettings a) => O.OverloadedMethod PrintSettingsGetQualityMethodInfo a signature where
    overloadedMethod = printSettingsGetQuality

instance O.OverloadedMethodInfo PrintSettingsGetQualityMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintSettings.printSettingsGetQuality",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintSettings.html#v:printSettingsGetQuality"
        })


#endif

-- method PrintSettings::get_resolution
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "settings"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintSettings" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintSettings"
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

foreign import ccall "gtk_print_settings_get_resolution" gtk_print_settings_get_resolution :: 
    Ptr PrintSettings ->                    -- settings : TInterface (Name {namespace = "Gtk", name = "PrintSettings"})
    IO Int32

-- | Gets the value of 'GI.Gtk.Constants.PRINT_SETTINGS_RESOLUTION'.
-- 
-- /Since: 2.10/
printSettingsGetResolution ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintSettings a) =>
    a
    -- ^ /@settings@/: a t'GI.Gtk.Objects.PrintSettings.PrintSettings'
    -> m Int32
    -- ^ __Returns:__ the resolution in dpi
printSettingsGetResolution settings = liftIO $ do
    settings' <- unsafeManagedPtrCastPtr settings
    result <- gtk_print_settings_get_resolution settings'
    touchManagedPtr settings
    return result

#if defined(ENABLE_OVERLOADING)
data PrintSettingsGetResolutionMethodInfo
instance (signature ~ (m Int32), MonadIO m, IsPrintSettings a) => O.OverloadedMethod PrintSettingsGetResolutionMethodInfo a signature where
    overloadedMethod = printSettingsGetResolution

instance O.OverloadedMethodInfo PrintSettingsGetResolutionMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintSettings.printSettingsGetResolution",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintSettings.html#v:printSettingsGetResolution"
        })


#endif

-- method PrintSettings::get_resolution_x
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "settings"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintSettings" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintSettings"
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

foreign import ccall "gtk_print_settings_get_resolution_x" gtk_print_settings_get_resolution_x :: 
    Ptr PrintSettings ->                    -- settings : TInterface (Name {namespace = "Gtk", name = "PrintSettings"})
    IO Int32

-- | Gets the value of 'GI.Gtk.Constants.PRINT_SETTINGS_RESOLUTION_X'.
-- 
-- /Since: 2.16/
printSettingsGetResolutionX ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintSettings a) =>
    a
    -- ^ /@settings@/: a t'GI.Gtk.Objects.PrintSettings.PrintSettings'
    -> m Int32
    -- ^ __Returns:__ the horizontal resolution in dpi
printSettingsGetResolutionX settings = liftIO $ do
    settings' <- unsafeManagedPtrCastPtr settings
    result <- gtk_print_settings_get_resolution_x settings'
    touchManagedPtr settings
    return result

#if defined(ENABLE_OVERLOADING)
data PrintSettingsGetResolutionXMethodInfo
instance (signature ~ (m Int32), MonadIO m, IsPrintSettings a) => O.OverloadedMethod PrintSettingsGetResolutionXMethodInfo a signature where
    overloadedMethod = printSettingsGetResolutionX

instance O.OverloadedMethodInfo PrintSettingsGetResolutionXMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintSettings.printSettingsGetResolutionX",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintSettings.html#v:printSettingsGetResolutionX"
        })


#endif

-- method PrintSettings::get_resolution_y
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "settings"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintSettings" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintSettings"
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

foreign import ccall "gtk_print_settings_get_resolution_y" gtk_print_settings_get_resolution_y :: 
    Ptr PrintSettings ->                    -- settings : TInterface (Name {namespace = "Gtk", name = "PrintSettings"})
    IO Int32

-- | Gets the value of 'GI.Gtk.Constants.PRINT_SETTINGS_RESOLUTION_Y'.
-- 
-- /Since: 2.16/
printSettingsGetResolutionY ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintSettings a) =>
    a
    -- ^ /@settings@/: a t'GI.Gtk.Objects.PrintSettings.PrintSettings'
    -> m Int32
    -- ^ __Returns:__ the vertical resolution in dpi
printSettingsGetResolutionY settings = liftIO $ do
    settings' <- unsafeManagedPtrCastPtr settings
    result <- gtk_print_settings_get_resolution_y settings'
    touchManagedPtr settings
    return result

#if defined(ENABLE_OVERLOADING)
data PrintSettingsGetResolutionYMethodInfo
instance (signature ~ (m Int32), MonadIO m, IsPrintSettings a) => O.OverloadedMethod PrintSettingsGetResolutionYMethodInfo a signature where
    overloadedMethod = printSettingsGetResolutionY

instance O.OverloadedMethodInfo PrintSettingsGetResolutionYMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintSettings.printSettingsGetResolutionY",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintSettings.html#v:printSettingsGetResolutionY"
        })


#endif

-- method PrintSettings::get_reverse
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "settings"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintSettings" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintSettings"
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

foreign import ccall "gtk_print_settings_get_reverse" gtk_print_settings_get_reverse :: 
    Ptr PrintSettings ->                    -- settings : TInterface (Name {namespace = "Gtk", name = "PrintSettings"})
    IO CInt

-- | Gets the value of 'GI.Gtk.Constants.PRINT_SETTINGS_REVERSE'.
-- 
-- /Since: 2.10/
printSettingsGetReverse ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintSettings a) =>
    a
    -- ^ /@settings@/: a t'GI.Gtk.Objects.PrintSettings.PrintSettings'
    -> m Bool
    -- ^ __Returns:__ whether to reverse the order of the printed pages
printSettingsGetReverse settings = liftIO $ do
    settings' <- unsafeManagedPtrCastPtr settings
    result <- gtk_print_settings_get_reverse settings'
    let result' = (/= 0) result
    touchManagedPtr settings
    return result'

#if defined(ENABLE_OVERLOADING)
data PrintSettingsGetReverseMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsPrintSettings a) => O.OverloadedMethod PrintSettingsGetReverseMethodInfo a signature where
    overloadedMethod = printSettingsGetReverse

instance O.OverloadedMethodInfo PrintSettingsGetReverseMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintSettings.printSettingsGetReverse",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintSettings.html#v:printSettingsGetReverse"
        })


#endif

-- method PrintSettings::get_scale
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "settings"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintSettings" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintSettings"
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
-- returnType: Just (TBasicType TDouble)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_print_settings_get_scale" gtk_print_settings_get_scale :: 
    Ptr PrintSettings ->                    -- settings : TInterface (Name {namespace = "Gtk", name = "PrintSettings"})
    IO CDouble

-- | Gets the value of 'GI.Gtk.Constants.PRINT_SETTINGS_SCALE'.
-- 
-- /Since: 2.10/
printSettingsGetScale ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintSettings a) =>
    a
    -- ^ /@settings@/: a t'GI.Gtk.Objects.PrintSettings.PrintSettings'
    -> m Double
    -- ^ __Returns:__ the scale in percent
printSettingsGetScale settings = liftIO $ do
    settings' <- unsafeManagedPtrCastPtr settings
    result <- gtk_print_settings_get_scale settings'
    let result' = realToFrac result
    touchManagedPtr settings
    return result'

#if defined(ENABLE_OVERLOADING)
data PrintSettingsGetScaleMethodInfo
instance (signature ~ (m Double), MonadIO m, IsPrintSettings a) => O.OverloadedMethod PrintSettingsGetScaleMethodInfo a signature where
    overloadedMethod = printSettingsGetScale

instance O.OverloadedMethodInfo PrintSettingsGetScaleMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintSettings.printSettingsGetScale",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintSettings.html#v:printSettingsGetScale"
        })


#endif

-- method PrintSettings::get_use_color
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "settings"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintSettings" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintSettings"
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

foreign import ccall "gtk_print_settings_get_use_color" gtk_print_settings_get_use_color :: 
    Ptr PrintSettings ->                    -- settings : TInterface (Name {namespace = "Gtk", name = "PrintSettings"})
    IO CInt

-- | Gets the value of 'GI.Gtk.Constants.PRINT_SETTINGS_USE_COLOR'.
-- 
-- /Since: 2.10/
printSettingsGetUseColor ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintSettings a) =>
    a
    -- ^ /@settings@/: a t'GI.Gtk.Objects.PrintSettings.PrintSettings'
    -> m Bool
    -- ^ __Returns:__ whether to use color
printSettingsGetUseColor settings = liftIO $ do
    settings' <- unsafeManagedPtrCastPtr settings
    result <- gtk_print_settings_get_use_color settings'
    let result' = (/= 0) result
    touchManagedPtr settings
    return result'

#if defined(ENABLE_OVERLOADING)
data PrintSettingsGetUseColorMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsPrintSettings a) => O.OverloadedMethod PrintSettingsGetUseColorMethodInfo a signature where
    overloadedMethod = printSettingsGetUseColor

instance O.OverloadedMethodInfo PrintSettingsGetUseColorMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintSettings.printSettingsGetUseColor",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintSettings.html#v:printSettingsGetUseColor"
        })


#endif

-- method PrintSettings::has_key
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "settings"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintSettings" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintSettings"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "key"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a key" , sinceVersion = Nothing }
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

foreign import ccall "gtk_print_settings_has_key" gtk_print_settings_has_key :: 
    Ptr PrintSettings ->                    -- settings : TInterface (Name {namespace = "Gtk", name = "PrintSettings"})
    CString ->                              -- key : TBasicType TUTF8
    IO CInt

-- | Returns 'P.True', if a value is associated with /@key@/.
-- 
-- /Since: 2.10/
printSettingsHasKey ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintSettings a) =>
    a
    -- ^ /@settings@/: a t'GI.Gtk.Objects.PrintSettings.PrintSettings'
    -> T.Text
    -- ^ /@key@/: a key
    -> m Bool
    -- ^ __Returns:__ 'P.True', if /@key@/ has a value
printSettingsHasKey settings key = liftIO $ do
    settings' <- unsafeManagedPtrCastPtr settings
    key' <- textToCString key
    result <- gtk_print_settings_has_key settings' key'
    let result' = (/= 0) result
    touchManagedPtr settings
    freeMem key'
    return result'

#if defined(ENABLE_OVERLOADING)
data PrintSettingsHasKeyMethodInfo
instance (signature ~ (T.Text -> m Bool), MonadIO m, IsPrintSettings a) => O.OverloadedMethod PrintSettingsHasKeyMethodInfo a signature where
    overloadedMethod = printSettingsHasKey

instance O.OverloadedMethodInfo PrintSettingsHasKeyMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintSettings.printSettingsHasKey",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintSettings.html#v:printSettingsHasKey"
        })


#endif

-- method PrintSettings::load_file
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "settings"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintSettings" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintSettings"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "file_name"
--           , argType = TBasicType TFileName
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the filename to read the settings from"
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
-- throws : True
-- Skip return : False

foreign import ccall "gtk_print_settings_load_file" gtk_print_settings_load_file :: 
    Ptr PrintSettings ->                    -- settings : TInterface (Name {namespace = "Gtk", name = "PrintSettings"})
    CString ->                              -- file_name : TBasicType TFileName
    Ptr (Ptr GError) ->                     -- error
    IO CInt

-- | Reads the print settings from /@fileName@/. If the file could not be loaded
-- then error is set to either a t'GI.GLib.Enums.FileError' or t'GI.GLib.Enums.KeyFileError'.
-- See 'GI.Gtk.Objects.PrintSettings.printSettingsToFile'.
-- 
-- /Since: 2.14/
printSettingsLoadFile ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintSettings a) =>
    a
    -- ^ /@settings@/: a t'GI.Gtk.Objects.PrintSettings.PrintSettings'
    -> [Char]
    -- ^ /@fileName@/: the filename to read the settings from
    -> m ()
    -- ^ /(Can throw 'Data.GI.Base.GError.GError')/
printSettingsLoadFile settings fileName = liftIO $ do
    settings' <- unsafeManagedPtrCastPtr settings
    fileName' <- stringToCString fileName
    onException (do
        _ <- propagateGError $ gtk_print_settings_load_file settings' fileName'
        touchManagedPtr settings
        freeMem fileName'
        return ()
     ) (do
        freeMem fileName'
     )

#if defined(ENABLE_OVERLOADING)
data PrintSettingsLoadFileMethodInfo
instance (signature ~ ([Char] -> m ()), MonadIO m, IsPrintSettings a) => O.OverloadedMethod PrintSettingsLoadFileMethodInfo a signature where
    overloadedMethod = printSettingsLoadFile

instance O.OverloadedMethodInfo PrintSettingsLoadFileMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintSettings.printSettingsLoadFile",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintSettings.html#v:printSettingsLoadFile"
        })


#endif

-- method PrintSettings::load_key_file
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "settings"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintSettings" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintSettings"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "key_file"
--           , argType =
--               TInterface Name { namespace = "GLib" , name = "KeyFile" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the #GKeyFile to retrieve the settings from"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "group_name"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the name of the group to use, or %NULL to use the default\n    \8220Print Settings\8221"
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
-- throws : True
-- Skip return : False

foreign import ccall "gtk_print_settings_load_key_file" gtk_print_settings_load_key_file :: 
    Ptr PrintSettings ->                    -- settings : TInterface (Name {namespace = "Gtk", name = "PrintSettings"})
    Ptr GLib.KeyFile.KeyFile ->             -- key_file : TInterface (Name {namespace = "GLib", name = "KeyFile"})
    CString ->                              -- group_name : TBasicType TUTF8
    Ptr (Ptr GError) ->                     -- error
    IO CInt

-- | Reads the print settings from the group /@groupName@/ in /@keyFile@/. If the
-- file could not be loaded then error is set to either a t'GI.GLib.Enums.FileError' or
-- t'GI.GLib.Enums.KeyFileError'.
-- 
-- /Since: 2.14/
printSettingsLoadKeyFile ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintSettings a) =>
    a
    -- ^ /@settings@/: a t'GI.Gtk.Objects.PrintSettings.PrintSettings'
    -> GLib.KeyFile.KeyFile
    -- ^ /@keyFile@/: the t'GI.GLib.Structs.KeyFile.KeyFile' to retrieve the settings from
    -> Maybe (T.Text)
    -- ^ /@groupName@/: the name of the group to use, or 'P.Nothing' to use the default
    --     “Print Settings”
    -> m ()
    -- ^ /(Can throw 'Data.GI.Base.GError.GError')/
printSettingsLoadKeyFile settings keyFile groupName = liftIO $ do
    settings' <- unsafeManagedPtrCastPtr settings
    keyFile' <- unsafeManagedPtrGetPtr keyFile
    maybeGroupName <- case groupName of
        Nothing -> return nullPtr
        Just jGroupName -> do
            jGroupName' <- textToCString jGroupName
            return jGroupName'
    onException (do
        _ <- propagateGError $ gtk_print_settings_load_key_file settings' keyFile' maybeGroupName
        touchManagedPtr settings
        touchManagedPtr keyFile
        freeMem maybeGroupName
        return ()
     ) (do
        freeMem maybeGroupName
     )

#if defined(ENABLE_OVERLOADING)
data PrintSettingsLoadKeyFileMethodInfo
instance (signature ~ (GLib.KeyFile.KeyFile -> Maybe (T.Text) -> m ()), MonadIO m, IsPrintSettings a) => O.OverloadedMethod PrintSettingsLoadKeyFileMethodInfo a signature where
    overloadedMethod = printSettingsLoadKeyFile

instance O.OverloadedMethodInfo PrintSettingsLoadKeyFileMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintSettings.printSettingsLoadKeyFile",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintSettings.html#v:printSettingsLoadKeyFile"
        })


#endif

-- method PrintSettings::set
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "settings"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintSettings" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintSettings"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "key"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a key" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "value"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a string value, or %NULL"
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

foreign import ccall "gtk_print_settings_set" gtk_print_settings_set :: 
    Ptr PrintSettings ->                    -- settings : TInterface (Name {namespace = "Gtk", name = "PrintSettings"})
    CString ->                              -- key : TBasicType TUTF8
    CString ->                              -- value : TBasicType TUTF8
    IO ()

-- | Associates /@value@/ with /@key@/.
-- 
-- /Since: 2.10/
printSettingsSet ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintSettings a) =>
    a
    -- ^ /@settings@/: a t'GI.Gtk.Objects.PrintSettings.PrintSettings'
    -> T.Text
    -- ^ /@key@/: a key
    -> Maybe (T.Text)
    -- ^ /@value@/: a string value, or 'P.Nothing'
    -> m ()
printSettingsSet settings key value = liftIO $ do
    settings' <- unsafeManagedPtrCastPtr settings
    key' <- textToCString key
    maybeValue <- case value of
        Nothing -> return nullPtr
        Just jValue -> do
            jValue' <- textToCString jValue
            return jValue'
    gtk_print_settings_set settings' key' maybeValue
    touchManagedPtr settings
    freeMem key'
    freeMem maybeValue
    return ()

#if defined(ENABLE_OVERLOADING)
data PrintSettingsSetMethodInfo
instance (signature ~ (T.Text -> Maybe (T.Text) -> m ()), MonadIO m, IsPrintSettings a) => O.OverloadedMethod PrintSettingsSetMethodInfo a signature where
    overloadedMethod = printSettingsSet

instance O.OverloadedMethodInfo PrintSettingsSetMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintSettings.printSettingsSet",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintSettings.html#v:printSettingsSet"
        })


#endif

-- method PrintSettings::set_bool
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "settings"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintSettings" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintSettings"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "key"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a key" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "value"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a boolean" , sinceVersion = Nothing }
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

foreign import ccall "gtk_print_settings_set_bool" gtk_print_settings_set_bool :: 
    Ptr PrintSettings ->                    -- settings : TInterface (Name {namespace = "Gtk", name = "PrintSettings"})
    CString ->                              -- key : TBasicType TUTF8
    CInt ->                                 -- value : TBasicType TBoolean
    IO ()

-- | Sets /@key@/ to a boolean value.
-- 
-- /Since: 2.10/
printSettingsSetBool ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintSettings a) =>
    a
    -- ^ /@settings@/: a t'GI.Gtk.Objects.PrintSettings.PrintSettings'
    -> T.Text
    -- ^ /@key@/: a key
    -> Bool
    -- ^ /@value@/: a boolean
    -> m ()
printSettingsSetBool settings key value = liftIO $ do
    settings' <- unsafeManagedPtrCastPtr settings
    key' <- textToCString key
    let value' = (fromIntegral . fromEnum) value
    gtk_print_settings_set_bool settings' key' value'
    touchManagedPtr settings
    freeMem key'
    return ()

#if defined(ENABLE_OVERLOADING)
data PrintSettingsSetBoolMethodInfo
instance (signature ~ (T.Text -> Bool -> m ()), MonadIO m, IsPrintSettings a) => O.OverloadedMethod PrintSettingsSetBoolMethodInfo a signature where
    overloadedMethod = printSettingsSetBool

instance O.OverloadedMethodInfo PrintSettingsSetBoolMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintSettings.printSettingsSetBool",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintSettings.html#v:printSettingsSetBool"
        })


#endif

-- method PrintSettings::set_collate
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "settings"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintSettings" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintSettings"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "collate"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "whether to collate the output"
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

foreign import ccall "gtk_print_settings_set_collate" gtk_print_settings_set_collate :: 
    Ptr PrintSettings ->                    -- settings : TInterface (Name {namespace = "Gtk", name = "PrintSettings"})
    CInt ->                                 -- collate : TBasicType TBoolean
    IO ()

-- | Sets the value of 'GI.Gtk.Constants.PRINT_SETTINGS_COLLATE'.
-- 
-- /Since: 2.10/
printSettingsSetCollate ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintSettings a) =>
    a
    -- ^ /@settings@/: a t'GI.Gtk.Objects.PrintSettings.PrintSettings'
    -> Bool
    -- ^ /@collate@/: whether to collate the output
    -> m ()
printSettingsSetCollate settings collate = liftIO $ do
    settings' <- unsafeManagedPtrCastPtr settings
    let collate' = (fromIntegral . fromEnum) collate
    gtk_print_settings_set_collate settings' collate'
    touchManagedPtr settings
    return ()

#if defined(ENABLE_OVERLOADING)
data PrintSettingsSetCollateMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsPrintSettings a) => O.OverloadedMethod PrintSettingsSetCollateMethodInfo a signature where
    overloadedMethod = printSettingsSetCollate

instance O.OverloadedMethodInfo PrintSettingsSetCollateMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintSettings.printSettingsSetCollate",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintSettings.html#v:printSettingsSetCollate"
        })


#endif

-- method PrintSettings::set_default_source
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "settings"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintSettings" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintSettings"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "default_source"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the default source" , sinceVersion = Nothing }
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

foreign import ccall "gtk_print_settings_set_default_source" gtk_print_settings_set_default_source :: 
    Ptr PrintSettings ->                    -- settings : TInterface (Name {namespace = "Gtk", name = "PrintSettings"})
    CString ->                              -- default_source : TBasicType TUTF8
    IO ()

-- | Sets the value of 'GI.Gtk.Constants.PRINT_SETTINGS_DEFAULT_SOURCE'.
-- 
-- /Since: 2.10/
printSettingsSetDefaultSource ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintSettings a) =>
    a
    -- ^ /@settings@/: a t'GI.Gtk.Objects.PrintSettings.PrintSettings'
    -> T.Text
    -- ^ /@defaultSource@/: the default source
    -> m ()
printSettingsSetDefaultSource settings defaultSource = liftIO $ do
    settings' <- unsafeManagedPtrCastPtr settings
    defaultSource' <- textToCString defaultSource
    gtk_print_settings_set_default_source settings' defaultSource'
    touchManagedPtr settings
    freeMem defaultSource'
    return ()

#if defined(ENABLE_OVERLOADING)
data PrintSettingsSetDefaultSourceMethodInfo
instance (signature ~ (T.Text -> m ()), MonadIO m, IsPrintSettings a) => O.OverloadedMethod PrintSettingsSetDefaultSourceMethodInfo a signature where
    overloadedMethod = printSettingsSetDefaultSource

instance O.OverloadedMethodInfo PrintSettingsSetDefaultSourceMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintSettings.printSettingsSetDefaultSource",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintSettings.html#v:printSettingsSetDefaultSource"
        })


#endif

-- method PrintSettings::set_dither
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "settings"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintSettings" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintSettings"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "dither"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the dithering that is used"
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

foreign import ccall "gtk_print_settings_set_dither" gtk_print_settings_set_dither :: 
    Ptr PrintSettings ->                    -- settings : TInterface (Name {namespace = "Gtk", name = "PrintSettings"})
    CString ->                              -- dither : TBasicType TUTF8
    IO ()

-- | Sets the value of 'GI.Gtk.Constants.PRINT_SETTINGS_DITHER'.
-- 
-- /Since: 2.10/
printSettingsSetDither ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintSettings a) =>
    a
    -- ^ /@settings@/: a t'GI.Gtk.Objects.PrintSettings.PrintSettings'
    -> T.Text
    -- ^ /@dither@/: the dithering that is used
    -> m ()
printSettingsSetDither settings dither = liftIO $ do
    settings' <- unsafeManagedPtrCastPtr settings
    dither' <- textToCString dither
    gtk_print_settings_set_dither settings' dither'
    touchManagedPtr settings
    freeMem dither'
    return ()

#if defined(ENABLE_OVERLOADING)
data PrintSettingsSetDitherMethodInfo
instance (signature ~ (T.Text -> m ()), MonadIO m, IsPrintSettings a) => O.OverloadedMethod PrintSettingsSetDitherMethodInfo a signature where
    overloadedMethod = printSettingsSetDither

instance O.OverloadedMethodInfo PrintSettingsSetDitherMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintSettings.printSettingsSetDither",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintSettings.html#v:printSettingsSetDither"
        })


#endif

-- method PrintSettings::set_double
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "settings"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintSettings" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintSettings"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "key"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a key" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "value"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a double value" , sinceVersion = Nothing }
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

foreign import ccall "gtk_print_settings_set_double" gtk_print_settings_set_double :: 
    Ptr PrintSettings ->                    -- settings : TInterface (Name {namespace = "Gtk", name = "PrintSettings"})
    CString ->                              -- key : TBasicType TUTF8
    CDouble ->                              -- value : TBasicType TDouble
    IO ()

-- | Sets /@key@/ to a double value.
-- 
-- /Since: 2.10/
printSettingsSetDouble ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintSettings a) =>
    a
    -- ^ /@settings@/: a t'GI.Gtk.Objects.PrintSettings.PrintSettings'
    -> T.Text
    -- ^ /@key@/: a key
    -> Double
    -- ^ /@value@/: a double value
    -> m ()
printSettingsSetDouble settings key value = liftIO $ do
    settings' <- unsafeManagedPtrCastPtr settings
    key' <- textToCString key
    let value' = realToFrac value
    gtk_print_settings_set_double settings' key' value'
    touchManagedPtr settings
    freeMem key'
    return ()

#if defined(ENABLE_OVERLOADING)
data PrintSettingsSetDoubleMethodInfo
instance (signature ~ (T.Text -> Double -> m ()), MonadIO m, IsPrintSettings a) => O.OverloadedMethod PrintSettingsSetDoubleMethodInfo a signature where
    overloadedMethod = printSettingsSetDouble

instance O.OverloadedMethodInfo PrintSettingsSetDoubleMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintSettings.printSettingsSetDouble",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintSettings.html#v:printSettingsSetDouble"
        })


#endif

-- method PrintSettings::set_duplex
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "settings"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintSettings" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintSettings"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "duplex"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintDuplex" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintDuplex value"
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

foreign import ccall "gtk_print_settings_set_duplex" gtk_print_settings_set_duplex :: 
    Ptr PrintSettings ->                    -- settings : TInterface (Name {namespace = "Gtk", name = "PrintSettings"})
    CUInt ->                                -- duplex : TInterface (Name {namespace = "Gtk", name = "PrintDuplex"})
    IO ()

-- | Sets the value of 'GI.Gtk.Constants.PRINT_SETTINGS_DUPLEX'.
-- 
-- /Since: 2.10/
printSettingsSetDuplex ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintSettings a) =>
    a
    -- ^ /@settings@/: a t'GI.Gtk.Objects.PrintSettings.PrintSettings'
    -> Gtk.Enums.PrintDuplex
    -- ^ /@duplex@/: a t'GI.Gtk.Enums.PrintDuplex' value
    -> m ()
printSettingsSetDuplex settings duplex = liftIO $ do
    settings' <- unsafeManagedPtrCastPtr settings
    let duplex' = (fromIntegral . fromEnum) duplex
    gtk_print_settings_set_duplex settings' duplex'
    touchManagedPtr settings
    return ()

#if defined(ENABLE_OVERLOADING)
data PrintSettingsSetDuplexMethodInfo
instance (signature ~ (Gtk.Enums.PrintDuplex -> m ()), MonadIO m, IsPrintSettings a) => O.OverloadedMethod PrintSettingsSetDuplexMethodInfo a signature where
    overloadedMethod = printSettingsSetDuplex

instance O.OverloadedMethodInfo PrintSettingsSetDuplexMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintSettings.printSettingsSetDuplex",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintSettings.html#v:printSettingsSetDuplex"
        })


#endif

-- method PrintSettings::set_finishings
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "settings"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintSettings" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintSettings"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "finishings"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the finishings" , sinceVersion = Nothing }
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

foreign import ccall "gtk_print_settings_set_finishings" gtk_print_settings_set_finishings :: 
    Ptr PrintSettings ->                    -- settings : TInterface (Name {namespace = "Gtk", name = "PrintSettings"})
    CString ->                              -- finishings : TBasicType TUTF8
    IO ()

-- | Sets the value of 'GI.Gtk.Constants.PRINT_SETTINGS_FINISHINGS'.
-- 
-- /Since: 2.10/
printSettingsSetFinishings ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintSettings a) =>
    a
    -- ^ /@settings@/: a t'GI.Gtk.Objects.PrintSettings.PrintSettings'
    -> T.Text
    -- ^ /@finishings@/: the finishings
    -> m ()
printSettingsSetFinishings settings finishings = liftIO $ do
    settings' <- unsafeManagedPtrCastPtr settings
    finishings' <- textToCString finishings
    gtk_print_settings_set_finishings settings' finishings'
    touchManagedPtr settings
    freeMem finishings'
    return ()

#if defined(ENABLE_OVERLOADING)
data PrintSettingsSetFinishingsMethodInfo
instance (signature ~ (T.Text -> m ()), MonadIO m, IsPrintSettings a) => O.OverloadedMethod PrintSettingsSetFinishingsMethodInfo a signature where
    overloadedMethod = printSettingsSetFinishings

instance O.OverloadedMethodInfo PrintSettingsSetFinishingsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintSettings.printSettingsSetFinishings",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintSettings.html#v:printSettingsSetFinishings"
        })


#endif

-- method PrintSettings::set_int
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "settings"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintSettings" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintSettings"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "key"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a key" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "value"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "an integer" , sinceVersion = Nothing }
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

foreign import ccall "gtk_print_settings_set_int" gtk_print_settings_set_int :: 
    Ptr PrintSettings ->                    -- settings : TInterface (Name {namespace = "Gtk", name = "PrintSettings"})
    CString ->                              -- key : TBasicType TUTF8
    Int32 ->                                -- value : TBasicType TInt
    IO ()

-- | Sets /@key@/ to an integer value.
-- 
-- /Since: 2.10/
printSettingsSetInt ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintSettings a) =>
    a
    -- ^ /@settings@/: a t'GI.Gtk.Objects.PrintSettings.PrintSettings'
    -> T.Text
    -- ^ /@key@/: a key
    -> Int32
    -- ^ /@value@/: an integer
    -> m ()
printSettingsSetInt settings key value = liftIO $ do
    settings' <- unsafeManagedPtrCastPtr settings
    key' <- textToCString key
    gtk_print_settings_set_int settings' key' value
    touchManagedPtr settings
    freeMem key'
    return ()

#if defined(ENABLE_OVERLOADING)
data PrintSettingsSetIntMethodInfo
instance (signature ~ (T.Text -> Int32 -> m ()), MonadIO m, IsPrintSettings a) => O.OverloadedMethod PrintSettingsSetIntMethodInfo a signature where
    overloadedMethod = printSettingsSetInt

instance O.OverloadedMethodInfo PrintSettingsSetIntMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintSettings.printSettingsSetInt",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintSettings.html#v:printSettingsSetInt"
        })


#endif

-- method PrintSettings::set_length
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "settings"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintSettings" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintSettings"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "key"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a key" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "value"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a length" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "unit"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Unit" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the unit of @length"
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

foreign import ccall "gtk_print_settings_set_length" gtk_print_settings_set_length :: 
    Ptr PrintSettings ->                    -- settings : TInterface (Name {namespace = "Gtk", name = "PrintSettings"})
    CString ->                              -- key : TBasicType TUTF8
    CDouble ->                              -- value : TBasicType TDouble
    CUInt ->                                -- unit : TInterface (Name {namespace = "Gtk", name = "Unit"})
    IO ()

-- | Associates a length in units of /@unit@/ with /@key@/.
-- 
-- /Since: 2.10/
printSettingsSetLength ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintSettings a) =>
    a
    -- ^ /@settings@/: a t'GI.Gtk.Objects.PrintSettings.PrintSettings'
    -> T.Text
    -- ^ /@key@/: a key
    -> Double
    -- ^ /@value@/: a length
    -> Gtk.Enums.Unit
    -- ^ /@unit@/: the unit of /@length@/
    -> m ()
printSettingsSetLength settings key value unit = liftIO $ do
    settings' <- unsafeManagedPtrCastPtr settings
    key' <- textToCString key
    let value' = realToFrac value
    let unit' = (fromIntegral . fromEnum) unit
    gtk_print_settings_set_length settings' key' value' unit'
    touchManagedPtr settings
    freeMem key'
    return ()

#if defined(ENABLE_OVERLOADING)
data PrintSettingsSetLengthMethodInfo
instance (signature ~ (T.Text -> Double -> Gtk.Enums.Unit -> m ()), MonadIO m, IsPrintSettings a) => O.OverloadedMethod PrintSettingsSetLengthMethodInfo a signature where
    overloadedMethod = printSettingsSetLength

instance O.OverloadedMethodInfo PrintSettingsSetLengthMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintSettings.printSettingsSetLength",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintSettings.html#v:printSettingsSetLength"
        })


#endif

-- method PrintSettings::set_media_type
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "settings"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintSettings" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintSettings"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "media_type"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the media type" , sinceVersion = Nothing }
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

foreign import ccall "gtk_print_settings_set_media_type" gtk_print_settings_set_media_type :: 
    Ptr PrintSettings ->                    -- settings : TInterface (Name {namespace = "Gtk", name = "PrintSettings"})
    CString ->                              -- media_type : TBasicType TUTF8
    IO ()

-- | Sets the value of 'GI.Gtk.Constants.PRINT_SETTINGS_MEDIA_TYPE'.
-- 
-- The set of media types is defined in PWG 5101.1-2002 PWG.
-- 
-- /Since: 2.10/
printSettingsSetMediaType ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintSettings a) =>
    a
    -- ^ /@settings@/: a t'GI.Gtk.Objects.PrintSettings.PrintSettings'
    -> T.Text
    -- ^ /@mediaType@/: the media type
    -> m ()
printSettingsSetMediaType settings mediaType = liftIO $ do
    settings' <- unsafeManagedPtrCastPtr settings
    mediaType' <- textToCString mediaType
    gtk_print_settings_set_media_type settings' mediaType'
    touchManagedPtr settings
    freeMem mediaType'
    return ()

#if defined(ENABLE_OVERLOADING)
data PrintSettingsSetMediaTypeMethodInfo
instance (signature ~ (T.Text -> m ()), MonadIO m, IsPrintSettings a) => O.OverloadedMethod PrintSettingsSetMediaTypeMethodInfo a signature where
    overloadedMethod = printSettingsSetMediaType

instance O.OverloadedMethodInfo PrintSettingsSetMediaTypeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintSettings.printSettingsSetMediaType",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintSettings.html#v:printSettingsSetMediaType"
        })


#endif

-- method PrintSettings::set_n_copies
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "settings"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintSettings" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintSettings"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "num_copies"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the number of copies"
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

foreign import ccall "gtk_print_settings_set_n_copies" gtk_print_settings_set_n_copies :: 
    Ptr PrintSettings ->                    -- settings : TInterface (Name {namespace = "Gtk", name = "PrintSettings"})
    Int32 ->                                -- num_copies : TBasicType TInt
    IO ()

-- | Sets the value of 'GI.Gtk.Constants.PRINT_SETTINGS_N_COPIES'.
-- 
-- /Since: 2.10/
printSettingsSetNCopies ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintSettings a) =>
    a
    -- ^ /@settings@/: a t'GI.Gtk.Objects.PrintSettings.PrintSettings'
    -> Int32
    -- ^ /@numCopies@/: the number of copies
    -> m ()
printSettingsSetNCopies settings numCopies = liftIO $ do
    settings' <- unsafeManagedPtrCastPtr settings
    gtk_print_settings_set_n_copies settings' numCopies
    touchManagedPtr settings
    return ()

#if defined(ENABLE_OVERLOADING)
data PrintSettingsSetNCopiesMethodInfo
instance (signature ~ (Int32 -> m ()), MonadIO m, IsPrintSettings a) => O.OverloadedMethod PrintSettingsSetNCopiesMethodInfo a signature where
    overloadedMethod = printSettingsSetNCopies

instance O.OverloadedMethodInfo PrintSettingsSetNCopiesMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintSettings.printSettingsSetNCopies",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintSettings.html#v:printSettingsSetNCopies"
        })


#endif

-- method PrintSettings::set_number_up
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "settings"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintSettings" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintSettings"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "number_up"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the number of pages per sheet"
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

foreign import ccall "gtk_print_settings_set_number_up" gtk_print_settings_set_number_up :: 
    Ptr PrintSettings ->                    -- settings : TInterface (Name {namespace = "Gtk", name = "PrintSettings"})
    Int32 ->                                -- number_up : TBasicType TInt
    IO ()

-- | Sets the value of 'GI.Gtk.Constants.PRINT_SETTINGS_NUMBER_UP'.
-- 
-- /Since: 2.10/
printSettingsSetNumberUp ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintSettings a) =>
    a
    -- ^ /@settings@/: a t'GI.Gtk.Objects.PrintSettings.PrintSettings'
    -> Int32
    -- ^ /@numberUp@/: the number of pages per sheet
    -> m ()
printSettingsSetNumberUp settings numberUp = liftIO $ do
    settings' <- unsafeManagedPtrCastPtr settings
    gtk_print_settings_set_number_up settings' numberUp
    touchManagedPtr settings
    return ()

#if defined(ENABLE_OVERLOADING)
data PrintSettingsSetNumberUpMethodInfo
instance (signature ~ (Int32 -> m ()), MonadIO m, IsPrintSettings a) => O.OverloadedMethod PrintSettingsSetNumberUpMethodInfo a signature where
    overloadedMethod = printSettingsSetNumberUp

instance O.OverloadedMethodInfo PrintSettingsSetNumberUpMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintSettings.printSettingsSetNumberUp",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintSettings.html#v:printSettingsSetNumberUp"
        })


#endif

-- method PrintSettings::set_number_up_layout
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "settings"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintSettings" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintSettings"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "number_up_layout"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "NumberUpLayout" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkNumberUpLayout value"
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

foreign import ccall "gtk_print_settings_set_number_up_layout" gtk_print_settings_set_number_up_layout :: 
    Ptr PrintSettings ->                    -- settings : TInterface (Name {namespace = "Gtk", name = "PrintSettings"})
    CUInt ->                                -- number_up_layout : TInterface (Name {namespace = "Gtk", name = "NumberUpLayout"})
    IO ()

-- | Sets the value of 'GI.Gtk.Constants.PRINT_SETTINGS_NUMBER_UP_LAYOUT'.
-- 
-- /Since: 2.14/
printSettingsSetNumberUpLayout ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintSettings a) =>
    a
    -- ^ /@settings@/: a t'GI.Gtk.Objects.PrintSettings.PrintSettings'
    -> Gtk.Enums.NumberUpLayout
    -- ^ /@numberUpLayout@/: a t'GI.Gtk.Enums.NumberUpLayout' value
    -> m ()
printSettingsSetNumberUpLayout settings numberUpLayout = liftIO $ do
    settings' <- unsafeManagedPtrCastPtr settings
    let numberUpLayout' = (fromIntegral . fromEnum) numberUpLayout
    gtk_print_settings_set_number_up_layout settings' numberUpLayout'
    touchManagedPtr settings
    return ()

#if defined(ENABLE_OVERLOADING)
data PrintSettingsSetNumberUpLayoutMethodInfo
instance (signature ~ (Gtk.Enums.NumberUpLayout -> m ()), MonadIO m, IsPrintSettings a) => O.OverloadedMethod PrintSettingsSetNumberUpLayoutMethodInfo a signature where
    overloadedMethod = printSettingsSetNumberUpLayout

instance O.OverloadedMethodInfo PrintSettingsSetNumberUpLayoutMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintSettings.printSettingsSetNumberUpLayout",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintSettings.html#v:printSettingsSetNumberUpLayout"
        })


#endif

-- method PrintSettings::set_orientation
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "settings"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintSettings" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintSettings"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "orientation"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PageOrientation" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a page orientation" , sinceVersion = Nothing }
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

foreign import ccall "gtk_print_settings_set_orientation" gtk_print_settings_set_orientation :: 
    Ptr PrintSettings ->                    -- settings : TInterface (Name {namespace = "Gtk", name = "PrintSettings"})
    CUInt ->                                -- orientation : TInterface (Name {namespace = "Gtk", name = "PageOrientation"})
    IO ()

-- | Sets the value of 'GI.Gtk.Constants.PRINT_SETTINGS_ORIENTATION'.
-- 
-- /Since: 2.10/
printSettingsSetOrientation ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintSettings a) =>
    a
    -- ^ /@settings@/: a t'GI.Gtk.Objects.PrintSettings.PrintSettings'
    -> Gtk.Enums.PageOrientation
    -- ^ /@orientation@/: a page orientation
    -> m ()
printSettingsSetOrientation settings orientation = liftIO $ do
    settings' <- unsafeManagedPtrCastPtr settings
    let orientation' = (fromIntegral . fromEnum) orientation
    gtk_print_settings_set_orientation settings' orientation'
    touchManagedPtr settings
    return ()

#if defined(ENABLE_OVERLOADING)
data PrintSettingsSetOrientationMethodInfo
instance (signature ~ (Gtk.Enums.PageOrientation -> m ()), MonadIO m, IsPrintSettings a) => O.OverloadedMethod PrintSettingsSetOrientationMethodInfo a signature where
    overloadedMethod = printSettingsSetOrientation

instance O.OverloadedMethodInfo PrintSettingsSetOrientationMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintSettings.printSettingsSetOrientation",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintSettings.html#v:printSettingsSetOrientation"
        })


#endif

-- method PrintSettings::set_output_bin
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "settings"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintSettings" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintSettings"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "output_bin"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the output bin" , sinceVersion = Nothing }
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

foreign import ccall "gtk_print_settings_set_output_bin" gtk_print_settings_set_output_bin :: 
    Ptr PrintSettings ->                    -- settings : TInterface (Name {namespace = "Gtk", name = "PrintSettings"})
    CString ->                              -- output_bin : TBasicType TUTF8
    IO ()

-- | Sets the value of 'GI.Gtk.Constants.PRINT_SETTINGS_OUTPUT_BIN'.
-- 
-- /Since: 2.10/
printSettingsSetOutputBin ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintSettings a) =>
    a
    -- ^ /@settings@/: a t'GI.Gtk.Objects.PrintSettings.PrintSettings'
    -> T.Text
    -- ^ /@outputBin@/: the output bin
    -> m ()
printSettingsSetOutputBin settings outputBin = liftIO $ do
    settings' <- unsafeManagedPtrCastPtr settings
    outputBin' <- textToCString outputBin
    gtk_print_settings_set_output_bin settings' outputBin'
    touchManagedPtr settings
    freeMem outputBin'
    return ()

#if defined(ENABLE_OVERLOADING)
data PrintSettingsSetOutputBinMethodInfo
instance (signature ~ (T.Text -> m ()), MonadIO m, IsPrintSettings a) => O.OverloadedMethod PrintSettingsSetOutputBinMethodInfo a signature where
    overloadedMethod = printSettingsSetOutputBin

instance O.OverloadedMethodInfo PrintSettingsSetOutputBinMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintSettings.printSettingsSetOutputBin",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintSettings.html#v:printSettingsSetOutputBin"
        })


#endif

-- method PrintSettings::set_page_ranges
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "settings"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintSettings" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintSettings"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "page_ranges"
--           , argType =
--               TCArray
--                 False
--                 (-1)
--                 2
--                 (TInterface Name { namespace = "Gtk" , name = "PageRange" })
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "an array of #GtkPageRanges"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "num_ranges"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the length of @page_ranges"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: [ Arg
--              { argCName = "num_ranges"
--              , argType = TBasicType TInt
--              , direction = DirectionIn
--              , mayBeNull = False
--              , argDoc =
--                  Documentation
--                    { rawDocText = Just "the length of @page_ranges"
--                    , sinceVersion = Nothing
--                    }
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

foreign import ccall "gtk_print_settings_set_page_ranges" gtk_print_settings_set_page_ranges :: 
    Ptr PrintSettings ->                    -- settings : TInterface (Name {namespace = "Gtk", name = "PrintSettings"})
    Ptr Gtk.PageRange.PageRange ->          -- page_ranges : TCArray False (-1) 2 (TInterface (Name {namespace = "Gtk", name = "PageRange"}))
    Int32 ->                                -- num_ranges : TBasicType TInt
    IO ()

-- | Sets the value of 'GI.Gtk.Constants.PRINT_SETTINGS_PAGE_RANGES'.
-- 
-- /Since: 2.10/
printSettingsSetPageRanges ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintSettings a) =>
    a
    -- ^ /@settings@/: a t'GI.Gtk.Objects.PrintSettings.PrintSettings'
    -> [Gtk.PageRange.PageRange]
    -- ^ /@pageRanges@/: an array of @/GtkPageRanges/@
    -> m ()
printSettingsSetPageRanges settings pageRanges = liftIO $ do
    let numRanges = fromIntegral $ P.length pageRanges
    settings' <- unsafeManagedPtrCastPtr settings
    pageRanges' <- mapM unsafeManagedPtrGetPtr pageRanges
    pageRanges'' <- packBlockArray 8 pageRanges'
    gtk_print_settings_set_page_ranges settings' pageRanges'' numRanges
    touchManagedPtr settings
    mapM_ touchManagedPtr pageRanges
    freeMem pageRanges''
    return ()

#if defined(ENABLE_OVERLOADING)
data PrintSettingsSetPageRangesMethodInfo
instance (signature ~ ([Gtk.PageRange.PageRange] -> m ()), MonadIO m, IsPrintSettings a) => O.OverloadedMethod PrintSettingsSetPageRangesMethodInfo a signature where
    overloadedMethod = printSettingsSetPageRanges

instance O.OverloadedMethodInfo PrintSettingsSetPageRangesMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintSettings.printSettingsSetPageRanges",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintSettings.html#v:printSettingsSetPageRanges"
        })


#endif

-- method PrintSettings::set_page_set
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "settings"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintSettings" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintSettings"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "page_set"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PageSet" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPageSet value"
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

foreign import ccall "gtk_print_settings_set_page_set" gtk_print_settings_set_page_set :: 
    Ptr PrintSettings ->                    -- settings : TInterface (Name {namespace = "Gtk", name = "PrintSettings"})
    CUInt ->                                -- page_set : TInterface (Name {namespace = "Gtk", name = "PageSet"})
    IO ()

-- | Sets the value of 'GI.Gtk.Constants.PRINT_SETTINGS_PAGE_SET'.
-- 
-- /Since: 2.10/
printSettingsSetPageSet ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintSettings a) =>
    a
    -- ^ /@settings@/: a t'GI.Gtk.Objects.PrintSettings.PrintSettings'
    -> Gtk.Enums.PageSet
    -- ^ /@pageSet@/: a t'GI.Gtk.Enums.PageSet' value
    -> m ()
printSettingsSetPageSet settings pageSet = liftIO $ do
    settings' <- unsafeManagedPtrCastPtr settings
    let pageSet' = (fromIntegral . fromEnum) pageSet
    gtk_print_settings_set_page_set settings' pageSet'
    touchManagedPtr settings
    return ()

#if defined(ENABLE_OVERLOADING)
data PrintSettingsSetPageSetMethodInfo
instance (signature ~ (Gtk.Enums.PageSet -> m ()), MonadIO m, IsPrintSettings a) => O.OverloadedMethod PrintSettingsSetPageSetMethodInfo a signature where
    overloadedMethod = printSettingsSetPageSet

instance O.OverloadedMethodInfo PrintSettingsSetPageSetMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintSettings.printSettingsSetPageSet",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintSettings.html#v:printSettingsSetPageSet"
        })


#endif

-- method PrintSettings::set_paper_height
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "settings"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintSettings" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintSettings"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "height"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the paper height" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "unit"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Unit" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the units of @height"
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

foreign import ccall "gtk_print_settings_set_paper_height" gtk_print_settings_set_paper_height :: 
    Ptr PrintSettings ->                    -- settings : TInterface (Name {namespace = "Gtk", name = "PrintSettings"})
    CDouble ->                              -- height : TBasicType TDouble
    CUInt ->                                -- unit : TInterface (Name {namespace = "Gtk", name = "Unit"})
    IO ()

-- | Sets the value of 'GI.Gtk.Constants.PRINT_SETTINGS_PAPER_HEIGHT'.
-- 
-- /Since: 2.10/
printSettingsSetPaperHeight ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintSettings a) =>
    a
    -- ^ /@settings@/: a t'GI.Gtk.Objects.PrintSettings.PrintSettings'
    -> Double
    -- ^ /@height@/: the paper height
    -> Gtk.Enums.Unit
    -- ^ /@unit@/: the units of /@height@/
    -> m ()
printSettingsSetPaperHeight settings height unit = liftIO $ do
    settings' <- unsafeManagedPtrCastPtr settings
    let height' = realToFrac height
    let unit' = (fromIntegral . fromEnum) unit
    gtk_print_settings_set_paper_height settings' height' unit'
    touchManagedPtr settings
    return ()

#if defined(ENABLE_OVERLOADING)
data PrintSettingsSetPaperHeightMethodInfo
instance (signature ~ (Double -> Gtk.Enums.Unit -> m ()), MonadIO m, IsPrintSettings a) => O.OverloadedMethod PrintSettingsSetPaperHeightMethodInfo a signature where
    overloadedMethod = printSettingsSetPaperHeight

instance O.OverloadedMethodInfo PrintSettingsSetPaperHeightMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintSettings.printSettingsSetPaperHeight",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintSettings.html#v:printSettingsSetPaperHeight"
        })


#endif

-- method PrintSettings::set_paper_size
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "settings"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintSettings" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintSettings"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "paper_size"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PaperSize" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a paper size" , sinceVersion = Nothing }
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

foreign import ccall "gtk_print_settings_set_paper_size" gtk_print_settings_set_paper_size :: 
    Ptr PrintSettings ->                    -- settings : TInterface (Name {namespace = "Gtk", name = "PrintSettings"})
    Ptr Gtk.PaperSize.PaperSize ->          -- paper_size : TInterface (Name {namespace = "Gtk", name = "PaperSize"})
    IO ()

-- | Sets the value of 'GI.Gtk.Constants.PRINT_SETTINGS_PAPER_FORMAT',
-- 'GI.Gtk.Constants.PRINT_SETTINGS_PAPER_WIDTH' and
-- 'GI.Gtk.Constants.PRINT_SETTINGS_PAPER_HEIGHT'.
-- 
-- /Since: 2.10/
printSettingsSetPaperSize ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintSettings a) =>
    a
    -- ^ /@settings@/: a t'GI.Gtk.Objects.PrintSettings.PrintSettings'
    -> Gtk.PaperSize.PaperSize
    -- ^ /@paperSize@/: a paper size
    -> m ()
printSettingsSetPaperSize settings paperSize = liftIO $ do
    settings' <- unsafeManagedPtrCastPtr settings
    paperSize' <- unsafeManagedPtrGetPtr paperSize
    gtk_print_settings_set_paper_size settings' paperSize'
    touchManagedPtr settings
    touchManagedPtr paperSize
    return ()

#if defined(ENABLE_OVERLOADING)
data PrintSettingsSetPaperSizeMethodInfo
instance (signature ~ (Gtk.PaperSize.PaperSize -> m ()), MonadIO m, IsPrintSettings a) => O.OverloadedMethod PrintSettingsSetPaperSizeMethodInfo a signature where
    overloadedMethod = printSettingsSetPaperSize

instance O.OverloadedMethodInfo PrintSettingsSetPaperSizeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintSettings.printSettingsSetPaperSize",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintSettings.html#v:printSettingsSetPaperSize"
        })


#endif

-- method PrintSettings::set_paper_width
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "settings"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintSettings" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintSettings"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "width"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the paper width" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "unit"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Unit" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the units of @width"
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

foreign import ccall "gtk_print_settings_set_paper_width" gtk_print_settings_set_paper_width :: 
    Ptr PrintSettings ->                    -- settings : TInterface (Name {namespace = "Gtk", name = "PrintSettings"})
    CDouble ->                              -- width : TBasicType TDouble
    CUInt ->                                -- unit : TInterface (Name {namespace = "Gtk", name = "Unit"})
    IO ()

-- | Sets the value of 'GI.Gtk.Constants.PRINT_SETTINGS_PAPER_WIDTH'.
-- 
-- /Since: 2.10/
printSettingsSetPaperWidth ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintSettings a) =>
    a
    -- ^ /@settings@/: a t'GI.Gtk.Objects.PrintSettings.PrintSettings'
    -> Double
    -- ^ /@width@/: the paper width
    -> Gtk.Enums.Unit
    -- ^ /@unit@/: the units of /@width@/
    -> m ()
printSettingsSetPaperWidth settings width unit = liftIO $ do
    settings' <- unsafeManagedPtrCastPtr settings
    let width' = realToFrac width
    let unit' = (fromIntegral . fromEnum) unit
    gtk_print_settings_set_paper_width settings' width' unit'
    touchManagedPtr settings
    return ()

#if defined(ENABLE_OVERLOADING)
data PrintSettingsSetPaperWidthMethodInfo
instance (signature ~ (Double -> Gtk.Enums.Unit -> m ()), MonadIO m, IsPrintSettings a) => O.OverloadedMethod PrintSettingsSetPaperWidthMethodInfo a signature where
    overloadedMethod = printSettingsSetPaperWidth

instance O.OverloadedMethodInfo PrintSettingsSetPaperWidthMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintSettings.printSettingsSetPaperWidth",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintSettings.html#v:printSettingsSetPaperWidth"
        })


#endif

-- method PrintSettings::set_print_pages
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "settings"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintSettings" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintSettings"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "pages"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintPages" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintPages value"
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

foreign import ccall "gtk_print_settings_set_print_pages" gtk_print_settings_set_print_pages :: 
    Ptr PrintSettings ->                    -- settings : TInterface (Name {namespace = "Gtk", name = "PrintSettings"})
    CUInt ->                                -- pages : TInterface (Name {namespace = "Gtk", name = "PrintPages"})
    IO ()

-- | Sets the value of 'GI.Gtk.Constants.PRINT_SETTINGS_PRINT_PAGES'.
-- 
-- /Since: 2.10/
printSettingsSetPrintPages ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintSettings a) =>
    a
    -- ^ /@settings@/: a t'GI.Gtk.Objects.PrintSettings.PrintSettings'
    -> Gtk.Enums.PrintPages
    -- ^ /@pages@/: a t'GI.Gtk.Enums.PrintPages' value
    -> m ()
printSettingsSetPrintPages settings pages = liftIO $ do
    settings' <- unsafeManagedPtrCastPtr settings
    let pages' = (fromIntegral . fromEnum) pages
    gtk_print_settings_set_print_pages settings' pages'
    touchManagedPtr settings
    return ()

#if defined(ENABLE_OVERLOADING)
data PrintSettingsSetPrintPagesMethodInfo
instance (signature ~ (Gtk.Enums.PrintPages -> m ()), MonadIO m, IsPrintSettings a) => O.OverloadedMethod PrintSettingsSetPrintPagesMethodInfo a signature where
    overloadedMethod = printSettingsSetPrintPages

instance O.OverloadedMethodInfo PrintSettingsSetPrintPagesMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintSettings.printSettingsSetPrintPages",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintSettings.html#v:printSettingsSetPrintPages"
        })


#endif

-- method PrintSettings::set_printer
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "settings"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintSettings" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintSettings"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "printer"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the printer name" , sinceVersion = Nothing }
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

foreign import ccall "gtk_print_settings_set_printer" gtk_print_settings_set_printer :: 
    Ptr PrintSettings ->                    -- settings : TInterface (Name {namespace = "Gtk", name = "PrintSettings"})
    CString ->                              -- printer : TBasicType TUTF8
    IO ()

-- | Convenience function to set 'GI.Gtk.Constants.PRINT_SETTINGS_PRINTER'
-- to /@printer@/.
-- 
-- /Since: 2.10/
printSettingsSetPrinter ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintSettings a) =>
    a
    -- ^ /@settings@/: a t'GI.Gtk.Objects.PrintSettings.PrintSettings'
    -> T.Text
    -- ^ /@printer@/: the printer name
    -> m ()
printSettingsSetPrinter settings printer = liftIO $ do
    settings' <- unsafeManagedPtrCastPtr settings
    printer' <- textToCString printer
    gtk_print_settings_set_printer settings' printer'
    touchManagedPtr settings
    freeMem printer'
    return ()

#if defined(ENABLE_OVERLOADING)
data PrintSettingsSetPrinterMethodInfo
instance (signature ~ (T.Text -> m ()), MonadIO m, IsPrintSettings a) => O.OverloadedMethod PrintSettingsSetPrinterMethodInfo a signature where
    overloadedMethod = printSettingsSetPrinter

instance O.OverloadedMethodInfo PrintSettingsSetPrinterMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintSettings.printSettingsSetPrinter",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintSettings.html#v:printSettingsSetPrinter"
        })


#endif

-- method PrintSettings::set_printer_lpi
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "settings"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintSettings" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintSettings"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "lpi"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the resolution in lpi (lines per inch)"
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

foreign import ccall "gtk_print_settings_set_printer_lpi" gtk_print_settings_set_printer_lpi :: 
    Ptr PrintSettings ->                    -- settings : TInterface (Name {namespace = "Gtk", name = "PrintSettings"})
    CDouble ->                              -- lpi : TBasicType TDouble
    IO ()

-- | Sets the value of 'GI.Gtk.Constants.PRINT_SETTINGS_PRINTER_LPI'.
-- 
-- /Since: 2.16/
printSettingsSetPrinterLpi ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintSettings a) =>
    a
    -- ^ /@settings@/: a t'GI.Gtk.Objects.PrintSettings.PrintSettings'
    -> Double
    -- ^ /@lpi@/: the resolution in lpi (lines per inch)
    -> m ()
printSettingsSetPrinterLpi settings lpi = liftIO $ do
    settings' <- unsafeManagedPtrCastPtr settings
    let lpi' = realToFrac lpi
    gtk_print_settings_set_printer_lpi settings' lpi'
    touchManagedPtr settings
    return ()

#if defined(ENABLE_OVERLOADING)
data PrintSettingsSetPrinterLpiMethodInfo
instance (signature ~ (Double -> m ()), MonadIO m, IsPrintSettings a) => O.OverloadedMethod PrintSettingsSetPrinterLpiMethodInfo a signature where
    overloadedMethod = printSettingsSetPrinterLpi

instance O.OverloadedMethodInfo PrintSettingsSetPrinterLpiMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintSettings.printSettingsSetPrinterLpi",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintSettings.html#v:printSettingsSetPrinterLpi"
        })


#endif

-- method PrintSettings::set_quality
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "settings"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintSettings" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintSettings"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "quality"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintQuality" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintQuality value"
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

foreign import ccall "gtk_print_settings_set_quality" gtk_print_settings_set_quality :: 
    Ptr PrintSettings ->                    -- settings : TInterface (Name {namespace = "Gtk", name = "PrintSettings"})
    CUInt ->                                -- quality : TInterface (Name {namespace = "Gtk", name = "PrintQuality"})
    IO ()

-- | Sets the value of 'GI.Gtk.Constants.PRINT_SETTINGS_QUALITY'.
-- 
-- /Since: 2.10/
printSettingsSetQuality ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintSettings a) =>
    a
    -- ^ /@settings@/: a t'GI.Gtk.Objects.PrintSettings.PrintSettings'
    -> Gtk.Enums.PrintQuality
    -- ^ /@quality@/: a t'GI.Gtk.Enums.PrintQuality' value
    -> m ()
printSettingsSetQuality settings quality = liftIO $ do
    settings' <- unsafeManagedPtrCastPtr settings
    let quality' = (fromIntegral . fromEnum) quality
    gtk_print_settings_set_quality settings' quality'
    touchManagedPtr settings
    return ()

#if defined(ENABLE_OVERLOADING)
data PrintSettingsSetQualityMethodInfo
instance (signature ~ (Gtk.Enums.PrintQuality -> m ()), MonadIO m, IsPrintSettings a) => O.OverloadedMethod PrintSettingsSetQualityMethodInfo a signature where
    overloadedMethod = printSettingsSetQuality

instance O.OverloadedMethodInfo PrintSettingsSetQualityMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintSettings.printSettingsSetQuality",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintSettings.html#v:printSettingsSetQuality"
        })


#endif

-- method PrintSettings::set_resolution
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "settings"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintSettings" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintSettings"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "resolution"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the resolution in dpi"
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

foreign import ccall "gtk_print_settings_set_resolution" gtk_print_settings_set_resolution :: 
    Ptr PrintSettings ->                    -- settings : TInterface (Name {namespace = "Gtk", name = "PrintSettings"})
    Int32 ->                                -- resolution : TBasicType TInt
    IO ()

-- | Sets the values of 'GI.Gtk.Constants.PRINT_SETTINGS_RESOLUTION',
-- 'GI.Gtk.Constants.PRINT_SETTINGS_RESOLUTION_X' and
-- 'GI.Gtk.Constants.PRINT_SETTINGS_RESOLUTION_Y'.
-- 
-- /Since: 2.10/
printSettingsSetResolution ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintSettings a) =>
    a
    -- ^ /@settings@/: a t'GI.Gtk.Objects.PrintSettings.PrintSettings'
    -> Int32
    -- ^ /@resolution@/: the resolution in dpi
    -> m ()
printSettingsSetResolution settings resolution = liftIO $ do
    settings' <- unsafeManagedPtrCastPtr settings
    gtk_print_settings_set_resolution settings' resolution
    touchManagedPtr settings
    return ()

#if defined(ENABLE_OVERLOADING)
data PrintSettingsSetResolutionMethodInfo
instance (signature ~ (Int32 -> m ()), MonadIO m, IsPrintSettings a) => O.OverloadedMethod PrintSettingsSetResolutionMethodInfo a signature where
    overloadedMethod = printSettingsSetResolution

instance O.OverloadedMethodInfo PrintSettingsSetResolutionMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintSettings.printSettingsSetResolution",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintSettings.html#v:printSettingsSetResolution"
        })


#endif

-- method PrintSettings::set_resolution_xy
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "settings"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintSettings" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintSettings"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "resolution_x"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the horizontal resolution in dpi"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "resolution_y"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the vertical resolution in dpi"
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

foreign import ccall "gtk_print_settings_set_resolution_xy" gtk_print_settings_set_resolution_xy :: 
    Ptr PrintSettings ->                    -- settings : TInterface (Name {namespace = "Gtk", name = "PrintSettings"})
    Int32 ->                                -- resolution_x : TBasicType TInt
    Int32 ->                                -- resolution_y : TBasicType TInt
    IO ()

-- | Sets the values of 'GI.Gtk.Constants.PRINT_SETTINGS_RESOLUTION',
-- 'GI.Gtk.Constants.PRINT_SETTINGS_RESOLUTION_X' and
-- 'GI.Gtk.Constants.PRINT_SETTINGS_RESOLUTION_Y'.
-- 
-- /Since: 2.16/
printSettingsSetResolutionXy ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintSettings a) =>
    a
    -- ^ /@settings@/: a t'GI.Gtk.Objects.PrintSettings.PrintSettings'
    -> Int32
    -- ^ /@resolutionX@/: the horizontal resolution in dpi
    -> Int32
    -- ^ /@resolutionY@/: the vertical resolution in dpi
    -> m ()
printSettingsSetResolutionXy settings resolutionX resolutionY = liftIO $ do
    settings' <- unsafeManagedPtrCastPtr settings
    gtk_print_settings_set_resolution_xy settings' resolutionX resolutionY
    touchManagedPtr settings
    return ()

#if defined(ENABLE_OVERLOADING)
data PrintSettingsSetResolutionXyMethodInfo
instance (signature ~ (Int32 -> Int32 -> m ()), MonadIO m, IsPrintSettings a) => O.OverloadedMethod PrintSettingsSetResolutionXyMethodInfo a signature where
    overloadedMethod = printSettingsSetResolutionXy

instance O.OverloadedMethodInfo PrintSettingsSetResolutionXyMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintSettings.printSettingsSetResolutionXy",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintSettings.html#v:printSettingsSetResolutionXy"
        })


#endif

-- method PrintSettings::set_reverse
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "settings"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintSettings" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintSettings"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "reverse"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "whether to reverse the output"
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

foreign import ccall "gtk_print_settings_set_reverse" gtk_print_settings_set_reverse :: 
    Ptr PrintSettings ->                    -- settings : TInterface (Name {namespace = "Gtk", name = "PrintSettings"})
    CInt ->                                 -- reverse : TBasicType TBoolean
    IO ()

-- | Sets the value of 'GI.Gtk.Constants.PRINT_SETTINGS_REVERSE'.
-- 
-- /Since: 2.10/
printSettingsSetReverse ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintSettings a) =>
    a
    -- ^ /@settings@/: a t'GI.Gtk.Objects.PrintSettings.PrintSettings'
    -> Bool
    -- ^ /@reverse@/: whether to reverse the output
    -> m ()
printSettingsSetReverse settings reverse = liftIO $ do
    settings' <- unsafeManagedPtrCastPtr settings
    let reverse' = (fromIntegral . fromEnum) reverse
    gtk_print_settings_set_reverse settings' reverse'
    touchManagedPtr settings
    return ()

#if defined(ENABLE_OVERLOADING)
data PrintSettingsSetReverseMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsPrintSettings a) => O.OverloadedMethod PrintSettingsSetReverseMethodInfo a signature where
    overloadedMethod = printSettingsSetReverse

instance O.OverloadedMethodInfo PrintSettingsSetReverseMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintSettings.printSettingsSetReverse",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintSettings.html#v:printSettingsSetReverse"
        })


#endif

-- method PrintSettings::set_scale
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "settings"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintSettings" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintSettings"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "scale"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the scale in percent"
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

foreign import ccall "gtk_print_settings_set_scale" gtk_print_settings_set_scale :: 
    Ptr PrintSettings ->                    -- settings : TInterface (Name {namespace = "Gtk", name = "PrintSettings"})
    CDouble ->                              -- scale : TBasicType TDouble
    IO ()

-- | Sets the value of 'GI.Gtk.Constants.PRINT_SETTINGS_SCALE'.
-- 
-- /Since: 2.10/
printSettingsSetScale ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintSettings a) =>
    a
    -- ^ /@settings@/: a t'GI.Gtk.Objects.PrintSettings.PrintSettings'
    -> Double
    -- ^ /@scale@/: the scale in percent
    -> m ()
printSettingsSetScale settings scale = liftIO $ do
    settings' <- unsafeManagedPtrCastPtr settings
    let scale' = realToFrac scale
    gtk_print_settings_set_scale settings' scale'
    touchManagedPtr settings
    return ()

#if defined(ENABLE_OVERLOADING)
data PrintSettingsSetScaleMethodInfo
instance (signature ~ (Double -> m ()), MonadIO m, IsPrintSettings a) => O.OverloadedMethod PrintSettingsSetScaleMethodInfo a signature where
    overloadedMethod = printSettingsSetScale

instance O.OverloadedMethodInfo PrintSettingsSetScaleMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintSettings.printSettingsSetScale",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintSettings.html#v:printSettingsSetScale"
        })


#endif

-- method PrintSettings::set_use_color
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "settings"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintSettings" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintSettings"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "use_color"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "whether to use color"
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

foreign import ccall "gtk_print_settings_set_use_color" gtk_print_settings_set_use_color :: 
    Ptr PrintSettings ->                    -- settings : TInterface (Name {namespace = "Gtk", name = "PrintSettings"})
    CInt ->                                 -- use_color : TBasicType TBoolean
    IO ()

-- | Sets the value of 'GI.Gtk.Constants.PRINT_SETTINGS_USE_COLOR'.
-- 
-- /Since: 2.10/
printSettingsSetUseColor ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintSettings a) =>
    a
    -- ^ /@settings@/: a t'GI.Gtk.Objects.PrintSettings.PrintSettings'
    -> Bool
    -- ^ /@useColor@/: whether to use color
    -> m ()
printSettingsSetUseColor settings useColor = liftIO $ do
    settings' <- unsafeManagedPtrCastPtr settings
    let useColor' = (fromIntegral . fromEnum) useColor
    gtk_print_settings_set_use_color settings' useColor'
    touchManagedPtr settings
    return ()

#if defined(ENABLE_OVERLOADING)
data PrintSettingsSetUseColorMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsPrintSettings a) => O.OverloadedMethod PrintSettingsSetUseColorMethodInfo a signature where
    overloadedMethod = printSettingsSetUseColor

instance O.OverloadedMethodInfo PrintSettingsSetUseColorMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintSettings.printSettingsSetUseColor",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintSettings.html#v:printSettingsSetUseColor"
        })


#endif

-- method PrintSettings::to_file
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "settings"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintSettings" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintSettings"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "file_name"
--           , argType = TBasicType TFileName
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the file to save to"
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
-- throws : True
-- Skip return : False

foreign import ccall "gtk_print_settings_to_file" gtk_print_settings_to_file :: 
    Ptr PrintSettings ->                    -- settings : TInterface (Name {namespace = "Gtk", name = "PrintSettings"})
    CString ->                              -- file_name : TBasicType TFileName
    Ptr (Ptr GError) ->                     -- error
    IO CInt

-- | This function saves the print settings from /@settings@/ to /@fileName@/. If the
-- file could not be loaded then error is set to either a t'GI.GLib.Enums.FileError' or
-- t'GI.GLib.Enums.KeyFileError'.
-- 
-- /Since: 2.12/
printSettingsToFile ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintSettings a) =>
    a
    -- ^ /@settings@/: a t'GI.Gtk.Objects.PrintSettings.PrintSettings'
    -> [Char]
    -- ^ /@fileName@/: the file to save to
    -> m ()
    -- ^ /(Can throw 'Data.GI.Base.GError.GError')/
printSettingsToFile settings fileName = liftIO $ do
    settings' <- unsafeManagedPtrCastPtr settings
    fileName' <- stringToCString fileName
    onException (do
        _ <- propagateGError $ gtk_print_settings_to_file settings' fileName'
        touchManagedPtr settings
        freeMem fileName'
        return ()
     ) (do
        freeMem fileName'
     )

#if defined(ENABLE_OVERLOADING)
data PrintSettingsToFileMethodInfo
instance (signature ~ ([Char] -> m ()), MonadIO m, IsPrintSettings a) => O.OverloadedMethod PrintSettingsToFileMethodInfo a signature where
    overloadedMethod = printSettingsToFile

instance O.OverloadedMethodInfo PrintSettingsToFileMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintSettings.printSettingsToFile",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintSettings.html#v:printSettingsToFile"
        })


#endif

-- method PrintSettings::to_gvariant
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "settings"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintSettings" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintSettings"
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
-- returnType: Just TVariant
-- throws : False
-- Skip return : False

foreign import ccall "gtk_print_settings_to_gvariant" gtk_print_settings_to_gvariant :: 
    Ptr PrintSettings ->                    -- settings : TInterface (Name {namespace = "Gtk", name = "PrintSettings"})
    IO (Ptr GVariant)

-- | Serialize print settings to an a{sv} variant.
-- 
-- /Since: 3.22/
printSettingsToGvariant ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintSettings a) =>
    a
    -- ^ /@settings@/: a t'GI.Gtk.Objects.PrintSettings.PrintSettings'
    -> m GVariant
    -- ^ __Returns:__ a new, floating, t'GVariant'
printSettingsToGvariant settings = liftIO $ do
    settings' <- unsafeManagedPtrCastPtr settings
    result <- gtk_print_settings_to_gvariant settings'
    checkUnexpectedReturnNULL "printSettingsToGvariant" result
    result' <- B.GVariant.newGVariantFromPtr result
    touchManagedPtr settings
    return result'

#if defined(ENABLE_OVERLOADING)
data PrintSettingsToGvariantMethodInfo
instance (signature ~ (m GVariant), MonadIO m, IsPrintSettings a) => O.OverloadedMethod PrintSettingsToGvariantMethodInfo a signature where
    overloadedMethod = printSettingsToGvariant

instance O.OverloadedMethodInfo PrintSettingsToGvariantMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintSettings.printSettingsToGvariant",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintSettings.html#v:printSettingsToGvariant"
        })


#endif

-- method PrintSettings::to_key_file
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "settings"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintSettings" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintSettings"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "key_file"
--           , argType =
--               TInterface Name { namespace = "GLib" , name = "KeyFile" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the #GKeyFile to save the print settings to"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "group_name"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the group to add the settings to in @key_file, or\n    %NULL to use the default \8220Print Settings\8221"
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

foreign import ccall "gtk_print_settings_to_key_file" gtk_print_settings_to_key_file :: 
    Ptr PrintSettings ->                    -- settings : TInterface (Name {namespace = "Gtk", name = "PrintSettings"})
    Ptr GLib.KeyFile.KeyFile ->             -- key_file : TInterface (Name {namespace = "GLib", name = "KeyFile"})
    CString ->                              -- group_name : TBasicType TUTF8
    IO ()

-- | This function adds the print settings from /@settings@/ to /@keyFile@/.
-- 
-- /Since: 2.12/
printSettingsToKeyFile ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintSettings a) =>
    a
    -- ^ /@settings@/: a t'GI.Gtk.Objects.PrintSettings.PrintSettings'
    -> GLib.KeyFile.KeyFile
    -- ^ /@keyFile@/: the t'GI.GLib.Structs.KeyFile.KeyFile' to save the print settings to
    -> Maybe (T.Text)
    -- ^ /@groupName@/: the group to add the settings to in /@keyFile@/, or
    --     'P.Nothing' to use the default “Print Settings”
    -> m ()
printSettingsToKeyFile settings keyFile groupName = liftIO $ do
    settings' <- unsafeManagedPtrCastPtr settings
    keyFile' <- unsafeManagedPtrGetPtr keyFile
    maybeGroupName <- case groupName of
        Nothing -> return nullPtr
        Just jGroupName -> do
            jGroupName' <- textToCString jGroupName
            return jGroupName'
    gtk_print_settings_to_key_file settings' keyFile' maybeGroupName
    touchManagedPtr settings
    touchManagedPtr keyFile
    freeMem maybeGroupName
    return ()

#if defined(ENABLE_OVERLOADING)
data PrintSettingsToKeyFileMethodInfo
instance (signature ~ (GLib.KeyFile.KeyFile -> Maybe (T.Text) -> m ()), MonadIO m, IsPrintSettings a) => O.OverloadedMethod PrintSettingsToKeyFileMethodInfo a signature where
    overloadedMethod = printSettingsToKeyFile

instance O.OverloadedMethodInfo PrintSettingsToKeyFileMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintSettings.printSettingsToKeyFile",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintSettings.html#v:printSettingsToKeyFile"
        })


#endif

-- method PrintSettings::unset
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "settings"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintSettings" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintSettings"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "key"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a key" , sinceVersion = Nothing }
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

foreign import ccall "gtk_print_settings_unset" gtk_print_settings_unset :: 
    Ptr PrintSettings ->                    -- settings : TInterface (Name {namespace = "Gtk", name = "PrintSettings"})
    CString ->                              -- key : TBasicType TUTF8
    IO ()

-- | Removes any value associated with /@key@/.
-- This has the same effect as setting the value to 'P.Nothing'.
-- 
-- /Since: 2.10/
printSettingsUnset ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintSettings a) =>
    a
    -- ^ /@settings@/: a t'GI.Gtk.Objects.PrintSettings.PrintSettings'
    -> T.Text
    -- ^ /@key@/: a key
    -> m ()
printSettingsUnset settings key = liftIO $ do
    settings' <- unsafeManagedPtrCastPtr settings
    key' <- textToCString key
    gtk_print_settings_unset settings' key'
    touchManagedPtr settings
    freeMem key'
    return ()

#if defined(ENABLE_OVERLOADING)
data PrintSettingsUnsetMethodInfo
instance (signature ~ (T.Text -> m ()), MonadIO m, IsPrintSettings a) => O.OverloadedMethod PrintSettingsUnsetMethodInfo a signature where
    overloadedMethod = printSettingsUnset

instance O.OverloadedMethodInfo PrintSettingsUnsetMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintSettings.printSettingsUnset",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintSettings.html#v:printSettingsUnset"
        })


#endif


