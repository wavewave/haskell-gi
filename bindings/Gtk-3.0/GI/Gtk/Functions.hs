

-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Functions
    ( 

 -- * Methods


-- ** accelGroupsActivate #method:accelGroupsActivate#

    accelGroupsActivate                     ,


-- ** accelGroupsFromObject #method:accelGroupsFromObject#

    accelGroupsFromObject                   ,


-- ** acceleratorGetDefaultModMask #method:acceleratorGetDefaultModMask#

    acceleratorGetDefaultModMask            ,


-- ** acceleratorGetLabel #method:acceleratorGetLabel#

    acceleratorGetLabel                     ,


-- ** acceleratorGetLabelWithKeycode #method:acceleratorGetLabelWithKeycode#

    acceleratorGetLabelWithKeycode          ,


-- ** acceleratorName #method:acceleratorName#

    acceleratorName                         ,


-- ** acceleratorNameWithKeycode #method:acceleratorNameWithKeycode#

    acceleratorNameWithKeycode              ,


-- ** acceleratorParse #method:acceleratorParse#

    acceleratorParse                        ,


-- ** acceleratorParseWithKeycode #method:acceleratorParseWithKeycode#

    acceleratorParseWithKeycode             ,


-- ** acceleratorSetDefaultModMask #method:acceleratorSetDefaultModMask#

    acceleratorSetDefaultModMask            ,


-- ** acceleratorValid #method:acceleratorValid#

    acceleratorValid                        ,


-- ** alternativeDialogButtonOrder #method:alternativeDialogButtonOrder#

    alternativeDialogButtonOrder            ,


-- ** bindingsActivate #method:bindingsActivate#

    bindingsActivate                        ,


-- ** bindingsActivateEvent #method:bindingsActivateEvent#

    bindingsActivateEvent                   ,


-- ** cairoShouldDrawWindow #method:cairoShouldDrawWindow#

    cairoShouldDrawWindow                   ,


-- ** cairoTransformToWindow #method:cairoTransformToWindow#

    cairoTransformToWindow                  ,


-- ** checkVersion #method:checkVersion#

    checkVersion                            ,


-- ** deviceGrabAdd #method:deviceGrabAdd#

    deviceGrabAdd                           ,


-- ** deviceGrabRemove #method:deviceGrabRemove#

    deviceGrabRemove                        ,


-- ** disableSetlocale #method:disableSetlocale#

    disableSetlocale                        ,


-- ** distributeNaturalAllocation #method:distributeNaturalAllocation#

    distributeNaturalAllocation             ,


-- ** dragCancel #method:dragCancel#

    dragCancel                              ,


-- ** dragFinish #method:dragFinish#

    dragFinish                              ,


-- ** dragGetSourceWidget #method:dragGetSourceWidget#

    dragGetSourceWidget                     ,


-- ** dragSetIconDefault #method:dragSetIconDefault#

    dragSetIconDefault                      ,


-- ** dragSetIconName #method:dragSetIconName#

    dragSetIconName                         ,


-- ** dragSetIconPixbuf #method:dragSetIconPixbuf#

    dragSetIconPixbuf                       ,


-- ** dragSetIconStock #method:dragSetIconStock#

    dragSetIconStock                        ,


-- ** dragSetIconSurface #method:dragSetIconSurface#

    dragSetIconSurface                      ,


-- ** dragSetIconWidget #method:dragSetIconWidget#

    dragSetIconWidget                       ,


-- ** drawInsertionCursor #method:drawInsertionCursor#

    drawInsertionCursor                     ,


-- ** eventsPending #method:eventsPending#

    eventsPending                           ,


-- ** false #method:false#

    false                                   ,


-- ** getBinaryAge #method:getBinaryAge#

    getBinaryAge                            ,


-- ** getCurrentEvent #method:getCurrentEvent#

    getCurrentEvent                         ,


-- ** getCurrentEventDevice #method:getCurrentEventDevice#

    getCurrentEventDevice                   ,


-- ** getCurrentEventState #method:getCurrentEventState#

    getCurrentEventState                    ,


-- ** getCurrentEventTime #method:getCurrentEventTime#

    getCurrentEventTime                     ,


-- ** getDebugFlags #method:getDebugFlags#

    getDebugFlags                           ,


-- ** getDefaultLanguage #method:getDefaultLanguage#

    getDefaultLanguage                      ,


-- ** getEventWidget #method:getEventWidget#

    getEventWidget                          ,


-- ** getInterfaceAge #method:getInterfaceAge#

    getInterfaceAge                         ,


-- ** getLocaleDirection #method:getLocaleDirection#

    getLocaleDirection                      ,


-- ** getMajorVersion #method:getMajorVersion#

    getMajorVersion                         ,


-- ** getMicroVersion #method:getMicroVersion#

    getMicroVersion                         ,


-- ** getMinorVersion #method:getMinorVersion#

    getMinorVersion                         ,


-- ** getOptionGroup #method:getOptionGroup#

    getOptionGroup                          ,


-- ** grabGetCurrent #method:grabGetCurrent#

    grabGetCurrent                          ,


-- ** init #method:init#

    init                                    ,


-- ** initCheck #method:initCheck#

    initCheck                               ,


-- ** initWithArgs #method:initWithArgs#

    initWithArgs                            ,


-- ** keySnooperRemove #method:keySnooperRemove#

    keySnooperRemove                        ,


-- ** main #method:main#

    main                                    ,


-- ** mainDoEvent #method:mainDoEvent#

    mainDoEvent                             ,


-- ** mainIteration #method:mainIteration#

    mainIteration                           ,


-- ** mainIterationDo #method:mainIterationDo#

    mainIterationDo                         ,


-- ** mainLevel #method:mainLevel#

    mainLevel                               ,


-- ** mainQuit #method:mainQuit#

    mainQuit                                ,


-- ** paintArrow #method:paintArrow#

    paintArrow                              ,


-- ** paintBox #method:paintBox#

    paintBox                                ,


-- ** paintBoxGap #method:paintBoxGap#

    paintBoxGap                             ,


-- ** paintCheck #method:paintCheck#

    paintCheck                              ,


-- ** paintDiamond #method:paintDiamond#

    paintDiamond                            ,


-- ** paintExpander #method:paintExpander#

    paintExpander                           ,


-- ** paintExtension #method:paintExtension#

    paintExtension                          ,


-- ** paintFlatBox #method:paintFlatBox#

    paintFlatBox                            ,


-- ** paintFocus #method:paintFocus#

    paintFocus                              ,


-- ** paintHandle #method:paintHandle#

    paintHandle                             ,


-- ** paintHline #method:paintHline#

    paintHline                              ,


-- ** paintLayout #method:paintLayout#

    paintLayout                             ,


-- ** paintOption #method:paintOption#

    paintOption                             ,


-- ** paintResizeGrip #method:paintResizeGrip#

    paintResizeGrip                         ,


-- ** paintShadow #method:paintShadow#

    paintShadow                             ,


-- ** paintShadowGap #method:paintShadowGap#

    paintShadowGap                          ,


-- ** paintSlider #method:paintSlider#

    paintSlider                             ,


-- ** paintSpinner #method:paintSpinner#

    paintSpinner                            ,


-- ** paintTab #method:paintTab#

    paintTab                                ,


-- ** paintVline #method:paintVline#

    paintVline                              ,


-- ** parseArgs #method:parseArgs#

    parseArgs                               ,


-- ** printRunPageSetupDialog #method:printRunPageSetupDialog#

    printRunPageSetupDialog                 ,


-- ** printRunPageSetupDialogAsync #method:printRunPageSetupDialogAsync#

    printRunPageSetupDialogAsync            ,


-- ** propagateEvent #method:propagateEvent#

    propagateEvent                          ,


-- ** rcAddDefaultFile #method:rcAddDefaultFile#

    rcAddDefaultFile                        ,


-- ** rcFindModuleInPath #method:rcFindModuleInPath#

    rcFindModuleInPath                      ,


-- ** rcFindPixmapInPath #method:rcFindPixmapInPath#

    rcFindPixmapInPath                      ,


-- ** rcGetDefaultFiles #method:rcGetDefaultFiles#

    rcGetDefaultFiles                       ,


-- ** rcGetImModuleFile #method:rcGetImModuleFile#

    rcGetImModuleFile                       ,


-- ** rcGetImModulePath #method:rcGetImModulePath#

    rcGetImModulePath                       ,


-- ** rcGetModuleDir #method:rcGetModuleDir#

    rcGetModuleDir                          ,


-- ** rcGetStyle #method:rcGetStyle#

    rcGetStyle                              ,


-- ** rcGetStyleByPaths #method:rcGetStyleByPaths#

    rcGetStyleByPaths                       ,


-- ** rcGetThemeDir #method:rcGetThemeDir#

    rcGetThemeDir                           ,


-- ** rcParse #method:rcParse#

    rcParse                                 ,


-- ** rcParseColor #method:rcParseColor#

    rcParseColor                            ,


-- ** rcParseColorFull #method:rcParseColorFull#

    rcParseColorFull                        ,


-- ** rcParsePriority #method:rcParsePriority#

    rcParsePriority                         ,


-- ** rcParseState #method:rcParseState#

    rcParseState                            ,


-- ** rcParseString #method:rcParseString#

    rcParseString                           ,


-- ** rcReparseAll #method:rcReparseAll#

    rcReparseAll                            ,


-- ** rcReparseAllForSettings #method:rcReparseAllForSettings#

    rcReparseAllForSettings                 ,


-- ** rcResetStyles #method:rcResetStyles#

    rcResetStyles                           ,


-- ** rcSetDefaultFiles #method:rcSetDefaultFiles#

    rcSetDefaultFiles                       ,


-- ** renderActivity #method:renderActivity#

    renderActivity                          ,


-- ** renderArrow #method:renderArrow#

    renderArrow                             ,


-- ** renderBackground #method:renderBackground#

    renderBackground                        ,


-- ** renderBackgroundGetClip #method:renderBackgroundGetClip#

    renderBackgroundGetClip                 ,


-- ** renderCheck #method:renderCheck#

    renderCheck                             ,


-- ** renderExpander #method:renderExpander#

    renderExpander                          ,


-- ** renderExtension #method:renderExtension#

    renderExtension                         ,


-- ** renderFocus #method:renderFocus#

    renderFocus                             ,


-- ** renderFrame #method:renderFrame#

    renderFrame                             ,


-- ** renderFrameGap #method:renderFrameGap#

    renderFrameGap                          ,


-- ** renderHandle #method:renderHandle#

    renderHandle                            ,


-- ** renderIcon #method:renderIcon#

    renderIcon                              ,


-- ** renderIconPixbuf #method:renderIconPixbuf#

    renderIconPixbuf                        ,


-- ** renderIconSurface #method:renderIconSurface#

    renderIconSurface                       ,


-- ** renderInsertionCursor #method:renderInsertionCursor#

    renderInsertionCursor                   ,


-- ** renderLayout #method:renderLayout#

    renderLayout                            ,


-- ** renderLine #method:renderLine#

    renderLine                              ,


-- ** renderOption #method:renderOption#

    renderOption                            ,


-- ** renderSlider #method:renderSlider#

    renderSlider                            ,


-- ** rgbToHsv #method:rgbToHsv#

    rgbToHsv                                ,


-- ** selectionAddTarget #method:selectionAddTarget#

    selectionAddTarget                      ,


-- ** selectionAddTargets #method:selectionAddTargets#

    selectionAddTargets                     ,


-- ** selectionClearTargets #method:selectionClearTargets#

    selectionClearTargets                   ,


-- ** selectionConvert #method:selectionConvert#

    selectionConvert                        ,


-- ** selectionOwnerSet #method:selectionOwnerSet#

    selectionOwnerSet                       ,


-- ** selectionOwnerSetForDisplay #method:selectionOwnerSetForDisplay#

    selectionOwnerSetForDisplay             ,


-- ** selectionRemoveAll #method:selectionRemoveAll#

    selectionRemoveAll                      ,


-- ** setDebugFlags #method:setDebugFlags#

    setDebugFlags                           ,


-- ** showUri #method:showUri#

    showUri                                 ,


-- ** showUriOnWindow #method:showUriOnWindow#

    showUriOnWindow                         ,


-- ** stockAdd #method:stockAdd#

    stockAdd                                ,


-- ** stockAddStatic #method:stockAddStatic#

    stockAddStatic                          ,


-- ** stockListIds #method:stockListIds#

    stockListIds                            ,


-- ** stockLookup #method:stockLookup#

    stockLookup                             ,


-- ** stockSetTranslateFunc #method:stockSetTranslateFunc#

    stockSetTranslateFunc                   ,


-- ** targetTableFree #method:targetTableFree#

    targetTableFree                         ,


-- ** targetTableNewFromList #method:targetTableNewFromList#

    targetTableNewFromList                  ,


-- ** targetsIncludeImage #method:targetsIncludeImage#

    targetsIncludeImage                     ,


-- ** targetsIncludeRichText #method:targetsIncludeRichText#

    targetsIncludeRichText                  ,


-- ** targetsIncludeText #method:targetsIncludeText#

    targetsIncludeText                      ,


-- ** targetsIncludeUri #method:targetsIncludeUri#

    targetsIncludeUri                       ,


-- ** testCreateSimpleWindow #method:testCreateSimpleWindow#

    testCreateSimpleWindow                  ,


-- ** testFindLabel #method:testFindLabel#

    testFindLabel                           ,


-- ** testFindSibling #method:testFindSibling#

    testFindSibling                         ,


-- ** testFindWidget #method:testFindWidget#

    testFindWidget                          ,


-- ** testListAllTypes #method:testListAllTypes#

    testListAllTypes                        ,


-- ** testRegisterAllTypes #method:testRegisterAllTypes#

    testRegisterAllTypes                    ,


-- ** testSliderGetValue #method:testSliderGetValue#

    testSliderGetValue                      ,


-- ** testSliderSetPerc #method:testSliderSetPerc#

    testSliderSetPerc                       ,


-- ** testSpinButtonClick #method:testSpinButtonClick#

    testSpinButtonClick                     ,


-- ** testTextGet #method:testTextGet#

    testTextGet                             ,


-- ** testTextSet #method:testTextSet#

    testTextSet                             ,


-- ** testWidgetClick #method:testWidgetClick#

    testWidgetClick                         ,


-- ** testWidgetSendKey #method:testWidgetSendKey#

    testWidgetSendKey                       ,


-- ** testWidgetWaitForDraw #method:testWidgetWaitForDraw#

    testWidgetWaitForDraw                   ,


-- ** treeGetRowDragData #method:treeGetRowDragData#

    treeGetRowDragData                      ,


-- ** treeSetRowDragData #method:treeSetRowDragData#

    treeSetRowDragData                      ,


-- ** true #method:true#

    true                                    ,




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

import qualified GI.Cairo.Structs.Context as Cairo.Context
import qualified GI.Cairo.Structs.Surface as Cairo.Surface
import qualified GI.GLib.Callbacks as GLib.Callbacks
import qualified GI.GLib.Structs.OptionEntry as GLib.OptionEntry
import qualified GI.GLib.Structs.OptionGroup as GLib.OptionGroup
import qualified GI.GLib.Structs.Scanner as GLib.Scanner
import qualified GI.GObject.Objects.Object as GObject.Object
import qualified GI.Gdk.Enums as Gdk.Enums
import qualified GI.Gdk.Flags as Gdk.Flags
import qualified GI.Gdk.Objects.Device as Gdk.Device
import qualified GI.Gdk.Objects.Display as Gdk.Display
import qualified GI.Gdk.Objects.DragContext as Gdk.DragContext
import qualified GI.Gdk.Objects.Screen as Gdk.Screen
import qualified GI.Gdk.Objects.Window as Gdk.Window
import qualified GI.Gdk.Structs.Atom as Gdk.Atom
import qualified GI.Gdk.Structs.Color as Gdk.Color
import qualified GI.Gdk.Structs.EventKey as Gdk.EventKey
import qualified GI.Gdk.Structs.Rectangle as Gdk.Rectangle
import qualified GI.Gdk.Unions.Event as Gdk.Event
import qualified GI.GdkPixbuf.Objects.Pixbuf as GdkPixbuf.Pixbuf
import qualified GI.Gtk.Callbacks as Gtk.Callbacks
import {-# SOURCE #-} qualified GI.Gtk.Enums as Gtk.Enums
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.TreeModel as Gtk.TreeModel
import {-# SOURCE #-} qualified GI.Gtk.Objects.AccelGroup as Gtk.AccelGroup
import {-# SOURCE #-} qualified GI.Gtk.Objects.PageSetup as Gtk.PageSetup
import {-# SOURCE #-} qualified GI.Gtk.Objects.PrintSettings as Gtk.PrintSettings
import {-# SOURCE #-} qualified GI.Gtk.Objects.RcStyle as Gtk.RcStyle
import {-# SOURCE #-} qualified GI.Gtk.Objects.Settings as Gtk.Settings
import {-# SOURCE #-} qualified GI.Gtk.Objects.SpinButton as Gtk.SpinButton
import {-# SOURCE #-} qualified GI.Gtk.Objects.Style as Gtk.Style
import {-# SOURCE #-} qualified GI.Gtk.Objects.StyleContext as Gtk.StyleContext
import {-# SOURCE #-} qualified GI.Gtk.Objects.TextBuffer as Gtk.TextBuffer
import {-# SOURCE #-} qualified GI.Gtk.Objects.Widget as Gtk.Widget
import {-# SOURCE #-} qualified GI.Gtk.Objects.Window as Gtk.Window
import {-# SOURCE #-} qualified GI.Gtk.Structs.IconSource as Gtk.IconSource
import {-# SOURCE #-} qualified GI.Gtk.Structs.RequestedSize as Gtk.RequestedSize
import {-# SOURCE #-} qualified GI.Gtk.Structs.SelectionData as Gtk.SelectionData
import {-# SOURCE #-} qualified GI.Gtk.Structs.StockItem as Gtk.StockItem
import {-# SOURCE #-} qualified GI.Gtk.Structs.TargetEntry as Gtk.TargetEntry
import {-# SOURCE #-} qualified GI.Gtk.Structs.TargetList as Gtk.TargetList
import {-# SOURCE #-} qualified GI.Gtk.Structs.TreePath as Gtk.TreePath
import qualified GI.Pango.Enums as Pango.Enums
import qualified GI.Pango.Objects.Layout as Pango.Layout
import qualified GI.Pango.Structs.Language as Pango.Language

-- function true
-- Args: []
-- Lengths: []
-- returnType: Just (TBasicType TBoolean)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_true" gtk_true :: 
    IO CInt

-- | All this function does it to return 'P.True'.
-- 
-- This can be useful for example if you want to inhibit the deletion
-- of a window. Of course you should not do this as the user expects
-- a reaction from clicking the close icon of the window...
-- 
-- == A persistent window
-- 
-- 
-- === /C code/
-- >
-- >#include <gtk/gtk.h>
-- >
-- >int
-- >main (int argc, char **argv)
-- >{
-- >  GtkWidget *win, *but;
-- >  const char *text = "Close yourself. I mean it!";
-- >
-- >  gtk_init (&argc, &argv);
-- >
-- >  win = gtk_window_new (GTK_WINDOW_TOPLEVEL);
-- >  g_signal_connect (win,
-- >                    "delete-event",
-- >                    G_CALLBACK (gtk_true),
-- >                    NULL);
-- >  g_signal_connect (win, "destroy",
-- >                    G_CALLBACK (gtk_main_quit),
-- >                    NULL);
-- >
-- >  but = gtk_button_new_with_label (text);
-- >  g_signal_connect_swapped (but, "clicked",
-- >                            G_CALLBACK (gtk_object_destroy),
-- >                            win);
-- >  gtk_container_add (GTK_CONTAINER (win), but);
-- >
-- >  gtk_widget_show_all (win);
-- >
-- >  gtk_main ();
-- >
-- >  return 0;
-- >}
true ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m Bool
    -- ^ __Returns:__ 'P.True'
true  = liftIO $ do
    result <- gtk_true
    let result' = (/= 0) result
    return result'


-- function tree_set_row_drag_data
-- Args: [ Arg
--           { argCName = "selection_data"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "SelectionData" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "some #GtkSelectionData"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "tree_model"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeModel" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeModel" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "path"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreePath" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a row in @tree_model"
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

foreign import ccall "gtk_tree_set_row_drag_data" gtk_tree_set_row_drag_data :: 
    Ptr Gtk.SelectionData.SelectionData ->  -- selection_data : TInterface (Name {namespace = "Gtk", name = "SelectionData"})
    Ptr Gtk.TreeModel.TreeModel ->          -- tree_model : TInterface (Name {namespace = "Gtk", name = "TreeModel"})
    Ptr Gtk.TreePath.TreePath ->            -- path : TInterface (Name {namespace = "Gtk", name = "TreePath"})
    IO CInt

-- | Sets selection data of target type @/GTK_TREE_MODEL_ROW/@. Normally used
-- in a drag_data_get handler.
treeSetRowDragData ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.TreeModel.IsTreeModel a) =>
    Gtk.SelectionData.SelectionData
    -- ^ /@selectionData@/: some t'GI.Gtk.Structs.SelectionData.SelectionData'
    -> a
    -- ^ /@treeModel@/: a t'GI.Gtk.Interfaces.TreeModel.TreeModel'
    -> Gtk.TreePath.TreePath
    -- ^ /@path@/: a row in /@treeModel@/
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the t'GI.Gtk.Structs.SelectionData.SelectionData' had the proper target type to allow us to set a tree row
treeSetRowDragData selectionData treeModel path = liftIO $ do
    selectionData' <- unsafeManagedPtrGetPtr selectionData
    treeModel' <- unsafeManagedPtrCastPtr treeModel
    path' <- unsafeManagedPtrGetPtr path
    result <- gtk_tree_set_row_drag_data selectionData' treeModel' path'
    let result' = (/= 0) result
    touchManagedPtr selectionData
    touchManagedPtr treeModel
    touchManagedPtr path
    return result'


-- function tree_get_row_drag_data
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
--           { argCName = "tree_model"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeModel" }
--           , direction = DirectionOut
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeModel" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "path"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreePath" }
--           , direction = DirectionOut
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "row in @tree_model" , sinceVersion = Nothing }
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

foreign import ccall "gtk_tree_get_row_drag_data" gtk_tree_get_row_drag_data :: 
    Ptr Gtk.SelectionData.SelectionData ->  -- selection_data : TInterface (Name {namespace = "Gtk", name = "SelectionData"})
    Ptr (Ptr Gtk.TreeModel.TreeModel) ->    -- tree_model : TInterface (Name {namespace = "Gtk", name = "TreeModel"})
    Ptr (Ptr Gtk.TreePath.TreePath) ->      -- path : TInterface (Name {namespace = "Gtk", name = "TreePath"})
    IO CInt

-- | Obtains a /@treeModel@/ and /@path@/ from selection data of target type
-- @/GTK_TREE_MODEL_ROW/@. Normally called from a drag_data_received handler.
-- This function can only be used if /@selectionData@/ originates from the same
-- process that’s calling this function, because a pointer to the tree model
-- is being passed around. If you aren’t in the same process, then you\'ll
-- get memory corruption. In the t'GI.Gtk.Interfaces.TreeDragDest.TreeDragDest' drag_data_received handler,
-- you can assume that selection data of type @/GTK_TREE_MODEL_ROW/@ is
-- in from the current process. The returned path must be freed with
-- 'GI.Gtk.Structs.TreePath.treePathFree'.
treeGetRowDragData ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    Gtk.SelectionData.SelectionData
    -- ^ /@selectionData@/: a t'GI.Gtk.Structs.SelectionData.SelectionData'
    -> m ((Bool, Maybe Gtk.TreeModel.TreeModel, Maybe Gtk.TreePath.TreePath))
    -- ^ __Returns:__ 'P.True' if /@selectionData@/ had target type @/GTK_TREE_MODEL_ROW/@ and
    --  is otherwise valid
treeGetRowDragData selectionData = liftIO $ do
    selectionData' <- unsafeManagedPtrGetPtr selectionData
    treeModel <- callocMem :: IO (Ptr (Ptr Gtk.TreeModel.TreeModel))
    path <- callocMem :: IO (Ptr (Ptr Gtk.TreePath.TreePath))
    result <- gtk_tree_get_row_drag_data selectionData' treeModel path
    let result' = (/= 0) result
    treeModel' <- peek treeModel
    maybeTreeModel' <- convertIfNonNull treeModel' $ \treeModel'' -> do
        treeModel''' <- (newObject Gtk.TreeModel.TreeModel) treeModel''
        return treeModel'''
    path' <- peek path
    maybePath' <- convertIfNonNull path' $ \path'' -> do
        path''' <- (wrapBoxed Gtk.TreePath.TreePath) path''
        return path'''
    touchManagedPtr selectionData
    freeMem treeModel
    freeMem path
    return (result', maybeTreeModel', maybePath')


-- function test_widget_wait_for_draw
-- Args: [ Arg
--           { argCName = "widget"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Widget" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the widget to wait for"
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

foreign import ccall "gtk_test_widget_wait_for_draw" gtk_test_widget_wait_for_draw :: 
    Ptr Gtk.Widget.Widget ->                -- widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    IO ()

-- | Enters the main loop and waits for /@widget@/ to be “drawn”. In this
-- context that means it waits for the frame clock of /@widget@/ to have
-- run a full styling, layout and drawing cycle.
-- 
-- This function is intended to be used for syncing with actions that
-- depend on /@widget@/ relayouting or on interaction with the display
-- server.
-- 
-- /Since: 3.10/
testWidgetWaitForDraw ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.Widget.IsWidget a) =>
    a
    -- ^ /@widget@/: the widget to wait for
    -> m ()
testWidgetWaitForDraw widget = liftIO $ do
    widget' <- unsafeManagedPtrCastPtr widget
    gtk_test_widget_wait_for_draw widget'
    touchManagedPtr widget
    return ()


-- function test_widget_send_key
-- Args: [ Arg
--           { argCName = "widget"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Widget" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "Widget to generate a key press and release on."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "keyval"
--           , argType = TBasicType TUInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A Gdk keyboard value."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "modifiers"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "ModifierType" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Keyboard modifiers the event is setup with."
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

foreign import ccall "gtk_test_widget_send_key" gtk_test_widget_send_key :: 
    Ptr Gtk.Widget.Widget ->                -- widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    Word32 ->                               -- keyval : TBasicType TUInt
    CUInt ->                                -- modifiers : TInterface (Name {namespace = "Gdk", name = "ModifierType"})
    IO CInt

-- | This function will generate keyboard press and release events in
-- the middle of the first GdkWindow found that belongs to /@widget@/.
-- For windowless widgets like t'GI.Gtk.Objects.Button.Button' (which returns 'P.False' from
-- 'GI.Gtk.Objects.Widget.widgetGetHasWindow'), this will often be an
-- input-only event window. For other widgets, this is usually widget->window.
-- Certain caveats should be considered when using this function, in
-- particular because the mouse pointer is warped to the key press
-- location, see 'GI.Gdk.Functions.testSimulateKey' for details.
-- 
-- /Since: 2.14/
testWidgetSendKey ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.Widget.IsWidget a) =>
    a
    -- ^ /@widget@/: Widget to generate a key press and release on.
    -> Word32
    -- ^ /@keyval@/: A Gdk keyboard value.
    -> [Gdk.Flags.ModifierType]
    -- ^ /@modifiers@/: Keyboard modifiers the event is setup with.
    -> m Bool
    -- ^ __Returns:__ whether all actions neccessary for the key event simulation were carried out successfully.
testWidgetSendKey widget keyval modifiers = liftIO $ do
    widget' <- unsafeManagedPtrCastPtr widget
    let modifiers' = gflagsToWord modifiers
    result <- gtk_test_widget_send_key widget' keyval modifiers'
    let result' = (/= 0) result
    touchManagedPtr widget
    return result'


-- function test_widget_click
-- Args: [ Arg
--           { argCName = "widget"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Widget" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Widget to generate a button click on."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "button"
--           , argType = TBasicType TUInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "Number of the pointer button for the event, usually 1, 2 or 3."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "modifiers"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "ModifierType" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Keyboard modifiers the event is setup with."
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

foreign import ccall "gtk_test_widget_click" gtk_test_widget_click :: 
    Ptr Gtk.Widget.Widget ->                -- widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    Word32 ->                               -- button : TBasicType TUInt
    CUInt ->                                -- modifiers : TInterface (Name {namespace = "Gdk", name = "ModifierType"})
    IO CInt

{-# DEPRECATED testWidgetClick ["(Since version 3.20)","This testing infrastructure is phased out in favor of reftests."] #-}
-- | This function will generate a /@button@/ click (button press and button
-- release event) in the middle of the first GdkWindow found that belongs
-- to /@widget@/.
-- For windowless widgets like t'GI.Gtk.Objects.Button.Button' (which returns 'P.False' from
-- 'GI.Gtk.Objects.Widget.widgetGetHasWindow'), this will often be an
-- input-only event window. For other widgets, this is usually widget->window.
-- Certain caveats should be considered when using this function, in
-- particular because the mouse pointer is warped to the button click
-- location, see 'GI.Gdk.Functions.testSimulateButton' for details.
-- 
-- /Since: 2.14/
testWidgetClick ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.Widget.IsWidget a) =>
    a
    -- ^ /@widget@/: Widget to generate a button click on.
    -> Word32
    -- ^ /@button@/: Number of the pointer button for the event, usually 1, 2 or 3.
    -> [Gdk.Flags.ModifierType]
    -- ^ /@modifiers@/: Keyboard modifiers the event is setup with.
    -> m Bool
    -- ^ __Returns:__ whether all actions neccessary for the button click simulation were carried out successfully.
testWidgetClick widget button modifiers = liftIO $ do
    widget' <- unsafeManagedPtrCastPtr widget
    let modifiers' = gflagsToWord modifiers
    result <- gtk_test_widget_click widget' button modifiers'
    let result' = (/= 0) result
    touchManagedPtr widget
    return result'


-- function test_text_set
-- Args: [ Arg
--           { argCName = "widget"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Widget" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "valid widget pointer."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "string"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a 0-terminated C string"
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

foreign import ccall "gtk_test_text_set" gtk_test_text_set :: 
    Ptr Gtk.Widget.Widget ->                -- widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    CString ->                              -- string : TBasicType TUTF8
    IO ()

{-# DEPRECATED testTextSet ["(Since version 3.20)","This testing infrastructure is phased out in favor of reftests."] #-}
-- | Set the text string of /@widget@/ to /@string@/ if it is a GtkLabel,
-- GtkEditable (entry and text widgets) or GtkTextView.
-- 
-- /Since: 2.14/
testTextSet ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.Widget.IsWidget a) =>
    a
    -- ^ /@widget@/: valid widget pointer.
    -> T.Text
    -- ^ /@string@/: a 0-terminated C string
    -> m ()
testTextSet widget string = liftIO $ do
    widget' <- unsafeManagedPtrCastPtr widget
    string' <- textToCString string
    gtk_test_text_set widget' string'
    touchManagedPtr widget
    freeMem string'
    return ()


-- function test_text_get
-- Args: [ Arg
--           { argCName = "widget"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Widget" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "valid widget pointer."
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

foreign import ccall "gtk_test_text_get" gtk_test_text_get :: 
    Ptr Gtk.Widget.Widget ->                -- widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    IO CString

{-# DEPRECATED testTextGet ["(Since version 3.20)","This testing infrastructure is phased out in favor of reftests."] #-}
-- | Retrive the text string of /@widget@/ if it is a GtkLabel,
-- GtkEditable (entry and text widgets) or GtkTextView.
-- 
-- /Since: 2.14/
testTextGet ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.Widget.IsWidget a) =>
    a
    -- ^ /@widget@/: valid widget pointer.
    -> m T.Text
    -- ^ __Returns:__ new 0-terminated C string, needs to be released with 'GI.GLib.Functions.free'.
testTextGet widget = liftIO $ do
    widget' <- unsafeManagedPtrCastPtr widget
    result <- gtk_test_text_get widget'
    checkUnexpectedReturnNULL "testTextGet" result
    result' <- cstringToText result
    freeMem result
    touchManagedPtr widget
    return result'


-- function test_spin_button_click
-- Args: [ Arg
--           { argCName = "spinner"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "SpinButton" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "valid GtkSpinButton widget."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "button"
--           , argType = TBasicType TUInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "Number of the pointer button for the event, usually 1, 2 or 3."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "upwards"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "%TRUE for upwards arrow click, %FALSE for downwards arrow click."
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

foreign import ccall "gtk_test_spin_button_click" gtk_test_spin_button_click :: 
    Ptr Gtk.SpinButton.SpinButton ->        -- spinner : TInterface (Name {namespace = "Gtk", name = "SpinButton"})
    Word32 ->                               -- button : TBasicType TUInt
    CInt ->                                 -- upwards : TBasicType TBoolean
    IO CInt

{-# DEPRECATED testSpinButtonClick ["(Since version 3.20)","This testing infrastructure is phased out in favor of reftests."] #-}
-- | This function will generate a /@button@/ click in the upwards or downwards
-- spin button arrow areas, usually leading to an increase or decrease of
-- spin button’s value.
-- 
-- /Since: 2.14/
testSpinButtonClick ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.SpinButton.IsSpinButton a) =>
    a
    -- ^ /@spinner@/: valid GtkSpinButton widget.
    -> Word32
    -- ^ /@button@/: Number of the pointer button for the event, usually 1, 2 or 3.
    -> Bool
    -- ^ /@upwards@/: 'P.True' for upwards arrow click, 'P.False' for downwards arrow click.
    -> m Bool
    -- ^ __Returns:__ whether all actions neccessary for the button click simulation were carried out successfully.
testSpinButtonClick spinner button upwards = liftIO $ do
    spinner' <- unsafeManagedPtrCastPtr spinner
    let upwards' = (fromIntegral . fromEnum) upwards
    result <- gtk_test_spin_button_click spinner' button upwards'
    let result' = (/= 0) result
    touchManagedPtr spinner
    return result'


-- function test_slider_set_perc
-- Args: [ Arg
--           { argCName = "widget"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Widget" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "valid widget pointer."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "percentage"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "value between 0 and 100."
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

foreign import ccall "gtk_test_slider_set_perc" gtk_test_slider_set_perc :: 
    Ptr Gtk.Widget.Widget ->                -- widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    CDouble ->                              -- percentage : TBasicType TDouble
    IO ()

{-# DEPRECATED testSliderSetPerc ["(Since version 3.20)","This testing infrastructure is phased out in favor of reftests."] #-}
-- | This function will adjust the slider position of all GtkRange
-- based widgets, such as scrollbars or scales, it’ll also adjust
-- spin buttons. The adjustment value of these widgets is set to
-- a value between the lower and upper limits, according to the
-- /@percentage@/ argument.
-- 
-- /Since: 2.14/
testSliderSetPerc ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.Widget.IsWidget a) =>
    a
    -- ^ /@widget@/: valid widget pointer.
    -> Double
    -- ^ /@percentage@/: value between 0 and 100.
    -> m ()
testSliderSetPerc widget percentage = liftIO $ do
    widget' <- unsafeManagedPtrCastPtr widget
    let percentage' = realToFrac percentage
    gtk_test_slider_set_perc widget' percentage'
    touchManagedPtr widget
    return ()


-- function test_slider_get_value
-- Args: [ Arg
--           { argCName = "widget"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Widget" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "valid widget pointer."
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

foreign import ccall "gtk_test_slider_get_value" gtk_test_slider_get_value :: 
    Ptr Gtk.Widget.Widget ->                -- widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    IO CDouble

{-# DEPRECATED testSliderGetValue ["(Since version 3.20)","This testing infrastructure is phased out in favor of reftests."] #-}
-- | Retrive the literal adjustment value for GtkRange based
-- widgets and spin buttons. Note that the value returned by
-- this function is anything between the lower and upper bounds
-- of the adjustment belonging to /@widget@/, and is not a percentage
-- as passed in to 'GI.Gtk.Functions.testSliderSetPerc'.
-- 
-- /Since: 2.14/
testSliderGetValue ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.Widget.IsWidget a) =>
    a
    -- ^ /@widget@/: valid widget pointer.
    -> m Double
    -- ^ __Returns:__ gtk_adjustment_get_value (adjustment) for an adjustment belonging to /@widget@/.
testSliderGetValue widget = liftIO $ do
    widget' <- unsafeManagedPtrCastPtr widget
    result <- gtk_test_slider_get_value widget'
    let result' = realToFrac result
    touchManagedPtr widget
    return result'


-- function test_register_all_types
-- Args: []
-- Lengths: []
-- returnType: Nothing
-- throws : False
-- Skip return : False

foreign import ccall "gtk_test_register_all_types" gtk_test_register_all_types :: 
    IO ()

-- | Force registration of all core Gtk+ and Gdk object types.
-- This allowes to refer to any of those object types via
-- 'GI.GObject.Functions.typeFromName' after calling this function.
-- 
-- /Since: 2.14/
testRegisterAllTypes ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m ()
testRegisterAllTypes  = liftIO $ do
    gtk_test_register_all_types
    return ()


-- function test_list_all_types
-- Args: [ Arg
--           { argCName = "n_types"
--           , argType = TBasicType TUInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "location to store number of types"
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
-- returnType: Just (TCArray True (-1) 0 (TBasicType TGType))
-- throws : False
-- Skip return : False

foreign import ccall "gtk_test_list_all_types" gtk_test_list_all_types :: 
    Ptr Word32 ->                           -- n_types : TBasicType TUInt
    IO (Ptr CGType)

-- | Return the type ids that have been registered after
-- calling 'GI.Gtk.Functions.testRegisterAllTypes'.
-- 
-- /Since: 2.14/
testListAllTypes ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m (([GType], Word32))
    -- ^ __Returns:__ 
    --    0-terminated array of type ids
testListAllTypes  = liftIO $ do
    nTypes <- allocMem :: IO (Ptr Word32)
    result <- gtk_test_list_all_types nTypes
    checkUnexpectedReturnNULL "testListAllTypes" result
    result' <- (unpackMapZeroTerminatedStorableArray GType) result
    nTypes' <- peek nTypes
    freeMem nTypes
    return (result', nTypes')


-- function test_find_widget
-- Args: [ Arg
--           { argCName = "widget"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Widget" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Container widget, usually a GtkWindow."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "label_pattern"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Shell-glob pattern to match a label string."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "widget_type"
--           , argType = TBasicType TGType
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Type of a aearched for label sibling widget."
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "Widget" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_test_find_widget" gtk_test_find_widget :: 
    Ptr Gtk.Widget.Widget ->                -- widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    CString ->                              -- label_pattern : TBasicType TUTF8
    CGType ->                               -- widget_type : TBasicType TGType
    IO (Ptr Gtk.Widget.Widget)

-- | This function will search the descendants of /@widget@/ for a widget
-- of type /@widgetType@/ that has a label matching /@labelPattern@/ next
-- to it. This is most useful for automated GUI testing, e.g. to find
-- the “OK” button in a dialog and synthesize clicks on it.
-- However see 'GI.Gtk.Functions.testFindLabel', 'GI.Gtk.Functions.testFindSibling' and
-- 'GI.Gtk.Functions.testWidgetClick' for possible caveats involving the search of
-- such widgets and synthesizing widget events.
-- 
-- /Since: 2.14/
testFindWidget ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.Widget.IsWidget a) =>
    a
    -- ^ /@widget@/: Container widget, usually a GtkWindow.
    -> T.Text
    -- ^ /@labelPattern@/: Shell-glob pattern to match a label string.
    -> GType
    -- ^ /@widgetType@/: Type of a aearched for label sibling widget.
    -> m (Maybe Gtk.Widget.Widget)
    -- ^ __Returns:__ a valid widget if any is found or 'P.Nothing'.
testFindWidget widget labelPattern widgetType = liftIO $ do
    widget' <- unsafeManagedPtrCastPtr widget
    labelPattern' <- textToCString labelPattern
    let widgetType' = gtypeToCGType widgetType
    result <- gtk_test_find_widget widget' labelPattern' widgetType'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (newObject Gtk.Widget.Widget) result'
        return result''
    touchManagedPtr widget
    freeMem labelPattern'
    return maybeResult


-- function test_find_sibling
-- Args: [ Arg
--           { argCName = "base_widget"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Widget" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Valid widget, part of a widget hierarchy"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "widget_type"
--           , argType = TBasicType TGType
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Type of a aearched for sibling widget"
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "Widget" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_test_find_sibling" gtk_test_find_sibling :: 
    Ptr Gtk.Widget.Widget ->                -- base_widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    CGType ->                               -- widget_type : TBasicType TGType
    IO (Ptr Gtk.Widget.Widget)

-- | This function will search siblings of /@baseWidget@/ and siblings of its
-- ancestors for all widgets matching /@widgetType@/.
-- Of the matching widgets, the one that is geometrically closest to
-- /@baseWidget@/ will be returned.
-- The general purpose of this function is to find the most likely “action”
-- widget, relative to another labeling widget. Such as finding a
-- button or text entry widget, given its corresponding label widget.
-- 
-- /Since: 2.14/
testFindSibling ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.Widget.IsWidget a) =>
    a
    -- ^ /@baseWidget@/: Valid widget, part of a widget hierarchy
    -> GType
    -- ^ /@widgetType@/: Type of a aearched for sibling widget
    -> m Gtk.Widget.Widget
    -- ^ __Returns:__ a widget of type /@widgetType@/ if any is found.
testFindSibling baseWidget widgetType = liftIO $ do
    baseWidget' <- unsafeManagedPtrCastPtr baseWidget
    let widgetType' = gtypeToCGType widgetType
    result <- gtk_test_find_sibling baseWidget' widgetType'
    checkUnexpectedReturnNULL "testFindSibling" result
    result' <- (newObject Gtk.Widget.Widget) result
    touchManagedPtr baseWidget
    return result'


-- function test_find_label
-- Args: [ Arg
--           { argCName = "widget"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Widget" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Valid label or container widget."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "label_pattern"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Shell-glob pattern to match a label string."
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "Widget" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_test_find_label" gtk_test_find_label :: 
    Ptr Gtk.Widget.Widget ->                -- widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    CString ->                              -- label_pattern : TBasicType TUTF8
    IO (Ptr Gtk.Widget.Widget)

-- | This function will search /@widget@/ and all its descendants for a GtkLabel
-- widget with a text string matching /@labelPattern@/.
-- The /@labelPattern@/ may contain asterisks “*” and question marks “?” as
-- placeholders, @/g_pattern_match()/@ is used for the matching.
-- Note that locales other than \"C“ tend to alter (translate” label strings,
-- so this function is genrally only useful in test programs with
-- predetermined locales, see @/gtk_test_init()/@ for more details.
-- 
-- /Since: 2.14/
testFindLabel ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.Widget.IsWidget a) =>
    a
    -- ^ /@widget@/: Valid label or container widget.
    -> T.Text
    -- ^ /@labelPattern@/: Shell-glob pattern to match a label string.
    -> m Gtk.Widget.Widget
    -- ^ __Returns:__ a GtkLabel widget if any is found.
testFindLabel widget labelPattern = liftIO $ do
    widget' <- unsafeManagedPtrCastPtr widget
    labelPattern' <- textToCString labelPattern
    result <- gtk_test_find_label widget' labelPattern'
    checkUnexpectedReturnNULL "testFindLabel" result
    result' <- (newObject Gtk.Widget.Widget) result
    touchManagedPtr widget
    freeMem labelPattern'
    return result'


-- function test_create_simple_window
-- Args: [ Arg
--           { argCName = "window_title"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Title of the window to be displayed."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "dialog_text"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Text inside the window to be displayed."
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "Widget" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_test_create_simple_window" gtk_test_create_simple_window :: 
    CString ->                              -- window_title : TBasicType TUTF8
    CString ->                              -- dialog_text : TBasicType TUTF8
    IO (Ptr Gtk.Widget.Widget)

{-# DEPRECATED testCreateSimpleWindow ["(Since version 3.20)","This testing infrastructure is phased out in favor of reftests."] #-}
-- | Create a simple window with window title /@windowTitle@/ and
-- text contents /@dialogText@/.
-- The window will quit any running 'GI.Gtk.Functions.main'-loop when destroyed, and it
-- will automatically be destroyed upon test function teardown.
-- 
-- /Since: 2.14/
testCreateSimpleWindow ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    T.Text
    -- ^ /@windowTitle@/: Title of the window to be displayed.
    -> T.Text
    -- ^ /@dialogText@/: Text inside the window to be displayed.
    -> m Gtk.Widget.Widget
    -- ^ __Returns:__ a widget pointer to the newly created GtkWindow.
testCreateSimpleWindow windowTitle dialogText = liftIO $ do
    windowTitle' <- textToCString windowTitle
    dialogText' <- textToCString dialogText
    result <- gtk_test_create_simple_window windowTitle' dialogText'
    checkUnexpectedReturnNULL "testCreateSimpleWindow" result
    result' <- (newObject Gtk.Widget.Widget) result
    freeMem windowTitle'
    freeMem dialogText'
    return result'


-- function targets_include_uri
-- Args: [ Arg
--           { argCName = "targets"
--           , argType =
--               TCArray
--                 False
--                 (-1)
--                 1
--                 (TInterface Name { namespace = "Gdk" , name = "Atom" })
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "an array of #GdkAtoms"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "n_targets"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the length of @targets"
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
--              { argCName = "n_targets"
--              , argType = TBasicType TInt
--              , direction = DirectionIn
--              , mayBeNull = False
--              , argDoc =
--                  Documentation
--                    { rawDocText = Just "the length of @targets"
--                    , sinceVersion = Nothing
--                    }
--              , argScope = ScopeTypeInvalid
--              , argClosure = -1
--              , argDestroy = -1
--              , argCallerAllocates = False
--              , transfer = TransferNothing
--              }
--          ]
-- returnType: Just (TBasicType TBoolean)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_targets_include_uri" gtk_targets_include_uri :: 
    Ptr (Ptr Gdk.Atom.Atom) ->              -- targets : TCArray False (-1) 1 (TInterface (Name {namespace = "Gdk", name = "Atom"}))
    Int32 ->                                -- n_targets : TBasicType TInt
    IO CInt

-- | Determines if any of the targets in /@targets@/ can be used to
-- provide an uri list.
-- 
-- /Since: 2.10/
targetsIncludeUri ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    [Gdk.Atom.Atom]
    -- ^ /@targets@/: an array of @/GdkAtoms/@
    -> m Bool
    -- ^ __Returns:__ 'P.True' if /@targets@/ include a suitable target for uri lists,
    --   otherwise 'P.False'.
targetsIncludeUri targets = liftIO $ do
    let nTargets = fromIntegral $ P.length targets
    targets' <- mapM unsafeManagedPtrGetPtr targets
    targets'' <- packPtrArray targets'
    result <- gtk_targets_include_uri targets'' nTargets
    let result' = (/= 0) result
    mapM_ touchManagedPtr targets
    freeMem targets''
    return result'


-- function targets_include_text
-- Args: [ Arg
--           { argCName = "targets"
--           , argType =
--               TCArray
--                 False
--                 (-1)
--                 1
--                 (TInterface Name { namespace = "Gdk" , name = "Atom" })
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "an array of #GdkAtoms"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "n_targets"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the length of @targets"
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
--              { argCName = "n_targets"
--              , argType = TBasicType TInt
--              , direction = DirectionIn
--              , mayBeNull = False
--              , argDoc =
--                  Documentation
--                    { rawDocText = Just "the length of @targets"
--                    , sinceVersion = Nothing
--                    }
--              , argScope = ScopeTypeInvalid
--              , argClosure = -1
--              , argDestroy = -1
--              , argCallerAllocates = False
--              , transfer = TransferNothing
--              }
--          ]
-- returnType: Just (TBasicType TBoolean)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_targets_include_text" gtk_targets_include_text :: 
    Ptr (Ptr Gdk.Atom.Atom) ->              -- targets : TCArray False (-1) 1 (TInterface (Name {namespace = "Gdk", name = "Atom"}))
    Int32 ->                                -- n_targets : TBasicType TInt
    IO CInt

-- | Determines if any of the targets in /@targets@/ can be used to
-- provide text.
-- 
-- /Since: 2.10/
targetsIncludeText ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    [Gdk.Atom.Atom]
    -- ^ /@targets@/: an array of @/GdkAtoms/@
    -> m Bool
    -- ^ __Returns:__ 'P.True' if /@targets@/ include a suitable target for text,
    --   otherwise 'P.False'.
targetsIncludeText targets = liftIO $ do
    let nTargets = fromIntegral $ P.length targets
    targets' <- mapM unsafeManagedPtrGetPtr targets
    targets'' <- packPtrArray targets'
    result <- gtk_targets_include_text targets'' nTargets
    let result' = (/= 0) result
    mapM_ touchManagedPtr targets
    freeMem targets''
    return result'


-- function targets_include_rich_text
-- Args: [ Arg
--           { argCName = "targets"
--           , argType =
--               TCArray
--                 False
--                 (-1)
--                 1
--                 (TInterface Name { namespace = "Gdk" , name = "Atom" })
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "an array of #GdkAtoms"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "n_targets"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the length of @targets"
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
-- Lengths: [ Arg
--              { argCName = "n_targets"
--              , argType = TBasicType TInt
--              , direction = DirectionIn
--              , mayBeNull = False
--              , argDoc =
--                  Documentation
--                    { rawDocText = Just "the length of @targets"
--                    , sinceVersion = Nothing
--                    }
--              , argScope = ScopeTypeInvalid
--              , argClosure = -1
--              , argDestroy = -1
--              , argCallerAllocates = False
--              , transfer = TransferNothing
--              }
--          ]
-- returnType: Just (TBasicType TBoolean)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_targets_include_rich_text" gtk_targets_include_rich_text :: 
    Ptr (Ptr Gdk.Atom.Atom) ->              -- targets : TCArray False (-1) 1 (TInterface (Name {namespace = "Gdk", name = "Atom"}))
    Int32 ->                                -- n_targets : TBasicType TInt
    Ptr Gtk.TextBuffer.TextBuffer ->        -- buffer : TInterface (Name {namespace = "Gtk", name = "TextBuffer"})
    IO CInt

-- | Determines if any of the targets in /@targets@/ can be used to
-- provide rich text.
-- 
-- /Since: 2.10/
targetsIncludeRichText ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.TextBuffer.IsTextBuffer a) =>
    [Gdk.Atom.Atom]
    -- ^ /@targets@/: an array of @/GdkAtoms/@
    -> a
    -- ^ /@buffer@/: a t'GI.Gtk.Objects.TextBuffer.TextBuffer'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if /@targets@/ include a suitable target for rich text,
    --               otherwise 'P.False'.
targetsIncludeRichText targets buffer = liftIO $ do
    let nTargets = fromIntegral $ P.length targets
    targets' <- mapM unsafeManagedPtrGetPtr targets
    targets'' <- packPtrArray targets'
    buffer' <- unsafeManagedPtrCastPtr buffer
    result <- gtk_targets_include_rich_text targets'' nTargets buffer'
    let result' = (/= 0) result
    mapM_ touchManagedPtr targets
    touchManagedPtr buffer
    freeMem targets''
    return result'


-- function targets_include_image
-- Args: [ Arg
--           { argCName = "targets"
--           , argType =
--               TCArray
--                 False
--                 (-1)
--                 1
--                 (TInterface Name { namespace = "Gdk" , name = "Atom" })
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "an array of #GdkAtoms"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "n_targets"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the length of @targets"
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
-- Lengths: [ Arg
--              { argCName = "n_targets"
--              , argType = TBasicType TInt
--              , direction = DirectionIn
--              , mayBeNull = False
--              , argDoc =
--                  Documentation
--                    { rawDocText = Just "the length of @targets"
--                    , sinceVersion = Nothing
--                    }
--              , argScope = ScopeTypeInvalid
--              , argClosure = -1
--              , argDestroy = -1
--              , argCallerAllocates = False
--              , transfer = TransferNothing
--              }
--          ]
-- returnType: Just (TBasicType TBoolean)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_targets_include_image" gtk_targets_include_image :: 
    Ptr (Ptr Gdk.Atom.Atom) ->              -- targets : TCArray False (-1) 1 (TInterface (Name {namespace = "Gdk", name = "Atom"}))
    Int32 ->                                -- n_targets : TBasicType TInt
    CInt ->                                 -- writable : TBasicType TBoolean
    IO CInt

-- | Determines if any of the targets in /@targets@/ can be used to
-- provide a t'GI.GdkPixbuf.Objects.Pixbuf.Pixbuf'.
-- 
-- /Since: 2.10/
targetsIncludeImage ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    [Gdk.Atom.Atom]
    -- ^ /@targets@/: an array of @/GdkAtoms/@
    -> Bool
    -- ^ /@writable@/: whether to accept only targets for which GTK+ knows
    --   how to convert a pixbuf into the format
    -> m Bool
    -- ^ __Returns:__ 'P.True' if /@targets@/ include a suitable target for images,
    --   otherwise 'P.False'.
targetsIncludeImage targets writable = liftIO $ do
    let nTargets = fromIntegral $ P.length targets
    targets' <- mapM unsafeManagedPtrGetPtr targets
    targets'' <- packPtrArray targets'
    let writable' = (fromIntegral . fromEnum) writable
    result <- gtk_targets_include_image targets'' nTargets writable'
    let result' = (/= 0) result
    mapM_ touchManagedPtr targets
    freeMem targets''
    return result'


-- function target_table_new_from_list
-- Args: [ Arg
--           { argCName = "list"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TargetList" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTargetList" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "n_targets"
--           , argType = TBasicType TInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "return location for the number ot targets in the table"
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
--              { argCName = "n_targets"
--              , argType = TBasicType TInt
--              , direction = DirectionOut
--              , mayBeNull = False
--              , argDoc =
--                  Documentation
--                    { rawDocText =
--                        Just "return location for the number ot targets in the table"
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
--                  (TInterface Name { namespace = "Gtk" , name = "TargetEntry" }))
-- throws : False
-- Skip return : False

foreign import ccall "gtk_target_table_new_from_list" gtk_target_table_new_from_list :: 
    Ptr Gtk.TargetList.TargetList ->        -- list : TInterface (Name {namespace = "Gtk", name = "TargetList"})
    Ptr Int32 ->                            -- n_targets : TBasicType TInt
    IO (Ptr Gtk.TargetEntry.TargetEntry)

-- | This function creates an t'GI.Gtk.Structs.TargetEntry.TargetEntry' array that contains the
-- same targets as the passed @/list/@. The returned table is newly
-- allocated and should be freed using 'GI.Gtk.Functions.targetTableFree' when no
-- longer needed.
-- 
-- /Since: 2.10/
targetTableNewFromList ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    Gtk.TargetList.TargetList
    -- ^ /@list@/: a t'GI.Gtk.Structs.TargetList.TargetList'
    -> m [Gtk.TargetEntry.TargetEntry]
    -- ^ __Returns:__ the new table.
targetTableNewFromList list = liftIO $ do
    list' <- unsafeManagedPtrGetPtr list
    nTargets <- allocMem :: IO (Ptr Int32)
    result <- gtk_target_table_new_from_list list' nTargets
    nTargets' <- peek nTargets
    checkUnexpectedReturnNULL "targetTableNewFromList" result
    result' <- (unpackBoxedArrayWithLength 16 nTargets') result
    result'' <- mapM (wrapBoxed Gtk.TargetEntry.TargetEntry) result'
    freeMem result
    touchManagedPtr list
    freeMem nTargets
    return result''


-- function target_table_free
-- Args: [ Arg
--           { argCName = "targets"
--           , argType =
--               TCArray
--                 False
--                 (-1)
--                 1
--                 (TInterface Name { namespace = "Gtk" , name = "TargetEntry" })
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTargetEntry array"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "n_targets"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the number of entries in the array"
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
--              { argCName = "n_targets"
--              , argType = TBasicType TInt
--              , direction = DirectionIn
--              , mayBeNull = False
--              , argDoc =
--                  Documentation
--                    { rawDocText = Just "the number of entries in the array"
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

foreign import ccall "gtk_target_table_free" gtk_target_table_free :: 
    Ptr Gtk.TargetEntry.TargetEntry ->      -- targets : TCArray False (-1) 1 (TInterface (Name {namespace = "Gtk", name = "TargetEntry"}))
    Int32 ->                                -- n_targets : TBasicType TInt
    IO ()

-- | This function frees a target table as returned by
-- 'GI.Gtk.Functions.targetTableNewFromList'
-- 
-- /Since: 2.10/
targetTableFree ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    [Gtk.TargetEntry.TargetEntry]
    -- ^ /@targets@/: a t'GI.Gtk.Structs.TargetEntry.TargetEntry' array
    -> m ()
targetTableFree targets = liftIO $ do
    let nTargets = fromIntegral $ P.length targets
    targets' <- mapM unsafeManagedPtrGetPtr targets
    targets'' <- packBlockArray 16 targets'
    gtk_target_table_free targets'' nTargets
    mapM_ touchManagedPtr targets
    freeMem targets''
    return ()


-- function stock_set_translate_func
-- Args: [ Arg
--           { argCName = "domain"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "the translation domain for which @func shall be used"
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
--               TInterface Name { namespace = "Gtk" , name = "TranslateFunc" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTranslateFunc"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeNotified
--           , argClosure = 2
--           , argDestroy = 3
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
--                 { rawDocText = Just "data to pass to @func"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "notify"
--           , argType =
--               TInterface Name { namespace = "GLib" , name = "DestroyNotify" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "a #GDestroyNotify that is called when @data is\n  no longer needed"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeAsync
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

foreign import ccall "gtk_stock_set_translate_func" gtk_stock_set_translate_func :: 
    CString ->                              -- domain : TBasicType TUTF8
    FunPtr Gtk.Callbacks.C_TranslateFunc -> -- func : TInterface (Name {namespace = "Gtk", name = "TranslateFunc"})
    Ptr () ->                               -- data : TBasicType TPtr
    FunPtr GLib.Callbacks.C_DestroyNotify -> -- notify : TInterface (Name {namespace = "GLib", name = "DestroyNotify"})
    IO ()

{-# DEPRECATED stockSetTranslateFunc ["(Since version 3.10)"] #-}
-- | Sets a function to be used for translating the /@label@/ of
-- a stock item.
-- 
-- If no function is registered for a translation domain,
-- 'GI.GLib.Functions.dgettext' is used.
-- 
-- The function is used for all stock items whose
-- /@translationDomain@/ matches /@domain@/. Note that it is possible
-- to use strings different from the actual gettext translation domain
-- of your application for this, as long as your t'GI.Gtk.Callbacks.TranslateFunc' uses
-- the correct domain when calling @/dgettext()/@. This can be useful, e.g.
-- when dealing with message contexts:
-- 
-- 
-- === /C code/
-- >
-- >GtkStockItem items[] = {
-- > { MY_ITEM1, NC_("odd items", "Item 1"), 0, 0, "odd-item-domain" },
-- > { MY_ITEM2, NC_("even items", "Item 2"), 0, 0, "even-item-domain" },
-- >};
-- >
-- >gchar *
-- >my_translate_func (const gchar *msgid,
-- >                   gpointer     data)
-- >{
-- >  gchar *msgctxt = data;
-- >
-- >  return (gchar*)g_dpgettext2 (GETTEXT_PACKAGE, msgctxt, msgid);
-- >}
-- >
-- >...
-- >
-- >gtk_stock_add (items, G_N_ELEMENTS (items));
-- >gtk_stock_set_translate_func ("odd-item-domain", my_translate_func, "odd items");
-- >gtk_stock_set_translate_func ("even-item-domain", my_translate_func, "even items");
-- 
-- 
-- /Since: 2.8/
stockSetTranslateFunc ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    T.Text
    -- ^ /@domain@/: the translation domain for which /@func@/ shall be used
    -> Gtk.Callbacks.TranslateFunc
    -- ^ /@func@/: a t'GI.Gtk.Callbacks.TranslateFunc'
    -> m ()
stockSetTranslateFunc domain func = liftIO $ do
    domain' <- textToCString domain
    func' <- Gtk.Callbacks.mk_TranslateFunc (Gtk.Callbacks.wrap_TranslateFunc Nothing (Gtk.Callbacks.drop_closures_TranslateFunc func))
    let data_ = castFunPtrToPtr func'
    let notify = SP.safeFreeFunPtrPtr
    gtk_stock_set_translate_func domain' func' data_ notify
    freeMem domain'
    return ()


-- function stock_lookup
-- Args: [ Arg
--           { argCName = "stock_id"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a stock item name" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "item"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StockItem" }
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "stock item to initialize with values"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = True
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TBasicType TBoolean)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_stock_lookup" gtk_stock_lookup :: 
    CString ->                              -- stock_id : TBasicType TUTF8
    Ptr Gtk.StockItem.StockItem ->          -- item : TInterface (Name {namespace = "Gtk", name = "StockItem"})
    IO CInt

{-# DEPRECATED stockLookup ["(Since version 3.10)"] #-}
-- | Fills /@item@/ with the registered values for /@stockId@/, returning 'P.True'
-- if /@stockId@/ was known.
stockLookup ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    T.Text
    -- ^ /@stockId@/: a stock item name
    -> m ((Bool, Gtk.StockItem.StockItem))
    -- ^ __Returns:__ 'P.True' if /@item@/ was initialized
stockLookup stockId = liftIO $ do
    stockId' <- textToCString stockId
    item <- SP.callocBytes 32 :: IO (Ptr Gtk.StockItem.StockItem)
    result <- gtk_stock_lookup stockId' item
    let result' = (/= 0) result
    item' <- (wrapPtr Gtk.StockItem.StockItem) item
    freeMem stockId'
    return (result', item')


-- function stock_list_ids
-- Args: []
-- Lengths: []
-- returnType: Just (TGSList (TBasicType TUTF8))
-- throws : False
-- Skip return : False

foreign import ccall "gtk_stock_list_ids" gtk_stock_list_ids :: 
    IO (Ptr (GSList CString))

{-# DEPRECATED stockListIds ["(Since version 3.10)"] #-}
-- | Retrieves a list of all known stock IDs added to a t'GI.Gtk.Objects.IconFactory.IconFactory'
-- or registered with 'GI.Gtk.Functions.stockAdd'. The list must be freed with @/g_slist_free()/@,
-- and each string in the list must be freed with 'GI.GLib.Functions.free'.
stockListIds ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m [T.Text]
    -- ^ __Returns:__ a list of known stock IDs
stockListIds  = liftIO $ do
    result <- gtk_stock_list_ids
    result' <- unpackGSList result
    result'' <- mapM cstringToText result'
    mapGSList freeMem result
    g_slist_free result
    return result''


-- function stock_add_static
-- Args: [ Arg
--           { argCName = "items"
--           , argType =
--               TCArray
--                 False
--                 (-1)
--                 1
--                 (TInterface Name { namespace = "Gtk" , name = "StockItem" })
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStockItem or array of #GtkStockItem"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "n_items"
--           , argType = TBasicType TUInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "number of items" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: [ Arg
--              { argCName = "n_items"
--              , argType = TBasicType TUInt
--              , direction = DirectionIn
--              , mayBeNull = False
--              , argDoc =
--                  Documentation
--                    { rawDocText = Just "number of items" , sinceVersion = Nothing }
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

foreign import ccall "gtk_stock_add_static" gtk_stock_add_static :: 
    Ptr Gtk.StockItem.StockItem ->          -- items : TCArray False (-1) 1 (TInterface (Name {namespace = "Gtk", name = "StockItem"}))
    Word32 ->                               -- n_items : TBasicType TUInt
    IO ()

{-# DEPRECATED stockAddStatic ["(Since version 3.10)"] #-}
-- | Same as 'GI.Gtk.Functions.stockAdd', but doesn’t copy /@items@/, so
-- /@items@/ must persist until application exit.
stockAddStatic ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    [Gtk.StockItem.StockItem]
    -- ^ /@items@/: a t'GI.Gtk.Structs.StockItem.StockItem' or array of t'GI.Gtk.Structs.StockItem.StockItem'
    -> m ()
stockAddStatic items = liftIO $ do
    let nItems = fromIntegral $ P.length items
    items' <- mapM unsafeManagedPtrGetPtr items
    items'' <- packBlockArray 32 items'
    gtk_stock_add_static items'' nItems
    mapM_ touchManagedPtr items
    freeMem items''
    return ()


-- function stock_add
-- Args: [ Arg
--           { argCName = "items"
--           , argType =
--               TCArray
--                 False
--                 (-1)
--                 1
--                 (TInterface Name { namespace = "Gtk" , name = "StockItem" })
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStockItem or array of items"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "n_items"
--           , argType = TBasicType TUInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "number of #GtkStockItem in @items"
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
--              { argCName = "n_items"
--              , argType = TBasicType TUInt
--              , direction = DirectionIn
--              , mayBeNull = False
--              , argDoc =
--                  Documentation
--                    { rawDocText = Just "number of #GtkStockItem in @items"
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

foreign import ccall "gtk_stock_add" gtk_stock_add :: 
    Ptr Gtk.StockItem.StockItem ->          -- items : TCArray False (-1) 1 (TInterface (Name {namespace = "Gtk", name = "StockItem"}))
    Word32 ->                               -- n_items : TBasicType TUInt
    IO ()

{-# DEPRECATED stockAdd ["(Since version 3.10)"] #-}
-- | Registers each of the stock items in /@items@/. If an item already
-- exists with the same stock ID as one of the /@items@/, the old item
-- gets replaced. The stock items are copied, so GTK+ does not hold
-- any pointer into /@items@/ and /@items@/ can be freed. Use
-- 'GI.Gtk.Functions.stockAddStatic' if /@items@/ is persistent and GTK+ need not
-- copy the array.
stockAdd ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    [Gtk.StockItem.StockItem]
    -- ^ /@items@/: a t'GI.Gtk.Structs.StockItem.StockItem' or array of items
    -> m ()
stockAdd items = liftIO $ do
    let nItems = fromIntegral $ P.length items
    items' <- mapM unsafeManagedPtrGetPtr items
    items'' <- packBlockArray 32 items'
    gtk_stock_add items'' nItems
    mapM_ touchManagedPtr items
    freeMem items''
    return ()


-- function show_uri_on_window
-- Args: [ Arg
--           { argCName = "parent"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "parent window" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "uri"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the uri to show" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "timestamp"
--           , argType = TBasicType TUInt32
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a timestamp to prevent focus stealing"
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

foreign import ccall "gtk_show_uri_on_window" gtk_show_uri_on_window :: 
    Ptr Gtk.Window.Window ->                -- parent : TInterface (Name {namespace = "Gtk", name = "Window"})
    CString ->                              -- uri : TBasicType TUTF8
    Word32 ->                               -- timestamp : TBasicType TUInt32
    Ptr (Ptr GError) ->                     -- error
    IO CInt

-- | This is a convenience function for launching the default application
-- to show the uri. The uri must be of a form understood by GIO (i.e. you
-- need to install gvfs to get support for uri schemes such as http:\/\/
-- or ftp:\/\/, as only local files are handled by GIO itself).
-- Typical examples are
-- 
-- * @file:\/\/\/home\/gnome\/pict.jpg@
-- * @http:\/\/www.gnome.org@
-- * @mailto:me\@gnome.org@
-- 
-- 
-- Ideally the timestamp is taken from the event triggering
-- the 'GI.Gtk.Functions.showUri' call. If timestamp is not known you can take
-- 'GI.Gdk.Constants.CURRENT_TIME'.
-- 
-- This is the recommended call to be used as it passes information
-- necessary for sandbox helpers to parent their dialogs properly.
-- 
-- /Since: 3.22/
showUriOnWindow ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.Window.IsWindow a) =>
    Maybe (a)
    -- ^ /@parent@/: parent window
    -> T.Text
    -- ^ /@uri@/: the uri to show
    -> Word32
    -- ^ /@timestamp@/: a timestamp to prevent focus stealing
    -> m ()
    -- ^ /(Can throw 'Data.GI.Base.GError.GError')/
showUriOnWindow parent uri timestamp = liftIO $ do
    maybeParent <- case parent of
        Nothing -> return nullPtr
        Just jParent -> do
            jParent' <- unsafeManagedPtrCastPtr jParent
            return jParent'
    uri' <- textToCString uri
    onException (do
        _ <- propagateGError $ gtk_show_uri_on_window maybeParent uri' timestamp
        whenJust parent touchManagedPtr
        freeMem uri'
        return ()
     ) (do
        freeMem uri'
     )


-- function show_uri
-- Args: [ Arg
--           { argCName = "screen"
--           , argType = TInterface Name { namespace = "Gdk" , name = "Screen" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "screen to show the uri on\n    or %NULL for the default screen"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "uri"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the uri to show" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "timestamp"
--           , argType = TBasicType TUInt32
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a timestamp to prevent focus stealing"
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

foreign import ccall "gtk_show_uri" gtk_show_uri :: 
    Ptr Gdk.Screen.Screen ->                -- screen : TInterface (Name {namespace = "Gdk", name = "Screen"})
    CString ->                              -- uri : TBasicType TUTF8
    Word32 ->                               -- timestamp : TBasicType TUInt32
    Ptr (Ptr GError) ->                     -- error
    IO CInt

{-# DEPRECATED showUri ["(Since version 3.22)","Use 'GI.Gtk.Functions.showUriOnWindow' instead."] #-}
-- | A convenience function for launching the default application
-- to show the uri. Like 'GI.Gtk.Functions.showUriOnWindow', but takes a screen
-- as transient parent instead of a window.
-- 
-- Note that this function is deprecated as it does not pass the necessary
-- information for helpers to parent their dialog properly, when run from
-- sandboxed applications for example.
-- 
-- /Since: 2.14/
showUri ::
    (B.CallStack.HasCallStack, MonadIO m, Gdk.Screen.IsScreen a) =>
    Maybe (a)
    -- ^ /@screen@/: screen to show the uri on
    --     or 'P.Nothing' for the default screen
    -> T.Text
    -- ^ /@uri@/: the uri to show
    -> Word32
    -- ^ /@timestamp@/: a timestamp to prevent focus stealing
    -> m ()
    -- ^ /(Can throw 'Data.GI.Base.GError.GError')/
showUri screen uri timestamp = liftIO $ do
    maybeScreen <- case screen of
        Nothing -> return nullPtr
        Just jScreen -> do
            jScreen' <- unsafeManagedPtrCastPtr jScreen
            return jScreen'
    uri' <- textToCString uri
    onException (do
        _ <- propagateGError $ gtk_show_uri maybeScreen uri' timestamp
        whenJust screen touchManagedPtr
        freeMem uri'
        return ()
     ) (do
        freeMem uri'
     )


-- function set_debug_flags
-- Args: [ Arg
--           { argCName = "flags"
--           , argType = TBasicType TUInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation { rawDocText = Nothing , sinceVersion = Nothing }
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

foreign import ccall "gtk_set_debug_flags" gtk_set_debug_flags :: 
    Word32 ->                               -- flags : TBasicType TUInt
    IO ()

-- | Sets the GTK+ debug flags.
setDebugFlags ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    Word32
    -> m ()
setDebugFlags flags = liftIO $ do
    gtk_set_debug_flags flags
    return ()


-- function selection_remove_all
-- Args: [ Arg
--           { argCName = "widget"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Widget" }
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
-- returnType: Nothing
-- throws : False
-- Skip return : False

foreign import ccall "gtk_selection_remove_all" gtk_selection_remove_all :: 
    Ptr Gtk.Widget.Widget ->                -- widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    IO ()

-- | Removes all handlers and unsets ownership of all
-- selections for a widget. Called when widget is being
-- destroyed. This function will not generally be
-- called by applications.
selectionRemoveAll ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.Widget.IsWidget a) =>
    a
    -- ^ /@widget@/: a t'GI.Gtk.Objects.Widget.Widget'
    -> m ()
selectionRemoveAll widget = liftIO $ do
    widget' <- unsafeManagedPtrCastPtr widget
    gtk_selection_remove_all widget'
    touchManagedPtr widget
    return ()


-- function selection_owner_set_for_display
-- Args: [ Arg
--           { argCName = "display"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "Display" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the #GdkDisplay where the selection is set"
--                 , sinceVersion = Nothing
--                 }
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
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "new selection owner (a #GtkWidget), or %NULL."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "selection"
--           , argType = TInterface Name { namespace = "Gdk" , name = "Atom" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "an interned atom representing the selection to claim."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "time_"
--           , argType = TBasicType TUInt32
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "timestamp with which to claim the selection"
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

foreign import ccall "gtk_selection_owner_set_for_display" gtk_selection_owner_set_for_display :: 
    Ptr Gdk.Display.Display ->              -- display : TInterface (Name {namespace = "Gdk", name = "Display"})
    Ptr Gtk.Widget.Widget ->                -- widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    Ptr Gdk.Atom.Atom ->                    -- selection : TInterface (Name {namespace = "Gdk", name = "Atom"})
    Word32 ->                               -- time_ : TBasicType TUInt32
    IO CInt

-- | Claim ownership of a given selection for a particular widget, or,
-- if /@widget@/ is 'P.Nothing', release ownership of the selection.
-- 
-- /Since: 2.2/
selectionOwnerSetForDisplay ::
    (B.CallStack.HasCallStack, MonadIO m, Gdk.Display.IsDisplay a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@display@/: the t'GI.Gdk.Objects.Display.Display' where the selection is set
    -> Maybe (b)
    -- ^ /@widget@/: new selection owner (a t'GI.Gtk.Objects.Widget.Widget'), or 'P.Nothing'.
    -> Gdk.Atom.Atom
    -- ^ /@selection@/: an interned atom representing the selection to claim.
    -> Word32
    -- ^ /@time_@/: timestamp with which to claim the selection
    -> m Bool
    -- ^ __Returns:__ TRUE if the operation succeeded
selectionOwnerSetForDisplay display widget selection time_ = liftIO $ do
    display' <- unsafeManagedPtrCastPtr display
    maybeWidget <- case widget of
        Nothing -> return nullPtr
        Just jWidget -> do
            jWidget' <- unsafeManagedPtrCastPtr jWidget
            return jWidget'
    selection' <- unsafeManagedPtrGetPtr selection
    result <- gtk_selection_owner_set_for_display display' maybeWidget selection' time_
    let result' = (/= 0) result
    touchManagedPtr display
    whenJust widget touchManagedPtr
    touchManagedPtr selection
    return result'


-- function selection_owner_set
-- Args: [ Arg
--           { argCName = "widget"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Widget" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWidget, or %NULL."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "selection"
--           , argType = TInterface Name { namespace = "Gdk" , name = "Atom" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "an interned atom representing the selection to claim"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "time_"
--           , argType = TBasicType TUInt32
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "timestamp with which to claim the selection"
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

foreign import ccall "gtk_selection_owner_set" gtk_selection_owner_set :: 
    Ptr Gtk.Widget.Widget ->                -- widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    Ptr Gdk.Atom.Atom ->                    -- selection : TInterface (Name {namespace = "Gdk", name = "Atom"})
    Word32 ->                               -- time_ : TBasicType TUInt32
    IO CInt

-- | Claims ownership of a given selection for a particular widget,
-- or, if /@widget@/ is 'P.Nothing', release ownership of the selection.
selectionOwnerSet ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.Widget.IsWidget a) =>
    Maybe (a)
    -- ^ /@widget@/: a t'GI.Gtk.Objects.Widget.Widget', or 'P.Nothing'.
    -> Gdk.Atom.Atom
    -- ^ /@selection@/: an interned atom representing the selection to claim
    -> Word32
    -- ^ /@time_@/: timestamp with which to claim the selection
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the operation succeeded
selectionOwnerSet widget selection time_ = liftIO $ do
    maybeWidget <- case widget of
        Nothing -> return nullPtr
        Just jWidget -> do
            jWidget' <- unsafeManagedPtrCastPtr jWidget
            return jWidget'
    selection' <- unsafeManagedPtrGetPtr selection
    result <- gtk_selection_owner_set maybeWidget selection' time_
    let result' = (/= 0) result
    whenJust widget touchManagedPtr
    touchManagedPtr selection
    return result'


-- function selection_convert
-- Args: [ Arg
--           { argCName = "widget"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Widget" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "The widget which acts as requestor"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "selection"
--           , argType = TInterface Name { namespace = "Gdk" , name = "Atom" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Which selection to get"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "target"
--           , argType = TInterface Name { namespace = "Gdk" , name = "Atom" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Form of information desired (e.g., STRING)"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "time_"
--           , argType = TBasicType TUInt32
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "Time of request (usually of triggering event)\n       In emergency, you could use #GDK_CURRENT_TIME"
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

foreign import ccall "gtk_selection_convert" gtk_selection_convert :: 
    Ptr Gtk.Widget.Widget ->                -- widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    Ptr Gdk.Atom.Atom ->                    -- selection : TInterface (Name {namespace = "Gdk", name = "Atom"})
    Ptr Gdk.Atom.Atom ->                    -- target : TInterface (Name {namespace = "Gdk", name = "Atom"})
    Word32 ->                               -- time_ : TBasicType TUInt32
    IO CInt

-- | Requests the contents of a selection. When received,
-- a “selection-received” signal will be generated.
selectionConvert ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.Widget.IsWidget a) =>
    a
    -- ^ /@widget@/: The widget which acts as requestor
    -> Gdk.Atom.Atom
    -- ^ /@selection@/: Which selection to get
    -> Gdk.Atom.Atom
    -- ^ /@target@/: Form of information desired (e.g., STRING)
    -> Word32
    -- ^ /@time_@/: Time of request (usually of triggering event)
    --        In emergency, you could use 'GI.Gdk.Constants.CURRENT_TIME'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if requested succeeded. 'P.False' if we could not process
    --          request. (e.g., there was already a request in process for
    --          this widget).
selectionConvert widget selection target time_ = liftIO $ do
    widget' <- unsafeManagedPtrCastPtr widget
    selection' <- unsafeManagedPtrGetPtr selection
    target' <- unsafeManagedPtrGetPtr target
    result <- gtk_selection_convert widget' selection' target' time_
    let result' = (/= 0) result
    touchManagedPtr widget
    touchManagedPtr selection
    touchManagedPtr target
    return result'


-- function selection_clear_targets
-- Args: [ Arg
--           { argCName = "widget"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Widget" }
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
--           { argCName = "selection"
--           , argType = TInterface Name { namespace = "Gdk" , name = "Atom" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "an atom representing a selection"
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

foreign import ccall "gtk_selection_clear_targets" gtk_selection_clear_targets :: 
    Ptr Gtk.Widget.Widget ->                -- widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    Ptr Gdk.Atom.Atom ->                    -- selection : TInterface (Name {namespace = "Gdk", name = "Atom"})
    IO ()

-- | Remove all targets registered for the given selection for the
-- widget.
selectionClearTargets ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.Widget.IsWidget a) =>
    a
    -- ^ /@widget@/: a t'GI.Gtk.Objects.Widget.Widget'
    -> Gdk.Atom.Atom
    -- ^ /@selection@/: an atom representing a selection
    -> m ()
selectionClearTargets widget selection = liftIO $ do
    widget' <- unsafeManagedPtrCastPtr widget
    selection' <- unsafeManagedPtrGetPtr selection
    gtk_selection_clear_targets widget' selection'
    touchManagedPtr widget
    touchManagedPtr selection
    return ()


-- function selection_add_targets
-- Args: [ Arg
--           { argCName = "widget"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Widget" }
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
--           { argCName = "selection"
--           , argType = TInterface Name { namespace = "Gdk" , name = "Atom" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the selection" , sinceVersion = Nothing }
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
--                 3
--                 (TInterface Name { namespace = "Gtk" , name = "TargetEntry" })
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a table of targets to add"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "ntargets"
--           , argType = TBasicType TUInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "number of entries in @targets"
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
--              { argCName = "ntargets"
--              , argType = TBasicType TUInt
--              , direction = DirectionIn
--              , mayBeNull = False
--              , argDoc =
--                  Documentation
--                    { rawDocText = Just "number of entries in @targets"
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

foreign import ccall "gtk_selection_add_targets" gtk_selection_add_targets :: 
    Ptr Gtk.Widget.Widget ->                -- widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    Ptr Gdk.Atom.Atom ->                    -- selection : TInterface (Name {namespace = "Gdk", name = "Atom"})
    Ptr Gtk.TargetEntry.TargetEntry ->      -- targets : TCArray False (-1) 3 (TInterface (Name {namespace = "Gtk", name = "TargetEntry"}))
    Word32 ->                               -- ntargets : TBasicType TUInt
    IO ()

-- | Prepends a table of targets to the list of supported targets
-- for a given widget and selection.
selectionAddTargets ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.Widget.IsWidget a) =>
    a
    -- ^ /@widget@/: a t'GI.Gtk.Objects.Widget.Widget'
    -> Gdk.Atom.Atom
    -- ^ /@selection@/: the selection
    -> [Gtk.TargetEntry.TargetEntry]
    -- ^ /@targets@/: a table of targets to add
    -> m ()
selectionAddTargets widget selection targets = liftIO $ do
    let ntargets = fromIntegral $ P.length targets
    widget' <- unsafeManagedPtrCastPtr widget
    selection' <- unsafeManagedPtrGetPtr selection
    targets' <- mapM unsafeManagedPtrGetPtr targets
    targets'' <- packBlockArray 16 targets'
    gtk_selection_add_targets widget' selection' targets'' ntargets
    touchManagedPtr widget
    touchManagedPtr selection
    mapM_ touchManagedPtr targets
    freeMem targets''
    return ()


-- function selection_add_target
-- Args: [ Arg
--           { argCName = "widget"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Widget" }
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
--           { argCName = "selection"
--           , argType = TInterface Name { namespace = "Gdk" , name = "Atom" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the selection" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "target"
--           , argType = TInterface Name { namespace = "Gdk" , name = "Atom" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "target to add." , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "info"
--           , argType = TBasicType TUInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "A unsigned integer which will be passed back to the application."
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

foreign import ccall "gtk_selection_add_target" gtk_selection_add_target :: 
    Ptr Gtk.Widget.Widget ->                -- widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    Ptr Gdk.Atom.Atom ->                    -- selection : TInterface (Name {namespace = "Gdk", name = "Atom"})
    Ptr Gdk.Atom.Atom ->                    -- target : TInterface (Name {namespace = "Gdk", name = "Atom"})
    Word32 ->                               -- info : TBasicType TUInt
    IO ()

-- | Appends a specified target to the list of supported targets for a
-- given widget and selection.
selectionAddTarget ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.Widget.IsWidget a) =>
    a
    -- ^ /@widget@/: a t'GI.Gtk.Objects.Widget.Widget'
    -> Gdk.Atom.Atom
    -- ^ /@selection@/: the selection
    -> Gdk.Atom.Atom
    -- ^ /@target@/: target to add.
    -> Word32
    -- ^ /@info@/: A unsigned integer which will be passed back to the application.
    -> m ()
selectionAddTarget widget selection target info = liftIO $ do
    widget' <- unsafeManagedPtrCastPtr widget
    selection' <- unsafeManagedPtrGetPtr selection
    target' <- unsafeManagedPtrGetPtr target
    gtk_selection_add_target widget' selection' target' info
    touchManagedPtr widget
    touchManagedPtr selection
    touchManagedPtr target
    return ()


-- function rgb_to_hsv
-- Args: [ Arg
--           { argCName = "r"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation { rawDocText = Just "Red" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "g"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Green" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "b"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation { rawDocText = Just "Blue" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "h"
--           , argType = TBasicType TDouble
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Return value for the hue component"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       , Arg
--           { argCName = "s"
--           , argType = TBasicType TDouble
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Return value for the saturation component"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       , Arg
--           { argCName = "v"
--           , argType = TBasicType TDouble
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Return value for the value component"
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
-- returnType: Nothing
-- throws : False
-- Skip return : False

foreign import ccall "gtk_rgb_to_hsv" gtk_rgb_to_hsv :: 
    CDouble ->                              -- r : TBasicType TDouble
    CDouble ->                              -- g : TBasicType TDouble
    CDouble ->                              -- b : TBasicType TDouble
    Ptr CDouble ->                          -- h : TBasicType TDouble
    Ptr CDouble ->                          -- s : TBasicType TDouble
    Ptr CDouble ->                          -- v : TBasicType TDouble
    IO ()

-- | Converts a color from RGB space to HSV.
-- 
-- Input values must be in the [0.0, 1.0] range;
-- output values will be in the same range.
-- 
-- /Since: 2.14/
rgbToHsv ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    Double
    -- ^ /@r@/: Red
    -> Double
    -- ^ /@g@/: Green
    -> Double
    -- ^ /@b@/: Blue
    -> m ((Double, Double, Double))
rgbToHsv r g b = liftIO $ do
    let r' = realToFrac r
    let g' = realToFrac g
    let b' = realToFrac b
    h <- allocMem :: IO (Ptr CDouble)
    s <- allocMem :: IO (Ptr CDouble)
    v <- allocMem :: IO (Ptr CDouble)
    gtk_rgb_to_hsv r' g' b' h s v
    h' <- peek h
    let h'' = realToFrac h'
    s' <- peek s
    let s'' = realToFrac s'
    v' <- peek v
    let v'' = realToFrac v'
    freeMem h
    freeMem s
    freeMem v
    return (h'', s'', v'')


-- function render_slider
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StyleContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyleContext" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "cr"
--           , argType =
--               TInterface Name { namespace = "cairo" , name = "Context" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #cairo_t" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "x"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "X origin of the rectangle"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "y"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Y origin of the rectangle"
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
--                 { rawDocText = Just "rectangle width" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "rectangle height" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "orientation"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Orientation" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "orientation of the slider"
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

foreign import ccall "gtk_render_slider" gtk_render_slider :: 
    Ptr Gtk.StyleContext.StyleContext ->    -- context : TInterface (Name {namespace = "Gtk", name = "StyleContext"})
    Ptr Cairo.Context.Context ->            -- cr : TInterface (Name {namespace = "cairo", name = "Context"})
    CDouble ->                              -- x : TBasicType TDouble
    CDouble ->                              -- y : TBasicType TDouble
    CDouble ->                              -- width : TBasicType TDouble
    CDouble ->                              -- height : TBasicType TDouble
    CUInt ->                                -- orientation : TInterface (Name {namespace = "Gtk", name = "Orientation"})
    IO ()

-- | Renders a slider (as in t'GI.Gtk.Objects.Scale.Scale') in the rectangle defined by /@x@/, /@y@/,
-- /@width@/, /@height@/. /@orientation@/ defines whether the slider is vertical
-- or horizontal.
-- 
-- Typical slider rendering:
-- 
-- <<https://developer.gnome.org/gtk3/stable/sliders.png>>
-- 
-- /Since: 3.0/
renderSlider ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.StyleContext.IsStyleContext a) =>
    a
    -- ^ /@context@/: a t'GI.Gtk.Objects.StyleContext.StyleContext'
    -> Cairo.Context.Context
    -- ^ /@cr@/: a t'GI.Cairo.Structs.Context.Context'
    -> Double
    -- ^ /@x@/: X origin of the rectangle
    -> Double
    -- ^ /@y@/: Y origin of the rectangle
    -> Double
    -- ^ /@width@/: rectangle width
    -> Double
    -- ^ /@height@/: rectangle height
    -> Gtk.Enums.Orientation
    -- ^ /@orientation@/: orientation of the slider
    -> m ()
renderSlider context cr x y width height orientation = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    cr' <- unsafeManagedPtrGetPtr cr
    let x' = realToFrac x
    let y' = realToFrac y
    let width' = realToFrac width
    let height' = realToFrac height
    let orientation' = (fromIntegral . fromEnum) orientation
    gtk_render_slider context' cr' x' y' width' height' orientation'
    touchManagedPtr context
    touchManagedPtr cr
    return ()


-- function render_option
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StyleContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyleContext" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "cr"
--           , argType =
--               TInterface Name { namespace = "cairo" , name = "Context" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #cairo_t" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "x"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "X origin of the rectangle"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "y"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Y origin of the rectangle"
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
--                 { rawDocText = Just "rectangle width" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "rectangle height" , sinceVersion = Nothing }
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

foreign import ccall "gtk_render_option" gtk_render_option :: 
    Ptr Gtk.StyleContext.StyleContext ->    -- context : TInterface (Name {namespace = "Gtk", name = "StyleContext"})
    Ptr Cairo.Context.Context ->            -- cr : TInterface (Name {namespace = "cairo", name = "Context"})
    CDouble ->                              -- x : TBasicType TDouble
    CDouble ->                              -- y : TBasicType TDouble
    CDouble ->                              -- width : TBasicType TDouble
    CDouble ->                              -- height : TBasicType TDouble
    IO ()

-- | Renders an option mark (as in a t'GI.Gtk.Objects.RadioButton.RadioButton'), the 'GI.Gtk.Flags.StateFlagsChecked'
-- state will determine whether the option is on or off, and
-- 'GI.Gtk.Flags.StateFlagsInconsistent' whether it should be marked as undefined.
-- 
-- Typical option mark rendering:
-- 
-- <<https://developer.gnome.org/gtk3/stable/options.png>>
-- 
-- /Since: 3.0/
renderOption ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.StyleContext.IsStyleContext a) =>
    a
    -- ^ /@context@/: a t'GI.Gtk.Objects.StyleContext.StyleContext'
    -> Cairo.Context.Context
    -- ^ /@cr@/: a t'GI.Cairo.Structs.Context.Context'
    -> Double
    -- ^ /@x@/: X origin of the rectangle
    -> Double
    -- ^ /@y@/: Y origin of the rectangle
    -> Double
    -- ^ /@width@/: rectangle width
    -> Double
    -- ^ /@height@/: rectangle height
    -> m ()
renderOption context cr x y width height = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    cr' <- unsafeManagedPtrGetPtr cr
    let x' = realToFrac x
    let y' = realToFrac y
    let width' = realToFrac width
    let height' = realToFrac height
    gtk_render_option context' cr' x' y' width' height'
    touchManagedPtr context
    touchManagedPtr cr
    return ()


-- function render_line
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StyleContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyleContext" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "cr"
--           , argType =
--               TInterface Name { namespace = "cairo" , name = "Context" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #cairo_t" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "x0"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "X coordinate for the origin of the line"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "y0"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Y coordinate for the origin of the line"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "x1"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "X coordinate for the end of the line"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "y1"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Y coordinate for the end of the line"
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

foreign import ccall "gtk_render_line" gtk_render_line :: 
    Ptr Gtk.StyleContext.StyleContext ->    -- context : TInterface (Name {namespace = "Gtk", name = "StyleContext"})
    Ptr Cairo.Context.Context ->            -- cr : TInterface (Name {namespace = "cairo", name = "Context"})
    CDouble ->                              -- x0 : TBasicType TDouble
    CDouble ->                              -- y0 : TBasicType TDouble
    CDouble ->                              -- x1 : TBasicType TDouble
    CDouble ->                              -- y1 : TBasicType TDouble
    IO ()

-- | Renders a line from (x0, y0) to (x1, y1).
-- 
-- /Since: 3.0/
renderLine ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.StyleContext.IsStyleContext a) =>
    a
    -- ^ /@context@/: a t'GI.Gtk.Objects.StyleContext.StyleContext'
    -> Cairo.Context.Context
    -- ^ /@cr@/: a t'GI.Cairo.Structs.Context.Context'
    -> Double
    -- ^ /@x0@/: X coordinate for the origin of the line
    -> Double
    -- ^ /@y0@/: Y coordinate for the origin of the line
    -> Double
    -- ^ /@x1@/: X coordinate for the end of the line
    -> Double
    -- ^ /@y1@/: Y coordinate for the end of the line
    -> m ()
renderLine context cr x0 y0 x1 y1 = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    cr' <- unsafeManagedPtrGetPtr cr
    let x0' = realToFrac x0
    let y0' = realToFrac y0
    let x1' = realToFrac x1
    let y1' = realToFrac y1
    gtk_render_line context' cr' x0' y0' x1' y1'
    touchManagedPtr context
    touchManagedPtr cr
    return ()


-- function render_layout
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StyleContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyleContext" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "cr"
--           , argType =
--               TInterface Name { namespace = "cairo" , name = "Context" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #cairo_t" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "x"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "X origin" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "y"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Y origin" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "layout"
--           , argType =
--               TInterface Name { namespace = "Pango" , name = "Layout" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the #PangoLayout to render"
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

foreign import ccall "gtk_render_layout" gtk_render_layout :: 
    Ptr Gtk.StyleContext.StyleContext ->    -- context : TInterface (Name {namespace = "Gtk", name = "StyleContext"})
    Ptr Cairo.Context.Context ->            -- cr : TInterface (Name {namespace = "cairo", name = "Context"})
    CDouble ->                              -- x : TBasicType TDouble
    CDouble ->                              -- y : TBasicType TDouble
    Ptr Pango.Layout.Layout ->              -- layout : TInterface (Name {namespace = "Pango", name = "Layout"})
    IO ()

-- | Renders /@layout@/ on the coordinates /@x@/, /@y@/
-- 
-- /Since: 3.0/
renderLayout ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.StyleContext.IsStyleContext a, Pango.Layout.IsLayout b) =>
    a
    -- ^ /@context@/: a t'GI.Gtk.Objects.StyleContext.StyleContext'
    -> Cairo.Context.Context
    -- ^ /@cr@/: a t'GI.Cairo.Structs.Context.Context'
    -> Double
    -- ^ /@x@/: X origin
    -> Double
    -- ^ /@y@/: Y origin
    -> b
    -- ^ /@layout@/: the t'GI.Pango.Objects.Layout.Layout' to render
    -> m ()
renderLayout context cr x y layout = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    cr' <- unsafeManagedPtrGetPtr cr
    let x' = realToFrac x
    let y' = realToFrac y
    layout' <- unsafeManagedPtrCastPtr layout
    gtk_render_layout context' cr' x' y' layout'
    touchManagedPtr context
    touchManagedPtr cr
    touchManagedPtr layout
    return ()


-- function render_insertion_cursor
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StyleContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyleContext" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "cr"
--           , argType =
--               TInterface Name { namespace = "cairo" , name = "Context" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #cairo_t" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "x"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "X origin" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "y"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Y origin" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "layout"
--           , argType =
--               TInterface Name { namespace = "Pango" , name = "Layout" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the #PangoLayout of the text"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "index"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the index in the #PangoLayout"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "direction"
--           , argType =
--               TInterface Name { namespace = "Pango" , name = "Direction" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the #PangoDirection of the text"
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

foreign import ccall "gtk_render_insertion_cursor" gtk_render_insertion_cursor :: 
    Ptr Gtk.StyleContext.StyleContext ->    -- context : TInterface (Name {namespace = "Gtk", name = "StyleContext"})
    Ptr Cairo.Context.Context ->            -- cr : TInterface (Name {namespace = "cairo", name = "Context"})
    CDouble ->                              -- x : TBasicType TDouble
    CDouble ->                              -- y : TBasicType TDouble
    Ptr Pango.Layout.Layout ->              -- layout : TInterface (Name {namespace = "Pango", name = "Layout"})
    Int32 ->                                -- index : TBasicType TInt
    CUInt ->                                -- direction : TInterface (Name {namespace = "Pango", name = "Direction"})
    IO ()

-- | Draws a text caret on /@cr@/ at the specified index of /@layout@/.
-- 
-- /Since: 3.4/
renderInsertionCursor ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.StyleContext.IsStyleContext a, Pango.Layout.IsLayout b) =>
    a
    -- ^ /@context@/: a t'GI.Gtk.Objects.StyleContext.StyleContext'
    -> Cairo.Context.Context
    -- ^ /@cr@/: a t'GI.Cairo.Structs.Context.Context'
    -> Double
    -- ^ /@x@/: X origin
    -> Double
    -- ^ /@y@/: Y origin
    -> b
    -- ^ /@layout@/: the t'GI.Pango.Objects.Layout.Layout' of the text
    -> Int32
    -- ^ /@index@/: the index in the t'GI.Pango.Objects.Layout.Layout'
    -> Pango.Enums.Direction
    -- ^ /@direction@/: the t'GI.Pango.Enums.Direction' of the text
    -> m ()
renderInsertionCursor context cr x y layout index direction = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    cr' <- unsafeManagedPtrGetPtr cr
    let x' = realToFrac x
    let y' = realToFrac y
    layout' <- unsafeManagedPtrCastPtr layout
    let direction' = (fromIntegral . fromEnum) direction
    gtk_render_insertion_cursor context' cr' x' y' layout' index direction'
    touchManagedPtr context
    touchManagedPtr cr
    touchManagedPtr layout
    return ()


-- function render_icon_surface
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StyleContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyleContext" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "cr"
--           , argType =
--               TInterface Name { namespace = "cairo" , name = "Context" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #cairo_t" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "surface"
--           , argType =
--               TInterface Name { namespace = "cairo" , name = "Surface" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "a #cairo_surface_t containing the icon to draw"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "x"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "X position for the @icon"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "y"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Y position for the @incon"
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

foreign import ccall "gtk_render_icon_surface" gtk_render_icon_surface :: 
    Ptr Gtk.StyleContext.StyleContext ->    -- context : TInterface (Name {namespace = "Gtk", name = "StyleContext"})
    Ptr Cairo.Context.Context ->            -- cr : TInterface (Name {namespace = "cairo", name = "Context"})
    Ptr Cairo.Surface.Surface ->            -- surface : TInterface (Name {namespace = "cairo", name = "Surface"})
    CDouble ->                              -- x : TBasicType TDouble
    CDouble ->                              -- y : TBasicType TDouble
    IO ()

-- | Renders the icon in /@surface@/ at the specified /@x@/ and /@y@/ coordinates.
-- 
-- /Since: 3.10/
renderIconSurface ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.StyleContext.IsStyleContext a) =>
    a
    -- ^ /@context@/: a t'GI.Gtk.Objects.StyleContext.StyleContext'
    -> Cairo.Context.Context
    -- ^ /@cr@/: a t'GI.Cairo.Structs.Context.Context'
    -> Cairo.Surface.Surface
    -- ^ /@surface@/: a t'GI.Cairo.Structs.Surface.Surface' containing the icon to draw
    -> Double
    -- ^ /@x@/: X position for the /@icon@/
    -> Double
    -- ^ /@y@/: Y position for the /@incon@/
    -> m ()
renderIconSurface context cr surface x y = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    cr' <- unsafeManagedPtrGetPtr cr
    surface' <- unsafeManagedPtrGetPtr surface
    let x' = realToFrac x
    let y' = realToFrac y
    gtk_render_icon_surface context' cr' surface' x' y'
    touchManagedPtr context
    touchManagedPtr cr
    touchManagedPtr surface
    return ()


-- function render_icon_pixbuf
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StyleContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyleContext" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "source"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "IconSource" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "the #GtkIconSource specifying the icon to render"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "size"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the size (#GtkIconSize) to render the icon at.\n       A size of `(GtkIconSize) -1` means render at the size of the source\n       and don\8217t scale."
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

foreign import ccall "gtk_render_icon_pixbuf" gtk_render_icon_pixbuf :: 
    Ptr Gtk.StyleContext.StyleContext ->    -- context : TInterface (Name {namespace = "Gtk", name = "StyleContext"})
    Ptr Gtk.IconSource.IconSource ->        -- source : TInterface (Name {namespace = "Gtk", name = "IconSource"})
    Int32 ->                                -- size : TBasicType TInt
    IO (Ptr GdkPixbuf.Pixbuf.Pixbuf)

{-# DEPRECATED renderIconPixbuf ["(Since version 3.10)","Use 'GI.Gtk.Objects.IconTheme.iconThemeLoadIcon' instead."] #-}
-- | Renders the icon specified by /@source@/ at the given /@size@/, returning the result
-- in a pixbuf.
-- 
-- /Since: 3.0/
renderIconPixbuf ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.StyleContext.IsStyleContext a) =>
    a
    -- ^ /@context@/: a t'GI.Gtk.Objects.StyleContext.StyleContext'
    -> Gtk.IconSource.IconSource
    -- ^ /@source@/: the t'GI.Gtk.Structs.IconSource.IconSource' specifying the icon to render
    -> Int32
    -- ^ /@size@/: the size (t'GI.Gtk.Enums.IconSize') to render the icon at.
    --        A size of @(GtkIconSize) -1@ means render at the size of the source
    --        and don’t scale.
    -> m GdkPixbuf.Pixbuf.Pixbuf
    -- ^ __Returns:__ a newly-created t'GI.GdkPixbuf.Objects.Pixbuf.Pixbuf' containing the rendered icon
renderIconPixbuf context source size = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    source' <- unsafeManagedPtrGetPtr source
    result <- gtk_render_icon_pixbuf context' source' size
    checkUnexpectedReturnNULL "renderIconPixbuf" result
    result' <- (wrapObject GdkPixbuf.Pixbuf.Pixbuf) result
    touchManagedPtr context
    touchManagedPtr source
    return result'


-- function render_icon
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StyleContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyleContext" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "cr"
--           , argType =
--               TInterface Name { namespace = "cairo" , name = "Context" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #cairo_t" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "a #GdkPixbuf containing the icon to draw"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "x"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "X position for the @pixbuf"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "y"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Y position for the @pixbuf"
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

foreign import ccall "gtk_render_icon" gtk_render_icon :: 
    Ptr Gtk.StyleContext.StyleContext ->    -- context : TInterface (Name {namespace = "Gtk", name = "StyleContext"})
    Ptr Cairo.Context.Context ->            -- cr : TInterface (Name {namespace = "cairo", name = "Context"})
    Ptr GdkPixbuf.Pixbuf.Pixbuf ->          -- pixbuf : TInterface (Name {namespace = "GdkPixbuf", name = "Pixbuf"})
    CDouble ->                              -- x : TBasicType TDouble
    CDouble ->                              -- y : TBasicType TDouble
    IO ()

-- | Renders the icon in /@pixbuf@/ at the specified /@x@/ and /@y@/ coordinates.
-- 
-- This function will render the icon in /@pixbuf@/ at exactly its size,
-- regardless of scaling factors, which may not be appropriate when
-- drawing on displays with high pixel densities.
-- 
-- You probably want to use 'GI.Gtk.Functions.renderIconSurface' instead, if you
-- already have a Cairo surface.
-- 
-- /Since: 3.2/
renderIcon ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.StyleContext.IsStyleContext a, GdkPixbuf.Pixbuf.IsPixbuf b) =>
    a
    -- ^ /@context@/: a t'GI.Gtk.Objects.StyleContext.StyleContext'
    -> Cairo.Context.Context
    -- ^ /@cr@/: a t'GI.Cairo.Structs.Context.Context'
    -> b
    -- ^ /@pixbuf@/: a t'GI.GdkPixbuf.Objects.Pixbuf.Pixbuf' containing the icon to draw
    -> Double
    -- ^ /@x@/: X position for the /@pixbuf@/
    -> Double
    -- ^ /@y@/: Y position for the /@pixbuf@/
    -> m ()
renderIcon context cr pixbuf x y = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    cr' <- unsafeManagedPtrGetPtr cr
    pixbuf' <- unsafeManagedPtrCastPtr pixbuf
    let x' = realToFrac x
    let y' = realToFrac y
    gtk_render_icon context' cr' pixbuf' x' y'
    touchManagedPtr context
    touchManagedPtr cr
    touchManagedPtr pixbuf
    return ()


-- function render_handle
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StyleContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyleContext" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "cr"
--           , argType =
--               TInterface Name { namespace = "cairo" , name = "Context" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #cairo_t" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "x"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "X origin of the rectangle"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "y"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Y origin of the rectangle"
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
--                 { rawDocText = Just "rectangle width" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "rectangle height" , sinceVersion = Nothing }
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

foreign import ccall "gtk_render_handle" gtk_render_handle :: 
    Ptr Gtk.StyleContext.StyleContext ->    -- context : TInterface (Name {namespace = "Gtk", name = "StyleContext"})
    Ptr Cairo.Context.Context ->            -- cr : TInterface (Name {namespace = "cairo", name = "Context"})
    CDouble ->                              -- x : TBasicType TDouble
    CDouble ->                              -- y : TBasicType TDouble
    CDouble ->                              -- width : TBasicType TDouble
    CDouble ->                              -- height : TBasicType TDouble
    IO ()

-- | Renders a handle (as in t'GI.Gtk.Objects.HandleBox.HandleBox', t'GI.Gtk.Objects.Paned.Paned' and
-- t'GI.Gtk.Objects.Window.Window'’s resize grip), in the rectangle
-- determined by /@x@/, /@y@/, /@width@/, /@height@/.
-- 
-- Handles rendered for the paned and grip classes:
-- 
-- <<https://developer.gnome.org/gtk3/stable/handles.png>>
-- 
-- /Since: 3.0/
renderHandle ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.StyleContext.IsStyleContext a) =>
    a
    -- ^ /@context@/: a t'GI.Gtk.Objects.StyleContext.StyleContext'
    -> Cairo.Context.Context
    -- ^ /@cr@/: a t'GI.Cairo.Structs.Context.Context'
    -> Double
    -- ^ /@x@/: X origin of the rectangle
    -> Double
    -- ^ /@y@/: Y origin of the rectangle
    -> Double
    -- ^ /@width@/: rectangle width
    -> Double
    -- ^ /@height@/: rectangle height
    -> m ()
renderHandle context cr x y width height = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    cr' <- unsafeManagedPtrGetPtr cr
    let x' = realToFrac x
    let y' = realToFrac y
    let width' = realToFrac width
    let height' = realToFrac height
    gtk_render_handle context' cr' x' y' width' height'
    touchManagedPtr context
    touchManagedPtr cr
    return ()


-- function render_frame_gap
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StyleContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyleContext" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "cr"
--           , argType =
--               TInterface Name { namespace = "cairo" , name = "Context" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #cairo_t" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "x"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "X origin of the rectangle"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "y"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Y origin of the rectangle"
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
--                 { rawDocText = Just "rectangle width" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "rectangle height" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "gap_side"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PositionType" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "side where the gap is"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "xy0_gap"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "initial coordinate (X or Y depending on @gap_side) for the gap"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "xy1_gap"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "end coordinate (X or Y depending on @gap_side) for the gap"
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

foreign import ccall "gtk_render_frame_gap" gtk_render_frame_gap :: 
    Ptr Gtk.StyleContext.StyleContext ->    -- context : TInterface (Name {namespace = "Gtk", name = "StyleContext"})
    Ptr Cairo.Context.Context ->            -- cr : TInterface (Name {namespace = "cairo", name = "Context"})
    CDouble ->                              -- x : TBasicType TDouble
    CDouble ->                              -- y : TBasicType TDouble
    CDouble ->                              -- width : TBasicType TDouble
    CDouble ->                              -- height : TBasicType TDouble
    CUInt ->                                -- gap_side : TInterface (Name {namespace = "Gtk", name = "PositionType"})
    CDouble ->                              -- xy0_gap : TBasicType TDouble
    CDouble ->                              -- xy1_gap : TBasicType TDouble
    IO ()

{-# DEPRECATED renderFrameGap ["(Since version 3.24)","Use 'GI.Gtk.Functions.renderFrame' instead. Themes can create gaps","    by omitting borders via CSS."] #-}
-- | Renders a frame around the rectangle defined by (/@x@/, /@y@/, /@width@/, /@height@/),
-- leaving a gap on one side. /@xy0Gap@/ and /@xy1Gap@/ will mean X coordinates
-- for 'GI.Gtk.Enums.PositionTypeTop' and 'GI.Gtk.Enums.PositionTypeBottom' gap sides, and Y coordinates for
-- 'GI.Gtk.Enums.PositionTypeLeft' and 'GI.Gtk.Enums.PositionTypeRight'.
-- 
-- Typical rendering of a frame with a gap:
-- 
-- <<https://developer.gnome.org/gtk3/stable/frame-gap.png>>
-- 
-- /Since: 3.0/
renderFrameGap ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.StyleContext.IsStyleContext a) =>
    a
    -- ^ /@context@/: a t'GI.Gtk.Objects.StyleContext.StyleContext'
    -> Cairo.Context.Context
    -- ^ /@cr@/: a t'GI.Cairo.Structs.Context.Context'
    -> Double
    -- ^ /@x@/: X origin of the rectangle
    -> Double
    -- ^ /@y@/: Y origin of the rectangle
    -> Double
    -- ^ /@width@/: rectangle width
    -> Double
    -- ^ /@height@/: rectangle height
    -> Gtk.Enums.PositionType
    -- ^ /@gapSide@/: side where the gap is
    -> Double
    -- ^ /@xy0Gap@/: initial coordinate (X or Y depending on /@gapSide@/) for the gap
    -> Double
    -- ^ /@xy1Gap@/: end coordinate (X or Y depending on /@gapSide@/) for the gap
    -> m ()
renderFrameGap context cr x y width height gapSide xy0Gap xy1Gap = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    cr' <- unsafeManagedPtrGetPtr cr
    let x' = realToFrac x
    let y' = realToFrac y
    let width' = realToFrac width
    let height' = realToFrac height
    let gapSide' = (fromIntegral . fromEnum) gapSide
    let xy0Gap' = realToFrac xy0Gap
    let xy1Gap' = realToFrac xy1Gap
    gtk_render_frame_gap context' cr' x' y' width' height' gapSide' xy0Gap' xy1Gap'
    touchManagedPtr context
    touchManagedPtr cr
    return ()


-- function render_frame
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StyleContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyleContext" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "cr"
--           , argType =
--               TInterface Name { namespace = "cairo" , name = "Context" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #cairo_t" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "x"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "X origin of the rectangle"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "y"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Y origin of the rectangle"
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
--                 { rawDocText = Just "rectangle width" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "rectangle height" , sinceVersion = Nothing }
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

foreign import ccall "gtk_render_frame" gtk_render_frame :: 
    Ptr Gtk.StyleContext.StyleContext ->    -- context : TInterface (Name {namespace = "Gtk", name = "StyleContext"})
    Ptr Cairo.Context.Context ->            -- cr : TInterface (Name {namespace = "cairo", name = "Context"})
    CDouble ->                              -- x : TBasicType TDouble
    CDouble ->                              -- y : TBasicType TDouble
    CDouble ->                              -- width : TBasicType TDouble
    CDouble ->                              -- height : TBasicType TDouble
    IO ()

-- | Renders a frame around the rectangle defined by /@x@/, /@y@/, /@width@/, /@height@/.
-- 
-- Examples of frame rendering, showing the effect of @border-image@,
-- @border-color@, @border-width@, @border-radius@ and junctions:
-- 
-- <<https://developer.gnome.org/gtk3/stable/frames.png>>
-- 
-- /Since: 3.0/
renderFrame ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.StyleContext.IsStyleContext a) =>
    a
    -- ^ /@context@/: a t'GI.Gtk.Objects.StyleContext.StyleContext'
    -> Cairo.Context.Context
    -- ^ /@cr@/: a t'GI.Cairo.Structs.Context.Context'
    -> Double
    -- ^ /@x@/: X origin of the rectangle
    -> Double
    -- ^ /@y@/: Y origin of the rectangle
    -> Double
    -- ^ /@width@/: rectangle width
    -> Double
    -- ^ /@height@/: rectangle height
    -> m ()
renderFrame context cr x y width height = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    cr' <- unsafeManagedPtrGetPtr cr
    let x' = realToFrac x
    let y' = realToFrac y
    let width' = realToFrac width
    let height' = realToFrac height
    gtk_render_frame context' cr' x' y' width' height'
    touchManagedPtr context
    touchManagedPtr cr
    return ()


-- function render_focus
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StyleContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyleContext" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "cr"
--           , argType =
--               TInterface Name { namespace = "cairo" , name = "Context" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #cairo_t" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "x"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "X origin of the rectangle"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "y"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Y origin of the rectangle"
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
--                 { rawDocText = Just "rectangle width" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "rectangle height" , sinceVersion = Nothing }
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

foreign import ccall "gtk_render_focus" gtk_render_focus :: 
    Ptr Gtk.StyleContext.StyleContext ->    -- context : TInterface (Name {namespace = "Gtk", name = "StyleContext"})
    Ptr Cairo.Context.Context ->            -- cr : TInterface (Name {namespace = "cairo", name = "Context"})
    CDouble ->                              -- x : TBasicType TDouble
    CDouble ->                              -- y : TBasicType TDouble
    CDouble ->                              -- width : TBasicType TDouble
    CDouble ->                              -- height : TBasicType TDouble
    IO ()

-- | Renders a focus indicator on the rectangle determined by /@x@/, /@y@/, /@width@/, /@height@/.
-- 
-- Typical focus rendering:
-- 
-- <<https://developer.gnome.org/gtk3/stable/focus.png>>
-- 
-- /Since: 3.0/
renderFocus ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.StyleContext.IsStyleContext a) =>
    a
    -- ^ /@context@/: a t'GI.Gtk.Objects.StyleContext.StyleContext'
    -> Cairo.Context.Context
    -- ^ /@cr@/: a t'GI.Cairo.Structs.Context.Context'
    -> Double
    -- ^ /@x@/: X origin of the rectangle
    -> Double
    -- ^ /@y@/: Y origin of the rectangle
    -> Double
    -- ^ /@width@/: rectangle width
    -> Double
    -- ^ /@height@/: rectangle height
    -> m ()
renderFocus context cr x y width height = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    cr' <- unsafeManagedPtrGetPtr cr
    let x' = realToFrac x
    let y' = realToFrac y
    let width' = realToFrac width
    let height' = realToFrac height
    gtk_render_focus context' cr' x' y' width' height'
    touchManagedPtr context
    touchManagedPtr cr
    return ()


-- function render_extension
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StyleContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyleContext" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "cr"
--           , argType =
--               TInterface Name { namespace = "cairo" , name = "Context" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #cairo_t" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "x"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "X origin of the rectangle"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "y"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Y origin of the rectangle"
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
--                 { rawDocText = Just "rectangle width" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "rectangle height" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "gap_side"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PositionType" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "side where the gap is"
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

foreign import ccall "gtk_render_extension" gtk_render_extension :: 
    Ptr Gtk.StyleContext.StyleContext ->    -- context : TInterface (Name {namespace = "Gtk", name = "StyleContext"})
    Ptr Cairo.Context.Context ->            -- cr : TInterface (Name {namespace = "cairo", name = "Context"})
    CDouble ->                              -- x : TBasicType TDouble
    CDouble ->                              -- y : TBasicType TDouble
    CDouble ->                              -- width : TBasicType TDouble
    CDouble ->                              -- height : TBasicType TDouble
    CUInt ->                                -- gap_side : TInterface (Name {namespace = "Gtk", name = "PositionType"})
    IO ()

-- | Renders a extension (as in a t'GI.Gtk.Objects.Notebook.Notebook' tab) in the rectangle
-- defined by /@x@/, /@y@/, /@width@/, /@height@/. The side where the extension
-- connects to is defined by /@gapSide@/.
-- 
-- Typical extension rendering:
-- 
-- <<https://developer.gnome.org/gtk3/stable/extensions.png>>
-- 
-- /Since: 3.0/
renderExtension ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.StyleContext.IsStyleContext a) =>
    a
    -- ^ /@context@/: a t'GI.Gtk.Objects.StyleContext.StyleContext'
    -> Cairo.Context.Context
    -- ^ /@cr@/: a t'GI.Cairo.Structs.Context.Context'
    -> Double
    -- ^ /@x@/: X origin of the rectangle
    -> Double
    -- ^ /@y@/: Y origin of the rectangle
    -> Double
    -- ^ /@width@/: rectangle width
    -> Double
    -- ^ /@height@/: rectangle height
    -> Gtk.Enums.PositionType
    -- ^ /@gapSide@/: side where the gap is
    -> m ()
renderExtension context cr x y width height gapSide = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    cr' <- unsafeManagedPtrGetPtr cr
    let x' = realToFrac x
    let y' = realToFrac y
    let width' = realToFrac width
    let height' = realToFrac height
    let gapSide' = (fromIntegral . fromEnum) gapSide
    gtk_render_extension context' cr' x' y' width' height' gapSide'
    touchManagedPtr context
    touchManagedPtr cr
    return ()


-- function render_expander
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StyleContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyleContext" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "cr"
--           , argType =
--               TInterface Name { namespace = "cairo" , name = "Context" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #cairo_t" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "x"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "X origin of the rectangle"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "y"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Y origin of the rectangle"
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
--                 { rawDocText = Just "rectangle width" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "rectangle height" , sinceVersion = Nothing }
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

foreign import ccall "gtk_render_expander" gtk_render_expander :: 
    Ptr Gtk.StyleContext.StyleContext ->    -- context : TInterface (Name {namespace = "Gtk", name = "StyleContext"})
    Ptr Cairo.Context.Context ->            -- cr : TInterface (Name {namespace = "cairo", name = "Context"})
    CDouble ->                              -- x : TBasicType TDouble
    CDouble ->                              -- y : TBasicType TDouble
    CDouble ->                              -- width : TBasicType TDouble
    CDouble ->                              -- height : TBasicType TDouble
    IO ()

-- | Renders an expander (as used in t'GI.Gtk.Objects.TreeView.TreeView' and t'GI.Gtk.Objects.Expander.Expander') in the area
-- defined by /@x@/, /@y@/, /@width@/, /@height@/. The state 'GI.Gtk.Flags.StateFlagsChecked'
-- determines whether the expander is collapsed or expanded.
-- 
-- Typical expander rendering:
-- 
-- <<https://developer.gnome.org/gtk3/stable/expanders.png>>
-- 
-- /Since: 3.0/
renderExpander ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.StyleContext.IsStyleContext a) =>
    a
    -- ^ /@context@/: a t'GI.Gtk.Objects.StyleContext.StyleContext'
    -> Cairo.Context.Context
    -- ^ /@cr@/: a t'GI.Cairo.Structs.Context.Context'
    -> Double
    -- ^ /@x@/: X origin of the rectangle
    -> Double
    -- ^ /@y@/: Y origin of the rectangle
    -> Double
    -- ^ /@width@/: rectangle width
    -> Double
    -- ^ /@height@/: rectangle height
    -> m ()
renderExpander context cr x y width height = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    cr' <- unsafeManagedPtrGetPtr cr
    let x' = realToFrac x
    let y' = realToFrac y
    let width' = realToFrac width
    let height' = realToFrac height
    gtk_render_expander context' cr' x' y' width' height'
    touchManagedPtr context
    touchManagedPtr cr
    return ()


-- function render_check
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StyleContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyleContext" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "cr"
--           , argType =
--               TInterface Name { namespace = "cairo" , name = "Context" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #cairo_t" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "x"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "X origin of the rectangle"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "y"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Y origin of the rectangle"
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
--                 { rawDocText = Just "rectangle width" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "rectangle height" , sinceVersion = Nothing }
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

foreign import ccall "gtk_render_check" gtk_render_check :: 
    Ptr Gtk.StyleContext.StyleContext ->    -- context : TInterface (Name {namespace = "Gtk", name = "StyleContext"})
    Ptr Cairo.Context.Context ->            -- cr : TInterface (Name {namespace = "cairo", name = "Context"})
    CDouble ->                              -- x : TBasicType TDouble
    CDouble ->                              -- y : TBasicType TDouble
    CDouble ->                              -- width : TBasicType TDouble
    CDouble ->                              -- height : TBasicType TDouble
    IO ()

-- | Renders a checkmark (as in a t'GI.Gtk.Objects.CheckButton.CheckButton').
-- 
-- The 'GI.Gtk.Flags.StateFlagsChecked' state determines whether the check is
-- on or off, and 'GI.Gtk.Flags.StateFlagsInconsistent' determines whether it
-- should be marked as undefined.
-- 
-- Typical checkmark rendering:
-- 
-- <<https://developer.gnome.org/gtk3/stable/checks.png>>
-- 
-- /Since: 3.0/
renderCheck ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.StyleContext.IsStyleContext a) =>
    a
    -- ^ /@context@/: a t'GI.Gtk.Objects.StyleContext.StyleContext'
    -> Cairo.Context.Context
    -- ^ /@cr@/: a t'GI.Cairo.Structs.Context.Context'
    -> Double
    -- ^ /@x@/: X origin of the rectangle
    -> Double
    -- ^ /@y@/: Y origin of the rectangle
    -> Double
    -- ^ /@width@/: rectangle width
    -> Double
    -- ^ /@height@/: rectangle height
    -> m ()
renderCheck context cr x y width height = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    cr' <- unsafeManagedPtrGetPtr cr
    let x' = realToFrac x
    let y' = realToFrac y
    let width' = realToFrac width
    let height' = realToFrac height
    gtk_render_check context' cr' x' y' width' height'
    touchManagedPtr context
    touchManagedPtr cr
    return ()


-- function render_background_get_clip
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StyleContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyleContext" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "x"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "X origin of the rectangle"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "y"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Y origin of the rectangle"
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
--                 { rawDocText = Just "rectangle width" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "rectangle height" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "out_clip"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "Rectangle" }
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "return location for the clip"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = True
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Nothing
-- throws : False
-- Skip return : False

foreign import ccall "gtk_render_background_get_clip" gtk_render_background_get_clip :: 
    Ptr Gtk.StyleContext.StyleContext ->    -- context : TInterface (Name {namespace = "Gtk", name = "StyleContext"})
    CDouble ->                              -- x : TBasicType TDouble
    CDouble ->                              -- y : TBasicType TDouble
    CDouble ->                              -- width : TBasicType TDouble
    CDouble ->                              -- height : TBasicType TDouble
    Ptr Gdk.Rectangle.Rectangle ->          -- out_clip : TInterface (Name {namespace = "Gdk", name = "Rectangle"})
    IO ()

-- | Returns the area that will be affected (i.e. drawn to) when
-- calling 'GI.Gtk.Functions.renderBackground' for the given /@context@/ and
-- rectangle.
-- 
-- /Since: 3.20/
renderBackgroundGetClip ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.StyleContext.IsStyleContext a) =>
    a
    -- ^ /@context@/: a t'GI.Gtk.Objects.StyleContext.StyleContext'
    -> Double
    -- ^ /@x@/: X origin of the rectangle
    -> Double
    -- ^ /@y@/: Y origin of the rectangle
    -> Double
    -- ^ /@width@/: rectangle width
    -> Double
    -- ^ /@height@/: rectangle height
    -> m (Gdk.Rectangle.Rectangle)
renderBackgroundGetClip context x y width height = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    let x' = realToFrac x
    let y' = realToFrac y
    let width' = realToFrac width
    let height' = realToFrac height
    outClip <- SP.callocBoxedBytes 16 :: IO (Ptr Gdk.Rectangle.Rectangle)
    gtk_render_background_get_clip context' x' y' width' height' outClip
    outClip' <- (wrapBoxed Gdk.Rectangle.Rectangle) outClip
    touchManagedPtr context
    return outClip'


-- function render_background
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StyleContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyleContext" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "cr"
--           , argType =
--               TInterface Name { namespace = "cairo" , name = "Context" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #cairo_t" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "x"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "X origin of the rectangle"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "y"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Y origin of the rectangle"
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
--                 { rawDocText = Just "rectangle width" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "rectangle height" , sinceVersion = Nothing }
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

foreign import ccall "gtk_render_background" gtk_render_background :: 
    Ptr Gtk.StyleContext.StyleContext ->    -- context : TInterface (Name {namespace = "Gtk", name = "StyleContext"})
    Ptr Cairo.Context.Context ->            -- cr : TInterface (Name {namespace = "cairo", name = "Context"})
    CDouble ->                              -- x : TBasicType TDouble
    CDouble ->                              -- y : TBasicType TDouble
    CDouble ->                              -- width : TBasicType TDouble
    CDouble ->                              -- height : TBasicType TDouble
    IO ()

-- | Renders the background of an element.
-- 
-- Typical background rendering, showing the effect of
-- @background-image@, @border-width@ and @border-radius@:
-- 
-- <<https://developer.gnome.org/gtk3/stable/background.png>>
-- 
-- /Since: 3.0./
renderBackground ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.StyleContext.IsStyleContext a) =>
    a
    -- ^ /@context@/: a t'GI.Gtk.Objects.StyleContext.StyleContext'
    -> Cairo.Context.Context
    -- ^ /@cr@/: a t'GI.Cairo.Structs.Context.Context'
    -> Double
    -- ^ /@x@/: X origin of the rectangle
    -> Double
    -- ^ /@y@/: Y origin of the rectangle
    -> Double
    -- ^ /@width@/: rectangle width
    -> Double
    -- ^ /@height@/: rectangle height
    -> m ()
renderBackground context cr x y width height = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    cr' <- unsafeManagedPtrGetPtr cr
    let x' = realToFrac x
    let y' = realToFrac y
    let width' = realToFrac width
    let height' = realToFrac height
    gtk_render_background context' cr' x' y' width' height'
    touchManagedPtr context
    touchManagedPtr cr
    return ()


-- function render_arrow
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StyleContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyleContext" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "cr"
--           , argType =
--               TInterface Name { namespace = "cairo" , name = "Context" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #cairo_t" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "angle"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "arrow angle from 0 to 2 * %G_PI, being 0 the arrow pointing to the north"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "x"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "X origin of the render area"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "y"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Y origin of the render area"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "size"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "square side for render area"
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

foreign import ccall "gtk_render_arrow" gtk_render_arrow :: 
    Ptr Gtk.StyleContext.StyleContext ->    -- context : TInterface (Name {namespace = "Gtk", name = "StyleContext"})
    Ptr Cairo.Context.Context ->            -- cr : TInterface (Name {namespace = "cairo", name = "Context"})
    CDouble ->                              -- angle : TBasicType TDouble
    CDouble ->                              -- x : TBasicType TDouble
    CDouble ->                              -- y : TBasicType TDouble
    CDouble ->                              -- size : TBasicType TDouble
    IO ()

-- | Renders an arrow pointing to /@angle@/.
-- 
-- Typical arrow rendering at 0, 1⁄2 π;, π; and 3⁄2 π:
-- 
-- <<https://developer.gnome.org/gtk3/stable/arrows.png>>
-- 
-- /Since: 3.0/
renderArrow ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.StyleContext.IsStyleContext a) =>
    a
    -- ^ /@context@/: a t'GI.Gtk.Objects.StyleContext.StyleContext'
    -> Cairo.Context.Context
    -- ^ /@cr@/: a t'GI.Cairo.Structs.Context.Context'
    -> Double
    -- ^ /@angle@/: arrow angle from 0 to 2 * 'GI.GLib.Constants.PI', being 0 the arrow pointing to the north
    -> Double
    -- ^ /@x@/: X origin of the render area
    -> Double
    -- ^ /@y@/: Y origin of the render area
    -> Double
    -- ^ /@size@/: square side for render area
    -> m ()
renderArrow context cr angle x y size = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    cr' <- unsafeManagedPtrGetPtr cr
    let angle' = realToFrac angle
    let x' = realToFrac x
    let y' = realToFrac y
    let size' = realToFrac size
    gtk_render_arrow context' cr' angle' x' y' size'
    touchManagedPtr context
    touchManagedPtr cr
    return ()


-- function render_activity
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StyleContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyleContext" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "cr"
--           , argType =
--               TInterface Name { namespace = "cairo" , name = "Context" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #cairo_t" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "x"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "X origin of the rectangle"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "y"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Y origin of the rectangle"
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
--                 { rawDocText = Just "rectangle width" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "rectangle height" , sinceVersion = Nothing }
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

foreign import ccall "gtk_render_activity" gtk_render_activity :: 
    Ptr Gtk.StyleContext.StyleContext ->    -- context : TInterface (Name {namespace = "Gtk", name = "StyleContext"})
    Ptr Cairo.Context.Context ->            -- cr : TInterface (Name {namespace = "cairo", name = "Context"})
    CDouble ->                              -- x : TBasicType TDouble
    CDouble ->                              -- y : TBasicType TDouble
    CDouble ->                              -- width : TBasicType TDouble
    CDouble ->                              -- height : TBasicType TDouble
    IO ()

-- | Renders an activity indicator (such as in t'GI.Gtk.Objects.Spinner.Spinner').
-- The state 'GI.Gtk.Flags.StateFlagsChecked' determines whether there is
-- activity going on.
-- 
-- /Since: 3.0/
renderActivity ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.StyleContext.IsStyleContext a) =>
    a
    -- ^ /@context@/: a t'GI.Gtk.Objects.StyleContext.StyleContext'
    -> Cairo.Context.Context
    -- ^ /@cr@/: a t'GI.Cairo.Structs.Context.Context'
    -> Double
    -- ^ /@x@/: X origin of the rectangle
    -> Double
    -- ^ /@y@/: Y origin of the rectangle
    -> Double
    -- ^ /@width@/: rectangle width
    -> Double
    -- ^ /@height@/: rectangle height
    -> m ()
renderActivity context cr x y width height = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    cr' <- unsafeManagedPtrGetPtr cr
    let x' = realToFrac x
    let y' = realToFrac y
    let width' = realToFrac width
    let height' = realToFrac height
    gtk_render_activity context' cr' x' y' width' height'
    touchManagedPtr context
    touchManagedPtr cr
    return ()


-- function rc_set_default_files
-- Args: [ Arg
--           { argCName = "filenames"
--           , argType = TCArray True (-1) (-1) (TBasicType TFileName)
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A\n    %NULL-terminated list of filenames."
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

foreign import ccall "gtk_rc_set_default_files" gtk_rc_set_default_files :: 
    Ptr CString ->                          -- filenames : TCArray True (-1) (-1) (TBasicType TFileName)
    IO ()

{-# DEPRECATED rcSetDefaultFiles ["(Since version 3.0)","Use t'GI.Gtk.Objects.StyleContext.StyleContext' with a custom t'GI.Gtk.Interfaces.StyleProvider.StyleProvider' instead"] #-}
-- | Sets the list of files that GTK+ will read at the
-- end of 'GI.Gtk.Functions.init'.
rcSetDefaultFiles ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    [[Char]]
    -- ^ /@filenames@/: A
    --     'P.Nothing'-terminated list of filenames.
    -> m ()
rcSetDefaultFiles filenames = liftIO $ do
    filenames' <- packZeroTerminatedFileNameArray filenames
    gtk_rc_set_default_files filenames'
    mapZeroTerminatedCArray freeMem filenames'
    freeMem filenames'
    return ()


-- function rc_reset_styles
-- Args: [ Arg
--           { argCName = "settings"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Settings" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkSettings" , sinceVersion = Nothing }
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

foreign import ccall "gtk_rc_reset_styles" gtk_rc_reset_styles :: 
    Ptr Gtk.Settings.Settings ->            -- settings : TInterface (Name {namespace = "Gtk", name = "Settings"})
    IO ()

{-# DEPRECATED rcResetStyles ["(Since version 3.0)","Use t'GI.Gtk.Objects.CssProvider.CssProvider' instead."] #-}
-- | This function recomputes the styles for all widgets that use a
-- particular t'GI.Gtk.Objects.Settings.Settings' object. (There is one t'GI.Gtk.Objects.Settings.Settings' object
-- per t'GI.Gdk.Objects.Screen.Screen', see 'GI.Gtk.Objects.Settings.settingsGetForScreen'); It is useful
-- when some global parameter has changed that affects the appearance
-- of all widgets, because when a widget gets a new style, it will
-- both redraw and recompute any cached information about its
-- appearance. As an example, it is used when the default font size
-- set by the operating system changes. Note that this function
-- doesn’t affect widgets that have a style set explicitly on them
-- with 'GI.Gtk.Objects.Widget.widgetSetStyle'.
-- 
-- /Since: 2.4/
rcResetStyles ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.Settings.IsSettings a) =>
    a
    -- ^ /@settings@/: a t'GI.Gtk.Objects.Settings.Settings'
    -> m ()
rcResetStyles settings = liftIO $ do
    settings' <- unsafeManagedPtrCastPtr settings
    gtk_rc_reset_styles settings'
    touchManagedPtr settings
    return ()


-- function rc_reparse_all_for_settings
-- Args: [ Arg
--           { argCName = "settings"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Settings" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkSettings" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "force_load"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "load whether or not anything changed"
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

foreign import ccall "gtk_rc_reparse_all_for_settings" gtk_rc_reparse_all_for_settings :: 
    Ptr Gtk.Settings.Settings ->            -- settings : TInterface (Name {namespace = "Gtk", name = "Settings"})
    CInt ->                                 -- force_load : TBasicType TBoolean
    IO CInt

{-# DEPRECATED rcReparseAllForSettings ["(Since version 3.0)","Use t'GI.Gtk.Objects.CssProvider.CssProvider' instead."] #-}
-- | If the modification time on any previously read file
-- for the given t'GI.Gtk.Objects.Settings.Settings' has changed, discard all style information
-- and then reread all previously read RC files.
rcReparseAllForSettings ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.Settings.IsSettings a) =>
    a
    -- ^ /@settings@/: a t'GI.Gtk.Objects.Settings.Settings'
    -> Bool
    -- ^ /@forceLoad@/: load whether or not anything changed
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the files were reread.
rcReparseAllForSettings settings forceLoad = liftIO $ do
    settings' <- unsafeManagedPtrCastPtr settings
    let forceLoad' = (fromIntegral . fromEnum) forceLoad
    result <- gtk_rc_reparse_all_for_settings settings' forceLoad'
    let result' = (/= 0) result
    touchManagedPtr settings
    return result'


-- function rc_reparse_all
-- Args: []
-- Lengths: []
-- returnType: Just (TBasicType TBoolean)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_rc_reparse_all" gtk_rc_reparse_all :: 
    IO CInt

{-# DEPRECATED rcReparseAll ["(Since version 3.0)","Use t'GI.Gtk.Objects.CssProvider.CssProvider' instead."] #-}
-- | If the modification time on any previously read file for the
-- default t'GI.Gtk.Objects.Settings.Settings' has changed, discard all style information
-- and then reread all previously read RC files.
rcReparseAll ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m Bool
    -- ^ __Returns:__ 'P.True' if the files were reread.
rcReparseAll  = liftIO $ do
    result <- gtk_rc_reparse_all
    let result' = (/= 0) result
    return result'


-- function rc_parse_string
-- Args: [ Arg
--           { argCName = "rc_string"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a string to parse." , sinceVersion = Nothing }
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

foreign import ccall "gtk_rc_parse_string" gtk_rc_parse_string :: 
    CString ->                              -- rc_string : TBasicType TUTF8
    IO ()

{-# DEPRECATED rcParseString ["(Since version 3.0)","Use t'GI.Gtk.Objects.CssProvider.CssProvider' instead."] #-}
-- | Parses resource information directly from a string.
rcParseString ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    T.Text
    -- ^ /@rcString@/: a string to parse.
    -> m ()
rcParseString rcString = liftIO $ do
    rcString' <- textToCString rcString
    gtk_rc_parse_string rcString'
    freeMem rcString'
    return ()


-- function rc_parse_state
-- Args: [ Arg
--           { argCName = "scanner"
--           , argType =
--               TInterface Name { namespace = "GLib" , name = "Scanner" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "a #GScanner (must be initialized for parsing an RC file)"
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
--               TInterface Name { namespace = "Gtk" , name = "StateType" }
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "A pointer to a #GtkStateType variable in which to\n store the result."
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
-- returnType: Just (TBasicType TUInt)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_rc_parse_state" gtk_rc_parse_state :: 
    Ptr GLib.Scanner.Scanner ->             -- scanner : TInterface (Name {namespace = "GLib", name = "Scanner"})
    Ptr CUInt ->                            -- state : TInterface (Name {namespace = "Gtk", name = "StateType"})
    IO Word32

{-# DEPRECATED rcParseState ["(Since version 3.0)","Use t'GI.Gtk.Objects.CssProvider.CssProvider' instead"] #-}
-- | Parses a t'GI.Gtk.Enums.StateType' variable from the format expected
-- in a RC file.
rcParseState ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    GLib.Scanner.Scanner
    -- ^ /@scanner@/: a t'GI.GLib.Structs.Scanner.Scanner' (must be initialized for parsing an RC file)
    -> m ((Word32, Gtk.Enums.StateType))
    -- ^ __Returns:__ 'GI.GLib.Enums.TokenTypeNone' if parsing succeeded, otherwise the token
    --   that was expected but not found.
rcParseState scanner = liftIO $ do
    scanner' <- unsafeManagedPtrGetPtr scanner
    state <- allocMem :: IO (Ptr CUInt)
    result <- gtk_rc_parse_state scanner' state
    state' <- peek state
    let state'' = (toEnum . fromIntegral) state'
    touchManagedPtr scanner
    freeMem state
    return (result, state'')


-- function rc_parse_priority
-- Args: [ Arg
--           { argCName = "scanner"
--           , argType =
--               TInterface Name { namespace = "GLib" , name = "Scanner" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "a #GScanner (must be initialized for parsing an RC file)"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "priority"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PathPriorityType" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "A pointer to #GtkPathPriorityType variable in which\n to store the result."
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

foreign import ccall "gtk_rc_parse_priority" gtk_rc_parse_priority :: 
    Ptr GLib.Scanner.Scanner ->             -- scanner : TInterface (Name {namespace = "GLib", name = "Scanner"})
    CUInt ->                                -- priority : TInterface (Name {namespace = "Gtk", name = "PathPriorityType"})
    IO Word32

{-# DEPRECATED rcParsePriority ["(Since version 3.0)","Use t'GI.Gtk.Objects.CssProvider.CssProvider' instead"] #-}
-- | Parses a t'GI.Gtk.Enums.PathPriorityType' variable from the format expected
-- in a RC file.
rcParsePriority ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    GLib.Scanner.Scanner
    -- ^ /@scanner@/: a t'GI.GLib.Structs.Scanner.Scanner' (must be initialized for parsing an RC file)
    -> Gtk.Enums.PathPriorityType
    -- ^ /@priority@/: A pointer to t'GI.Gtk.Enums.PathPriorityType' variable in which
    --  to store the result.
    -> m Word32
    -- ^ __Returns:__ 'GI.GLib.Enums.TokenTypeNone' if parsing succeeded, otherwise the token
    --   that was expected but not found.
rcParsePriority scanner priority = liftIO $ do
    scanner' <- unsafeManagedPtrGetPtr scanner
    let priority' = (fromIntegral . fromEnum) priority
    result <- gtk_rc_parse_priority scanner' priority'
    touchManagedPtr scanner
    return result


-- function rc_parse_color_full
-- Args: [ Arg
--           { argCName = "scanner"
--           , argType =
--               TInterface Name { namespace = "GLib" , name = "Scanner" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GScanner" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "style"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "RcStyle" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkRcStyle, or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "color"
--           , argType = TInterface Name { namespace = "Gdk" , name = "Color" }
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "a pointer to a #GdkColor in which to store\n    the result"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = True
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TBasicType TUInt)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_rc_parse_color_full" gtk_rc_parse_color_full :: 
    Ptr GLib.Scanner.Scanner ->             -- scanner : TInterface (Name {namespace = "GLib", name = "Scanner"})
    Ptr Gtk.RcStyle.RcStyle ->              -- style : TInterface (Name {namespace = "Gtk", name = "RcStyle"})
    Ptr Gdk.Color.Color ->                  -- color : TInterface (Name {namespace = "Gdk", name = "Color"})
    IO Word32

{-# DEPRECATED rcParseColorFull ["(Since version 3.0)","Use t'GI.Gtk.Objects.CssProvider.CssProvider' instead"] #-}
-- | Parses a color in the format expected
-- in a RC file. If /@style@/ is not 'P.Nothing', it will be consulted to resolve
-- references to symbolic colors.
-- 
-- /Since: 2.12/
rcParseColorFull ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.RcStyle.IsRcStyle a) =>
    GLib.Scanner.Scanner
    -- ^ /@scanner@/: a t'GI.GLib.Structs.Scanner.Scanner'
    -> Maybe (a)
    -- ^ /@style@/: a t'GI.Gtk.Objects.RcStyle.RcStyle', or 'P.Nothing'
    -> m ((Word32, Gdk.Color.Color))
    -- ^ __Returns:__ 'GI.GLib.Enums.TokenTypeNone' if parsing succeeded, otherwise the token
    --     that was expected but not found
rcParseColorFull scanner style = liftIO $ do
    scanner' <- unsafeManagedPtrGetPtr scanner
    maybeStyle <- case style of
        Nothing -> return nullPtr
        Just jStyle -> do
            jStyle' <- unsafeManagedPtrCastPtr jStyle
            return jStyle'
    color <- SP.callocBoxedBytes 12 :: IO (Ptr Gdk.Color.Color)
    result <- gtk_rc_parse_color_full scanner' maybeStyle color
    color' <- (wrapBoxed Gdk.Color.Color) color
    touchManagedPtr scanner
    whenJust style touchManagedPtr
    return (result, color')


-- function rc_parse_color
-- Args: [ Arg
--           { argCName = "scanner"
--           , argType =
--               TInterface Name { namespace = "GLib" , name = "Scanner" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GScanner" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "color"
--           , argType = TInterface Name { namespace = "Gdk" , name = "Color" }
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "a pointer to a #GdkColor in which to store\n    the result"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = True
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TBasicType TUInt)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_rc_parse_color" gtk_rc_parse_color :: 
    Ptr GLib.Scanner.Scanner ->             -- scanner : TInterface (Name {namespace = "GLib", name = "Scanner"})
    Ptr Gdk.Color.Color ->                  -- color : TInterface (Name {namespace = "Gdk", name = "Color"})
    IO Word32

{-# DEPRECATED rcParseColor ["(Since version 3.0)","Use t'GI.Gtk.Objects.CssProvider.CssProvider' instead"] #-}
-- | Parses a color in the format expected
-- in a RC file.
-- 
-- Note that theme engines should use 'GI.Gtk.Functions.rcParseColorFull' in
-- order to support symbolic colors.
rcParseColor ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    GLib.Scanner.Scanner
    -- ^ /@scanner@/: a t'GI.GLib.Structs.Scanner.Scanner'
    -> m ((Word32, Gdk.Color.Color))
    -- ^ __Returns:__ 'GI.GLib.Enums.TokenTypeNone' if parsing succeeded, otherwise the token
    --     that was expected but not found
rcParseColor scanner = liftIO $ do
    scanner' <- unsafeManagedPtrGetPtr scanner
    color <- SP.callocBoxedBytes 12 :: IO (Ptr Gdk.Color.Color)
    result <- gtk_rc_parse_color scanner' color
    color' <- (wrapBoxed Gdk.Color.Color) color
    touchManagedPtr scanner
    return (result, color')


-- function rc_parse
-- Args: [ Arg
--           { argCName = "filename"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the filename of a file to parse. If @filename is not absolute, it\n is searched in the current directory."
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

foreign import ccall "gtk_rc_parse" gtk_rc_parse :: 
    CString ->                              -- filename : TBasicType TUTF8
    IO ()

{-# DEPRECATED rcParse ["(Since version 3.0)","Use t'GI.Gtk.Objects.CssProvider.CssProvider' instead."] #-}
-- | Parses a given resource file.
rcParse ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    T.Text
    -- ^ /@filename@/: the filename of a file to parse. If /@filename@/ is not absolute, it
    --  is searched in the current directory.
    -> m ()
rcParse filename = liftIO $ do
    filename' <- textToCString filename
    gtk_rc_parse filename'
    freeMem filename'
    return ()


-- function rc_get_theme_dir
-- Args: []
-- Lengths: []
-- returnType: Just (TBasicType TUTF8)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_rc_get_theme_dir" gtk_rc_get_theme_dir :: 
    IO CString

{-# DEPRECATED rcGetThemeDir ["(Since version 3.0)","Use t'GI.Gtk.Objects.CssProvider.CssProvider' instead."] #-}
-- | Returns the standard directory in which themes should
-- be installed. (GTK+ does not actually use this directory
-- itself.)
rcGetThemeDir ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m T.Text
    -- ^ __Returns:__ The directory (must be freed with 'GI.GLib.Functions.free').
rcGetThemeDir  = liftIO $ do
    result <- gtk_rc_get_theme_dir
    checkUnexpectedReturnNULL "rcGetThemeDir" result
    result' <- cstringToText result
    freeMem result
    return result'


-- function rc_get_style_by_paths
-- Args: [ Arg
--           { argCName = "settings"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Settings" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkSettings object"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "widget_path"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the widget path to use when looking up the\n    style, or %NULL if no matching against the widget path should be done"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "class_path"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the class path to use when looking up the style,\n    or %NULL if no matching against the class path should be done."
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
--                 { rawDocText =
--                     Just
--                       "a type that will be used along with parent types of this type\n    when matching against class styles, or #G_TYPE_NONE"
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "Style" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_rc_get_style_by_paths" gtk_rc_get_style_by_paths :: 
    Ptr Gtk.Settings.Settings ->            -- settings : TInterface (Name {namespace = "Gtk", name = "Settings"})
    CString ->                              -- widget_path : TBasicType TUTF8
    CString ->                              -- class_path : TBasicType TUTF8
    CGType ->                               -- type : TBasicType TGType
    IO (Ptr Gtk.Style.Style)

{-# DEPRECATED rcGetStyleByPaths ["(Since version 3.0)","Use t'GI.Gtk.Objects.StyleContext.StyleContext' instead"] #-}
-- | Creates up a t'GI.Gtk.Objects.Style.Style' from styles defined in a RC file by providing
-- the raw components used in matching. This function may be useful
-- when creating pseudo-widgets that should be themed like widgets but
-- don’t actually have corresponding GTK+ widgets. An example of this
-- would be items inside a GNOME canvas widget.
-- 
-- The action of 'GI.Gtk.Functions.rcGetStyle' is similar to:
-- 
-- === /C code/
-- >
-- > gtk_widget_path (widget, NULL, &path, NULL);
-- > gtk_widget_class_path (widget, NULL, &class_path, NULL);
-- > gtk_rc_get_style_by_paths (gtk_widget_get_settings (widget),
-- >                            path, class_path,
-- >                            G_OBJECT_TYPE (widget));
rcGetStyleByPaths ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.Settings.IsSettings a) =>
    a
    -- ^ /@settings@/: a t'GI.Gtk.Objects.Settings.Settings' object
    -> Maybe (T.Text)
    -- ^ /@widgetPath@/: the widget path to use when looking up the
    --     style, or 'P.Nothing' if no matching against the widget path should be done
    -> Maybe (T.Text)
    -- ^ /@classPath@/: the class path to use when looking up the style,
    --     or 'P.Nothing' if no matching against the class path should be done.
    -> GType
    -- ^ /@type@/: a type that will be used along with parent types of this type
    --     when matching against class styles, or @/G_TYPE_NONE/@
    -> m (Maybe Gtk.Style.Style)
    -- ^ __Returns:__ A style created by matching
    --     with the supplied paths, or 'P.Nothing' if nothing matching was
    --     specified and the default style should be used. The returned
    --     value is owned by GTK+ as part of an internal cache, so you
    --     must call 'GI.GObject.Objects.Object.objectRef' on the returned value if you want to
    --     keep a reference to it.
rcGetStyleByPaths settings widgetPath classPath type_ = liftIO $ do
    settings' <- unsafeManagedPtrCastPtr settings
    maybeWidgetPath <- case widgetPath of
        Nothing -> return nullPtr
        Just jWidgetPath -> do
            jWidgetPath' <- textToCString jWidgetPath
            return jWidgetPath'
    maybeClassPath <- case classPath of
        Nothing -> return nullPtr
        Just jClassPath -> do
            jClassPath' <- textToCString jClassPath
            return jClassPath'
    let type_' = gtypeToCGType type_
    result <- gtk_rc_get_style_by_paths settings' maybeWidgetPath maybeClassPath type_'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (newObject Gtk.Style.Style) result'
        return result''
    touchManagedPtr settings
    freeMem maybeWidgetPath
    freeMem maybeClassPath
    return maybeResult


-- function rc_get_style
-- Args: [ Arg
--           { argCName = "widget"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Widget" }
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "Style" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_rc_get_style" gtk_rc_get_style :: 
    Ptr Gtk.Widget.Widget ->                -- widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    IO (Ptr Gtk.Style.Style)

{-# DEPRECATED rcGetStyle ["(Since version 3.0)","Use t'GI.Gtk.Objects.StyleContext.StyleContext' instead"] #-}
-- | Finds all matching RC styles for a given widget,
-- composites them together, and then creates a
-- t'GI.Gtk.Objects.Style.Style' representing the composite appearance.
-- (GTK+ actually keeps a cache of previously
-- created styles, so a new style may not be
-- created.)
rcGetStyle ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.Widget.IsWidget a) =>
    a
    -- ^ /@widget@/: a t'GI.Gtk.Objects.Widget.Widget'
    -> m Gtk.Style.Style
    -- ^ __Returns:__ the resulting style. No refcount is added
    --   to the returned style, so if you want to save this style around,
    --   you should add a reference yourself.
rcGetStyle widget = liftIO $ do
    widget' <- unsafeManagedPtrCastPtr widget
    result <- gtk_rc_get_style widget'
    checkUnexpectedReturnNULL "rcGetStyle" result
    result' <- (newObject Gtk.Style.Style) result
    touchManagedPtr widget
    return result'


-- function rc_get_module_dir
-- Args: []
-- Lengths: []
-- returnType: Just (TBasicType TFileName)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_rc_get_module_dir" gtk_rc_get_module_dir :: 
    IO CString

{-# DEPRECATED rcGetModuleDir ["(Since version 3.0)","Use t'GI.Gtk.Objects.CssProvider.CssProvider' instead."] #-}
-- | Returns a directory in which GTK+ looks for theme engines.
-- For full information about the search for theme engines,
-- see the docs for @GTK_PATH@ in [Running GTK+ Applications][gtk-running].
rcGetModuleDir ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m [Char]
    -- ^ __Returns:__ the directory. (Must be freed with 'GI.GLib.Functions.free')
rcGetModuleDir  = liftIO $ do
    result <- gtk_rc_get_module_dir
    checkUnexpectedReturnNULL "rcGetModuleDir" result
    result' <- cstringToString result
    freeMem result
    return result'


-- function rc_get_im_module_path
-- Args: []
-- Lengths: []
-- returnType: Just (TBasicType TFileName)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_rc_get_im_module_path" gtk_rc_get_im_module_path :: 
    IO CString

{-# DEPRECATED rcGetImModulePath ["(Since version 3.0)","Use t'GI.Gtk.Objects.CssProvider.CssProvider' instead."] #-}
-- | Obtains the path in which to look for IM modules. See the documentation
-- of the @GTK_PATH@
-- environment variable for more details about looking up modules. This
-- function is useful solely for utilities supplied with GTK+ and should
-- not be used by applications under normal circumstances.
rcGetImModulePath ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m [Char]
    -- ^ __Returns:__ a newly-allocated string containing the
    --    path in which to look for IM modules.
rcGetImModulePath  = liftIO $ do
    result <- gtk_rc_get_im_module_path
    checkUnexpectedReturnNULL "rcGetImModulePath" result
    result' <- cstringToString result
    freeMem result
    return result'


-- function rc_get_im_module_file
-- Args: []
-- Lengths: []
-- returnType: Just (TBasicType TFileName)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_rc_get_im_module_file" gtk_rc_get_im_module_file :: 
    IO CString

{-# DEPRECATED rcGetImModuleFile ["(Since version 3.0)","Use t'GI.Gtk.Objects.CssProvider.CssProvider' instead."] #-}
-- | Obtains the path to the IM modules file. See the documentation
-- of the @GTK_IM_MODULE_FILE@
-- environment variable for more details.
rcGetImModuleFile ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m [Char]
    -- ^ __Returns:__ a newly-allocated string containing the
    --    name of the file listing the IM modules available for loading
rcGetImModuleFile  = liftIO $ do
    result <- gtk_rc_get_im_module_file
    checkUnexpectedReturnNULL "rcGetImModuleFile" result
    result' <- cstringToString result
    freeMem result
    return result'


-- function rc_get_default_files
-- Args: []
-- Lengths: []
-- returnType: Just (TCArray True (-1) (-1) (TBasicType TFileName))
-- throws : False
-- Skip return : False

foreign import ccall "gtk_rc_get_default_files" gtk_rc_get_default_files :: 
    IO (Ptr CString)

{-# DEPRECATED rcGetDefaultFiles ["(Since version 3.0)","Use t'GI.Gtk.Objects.StyleContext.StyleContext' instead"] #-}
-- | Retrieves the current list of RC files that will be parsed
-- at the end of 'GI.Gtk.Functions.init'.
rcGetDefaultFiles ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m [[Char]]
    -- ^ __Returns:__ 
    --      A 'P.Nothing'-terminated array of filenames.  This memory is owned
    --     by GTK+ and must not be freed by the application.  If you want
    --     to store this information, you should make a copy.
rcGetDefaultFiles  = liftIO $ do
    result <- gtk_rc_get_default_files
    checkUnexpectedReturnNULL "rcGetDefaultFiles" result
    result' <- unpackZeroTerminatedFileNameArray result
    return result'


-- function rc_find_pixmap_in_path
-- Args: [ Arg
--           { argCName = "settings"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Settings" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkSettings" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "scanner"
--           , argType =
--               TInterface Name { namespace = "GLib" , name = "Scanner" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "Scanner used to get line number information for the\n  warning message, or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "pixmap_file"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "name of the pixmap file to locate."
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
-- returnType: Just (TBasicType TFileName)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_rc_find_pixmap_in_path" gtk_rc_find_pixmap_in_path :: 
    Ptr Gtk.Settings.Settings ->            -- settings : TInterface (Name {namespace = "Gtk", name = "Settings"})
    Ptr GLib.Scanner.Scanner ->             -- scanner : TInterface (Name {namespace = "GLib", name = "Scanner"})
    CString ->                              -- pixmap_file : TBasicType TUTF8
    IO CString

{-# DEPRECATED rcFindPixmapInPath ["(Since version 3.0)","Use t'GI.Gtk.Objects.CssProvider.CssProvider' instead."] #-}
-- | Looks up a file in pixmap path for the specified t'GI.Gtk.Objects.Settings.Settings'.
-- If the file is not found, it outputs a warning message using
-- @/g_warning()/@ and returns 'P.Nothing'.
rcFindPixmapInPath ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.Settings.IsSettings a) =>
    a
    -- ^ /@settings@/: a t'GI.Gtk.Objects.Settings.Settings'
    -> GLib.Scanner.Scanner
    -- ^ /@scanner@/: Scanner used to get line number information for the
    --   warning message, or 'P.Nothing'
    -> T.Text
    -- ^ /@pixmapFile@/: name of the pixmap file to locate.
    -> m [Char]
    -- ^ __Returns:__ the filename.
rcFindPixmapInPath settings scanner pixmapFile = liftIO $ do
    settings' <- unsafeManagedPtrCastPtr settings
    scanner' <- unsafeManagedPtrGetPtr scanner
    pixmapFile' <- textToCString pixmapFile
    result <- gtk_rc_find_pixmap_in_path settings' scanner' pixmapFile'
    checkUnexpectedReturnNULL "rcFindPixmapInPath" result
    result' <- cstringToString result
    freeMem result
    touchManagedPtr settings
    touchManagedPtr scanner
    freeMem pixmapFile'
    return result'


-- function rc_find_module_in_path
-- Args: [ Arg
--           { argCName = "module_file"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "name of a theme engine"
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
-- returnType: Just (TBasicType TFileName)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_rc_find_module_in_path" gtk_rc_find_module_in_path :: 
    CString ->                              -- module_file : TBasicType TUTF8
    IO CString

{-# DEPRECATED rcFindModuleInPath ["(Since version 3.0)","Use t'GI.Gtk.Objects.CssProvider.CssProvider' instead."] #-}
-- | Searches for a theme engine in the GTK+ search path. This function
-- is not useful for applications and should not be used.
rcFindModuleInPath ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    T.Text
    -- ^ /@moduleFile@/: name of a theme engine
    -> m [Char]
    -- ^ __Returns:__ The filename, if found (must be
    --   freed with 'GI.GLib.Functions.free'), otherwise 'P.Nothing'.
rcFindModuleInPath moduleFile = liftIO $ do
    moduleFile' <- textToCString moduleFile
    result <- gtk_rc_find_module_in_path moduleFile'
    checkUnexpectedReturnNULL "rcFindModuleInPath" result
    result' <- cstringToString result
    freeMem result
    freeMem moduleFile'
    return result'


-- function rc_add_default_file
-- Args: [ Arg
--           { argCName = "filename"
--           , argType = TBasicType TFileName
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the pathname to the file. If @filename\n   is not absolute, it is searched in the current directory."
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

foreign import ccall "gtk_rc_add_default_file" gtk_rc_add_default_file :: 
    CString ->                              -- filename : TBasicType TFileName
    IO ()

{-# DEPRECATED rcAddDefaultFile ["(Since version 3.0)","Use t'GI.Gtk.Objects.StyleContext.StyleContext' with a custom t'GI.Gtk.Interfaces.StyleProvider.StyleProvider' instead"] #-}
-- | Adds a file to the list of files to be parsed at the
-- end of 'GI.Gtk.Functions.init'.
rcAddDefaultFile ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    [Char]
    -- ^ /@filename@/: the pathname to the file. If /@filename@/
    --    is not absolute, it is searched in the current directory.
    -> m ()
rcAddDefaultFile filename = liftIO $ do
    filename' <- stringToCString filename
    gtk_rc_add_default_file filename'
    freeMem filename'
    return ()


-- function propagate_event
-- Args: [ Arg
--           { argCName = "widget"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Widget" }
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
--           { argCName = "event"
--           , argType = TInterface Name { namespace = "Gdk" , name = "Event" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "an event" , sinceVersion = Nothing }
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

foreign import ccall "gtk_propagate_event" gtk_propagate_event :: 
    Ptr Gtk.Widget.Widget ->                -- widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    Ptr Gdk.Event.Event ->                  -- event : TInterface (Name {namespace = "Gdk", name = "Event"})
    IO ()

-- | Sends an event to a widget, propagating the event to parent widgets
-- if the event remains unhandled.
-- 
-- Events received by GTK+ from GDK normally begin in 'GI.Gtk.Functions.mainDoEvent'.
-- Depending on the type of event, existence of modal dialogs, grabs, etc.,
-- the event may be propagated; if so, this function is used.
-- 
-- 'GI.Gtk.Functions.propagateEvent' calls 'GI.Gtk.Objects.Widget.widgetEvent' on each widget it
-- decides to send the event to. So 'GI.Gtk.Objects.Widget.widgetEvent' is the lowest-level
-- function; it simply emits the [Widget::event]("GI.Gtk.Objects.Widget#g:signal:event") and possibly an
-- event-specific signal on a widget. 'GI.Gtk.Functions.propagateEvent' is a bit
-- higher-level, and 'GI.Gtk.Functions.mainDoEvent' is the highest level.
-- 
-- All that said, you most likely don’t want to use any of these
-- functions; synthesizing events is rarely needed. There are almost
-- certainly better ways to achieve your goals. For example, use
-- 'GI.Gdk.Objects.Window.windowInvalidateRect' or 'GI.Gtk.Objects.Widget.widgetQueueDraw' instead
-- of making up expose events.
propagateEvent ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.Widget.IsWidget a) =>
    a
    -- ^ /@widget@/: a t'GI.Gtk.Objects.Widget.Widget'
    -> Gdk.Event.Event
    -- ^ /@event@/: an event
    -> m ()
propagateEvent widget event = liftIO $ do
    widget' <- unsafeManagedPtrCastPtr widget
    event' <- unsafeManagedPtrGetPtr event
    gtk_propagate_event widget' event'
    touchManagedPtr widget
    touchManagedPtr event
    return ()


-- function print_run_page_setup_dialog_async
-- Args: [ Arg
--           { argCName = "parent"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "transient parent, or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "page_setup"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PageSetup" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "an existing #GtkPageSetup, or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
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
--           { argCName = "done_cb"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PageSetupDoneFunc" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "a function to call when the user saves\n          the modified page setup"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeAsync
--           , argClosure = 4
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
--                 { rawDocText = Just "user data to pass to @done_cb"
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

foreign import ccall "gtk_print_run_page_setup_dialog_async" gtk_print_run_page_setup_dialog_async :: 
    Ptr Gtk.Window.Window ->                -- parent : TInterface (Name {namespace = "Gtk", name = "Window"})
    Ptr Gtk.PageSetup.PageSetup ->          -- page_setup : TInterface (Name {namespace = "Gtk", name = "PageSetup"})
    Ptr Gtk.PrintSettings.PrintSettings ->  -- settings : TInterface (Name {namespace = "Gtk", name = "PrintSettings"})
    FunPtr Gtk.Callbacks.C_PageSetupDoneFunc -> -- done_cb : TInterface (Name {namespace = "Gtk", name = "PageSetupDoneFunc"})
    Ptr () ->                               -- data : TBasicType TPtr
    IO ()

-- | Runs a page setup dialog, letting the user modify the values from /@pageSetup@/.
-- 
-- In contrast to 'GI.Gtk.Functions.printRunPageSetupDialog', this function  returns after
-- showing the page setup dialog on platforms that support this, and calls /@doneCb@/
-- from a signal handler for the [response](#g:signal:response) signal of the dialog.
-- 
-- /Since: 2.10/
printRunPageSetupDialogAsync ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.Window.IsWindow a, Gtk.PageSetup.IsPageSetup b, Gtk.PrintSettings.IsPrintSettings c) =>
    Maybe (a)
    -- ^ /@parent@/: transient parent, or 'P.Nothing'
    -> Maybe (b)
    -- ^ /@pageSetup@/: an existing t'GI.Gtk.Objects.PageSetup.PageSetup', or 'P.Nothing'
    -> c
    -- ^ /@settings@/: a t'GI.Gtk.Objects.PrintSettings.PrintSettings'
    -> Gtk.Callbacks.PageSetupDoneFunc
    -- ^ /@doneCb@/: a function to call when the user saves
    --           the modified page setup
    -> m ()
printRunPageSetupDialogAsync parent pageSetup settings doneCb = liftIO $ do
    maybeParent <- case parent of
        Nothing -> return nullPtr
        Just jParent -> do
            jParent' <- unsafeManagedPtrCastPtr jParent
            return jParent'
    maybePageSetup <- case pageSetup of
        Nothing -> return nullPtr
        Just jPageSetup -> do
            jPageSetup' <- unsafeManagedPtrCastPtr jPageSetup
            return jPageSetup'
    settings' <- unsafeManagedPtrCastPtr settings
    ptrdoneCb <- callocMem :: IO (Ptr (FunPtr Gtk.Callbacks.C_PageSetupDoneFunc))
    doneCb' <- Gtk.Callbacks.mk_PageSetupDoneFunc (Gtk.Callbacks.wrap_PageSetupDoneFunc (Just ptrdoneCb) (Gtk.Callbacks.drop_closures_PageSetupDoneFunc doneCb))
    poke ptrdoneCb doneCb'
    let data_ = nullPtr
    gtk_print_run_page_setup_dialog_async maybeParent maybePageSetup settings' doneCb' data_
    whenJust parent touchManagedPtr
    whenJust pageSetup touchManagedPtr
    touchManagedPtr settings
    return ()


-- function print_run_page_setup_dialog
-- Args: [ Arg
--           { argCName = "parent"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "transient parent" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "page_setup"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PageSetup" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "an existing #GtkPageSetup"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "PageSetup" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_print_run_page_setup_dialog" gtk_print_run_page_setup_dialog :: 
    Ptr Gtk.Window.Window ->                -- parent : TInterface (Name {namespace = "Gtk", name = "Window"})
    Ptr Gtk.PageSetup.PageSetup ->          -- page_setup : TInterface (Name {namespace = "Gtk", name = "PageSetup"})
    Ptr Gtk.PrintSettings.PrintSettings ->  -- settings : TInterface (Name {namespace = "Gtk", name = "PrintSettings"})
    IO (Ptr Gtk.PageSetup.PageSetup)

-- | Runs a page setup dialog, letting the user modify the values from
-- /@pageSetup@/. If the user cancels the dialog, the returned t'GI.Gtk.Objects.PageSetup.PageSetup'
-- is identical to the passed in /@pageSetup@/, otherwise it contains the
-- modifications done in the dialog.
-- 
-- Note that this function may use a recursive mainloop to show the page
-- setup dialog. See 'GI.Gtk.Functions.printRunPageSetupDialogAsync' if this is
-- a problem.
-- 
-- /Since: 2.10/
printRunPageSetupDialog ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.Window.IsWindow a, Gtk.PageSetup.IsPageSetup b, Gtk.PrintSettings.IsPrintSettings c) =>
    Maybe (a)
    -- ^ /@parent@/: transient parent
    -> Maybe (b)
    -- ^ /@pageSetup@/: an existing t'GI.Gtk.Objects.PageSetup.PageSetup'
    -> c
    -- ^ /@settings@/: a t'GI.Gtk.Objects.PrintSettings.PrintSettings'
    -> m Gtk.PageSetup.PageSetup
    -- ^ __Returns:__ a new t'GI.Gtk.Objects.PageSetup.PageSetup'
printRunPageSetupDialog parent pageSetup settings = liftIO $ do
    maybeParent <- case parent of
        Nothing -> return nullPtr
        Just jParent -> do
            jParent' <- unsafeManagedPtrCastPtr jParent
            return jParent'
    maybePageSetup <- case pageSetup of
        Nothing -> return nullPtr
        Just jPageSetup -> do
            jPageSetup' <- unsafeManagedPtrCastPtr jPageSetup
            return jPageSetup'
    settings' <- unsafeManagedPtrCastPtr settings
    result <- gtk_print_run_page_setup_dialog maybeParent maybePageSetup settings'
    checkUnexpectedReturnNULL "printRunPageSetupDialog" result
    result' <- (wrapObject Gtk.PageSetup.PageSetup) result
    whenJust parent touchManagedPtr
    whenJust pageSetup touchManagedPtr
    touchManagedPtr settings
    return result'


-- function parse_args
-- Args: [ Arg
--           { argCName = "argc"
--           , argType = TBasicType TInt
--           , direction = DirectionInout
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "a pointer to the number of command line arguments"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       , Arg
--           { argCName = "argv"
--           , argType = TCArray False (-1) 0 (TBasicType TUTF8)
--           , direction = DirectionInout
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "a pointer to the array of\n    command line arguments"
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
--              { argCName = "argc"
--              , argType = TBasicType TInt
--              , direction = DirectionInout
--              , mayBeNull = False
--              , argDoc =
--                  Documentation
--                    { rawDocText =
--                        Just "a pointer to the number of command line arguments"
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

foreign import ccall "gtk_parse_args" gtk_parse_args :: 
    Ptr Int32 ->                            -- argc : TBasicType TInt
    Ptr (Ptr CString) ->                    -- argv : TCArray False (-1) 0 (TBasicType TUTF8)
    IO CInt

-- | Parses command line arguments, and initializes global
-- attributes of GTK+, but does not actually open a connection
-- to a display. (See 'GI.Gdk.Objects.Display.displayOpen', 'GI.Gdk.Functions.getDisplayArgName')
-- 
-- Any arguments used by GTK+ or GDK are removed from the array and
-- /@argc@/ and /@argv@/ are updated accordingly.
-- 
-- There is no need to call this function explicitly if you are using
-- 'GI.Gtk.Functions.init', or 'GI.Gtk.Functions.initCheck'.
-- 
-- Note that many aspects of GTK+ require a display connection to
-- function, so this way of initializing GTK+ is really only useful
-- for specialized use cases.
parseArgs ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    [T.Text]
    -- ^ /@argv@/: a pointer to the array of
    --     command line arguments
    -> m ((Bool, [T.Text]))
    -- ^ __Returns:__ 'P.True' if initialization succeeded, otherwise 'P.False'
parseArgs argv = liftIO $ do
    let argc = fromIntegral $ P.length argv
    argc' <- allocMem :: IO (Ptr Int32)
    poke argc' argc
    argv' <- packUTF8CArray argv
    argv'' <- allocMem :: IO (Ptr (Ptr CString))
    poke argv'' argv'
    result <- gtk_parse_args argc' argv''
    argc'' <- peek argc'
    let result' = (/= 0) result
    argv''' <- peek argv''
    argv'''' <- (unpackUTF8CArrayWithLength argc'') argv'''
    (mapCArrayWithLength argc'') freeMem argv'''
    freeMem argv'''
    freeMem argc'
    freeMem argv''
    return (result', argv'''')


-- function paint_vline
-- Args: [ Arg
--           { argCName = "style"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Style" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyle" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "cr"
--           , argType =
--               TInterface Name { namespace = "cairo" , name = "Context" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #cairo_t" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "state_type"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StateType" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a state" , sinceVersion = Nothing }
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
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the widget" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "detail"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a style detail" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "y1_"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the starting y coordinate"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "y2_"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the ending y coordinate"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "x"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the x coordinate" , sinceVersion = Nothing }
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

foreign import ccall "gtk_paint_vline" gtk_paint_vline :: 
    Ptr Gtk.Style.Style ->                  -- style : TInterface (Name {namespace = "Gtk", name = "Style"})
    Ptr Cairo.Context.Context ->            -- cr : TInterface (Name {namespace = "cairo", name = "Context"})
    CUInt ->                                -- state_type : TInterface (Name {namespace = "Gtk", name = "StateType"})
    Ptr Gtk.Widget.Widget ->                -- widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    CString ->                              -- detail : TBasicType TUTF8
    Int32 ->                                -- y1_ : TBasicType TInt
    Int32 ->                                -- y2_ : TBasicType TInt
    Int32 ->                                -- x : TBasicType TInt
    IO ()

{-# DEPRECATED paintVline ["(Since version 3.0)","Use 'GI.Gtk.Functions.renderLine' instead"] #-}
-- | Draws a vertical line from (/@x@/, /@y1_@/) to (/@x@/, /@y2_@/) in /@cr@/
-- using the given style and state.
paintVline ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.Style.IsStyle a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@style@/: a t'GI.Gtk.Objects.Style.Style'
    -> Cairo.Context.Context
    -- ^ /@cr@/: a t'GI.Cairo.Structs.Context.Context'
    -> Gtk.Enums.StateType
    -- ^ /@stateType@/: a state
    -> Maybe (b)
    -- ^ /@widget@/: the widget
    -> Maybe (T.Text)
    -- ^ /@detail@/: a style detail
    -> Int32
    -- ^ /@y1_@/: the starting y coordinate
    -> Int32
    -- ^ /@y2_@/: the ending y coordinate
    -> Int32
    -- ^ /@x@/: the x coordinate
    -> m ()
paintVline style cr stateType widget detail y1_ y2_ x = liftIO $ do
    style' <- unsafeManagedPtrCastPtr style
    cr' <- unsafeManagedPtrGetPtr cr
    let stateType' = (fromIntegral . fromEnum) stateType
    maybeWidget <- case widget of
        Nothing -> return nullPtr
        Just jWidget -> do
            jWidget' <- unsafeManagedPtrCastPtr jWidget
            return jWidget'
    maybeDetail <- case detail of
        Nothing -> return nullPtr
        Just jDetail -> do
            jDetail' <- textToCString jDetail
            return jDetail'
    gtk_paint_vline style' cr' stateType' maybeWidget maybeDetail y1_ y2_ x
    touchManagedPtr style
    touchManagedPtr cr
    whenJust widget touchManagedPtr
    freeMem maybeDetail
    return ()


-- function paint_tab
-- Args: [ Arg
--           { argCName = "style"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Style" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyle" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "cr"
--           , argType =
--               TInterface Name { namespace = "cairo" , name = "Context" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #cairo_t" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "state_type"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StateType" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a state" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "shadow_type"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ShadowType" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the type of shadow to draw"
--                 , sinceVersion = Nothing
--                 }
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
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the widget" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "detail"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a style detail" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "x"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "x origin of the rectangle to draw the tab in"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "y"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "y origin of the rectangle to draw the tab in"
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
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the width of the rectangle to draw the tab in"
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
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "the height of the rectangle to draw the tab in"
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

foreign import ccall "gtk_paint_tab" gtk_paint_tab :: 
    Ptr Gtk.Style.Style ->                  -- style : TInterface (Name {namespace = "Gtk", name = "Style"})
    Ptr Cairo.Context.Context ->            -- cr : TInterface (Name {namespace = "cairo", name = "Context"})
    CUInt ->                                -- state_type : TInterface (Name {namespace = "Gtk", name = "StateType"})
    CUInt ->                                -- shadow_type : TInterface (Name {namespace = "Gtk", name = "ShadowType"})
    Ptr Gtk.Widget.Widget ->                -- widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    CString ->                              -- detail : TBasicType TUTF8
    Int32 ->                                -- x : TBasicType TInt
    Int32 ->                                -- y : TBasicType TInt
    Int32 ->                                -- width : TBasicType TInt
    Int32 ->                                -- height : TBasicType TInt
    IO ()

{-# DEPRECATED paintTab ["(Since version 3.0)","Use cairo instead"] #-}
-- | Draws an option menu tab (i.e. the up and down pointing arrows)
-- in the given rectangle on /@cr@/ using the given parameters.
paintTab ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.Style.IsStyle a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@style@/: a t'GI.Gtk.Objects.Style.Style'
    -> Cairo.Context.Context
    -- ^ /@cr@/: a t'GI.Cairo.Structs.Context.Context'
    -> Gtk.Enums.StateType
    -- ^ /@stateType@/: a state
    -> Gtk.Enums.ShadowType
    -- ^ /@shadowType@/: the type of shadow to draw
    -> Maybe (b)
    -- ^ /@widget@/: the widget
    -> Maybe (T.Text)
    -- ^ /@detail@/: a style detail
    -> Int32
    -- ^ /@x@/: x origin of the rectangle to draw the tab in
    -> Int32
    -- ^ /@y@/: y origin of the rectangle to draw the tab in
    -> Int32
    -- ^ /@width@/: the width of the rectangle to draw the tab in
    -> Int32
    -- ^ /@height@/: the height of the rectangle to draw the tab in
    -> m ()
paintTab style cr stateType shadowType widget detail x y width height = liftIO $ do
    style' <- unsafeManagedPtrCastPtr style
    cr' <- unsafeManagedPtrGetPtr cr
    let stateType' = (fromIntegral . fromEnum) stateType
    let shadowType' = (fromIntegral . fromEnum) shadowType
    maybeWidget <- case widget of
        Nothing -> return nullPtr
        Just jWidget -> do
            jWidget' <- unsafeManagedPtrCastPtr jWidget
            return jWidget'
    maybeDetail <- case detail of
        Nothing -> return nullPtr
        Just jDetail -> do
            jDetail' <- textToCString jDetail
            return jDetail'
    gtk_paint_tab style' cr' stateType' shadowType' maybeWidget maybeDetail x y width height
    touchManagedPtr style
    touchManagedPtr cr
    whenJust widget touchManagedPtr
    freeMem maybeDetail
    return ()


-- function paint_spinner
-- Args: [ Arg
--           { argCName = "style"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Style" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyle" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "cr"
--           , argType =
--               TInterface Name { namespace = "cairo" , name = "Context" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #cairo_t" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "state_type"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StateType" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a state" , sinceVersion = Nothing }
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
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the widget (may be %NULL)"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "detail"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a style detail (may be %NULL)"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "step"
--           , argType = TBasicType TUInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the nth step" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "x"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "the x origin of the rectangle in which to draw the spinner"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "y"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "the y origin of the rectangle in which to draw the spinner"
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
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "the width of the rectangle in which to draw the spinner"
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
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "the height of the rectangle in which to draw the spinner"
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

foreign import ccall "gtk_paint_spinner" gtk_paint_spinner :: 
    Ptr Gtk.Style.Style ->                  -- style : TInterface (Name {namespace = "Gtk", name = "Style"})
    Ptr Cairo.Context.Context ->            -- cr : TInterface (Name {namespace = "cairo", name = "Context"})
    CUInt ->                                -- state_type : TInterface (Name {namespace = "Gtk", name = "StateType"})
    Ptr Gtk.Widget.Widget ->                -- widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    CString ->                              -- detail : TBasicType TUTF8
    Word32 ->                               -- step : TBasicType TUInt
    Int32 ->                                -- x : TBasicType TInt
    Int32 ->                                -- y : TBasicType TInt
    Int32 ->                                -- width : TBasicType TInt
    Int32 ->                                -- height : TBasicType TInt
    IO ()

{-# DEPRECATED paintSpinner ["(Since version 3.0)","Use 'GI.Gtk.Functions.renderIcon' and the t'GI.Gtk.Objects.StyleContext.StyleContext'","  you are drawing instead"] #-}
-- | Draws a spinner on /@window@/ using the given parameters.
paintSpinner ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.Style.IsStyle a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@style@/: a t'GI.Gtk.Objects.Style.Style'
    -> Cairo.Context.Context
    -- ^ /@cr@/: a t'GI.Cairo.Structs.Context.Context'
    -> Gtk.Enums.StateType
    -- ^ /@stateType@/: a state
    -> Maybe (b)
    -- ^ /@widget@/: the widget (may be 'P.Nothing')
    -> Maybe (T.Text)
    -- ^ /@detail@/: a style detail (may be 'P.Nothing')
    -> Word32
    -- ^ /@step@/: the nth step
    -> Int32
    -- ^ /@x@/: the x origin of the rectangle in which to draw the spinner
    -> Int32
    -- ^ /@y@/: the y origin of the rectangle in which to draw the spinner
    -> Int32
    -- ^ /@width@/: the width of the rectangle in which to draw the spinner
    -> Int32
    -- ^ /@height@/: the height of the rectangle in which to draw the spinner
    -> m ()
paintSpinner style cr stateType widget detail step x y width height = liftIO $ do
    style' <- unsafeManagedPtrCastPtr style
    cr' <- unsafeManagedPtrGetPtr cr
    let stateType' = (fromIntegral . fromEnum) stateType
    maybeWidget <- case widget of
        Nothing -> return nullPtr
        Just jWidget -> do
            jWidget' <- unsafeManagedPtrCastPtr jWidget
            return jWidget'
    maybeDetail <- case detail of
        Nothing -> return nullPtr
        Just jDetail -> do
            jDetail' <- textToCString jDetail
            return jDetail'
    gtk_paint_spinner style' cr' stateType' maybeWidget maybeDetail step x y width height
    touchManagedPtr style
    touchManagedPtr cr
    whenJust widget touchManagedPtr
    freeMem maybeDetail
    return ()


-- function paint_slider
-- Args: [ Arg
--           { argCName = "style"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Style" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyle" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "cr"
--           , argType =
--               TInterface Name { namespace = "cairo" , name = "Context" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #cairo_t" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "state_type"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StateType" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a state" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "shadow_type"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ShadowType" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a shadow" , sinceVersion = Nothing }
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
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the widget" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "detail"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a style detail" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "x"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "the x origin of the rectangle in which to draw a slider"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "y"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "the y origin of the rectangle in which to draw a slider"
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
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "the width of the rectangle in which to draw a slider"
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
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "the height of the rectangle in which to draw a slider"
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
--               TInterface Name { namespace = "Gtk" , name = "Orientation" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the orientation to be used"
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

foreign import ccall "gtk_paint_slider" gtk_paint_slider :: 
    Ptr Gtk.Style.Style ->                  -- style : TInterface (Name {namespace = "Gtk", name = "Style"})
    Ptr Cairo.Context.Context ->            -- cr : TInterface (Name {namespace = "cairo", name = "Context"})
    CUInt ->                                -- state_type : TInterface (Name {namespace = "Gtk", name = "StateType"})
    CUInt ->                                -- shadow_type : TInterface (Name {namespace = "Gtk", name = "ShadowType"})
    Ptr Gtk.Widget.Widget ->                -- widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    CString ->                              -- detail : TBasicType TUTF8
    Int32 ->                                -- x : TBasicType TInt
    Int32 ->                                -- y : TBasicType TInt
    Int32 ->                                -- width : TBasicType TInt
    Int32 ->                                -- height : TBasicType TInt
    CUInt ->                                -- orientation : TInterface (Name {namespace = "Gtk", name = "Orientation"})
    IO ()

{-# DEPRECATED paintSlider ["(Since version 3.0)","Use 'GI.Gtk.Functions.renderSlider' instead"] #-}
-- | Draws a slider in the given rectangle on /@cr@/ using the
-- given style and orientation.
paintSlider ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.Style.IsStyle a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@style@/: a t'GI.Gtk.Objects.Style.Style'
    -> Cairo.Context.Context
    -- ^ /@cr@/: a t'GI.Cairo.Structs.Context.Context'
    -> Gtk.Enums.StateType
    -- ^ /@stateType@/: a state
    -> Gtk.Enums.ShadowType
    -- ^ /@shadowType@/: a shadow
    -> Maybe (b)
    -- ^ /@widget@/: the widget
    -> Maybe (T.Text)
    -- ^ /@detail@/: a style detail
    -> Int32
    -- ^ /@x@/: the x origin of the rectangle in which to draw a slider
    -> Int32
    -- ^ /@y@/: the y origin of the rectangle in which to draw a slider
    -> Int32
    -- ^ /@width@/: the width of the rectangle in which to draw a slider
    -> Int32
    -- ^ /@height@/: the height of the rectangle in which to draw a slider
    -> Gtk.Enums.Orientation
    -- ^ /@orientation@/: the orientation to be used
    -> m ()
paintSlider style cr stateType shadowType widget detail x y width height orientation = liftIO $ do
    style' <- unsafeManagedPtrCastPtr style
    cr' <- unsafeManagedPtrGetPtr cr
    let stateType' = (fromIntegral . fromEnum) stateType
    let shadowType' = (fromIntegral . fromEnum) shadowType
    maybeWidget <- case widget of
        Nothing -> return nullPtr
        Just jWidget -> do
            jWidget' <- unsafeManagedPtrCastPtr jWidget
            return jWidget'
    maybeDetail <- case detail of
        Nothing -> return nullPtr
        Just jDetail -> do
            jDetail' <- textToCString jDetail
            return jDetail'
    let orientation' = (fromIntegral . fromEnum) orientation
    gtk_paint_slider style' cr' stateType' shadowType' maybeWidget maybeDetail x y width height orientation'
    touchManagedPtr style
    touchManagedPtr cr
    whenJust widget touchManagedPtr
    freeMem maybeDetail
    return ()


-- function paint_shadow_gap
-- Args: [ Arg
--           { argCName = "style"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Style" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyle" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "cr"
--           , argType =
--               TInterface Name { namespace = "cairo" , name = "Context" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #cairo_t" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "state_type"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StateType" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a state" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "shadow_type"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ShadowType" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "type of shadow to draw"
--                 , sinceVersion = Nothing
--                 }
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
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the widget" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "detail"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a style detail" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "x"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "x origin of the rectangle"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "y"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "y origin of the rectangle"
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
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "width of the rectangle"
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
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "width of the rectangle"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "gap_side"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PositionType" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "side in which to leave the gap"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "gap_x"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "starting position of the gap"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "gap_width"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "width of the gap" , sinceVersion = Nothing }
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

foreign import ccall "gtk_paint_shadow_gap" gtk_paint_shadow_gap :: 
    Ptr Gtk.Style.Style ->                  -- style : TInterface (Name {namespace = "Gtk", name = "Style"})
    Ptr Cairo.Context.Context ->            -- cr : TInterface (Name {namespace = "cairo", name = "Context"})
    CUInt ->                                -- state_type : TInterface (Name {namespace = "Gtk", name = "StateType"})
    CUInt ->                                -- shadow_type : TInterface (Name {namespace = "Gtk", name = "ShadowType"})
    Ptr Gtk.Widget.Widget ->                -- widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    CString ->                              -- detail : TBasicType TUTF8
    Int32 ->                                -- x : TBasicType TInt
    Int32 ->                                -- y : TBasicType TInt
    Int32 ->                                -- width : TBasicType TInt
    Int32 ->                                -- height : TBasicType TInt
    CUInt ->                                -- gap_side : TInterface (Name {namespace = "Gtk", name = "PositionType"})
    Int32 ->                                -- gap_x : TBasicType TInt
    Int32 ->                                -- gap_width : TBasicType TInt
    IO ()

{-# DEPRECATED paintShadowGap ["(Since version 3.0)","Use 'GI.Gtk.Functions.renderFrameGap' instead"] #-}
-- | Draws a shadow around the given rectangle in /@cr@/
-- using the given style and state and shadow type, leaving a
-- gap in one side.
paintShadowGap ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.Style.IsStyle a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@style@/: a t'GI.Gtk.Objects.Style.Style'
    -> Cairo.Context.Context
    -- ^ /@cr@/: a t'GI.Cairo.Structs.Context.Context'
    -> Gtk.Enums.StateType
    -- ^ /@stateType@/: a state
    -> Gtk.Enums.ShadowType
    -- ^ /@shadowType@/: type of shadow to draw
    -> Maybe (b)
    -- ^ /@widget@/: the widget
    -> Maybe (T.Text)
    -- ^ /@detail@/: a style detail
    -> Int32
    -- ^ /@x@/: x origin of the rectangle
    -> Int32
    -- ^ /@y@/: y origin of the rectangle
    -> Int32
    -- ^ /@width@/: width of the rectangle
    -> Int32
    -- ^ /@height@/: width of the rectangle
    -> Gtk.Enums.PositionType
    -- ^ /@gapSide@/: side in which to leave the gap
    -> Int32
    -- ^ /@gapX@/: starting position of the gap
    -> Int32
    -- ^ /@gapWidth@/: width of the gap
    -> m ()
paintShadowGap style cr stateType shadowType widget detail x y width height gapSide gapX gapWidth = liftIO $ do
    style' <- unsafeManagedPtrCastPtr style
    cr' <- unsafeManagedPtrGetPtr cr
    let stateType' = (fromIntegral . fromEnum) stateType
    let shadowType' = (fromIntegral . fromEnum) shadowType
    maybeWidget <- case widget of
        Nothing -> return nullPtr
        Just jWidget -> do
            jWidget' <- unsafeManagedPtrCastPtr jWidget
            return jWidget'
    maybeDetail <- case detail of
        Nothing -> return nullPtr
        Just jDetail -> do
            jDetail' <- textToCString jDetail
            return jDetail'
    let gapSide' = (fromIntegral . fromEnum) gapSide
    gtk_paint_shadow_gap style' cr' stateType' shadowType' maybeWidget maybeDetail x y width height gapSide' gapX gapWidth
    touchManagedPtr style
    touchManagedPtr cr
    whenJust widget touchManagedPtr
    freeMem maybeDetail
    return ()


-- function paint_shadow
-- Args: [ Arg
--           { argCName = "style"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Style" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyle" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "cr"
--           , argType =
--               TInterface Name { namespace = "cairo" , name = "Context" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #cairo_t" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "state_type"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StateType" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a state" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "shadow_type"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ShadowType" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "type of shadow to draw"
--                 , sinceVersion = Nothing
--                 }
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
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the widget" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "detail"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a style detail" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "x"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "x origin of the rectangle"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "y"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "y origin of the rectangle"
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
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "width of the rectangle"
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
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "width of the rectangle"
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

foreign import ccall "gtk_paint_shadow" gtk_paint_shadow :: 
    Ptr Gtk.Style.Style ->                  -- style : TInterface (Name {namespace = "Gtk", name = "Style"})
    Ptr Cairo.Context.Context ->            -- cr : TInterface (Name {namespace = "cairo", name = "Context"})
    CUInt ->                                -- state_type : TInterface (Name {namespace = "Gtk", name = "StateType"})
    CUInt ->                                -- shadow_type : TInterface (Name {namespace = "Gtk", name = "ShadowType"})
    Ptr Gtk.Widget.Widget ->                -- widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    CString ->                              -- detail : TBasicType TUTF8
    Int32 ->                                -- x : TBasicType TInt
    Int32 ->                                -- y : TBasicType TInt
    Int32 ->                                -- width : TBasicType TInt
    Int32 ->                                -- height : TBasicType TInt
    IO ()

{-# DEPRECATED paintShadow ["(Since version 3.0)","Use 'GI.Gtk.Functions.renderFrame' instead"] #-}
-- | Draws a shadow around the given rectangle in /@cr@/
-- using the given style and state and shadow type.
paintShadow ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.Style.IsStyle a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@style@/: a t'GI.Gtk.Objects.Style.Style'
    -> Cairo.Context.Context
    -- ^ /@cr@/: a t'GI.Cairo.Structs.Context.Context'
    -> Gtk.Enums.StateType
    -- ^ /@stateType@/: a state
    -> Gtk.Enums.ShadowType
    -- ^ /@shadowType@/: type of shadow to draw
    -> Maybe (b)
    -- ^ /@widget@/: the widget
    -> Maybe (T.Text)
    -- ^ /@detail@/: a style detail
    -> Int32
    -- ^ /@x@/: x origin of the rectangle
    -> Int32
    -- ^ /@y@/: y origin of the rectangle
    -> Int32
    -- ^ /@width@/: width of the rectangle
    -> Int32
    -- ^ /@height@/: width of the rectangle
    -> m ()
paintShadow style cr stateType shadowType widget detail x y width height = liftIO $ do
    style' <- unsafeManagedPtrCastPtr style
    cr' <- unsafeManagedPtrGetPtr cr
    let stateType' = (fromIntegral . fromEnum) stateType
    let shadowType' = (fromIntegral . fromEnum) shadowType
    maybeWidget <- case widget of
        Nothing -> return nullPtr
        Just jWidget -> do
            jWidget' <- unsafeManagedPtrCastPtr jWidget
            return jWidget'
    maybeDetail <- case detail of
        Nothing -> return nullPtr
        Just jDetail -> do
            jDetail' <- textToCString jDetail
            return jDetail'
    gtk_paint_shadow style' cr' stateType' shadowType' maybeWidget maybeDetail x y width height
    touchManagedPtr style
    touchManagedPtr cr
    whenJust widget touchManagedPtr
    freeMem maybeDetail
    return ()


-- function paint_resize_grip
-- Args: [ Arg
--           { argCName = "style"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Style" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyle" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "cr"
--           , argType =
--               TInterface Name { namespace = "cairo" , name = "Context" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #cairo_t" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "state_type"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StateType" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a state" , sinceVersion = Nothing }
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
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the widget" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "detail"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a style detail" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "edge"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "WindowEdge" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the edge in which to draw the resize grip"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "x"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the x origin of the rectangle in which to draw the resize grip"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "y"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the y origin of the rectangle in which to draw the resize grip"
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
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "the width of the rectangle in which to draw the resize grip"
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
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "the height of the rectangle in which to draw the resize grip"
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

foreign import ccall "gtk_paint_resize_grip" gtk_paint_resize_grip :: 
    Ptr Gtk.Style.Style ->                  -- style : TInterface (Name {namespace = "Gtk", name = "Style"})
    Ptr Cairo.Context.Context ->            -- cr : TInterface (Name {namespace = "cairo", name = "Context"})
    CUInt ->                                -- state_type : TInterface (Name {namespace = "Gtk", name = "StateType"})
    Ptr Gtk.Widget.Widget ->                -- widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    CString ->                              -- detail : TBasicType TUTF8
    CUInt ->                                -- edge : TInterface (Name {namespace = "Gdk", name = "WindowEdge"})
    Int32 ->                                -- x : TBasicType TInt
    Int32 ->                                -- y : TBasicType TInt
    Int32 ->                                -- width : TBasicType TInt
    Int32 ->                                -- height : TBasicType TInt
    IO ()

{-# DEPRECATED paintResizeGrip ["(Since version 3.0)","Use 'GI.Gtk.Functions.renderHandle' instead"] #-}
-- | Draws a resize grip in the given rectangle on /@cr@/ using the given
-- parameters.
paintResizeGrip ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.Style.IsStyle a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@style@/: a t'GI.Gtk.Objects.Style.Style'
    -> Cairo.Context.Context
    -- ^ /@cr@/: a t'GI.Cairo.Structs.Context.Context'
    -> Gtk.Enums.StateType
    -- ^ /@stateType@/: a state
    -> Maybe (b)
    -- ^ /@widget@/: the widget
    -> Maybe (T.Text)
    -- ^ /@detail@/: a style detail
    -> Gdk.Enums.WindowEdge
    -- ^ /@edge@/: the edge in which to draw the resize grip
    -> Int32
    -- ^ /@x@/: the x origin of the rectangle in which to draw the resize grip
    -> Int32
    -- ^ /@y@/: the y origin of the rectangle in which to draw the resize grip
    -> Int32
    -- ^ /@width@/: the width of the rectangle in which to draw the resize grip
    -> Int32
    -- ^ /@height@/: the height of the rectangle in which to draw the resize grip
    -> m ()
paintResizeGrip style cr stateType widget detail edge x y width height = liftIO $ do
    style' <- unsafeManagedPtrCastPtr style
    cr' <- unsafeManagedPtrGetPtr cr
    let stateType' = (fromIntegral . fromEnum) stateType
    maybeWidget <- case widget of
        Nothing -> return nullPtr
        Just jWidget -> do
            jWidget' <- unsafeManagedPtrCastPtr jWidget
            return jWidget'
    maybeDetail <- case detail of
        Nothing -> return nullPtr
        Just jDetail -> do
            jDetail' <- textToCString jDetail
            return jDetail'
    let edge' = (fromIntegral . fromEnum) edge
    gtk_paint_resize_grip style' cr' stateType' maybeWidget maybeDetail edge' x y width height
    touchManagedPtr style
    touchManagedPtr cr
    whenJust widget touchManagedPtr
    freeMem maybeDetail
    return ()


-- function paint_option
-- Args: [ Arg
--           { argCName = "style"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Style" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyle" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "cr"
--           , argType =
--               TInterface Name { namespace = "cairo" , name = "Context" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #cairo_t" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "state_type"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StateType" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a state" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "shadow_type"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ShadowType" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the type of shadow to draw"
--                 , sinceVersion = Nothing
--                 }
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
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the widget" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "detail"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a style detail" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "x"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "x origin of the rectangle to draw the option in"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "y"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "y origin of the rectangle to draw the option in"
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
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "the width of the rectangle to draw the option in"
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
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "the height of the rectangle to draw the option in"
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

foreign import ccall "gtk_paint_option" gtk_paint_option :: 
    Ptr Gtk.Style.Style ->                  -- style : TInterface (Name {namespace = "Gtk", name = "Style"})
    Ptr Cairo.Context.Context ->            -- cr : TInterface (Name {namespace = "cairo", name = "Context"})
    CUInt ->                                -- state_type : TInterface (Name {namespace = "Gtk", name = "StateType"})
    CUInt ->                                -- shadow_type : TInterface (Name {namespace = "Gtk", name = "ShadowType"})
    Ptr Gtk.Widget.Widget ->                -- widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    CString ->                              -- detail : TBasicType TUTF8
    Int32 ->                                -- x : TBasicType TInt
    Int32 ->                                -- y : TBasicType TInt
    Int32 ->                                -- width : TBasicType TInt
    Int32 ->                                -- height : TBasicType TInt
    IO ()

{-# DEPRECATED paintOption ["(Since version 3.0)","Use 'GI.Gtk.Functions.renderOption' instead"] #-}
-- | Draws a radio button indicator in the given rectangle on /@cr@/ with
-- the given parameters.
paintOption ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.Style.IsStyle a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@style@/: a t'GI.Gtk.Objects.Style.Style'
    -> Cairo.Context.Context
    -- ^ /@cr@/: a t'GI.Cairo.Structs.Context.Context'
    -> Gtk.Enums.StateType
    -- ^ /@stateType@/: a state
    -> Gtk.Enums.ShadowType
    -- ^ /@shadowType@/: the type of shadow to draw
    -> Maybe (b)
    -- ^ /@widget@/: the widget
    -> Maybe (T.Text)
    -- ^ /@detail@/: a style detail
    -> Int32
    -- ^ /@x@/: x origin of the rectangle to draw the option in
    -> Int32
    -- ^ /@y@/: y origin of the rectangle to draw the option in
    -> Int32
    -- ^ /@width@/: the width of the rectangle to draw the option in
    -> Int32
    -- ^ /@height@/: the height of the rectangle to draw the option in
    -> m ()
paintOption style cr stateType shadowType widget detail x y width height = liftIO $ do
    style' <- unsafeManagedPtrCastPtr style
    cr' <- unsafeManagedPtrGetPtr cr
    let stateType' = (fromIntegral . fromEnum) stateType
    let shadowType' = (fromIntegral . fromEnum) shadowType
    maybeWidget <- case widget of
        Nothing -> return nullPtr
        Just jWidget -> do
            jWidget' <- unsafeManagedPtrCastPtr jWidget
            return jWidget'
    maybeDetail <- case detail of
        Nothing -> return nullPtr
        Just jDetail -> do
            jDetail' <- textToCString jDetail
            return jDetail'
    gtk_paint_option style' cr' stateType' shadowType' maybeWidget maybeDetail x y width height
    touchManagedPtr style
    touchManagedPtr cr
    whenJust widget touchManagedPtr
    freeMem maybeDetail
    return ()


-- function paint_layout
-- Args: [ Arg
--           { argCName = "style"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Style" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyle" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "cr"
--           , argType =
--               TInterface Name { namespace = "cairo" , name = "Context" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #cairo_t" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "state_type"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StateType" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a state" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "use_text"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "whether to use the text or foreground\n           graphics context of @style"
--                 , sinceVersion = Nothing
--                 }
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
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the widget" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "detail"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a style detail" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "x"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "x origin" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "y"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "y origin" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "layout"
--           , argType =
--               TInterface Name { namespace = "Pango" , name = "Layout" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the layout to draw" , sinceVersion = Nothing }
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

foreign import ccall "gtk_paint_layout" gtk_paint_layout :: 
    Ptr Gtk.Style.Style ->                  -- style : TInterface (Name {namespace = "Gtk", name = "Style"})
    Ptr Cairo.Context.Context ->            -- cr : TInterface (Name {namespace = "cairo", name = "Context"})
    CUInt ->                                -- state_type : TInterface (Name {namespace = "Gtk", name = "StateType"})
    CInt ->                                 -- use_text : TBasicType TBoolean
    Ptr Gtk.Widget.Widget ->                -- widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    CString ->                              -- detail : TBasicType TUTF8
    Int32 ->                                -- x : TBasicType TInt
    Int32 ->                                -- y : TBasicType TInt
    Ptr Pango.Layout.Layout ->              -- layout : TInterface (Name {namespace = "Pango", name = "Layout"})
    IO ()

{-# DEPRECATED paintLayout ["(Since version 3.0)","Use 'GI.Gtk.Functions.renderLayout' instead"] #-}
-- | Draws a layout on /@cr@/ using the given parameters.
paintLayout ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.Style.IsStyle a, Gtk.Widget.IsWidget b, Pango.Layout.IsLayout c) =>
    a
    -- ^ /@style@/: a t'GI.Gtk.Objects.Style.Style'
    -> Cairo.Context.Context
    -- ^ /@cr@/: a t'GI.Cairo.Structs.Context.Context'
    -> Gtk.Enums.StateType
    -- ^ /@stateType@/: a state
    -> Bool
    -- ^ /@useText@/: whether to use the text or foreground
    --            graphics context of /@style@/
    -> Maybe (b)
    -- ^ /@widget@/: the widget
    -> Maybe (T.Text)
    -- ^ /@detail@/: a style detail
    -> Int32
    -- ^ /@x@/: x origin
    -> Int32
    -- ^ /@y@/: y origin
    -> c
    -- ^ /@layout@/: the layout to draw
    -> m ()
paintLayout style cr stateType useText widget detail x y layout = liftIO $ do
    style' <- unsafeManagedPtrCastPtr style
    cr' <- unsafeManagedPtrGetPtr cr
    let stateType' = (fromIntegral . fromEnum) stateType
    let useText' = (fromIntegral . fromEnum) useText
    maybeWidget <- case widget of
        Nothing -> return nullPtr
        Just jWidget -> do
            jWidget' <- unsafeManagedPtrCastPtr jWidget
            return jWidget'
    maybeDetail <- case detail of
        Nothing -> return nullPtr
        Just jDetail -> do
            jDetail' <- textToCString jDetail
            return jDetail'
    layout' <- unsafeManagedPtrCastPtr layout
    gtk_paint_layout style' cr' stateType' useText' maybeWidget maybeDetail x y layout'
    touchManagedPtr style
    touchManagedPtr cr
    whenJust widget touchManagedPtr
    touchManagedPtr layout
    freeMem maybeDetail
    return ()


-- function paint_hline
-- Args: [ Arg
--           { argCName = "style"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Style" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyle" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "cr"
--           , argType =
--               TInterface Name { namespace = "cairo" , name = "Context" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #caio_t" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "state_type"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StateType" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a state" , sinceVersion = Nothing }
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
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the widget" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "detail"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a style detail" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "x1"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the starting x coordinate"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "x2"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the ending x coordinate"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "y"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the y coordinate" , sinceVersion = Nothing }
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

foreign import ccall "gtk_paint_hline" gtk_paint_hline :: 
    Ptr Gtk.Style.Style ->                  -- style : TInterface (Name {namespace = "Gtk", name = "Style"})
    Ptr Cairo.Context.Context ->            -- cr : TInterface (Name {namespace = "cairo", name = "Context"})
    CUInt ->                                -- state_type : TInterface (Name {namespace = "Gtk", name = "StateType"})
    Ptr Gtk.Widget.Widget ->                -- widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    CString ->                              -- detail : TBasicType TUTF8
    Int32 ->                                -- x1 : TBasicType TInt
    Int32 ->                                -- x2 : TBasicType TInt
    Int32 ->                                -- y : TBasicType TInt
    IO ()

{-# DEPRECATED paintHline ["(Since version 3.0)","Use 'GI.Gtk.Functions.renderLine' instead"] #-}
-- | Draws a horizontal line from (/@x1@/, /@y@/) to (/@x2@/, /@y@/) in /@cr@/
-- using the given style and state.
paintHline ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.Style.IsStyle a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@style@/: a t'GI.Gtk.Objects.Style.Style'
    -> Cairo.Context.Context
    -- ^ /@cr@/: a @/caio_t/@
    -> Gtk.Enums.StateType
    -- ^ /@stateType@/: a state
    -> Maybe (b)
    -- ^ /@widget@/: the widget
    -> Maybe (T.Text)
    -- ^ /@detail@/: a style detail
    -> Int32
    -- ^ /@x1@/: the starting x coordinate
    -> Int32
    -- ^ /@x2@/: the ending x coordinate
    -> Int32
    -- ^ /@y@/: the y coordinate
    -> m ()
paintHline style cr stateType widget detail x1 x2 y = liftIO $ do
    style' <- unsafeManagedPtrCastPtr style
    cr' <- unsafeManagedPtrGetPtr cr
    let stateType' = (fromIntegral . fromEnum) stateType
    maybeWidget <- case widget of
        Nothing -> return nullPtr
        Just jWidget -> do
            jWidget' <- unsafeManagedPtrCastPtr jWidget
            return jWidget'
    maybeDetail <- case detail of
        Nothing -> return nullPtr
        Just jDetail -> do
            jDetail' <- textToCString jDetail
            return jDetail'
    gtk_paint_hline style' cr' stateType' maybeWidget maybeDetail x1 x2 y
    touchManagedPtr style
    touchManagedPtr cr
    whenJust widget touchManagedPtr
    freeMem maybeDetail
    return ()


-- function paint_handle
-- Args: [ Arg
--           { argCName = "style"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Style" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyle" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "cr"
--           , argType =
--               TInterface Name { namespace = "cairo" , name = "Context" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #cairo_t" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "state_type"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StateType" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a state" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "shadow_type"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ShadowType" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "type of shadow to draw"
--                 , sinceVersion = Nothing
--                 }
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
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the widget" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "detail"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a style detail" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "x"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "x origin of the handle"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "y"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "y origin of the handle"
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
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "with of the handle" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "height"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "height of the handle"
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
--               TInterface Name { namespace = "Gtk" , name = "Orientation" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the orientation of the handle"
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

foreign import ccall "gtk_paint_handle" gtk_paint_handle :: 
    Ptr Gtk.Style.Style ->                  -- style : TInterface (Name {namespace = "Gtk", name = "Style"})
    Ptr Cairo.Context.Context ->            -- cr : TInterface (Name {namespace = "cairo", name = "Context"})
    CUInt ->                                -- state_type : TInterface (Name {namespace = "Gtk", name = "StateType"})
    CUInt ->                                -- shadow_type : TInterface (Name {namespace = "Gtk", name = "ShadowType"})
    Ptr Gtk.Widget.Widget ->                -- widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    CString ->                              -- detail : TBasicType TUTF8
    Int32 ->                                -- x : TBasicType TInt
    Int32 ->                                -- y : TBasicType TInt
    Int32 ->                                -- width : TBasicType TInt
    Int32 ->                                -- height : TBasicType TInt
    CUInt ->                                -- orientation : TInterface (Name {namespace = "Gtk", name = "Orientation"})
    IO ()

{-# DEPRECATED paintHandle ["(Since version 3.0)","Use 'GI.Gtk.Functions.renderHandle' instead"] #-}
-- | Draws a handle as used in t'GI.Gtk.Objects.HandleBox.HandleBox' and t'GI.Gtk.Objects.Paned.Paned'.
paintHandle ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.Style.IsStyle a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@style@/: a t'GI.Gtk.Objects.Style.Style'
    -> Cairo.Context.Context
    -- ^ /@cr@/: a t'GI.Cairo.Structs.Context.Context'
    -> Gtk.Enums.StateType
    -- ^ /@stateType@/: a state
    -> Gtk.Enums.ShadowType
    -- ^ /@shadowType@/: type of shadow to draw
    -> Maybe (b)
    -- ^ /@widget@/: the widget
    -> Maybe (T.Text)
    -- ^ /@detail@/: a style detail
    -> Int32
    -- ^ /@x@/: x origin of the handle
    -> Int32
    -- ^ /@y@/: y origin of the handle
    -> Int32
    -- ^ /@width@/: with of the handle
    -> Int32
    -- ^ /@height@/: height of the handle
    -> Gtk.Enums.Orientation
    -- ^ /@orientation@/: the orientation of the handle
    -> m ()
paintHandle style cr stateType shadowType widget detail x y width height orientation = liftIO $ do
    style' <- unsafeManagedPtrCastPtr style
    cr' <- unsafeManagedPtrGetPtr cr
    let stateType' = (fromIntegral . fromEnum) stateType
    let shadowType' = (fromIntegral . fromEnum) shadowType
    maybeWidget <- case widget of
        Nothing -> return nullPtr
        Just jWidget -> do
            jWidget' <- unsafeManagedPtrCastPtr jWidget
            return jWidget'
    maybeDetail <- case detail of
        Nothing -> return nullPtr
        Just jDetail -> do
            jDetail' <- textToCString jDetail
            return jDetail'
    let orientation' = (fromIntegral . fromEnum) orientation
    gtk_paint_handle style' cr' stateType' shadowType' maybeWidget maybeDetail x y width height orientation'
    touchManagedPtr style
    touchManagedPtr cr
    whenJust widget touchManagedPtr
    freeMem maybeDetail
    return ()


-- function paint_focus
-- Args: [ Arg
--           { argCName = "style"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Style" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyle" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "cr"
--           , argType =
--               TInterface Name { namespace = "cairo" , name = "Context" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #cairo_t" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "state_type"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StateType" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a state" , sinceVersion = Nothing }
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
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the widget" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "detail"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a style detail" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "x"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the x origin of the rectangle around which to draw a focus indicator"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "y"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the y origin of the rectangle around which to draw a focus indicator"
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
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the width of the rectangle around which to draw a focus indicator"
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
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the height of the rectangle around which to draw a focus indicator"
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

foreign import ccall "gtk_paint_focus" gtk_paint_focus :: 
    Ptr Gtk.Style.Style ->                  -- style : TInterface (Name {namespace = "Gtk", name = "Style"})
    Ptr Cairo.Context.Context ->            -- cr : TInterface (Name {namespace = "cairo", name = "Context"})
    CUInt ->                                -- state_type : TInterface (Name {namespace = "Gtk", name = "StateType"})
    Ptr Gtk.Widget.Widget ->                -- widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    CString ->                              -- detail : TBasicType TUTF8
    Int32 ->                                -- x : TBasicType TInt
    Int32 ->                                -- y : TBasicType TInt
    Int32 ->                                -- width : TBasicType TInt
    Int32 ->                                -- height : TBasicType TInt
    IO ()

{-# DEPRECATED paintFocus ["(Since version 3.0)","Use 'GI.Gtk.Functions.renderFocus' instead"] #-}
-- | Draws a focus indicator around the given rectangle on /@cr@/ using the
-- given style.
paintFocus ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.Style.IsStyle a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@style@/: a t'GI.Gtk.Objects.Style.Style'
    -> Cairo.Context.Context
    -- ^ /@cr@/: a t'GI.Cairo.Structs.Context.Context'
    -> Gtk.Enums.StateType
    -- ^ /@stateType@/: a state
    -> Maybe (b)
    -- ^ /@widget@/: the widget
    -> Maybe (T.Text)
    -- ^ /@detail@/: a style detail
    -> Int32
    -- ^ /@x@/: the x origin of the rectangle around which to draw a focus indicator
    -> Int32
    -- ^ /@y@/: the y origin of the rectangle around which to draw a focus indicator
    -> Int32
    -- ^ /@width@/: the width of the rectangle around which to draw a focus indicator
    -> Int32
    -- ^ /@height@/: the height of the rectangle around which to draw a focus indicator
    -> m ()
paintFocus style cr stateType widget detail x y width height = liftIO $ do
    style' <- unsafeManagedPtrCastPtr style
    cr' <- unsafeManagedPtrGetPtr cr
    let stateType' = (fromIntegral . fromEnum) stateType
    maybeWidget <- case widget of
        Nothing -> return nullPtr
        Just jWidget -> do
            jWidget' <- unsafeManagedPtrCastPtr jWidget
            return jWidget'
    maybeDetail <- case detail of
        Nothing -> return nullPtr
        Just jDetail -> do
            jDetail' <- textToCString jDetail
            return jDetail'
    gtk_paint_focus style' cr' stateType' maybeWidget maybeDetail x y width height
    touchManagedPtr style
    touchManagedPtr cr
    whenJust widget touchManagedPtr
    freeMem maybeDetail
    return ()


-- function paint_flat_box
-- Args: [ Arg
--           { argCName = "style"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Style" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyle" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "cr"
--           , argType =
--               TInterface Name { namespace = "cairo" , name = "Context" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #cairo_t" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "state_type"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StateType" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a state" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "shadow_type"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ShadowType" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the type of shadow to draw"
--                 , sinceVersion = Nothing
--                 }
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
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the widget" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "detail"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a style detail" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "x"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "x origin of the box"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "y"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "y origin of the box"
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
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the width of the box"
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
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the height of the box"
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

foreign import ccall "gtk_paint_flat_box" gtk_paint_flat_box :: 
    Ptr Gtk.Style.Style ->                  -- style : TInterface (Name {namespace = "Gtk", name = "Style"})
    Ptr Cairo.Context.Context ->            -- cr : TInterface (Name {namespace = "cairo", name = "Context"})
    CUInt ->                                -- state_type : TInterface (Name {namespace = "Gtk", name = "StateType"})
    CUInt ->                                -- shadow_type : TInterface (Name {namespace = "Gtk", name = "ShadowType"})
    Ptr Gtk.Widget.Widget ->                -- widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    CString ->                              -- detail : TBasicType TUTF8
    Int32 ->                                -- x : TBasicType TInt
    Int32 ->                                -- y : TBasicType TInt
    Int32 ->                                -- width : TBasicType TInt
    Int32 ->                                -- height : TBasicType TInt
    IO ()

{-# DEPRECATED paintFlatBox ["(Since version 3.0)","Use 'GI.Gtk.Functions.renderFrame' and 'GI.Gtk.Functions.renderBackground' instead"] #-}
-- | Draws a flat box on /@cr@/ with the given parameters.
paintFlatBox ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.Style.IsStyle a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@style@/: a t'GI.Gtk.Objects.Style.Style'
    -> Cairo.Context.Context
    -- ^ /@cr@/: a t'GI.Cairo.Structs.Context.Context'
    -> Gtk.Enums.StateType
    -- ^ /@stateType@/: a state
    -> Gtk.Enums.ShadowType
    -- ^ /@shadowType@/: the type of shadow to draw
    -> Maybe (b)
    -- ^ /@widget@/: the widget
    -> Maybe (T.Text)
    -- ^ /@detail@/: a style detail
    -> Int32
    -- ^ /@x@/: x origin of the box
    -> Int32
    -- ^ /@y@/: y origin of the box
    -> Int32
    -- ^ /@width@/: the width of the box
    -> Int32
    -- ^ /@height@/: the height of the box
    -> m ()
paintFlatBox style cr stateType shadowType widget detail x y width height = liftIO $ do
    style' <- unsafeManagedPtrCastPtr style
    cr' <- unsafeManagedPtrGetPtr cr
    let stateType' = (fromIntegral . fromEnum) stateType
    let shadowType' = (fromIntegral . fromEnum) shadowType
    maybeWidget <- case widget of
        Nothing -> return nullPtr
        Just jWidget -> do
            jWidget' <- unsafeManagedPtrCastPtr jWidget
            return jWidget'
    maybeDetail <- case detail of
        Nothing -> return nullPtr
        Just jDetail -> do
            jDetail' <- textToCString jDetail
            return jDetail'
    gtk_paint_flat_box style' cr' stateType' shadowType' maybeWidget maybeDetail x y width height
    touchManagedPtr style
    touchManagedPtr cr
    whenJust widget touchManagedPtr
    freeMem maybeDetail
    return ()


-- function paint_extension
-- Args: [ Arg
--           { argCName = "style"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Style" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyle" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "cr"
--           , argType =
--               TInterface Name { namespace = "cairo" , name = "Context" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #cairo_t" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "state_type"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StateType" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a state" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "shadow_type"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ShadowType" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "type of shadow to draw"
--                 , sinceVersion = Nothing
--                 }
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
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the widget" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "detail"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a style detail" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "x"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "x origin of the extension"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "y"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "y origin of the extension"
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
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "width of the extension"
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
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "width of the extension"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "gap_side"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PositionType" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "the side on to which the extension is attached"
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

foreign import ccall "gtk_paint_extension" gtk_paint_extension :: 
    Ptr Gtk.Style.Style ->                  -- style : TInterface (Name {namespace = "Gtk", name = "Style"})
    Ptr Cairo.Context.Context ->            -- cr : TInterface (Name {namespace = "cairo", name = "Context"})
    CUInt ->                                -- state_type : TInterface (Name {namespace = "Gtk", name = "StateType"})
    CUInt ->                                -- shadow_type : TInterface (Name {namespace = "Gtk", name = "ShadowType"})
    Ptr Gtk.Widget.Widget ->                -- widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    CString ->                              -- detail : TBasicType TUTF8
    Int32 ->                                -- x : TBasicType TInt
    Int32 ->                                -- y : TBasicType TInt
    Int32 ->                                -- width : TBasicType TInt
    Int32 ->                                -- height : TBasicType TInt
    CUInt ->                                -- gap_side : TInterface (Name {namespace = "Gtk", name = "PositionType"})
    IO ()

{-# DEPRECATED paintExtension ["(Since version 3.0)","Use 'GI.Gtk.Functions.renderExtension' instead"] #-}
-- | Draws an extension, i.e. a notebook tab.
paintExtension ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.Style.IsStyle a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@style@/: a t'GI.Gtk.Objects.Style.Style'
    -> Cairo.Context.Context
    -- ^ /@cr@/: a t'GI.Cairo.Structs.Context.Context'
    -> Gtk.Enums.StateType
    -- ^ /@stateType@/: a state
    -> Gtk.Enums.ShadowType
    -- ^ /@shadowType@/: type of shadow to draw
    -> Maybe (b)
    -- ^ /@widget@/: the widget
    -> Maybe (T.Text)
    -- ^ /@detail@/: a style detail
    -> Int32
    -- ^ /@x@/: x origin of the extension
    -> Int32
    -- ^ /@y@/: y origin of the extension
    -> Int32
    -- ^ /@width@/: width of the extension
    -> Int32
    -- ^ /@height@/: width of the extension
    -> Gtk.Enums.PositionType
    -- ^ /@gapSide@/: the side on to which the extension is attached
    -> m ()
paintExtension style cr stateType shadowType widget detail x y width height gapSide = liftIO $ do
    style' <- unsafeManagedPtrCastPtr style
    cr' <- unsafeManagedPtrGetPtr cr
    let stateType' = (fromIntegral . fromEnum) stateType
    let shadowType' = (fromIntegral . fromEnum) shadowType
    maybeWidget <- case widget of
        Nothing -> return nullPtr
        Just jWidget -> do
            jWidget' <- unsafeManagedPtrCastPtr jWidget
            return jWidget'
    maybeDetail <- case detail of
        Nothing -> return nullPtr
        Just jDetail -> do
            jDetail' <- textToCString jDetail
            return jDetail'
    let gapSide' = (fromIntegral . fromEnum) gapSide
    gtk_paint_extension style' cr' stateType' shadowType' maybeWidget maybeDetail x y width height gapSide'
    touchManagedPtr style
    touchManagedPtr cr
    whenJust widget touchManagedPtr
    freeMem maybeDetail
    return ()


-- function paint_expander
-- Args: [ Arg
--           { argCName = "style"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Style" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyle" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "cr"
--           , argType =
--               TInterface Name { namespace = "cairo" , name = "Context" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #cairo_t" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "state_type"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StateType" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a state" , sinceVersion = Nothing }
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
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the widget" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "detail"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a style detail" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "x"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the x position to draw the expander at"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "y"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the y position to draw the expander at"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "expander_style"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ExpanderStyle" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the style to draw the expander in; determines\n  whether the expander is collapsed, expanded, or in an\n  intermediate state."
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

foreign import ccall "gtk_paint_expander" gtk_paint_expander :: 
    Ptr Gtk.Style.Style ->                  -- style : TInterface (Name {namespace = "Gtk", name = "Style"})
    Ptr Cairo.Context.Context ->            -- cr : TInterface (Name {namespace = "cairo", name = "Context"})
    CUInt ->                                -- state_type : TInterface (Name {namespace = "Gtk", name = "StateType"})
    Ptr Gtk.Widget.Widget ->                -- widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    CString ->                              -- detail : TBasicType TUTF8
    Int32 ->                                -- x : TBasicType TInt
    Int32 ->                                -- y : TBasicType TInt
    CUInt ->                                -- expander_style : TInterface (Name {namespace = "Gtk", name = "ExpanderStyle"})
    IO ()

{-# DEPRECATED paintExpander ["(Since version 3.0)","Use 'GI.Gtk.Functions.renderExpander' instead"] #-}
-- | Draws an expander as used in t'GI.Gtk.Objects.TreeView.TreeView'. /@x@/ and /@y@/ specify the
-- center the expander. The size of the expander is determined by the
-- “expander-size” style property of /@widget@/.  (If widget is not
-- specified or doesn’t have an “expander-size” property, an
-- unspecified default size will be used, since the caller doesn\'t
-- have sufficient information to position the expander, this is
-- likely not useful.) The expander is expander_size pixels tall
-- in the collapsed position and expander_size pixels wide in the
-- expanded position.
paintExpander ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.Style.IsStyle a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@style@/: a t'GI.Gtk.Objects.Style.Style'
    -> Cairo.Context.Context
    -- ^ /@cr@/: a t'GI.Cairo.Structs.Context.Context'
    -> Gtk.Enums.StateType
    -- ^ /@stateType@/: a state
    -> Maybe (b)
    -- ^ /@widget@/: the widget
    -> Maybe (T.Text)
    -- ^ /@detail@/: a style detail
    -> Int32
    -- ^ /@x@/: the x position to draw the expander at
    -> Int32
    -- ^ /@y@/: the y position to draw the expander at
    -> Gtk.Enums.ExpanderStyle
    -- ^ /@expanderStyle@/: the style to draw the expander in; determines
    --   whether the expander is collapsed, expanded, or in an
    --   intermediate state.
    -> m ()
paintExpander style cr stateType widget detail x y expanderStyle = liftIO $ do
    style' <- unsafeManagedPtrCastPtr style
    cr' <- unsafeManagedPtrGetPtr cr
    let stateType' = (fromIntegral . fromEnum) stateType
    maybeWidget <- case widget of
        Nothing -> return nullPtr
        Just jWidget -> do
            jWidget' <- unsafeManagedPtrCastPtr jWidget
            return jWidget'
    maybeDetail <- case detail of
        Nothing -> return nullPtr
        Just jDetail -> do
            jDetail' <- textToCString jDetail
            return jDetail'
    let expanderStyle' = (fromIntegral . fromEnum) expanderStyle
    gtk_paint_expander style' cr' stateType' maybeWidget maybeDetail x y expanderStyle'
    touchManagedPtr style
    touchManagedPtr cr
    whenJust widget touchManagedPtr
    freeMem maybeDetail
    return ()


-- function paint_diamond
-- Args: [ Arg
--           { argCName = "style"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Style" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyle" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "cr"
--           , argType =
--               TInterface Name { namespace = "cairo" , name = "Context" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #cairo_t" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "state_type"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StateType" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a state" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "shadow_type"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ShadowType" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the type of shadow to draw"
--                 , sinceVersion = Nothing
--                 }
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
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the widget" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "detail"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a style detail" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "x"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "x origin of the rectangle to draw the diamond in"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "y"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "y origin of the rectangle to draw the diamond in"
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
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "width of the rectangle to draw the diamond in"
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
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "height of the rectangle to draw the diamond in"
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

foreign import ccall "gtk_paint_diamond" gtk_paint_diamond :: 
    Ptr Gtk.Style.Style ->                  -- style : TInterface (Name {namespace = "Gtk", name = "Style"})
    Ptr Cairo.Context.Context ->            -- cr : TInterface (Name {namespace = "cairo", name = "Context"})
    CUInt ->                                -- state_type : TInterface (Name {namespace = "Gtk", name = "StateType"})
    CUInt ->                                -- shadow_type : TInterface (Name {namespace = "Gtk", name = "ShadowType"})
    Ptr Gtk.Widget.Widget ->                -- widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    CString ->                              -- detail : TBasicType TUTF8
    Int32 ->                                -- x : TBasicType TInt
    Int32 ->                                -- y : TBasicType TInt
    Int32 ->                                -- width : TBasicType TInt
    Int32 ->                                -- height : TBasicType TInt
    IO ()

{-# DEPRECATED paintDiamond ["(Since version 3.0)","Use cairo instead"] #-}
-- | Draws a diamond in the given rectangle on /@window@/ using the given
-- parameters.
paintDiamond ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.Style.IsStyle a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@style@/: a t'GI.Gtk.Objects.Style.Style'
    -> Cairo.Context.Context
    -- ^ /@cr@/: a t'GI.Cairo.Structs.Context.Context'
    -> Gtk.Enums.StateType
    -- ^ /@stateType@/: a state
    -> Gtk.Enums.ShadowType
    -- ^ /@shadowType@/: the type of shadow to draw
    -> Maybe (b)
    -- ^ /@widget@/: the widget
    -> Maybe (T.Text)
    -- ^ /@detail@/: a style detail
    -> Int32
    -- ^ /@x@/: x origin of the rectangle to draw the diamond in
    -> Int32
    -- ^ /@y@/: y origin of the rectangle to draw the diamond in
    -> Int32
    -- ^ /@width@/: width of the rectangle to draw the diamond in
    -> Int32
    -- ^ /@height@/: height of the rectangle to draw the diamond in
    -> m ()
paintDiamond style cr stateType shadowType widget detail x y width height = liftIO $ do
    style' <- unsafeManagedPtrCastPtr style
    cr' <- unsafeManagedPtrGetPtr cr
    let stateType' = (fromIntegral . fromEnum) stateType
    let shadowType' = (fromIntegral . fromEnum) shadowType
    maybeWidget <- case widget of
        Nothing -> return nullPtr
        Just jWidget -> do
            jWidget' <- unsafeManagedPtrCastPtr jWidget
            return jWidget'
    maybeDetail <- case detail of
        Nothing -> return nullPtr
        Just jDetail -> do
            jDetail' <- textToCString jDetail
            return jDetail'
    gtk_paint_diamond style' cr' stateType' shadowType' maybeWidget maybeDetail x y width height
    touchManagedPtr style
    touchManagedPtr cr
    whenJust widget touchManagedPtr
    freeMem maybeDetail
    return ()


-- function paint_check
-- Args: [ Arg
--           { argCName = "style"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Style" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyle" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "cr"
--           , argType =
--               TInterface Name { namespace = "cairo" , name = "Context" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #cairo_t" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "state_type"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StateType" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a state" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "shadow_type"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ShadowType" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the type of shadow to draw"
--                 , sinceVersion = Nothing
--                 }
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
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the widget" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "detail"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a style detail" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "x"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "x origin of the rectangle to draw the check in"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "y"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "y origin of the rectangle to draw the check in"
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
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "the width of the rectangle to draw the check in"
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
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "the height of the rectangle to draw the check in"
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

foreign import ccall "gtk_paint_check" gtk_paint_check :: 
    Ptr Gtk.Style.Style ->                  -- style : TInterface (Name {namespace = "Gtk", name = "Style"})
    Ptr Cairo.Context.Context ->            -- cr : TInterface (Name {namespace = "cairo", name = "Context"})
    CUInt ->                                -- state_type : TInterface (Name {namespace = "Gtk", name = "StateType"})
    CUInt ->                                -- shadow_type : TInterface (Name {namespace = "Gtk", name = "ShadowType"})
    Ptr Gtk.Widget.Widget ->                -- widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    CString ->                              -- detail : TBasicType TUTF8
    Int32 ->                                -- x : TBasicType TInt
    Int32 ->                                -- y : TBasicType TInt
    Int32 ->                                -- width : TBasicType TInt
    Int32 ->                                -- height : TBasicType TInt
    IO ()

{-# DEPRECATED paintCheck ["(Since version 3.0)","Use 'GI.Gtk.Functions.renderCheck' instead"] #-}
-- | Draws a check button indicator in the given rectangle on /@cr@/ with
-- the given parameters.
paintCheck ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.Style.IsStyle a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@style@/: a t'GI.Gtk.Objects.Style.Style'
    -> Cairo.Context.Context
    -- ^ /@cr@/: a t'GI.Cairo.Structs.Context.Context'
    -> Gtk.Enums.StateType
    -- ^ /@stateType@/: a state
    -> Gtk.Enums.ShadowType
    -- ^ /@shadowType@/: the type of shadow to draw
    -> Maybe (b)
    -- ^ /@widget@/: the widget
    -> Maybe (T.Text)
    -- ^ /@detail@/: a style detail
    -> Int32
    -- ^ /@x@/: x origin of the rectangle to draw the check in
    -> Int32
    -- ^ /@y@/: y origin of the rectangle to draw the check in
    -> Int32
    -- ^ /@width@/: the width of the rectangle to draw the check in
    -> Int32
    -- ^ /@height@/: the height of the rectangle to draw the check in
    -> m ()
paintCheck style cr stateType shadowType widget detail x y width height = liftIO $ do
    style' <- unsafeManagedPtrCastPtr style
    cr' <- unsafeManagedPtrGetPtr cr
    let stateType' = (fromIntegral . fromEnum) stateType
    let shadowType' = (fromIntegral . fromEnum) shadowType
    maybeWidget <- case widget of
        Nothing -> return nullPtr
        Just jWidget -> do
            jWidget' <- unsafeManagedPtrCastPtr jWidget
            return jWidget'
    maybeDetail <- case detail of
        Nothing -> return nullPtr
        Just jDetail -> do
            jDetail' <- textToCString jDetail
            return jDetail'
    gtk_paint_check style' cr' stateType' shadowType' maybeWidget maybeDetail x y width height
    touchManagedPtr style
    touchManagedPtr cr
    whenJust widget touchManagedPtr
    freeMem maybeDetail
    return ()


-- function paint_box_gap
-- Args: [ Arg
--           { argCName = "style"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Style" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyle" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "cr"
--           , argType =
--               TInterface Name { namespace = "cairo" , name = "Context" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #cairo_t" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "state_type"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StateType" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a state" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "shadow_type"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ShadowType" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "type of shadow to draw"
--                 , sinceVersion = Nothing
--                 }
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
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the widget" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "detail"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a style detail" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "x"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "x origin of the rectangle"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "y"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "y origin of the rectangle"
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
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "width of the rectangle"
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
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "width of the rectangle"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "gap_side"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PositionType" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "side in which to leave the gap"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "gap_x"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "starting position of the gap"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "gap_width"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "width of the gap" , sinceVersion = Nothing }
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

foreign import ccall "gtk_paint_box_gap" gtk_paint_box_gap :: 
    Ptr Gtk.Style.Style ->                  -- style : TInterface (Name {namespace = "Gtk", name = "Style"})
    Ptr Cairo.Context.Context ->            -- cr : TInterface (Name {namespace = "cairo", name = "Context"})
    CUInt ->                                -- state_type : TInterface (Name {namespace = "Gtk", name = "StateType"})
    CUInt ->                                -- shadow_type : TInterface (Name {namespace = "Gtk", name = "ShadowType"})
    Ptr Gtk.Widget.Widget ->                -- widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    CString ->                              -- detail : TBasicType TUTF8
    Int32 ->                                -- x : TBasicType TInt
    Int32 ->                                -- y : TBasicType TInt
    Int32 ->                                -- width : TBasicType TInt
    Int32 ->                                -- height : TBasicType TInt
    CUInt ->                                -- gap_side : TInterface (Name {namespace = "Gtk", name = "PositionType"})
    Int32 ->                                -- gap_x : TBasicType TInt
    Int32 ->                                -- gap_width : TBasicType TInt
    IO ()

{-# DEPRECATED paintBoxGap ["(Since version 3.0)","Use 'GI.Gtk.Functions.renderFrameGap' instead"] #-}
-- | Draws a box in /@cr@/ using the given style and state and shadow type,
-- leaving a gap in one side.
paintBoxGap ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.Style.IsStyle a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@style@/: a t'GI.Gtk.Objects.Style.Style'
    -> Cairo.Context.Context
    -- ^ /@cr@/: a t'GI.Cairo.Structs.Context.Context'
    -> Gtk.Enums.StateType
    -- ^ /@stateType@/: a state
    -> Gtk.Enums.ShadowType
    -- ^ /@shadowType@/: type of shadow to draw
    -> Maybe (b)
    -- ^ /@widget@/: the widget
    -> Maybe (T.Text)
    -- ^ /@detail@/: a style detail
    -> Int32
    -- ^ /@x@/: x origin of the rectangle
    -> Int32
    -- ^ /@y@/: y origin of the rectangle
    -> Int32
    -- ^ /@width@/: width of the rectangle
    -> Int32
    -- ^ /@height@/: width of the rectangle
    -> Gtk.Enums.PositionType
    -- ^ /@gapSide@/: side in which to leave the gap
    -> Int32
    -- ^ /@gapX@/: starting position of the gap
    -> Int32
    -- ^ /@gapWidth@/: width of the gap
    -> m ()
paintBoxGap style cr stateType shadowType widget detail x y width height gapSide gapX gapWidth = liftIO $ do
    style' <- unsafeManagedPtrCastPtr style
    cr' <- unsafeManagedPtrGetPtr cr
    let stateType' = (fromIntegral . fromEnum) stateType
    let shadowType' = (fromIntegral . fromEnum) shadowType
    maybeWidget <- case widget of
        Nothing -> return nullPtr
        Just jWidget -> do
            jWidget' <- unsafeManagedPtrCastPtr jWidget
            return jWidget'
    maybeDetail <- case detail of
        Nothing -> return nullPtr
        Just jDetail -> do
            jDetail' <- textToCString jDetail
            return jDetail'
    let gapSide' = (fromIntegral . fromEnum) gapSide
    gtk_paint_box_gap style' cr' stateType' shadowType' maybeWidget maybeDetail x y width height gapSide' gapX gapWidth
    touchManagedPtr style
    touchManagedPtr cr
    whenJust widget touchManagedPtr
    freeMem maybeDetail
    return ()


-- function paint_box
-- Args: [ Arg
--           { argCName = "style"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Style" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyle" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "cr"
--           , argType =
--               TInterface Name { namespace = "cairo" , name = "Context" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #cairo_t" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "state_type"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StateType" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a state" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "shadow_type"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ShadowType" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the type of shadow to draw"
--                 , sinceVersion = Nothing
--                 }
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
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the widget" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "detail"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a style detail" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "x"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "x origin of the box"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "y"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "y origin of the box"
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
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the width of the box"
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
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the height of the box"
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

foreign import ccall "gtk_paint_box" gtk_paint_box :: 
    Ptr Gtk.Style.Style ->                  -- style : TInterface (Name {namespace = "Gtk", name = "Style"})
    Ptr Cairo.Context.Context ->            -- cr : TInterface (Name {namespace = "cairo", name = "Context"})
    CUInt ->                                -- state_type : TInterface (Name {namespace = "Gtk", name = "StateType"})
    CUInt ->                                -- shadow_type : TInterface (Name {namespace = "Gtk", name = "ShadowType"})
    Ptr Gtk.Widget.Widget ->                -- widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    CString ->                              -- detail : TBasicType TUTF8
    Int32 ->                                -- x : TBasicType TInt
    Int32 ->                                -- y : TBasicType TInt
    Int32 ->                                -- width : TBasicType TInt
    Int32 ->                                -- height : TBasicType TInt
    IO ()

{-# DEPRECATED paintBox ["(Since version 3.0)","Use 'GI.Gtk.Functions.renderFrame' and 'GI.Gtk.Functions.renderBackground' instead"] #-}
-- | Draws a box on /@cr@/ with the given parameters.
paintBox ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.Style.IsStyle a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@style@/: a t'GI.Gtk.Objects.Style.Style'
    -> Cairo.Context.Context
    -- ^ /@cr@/: a t'GI.Cairo.Structs.Context.Context'
    -> Gtk.Enums.StateType
    -- ^ /@stateType@/: a state
    -> Gtk.Enums.ShadowType
    -- ^ /@shadowType@/: the type of shadow to draw
    -> Maybe (b)
    -- ^ /@widget@/: the widget
    -> Maybe (T.Text)
    -- ^ /@detail@/: a style detail
    -> Int32
    -- ^ /@x@/: x origin of the box
    -> Int32
    -- ^ /@y@/: y origin of the box
    -> Int32
    -- ^ /@width@/: the width of the box
    -> Int32
    -- ^ /@height@/: the height of the box
    -> m ()
paintBox style cr stateType shadowType widget detail x y width height = liftIO $ do
    style' <- unsafeManagedPtrCastPtr style
    cr' <- unsafeManagedPtrGetPtr cr
    let stateType' = (fromIntegral . fromEnum) stateType
    let shadowType' = (fromIntegral . fromEnum) shadowType
    maybeWidget <- case widget of
        Nothing -> return nullPtr
        Just jWidget -> do
            jWidget' <- unsafeManagedPtrCastPtr jWidget
            return jWidget'
    maybeDetail <- case detail of
        Nothing -> return nullPtr
        Just jDetail -> do
            jDetail' <- textToCString jDetail
            return jDetail'
    gtk_paint_box style' cr' stateType' shadowType' maybeWidget maybeDetail x y width height
    touchManagedPtr style
    touchManagedPtr cr
    whenJust widget touchManagedPtr
    freeMem maybeDetail
    return ()


-- function paint_arrow
-- Args: [ Arg
--           { argCName = "style"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Style" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyle" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "cr"
--           , argType =
--               TInterface Name { namespace = "cairo" , name = "Context" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #cairo_t" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "state_type"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StateType" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a state" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "shadow_type"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ShadowType" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the type of shadow to draw"
--                 , sinceVersion = Nothing
--                 }
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
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the widget" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "detail"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a style detail" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "arrow_type"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ArrowType" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the type of arrow to draw"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "fill"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "%TRUE if the arrow tip should be filled"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "x"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "x origin of the rectangle to draw the arrow in"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "y"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "y origin of the rectangle to draw the arrow in"
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
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "width of the rectangle to draw the arrow in"
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
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "height of the rectangle to draw the arrow in"
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

foreign import ccall "gtk_paint_arrow" gtk_paint_arrow :: 
    Ptr Gtk.Style.Style ->                  -- style : TInterface (Name {namespace = "Gtk", name = "Style"})
    Ptr Cairo.Context.Context ->            -- cr : TInterface (Name {namespace = "cairo", name = "Context"})
    CUInt ->                                -- state_type : TInterface (Name {namespace = "Gtk", name = "StateType"})
    CUInt ->                                -- shadow_type : TInterface (Name {namespace = "Gtk", name = "ShadowType"})
    Ptr Gtk.Widget.Widget ->                -- widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    CString ->                              -- detail : TBasicType TUTF8
    CUInt ->                                -- arrow_type : TInterface (Name {namespace = "Gtk", name = "ArrowType"})
    CInt ->                                 -- fill : TBasicType TBoolean
    Int32 ->                                -- x : TBasicType TInt
    Int32 ->                                -- y : TBasicType TInt
    Int32 ->                                -- width : TBasicType TInt
    Int32 ->                                -- height : TBasicType TInt
    IO ()

{-# DEPRECATED paintArrow ["(Since version 3.0)","Use 'GI.Gtk.Functions.renderArrow' instead"] #-}
-- | Draws an arrow in the given rectangle on /@cr@/ using the given
-- parameters. /@arrowType@/ determines the direction of the arrow.
paintArrow ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.Style.IsStyle a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@style@/: a t'GI.Gtk.Objects.Style.Style'
    -> Cairo.Context.Context
    -- ^ /@cr@/: a t'GI.Cairo.Structs.Context.Context'
    -> Gtk.Enums.StateType
    -- ^ /@stateType@/: a state
    -> Gtk.Enums.ShadowType
    -- ^ /@shadowType@/: the type of shadow to draw
    -> Maybe (b)
    -- ^ /@widget@/: the widget
    -> Maybe (T.Text)
    -- ^ /@detail@/: a style detail
    -> Gtk.Enums.ArrowType
    -- ^ /@arrowType@/: the type of arrow to draw
    -> Bool
    -- ^ /@fill@/: 'P.True' if the arrow tip should be filled
    -> Int32
    -- ^ /@x@/: x origin of the rectangle to draw the arrow in
    -> Int32
    -- ^ /@y@/: y origin of the rectangle to draw the arrow in
    -> Int32
    -- ^ /@width@/: width of the rectangle to draw the arrow in
    -> Int32
    -- ^ /@height@/: height of the rectangle to draw the arrow in
    -> m ()
paintArrow style cr stateType shadowType widget detail arrowType fill x y width height = liftIO $ do
    style' <- unsafeManagedPtrCastPtr style
    cr' <- unsafeManagedPtrGetPtr cr
    let stateType' = (fromIntegral . fromEnum) stateType
    let shadowType' = (fromIntegral . fromEnum) shadowType
    maybeWidget <- case widget of
        Nothing -> return nullPtr
        Just jWidget -> do
            jWidget' <- unsafeManagedPtrCastPtr jWidget
            return jWidget'
    maybeDetail <- case detail of
        Nothing -> return nullPtr
        Just jDetail -> do
            jDetail' <- textToCString jDetail
            return jDetail'
    let arrowType' = (fromIntegral . fromEnum) arrowType
    let fill' = (fromIntegral . fromEnum) fill
    gtk_paint_arrow style' cr' stateType' shadowType' maybeWidget maybeDetail arrowType' fill' x y width height
    touchManagedPtr style
    touchManagedPtr cr
    whenJust widget touchManagedPtr
    freeMem maybeDetail
    return ()


-- function main_quit
-- Args: []
-- Lengths: []
-- returnType: Nothing
-- throws : False
-- Skip return : False

foreign import ccall "gtk_main_quit" gtk_main_quit :: 
    IO ()

-- | Makes the innermost invocation of the main loop return
-- when it regains control.
mainQuit ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m ()
mainQuit  = liftIO $ do
    gtk_main_quit
    return ()


-- function main_level
-- Args: []
-- Lengths: []
-- returnType: Just (TBasicType TUInt)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_main_level" gtk_main_level :: 
    IO Word32

-- | Asks for the current nesting level of the main loop.
mainLevel ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m Word32
    -- ^ __Returns:__ the nesting level of the current invocation
    --     of the main loop
mainLevel  = liftIO $ do
    result <- gtk_main_level
    return result


-- function main_iteration_do
-- Args: [ Arg
--           { argCName = "blocking"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "%TRUE if you want GTK+ to block if no events are pending"
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

foreign import ccall "gtk_main_iteration_do" gtk_main_iteration_do :: 
    CInt ->                                 -- blocking : TBasicType TBoolean
    IO CInt

-- | Runs a single iteration of the mainloop.
-- If no events are available either return or block depending on
-- the value of /@blocking@/.
mainIterationDo ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    Bool
    -- ^ /@blocking@/: 'P.True' if you want GTK+ to block if no events are pending
    -> m Bool
    -- ^ __Returns:__ 'P.True' if 'GI.Gtk.Functions.mainQuit' has been called for the
    --     innermost mainloop
mainIterationDo blocking = liftIO $ do
    let blocking' = (fromIntegral . fromEnum) blocking
    result <- gtk_main_iteration_do blocking'
    let result' = (/= 0) result
    return result'


-- function main_iteration
-- Args: []
-- Lengths: []
-- returnType: Just (TBasicType TBoolean)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_main_iteration" gtk_main_iteration :: 
    IO CInt

-- | Runs a single iteration of the mainloop.
-- 
-- If no events are waiting to be processed GTK+ will block
-- until the next event is noticed. If you don’t want to block
-- look at 'GI.Gtk.Functions.mainIterationDo' or check if any events are
-- pending with 'GI.Gtk.Functions.eventsPending' first.
mainIteration ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m Bool
    -- ^ __Returns:__ 'P.True' if 'GI.Gtk.Functions.mainQuit' has been called for the
    --     innermost mainloop
mainIteration  = liftIO $ do
    result <- gtk_main_iteration
    let result' = (/= 0) result
    return result'


-- function main_do_event
-- Args: [ Arg
--           { argCName = "event"
--           , argType = TInterface Name { namespace = "Gdk" , name = "Event" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "An event to process (normally passed by GDK)"
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

foreign import ccall "gtk_main_do_event" gtk_main_do_event :: 
    Ptr Gdk.Event.Event ->                  -- event : TInterface (Name {namespace = "Gdk", name = "Event"})
    IO ()

-- | Processes a single GDK event.
-- 
-- This is public only to allow filtering of events between GDK and GTK+.
-- You will not usually need to call this function directly.
-- 
-- While you should not call this function directly, you might want to
-- know how exactly events are handled. So here is what this function
-- does with the event:
-- 
-- 1. Compress enter\/leave notify events. If the event passed build an
--    enter\/leave pair together with the next event (peeked from GDK), both
--    events are thrown away. This is to avoid a backlog of (de-)highlighting
--    widgets crossed by the pointer.
-- 
-- 2. Find the widget which got the event. If the widget can’t be determined
--    the event is thrown away unless it belongs to a INCR transaction.
-- 
-- 3. Then the event is pushed onto a stack so you can query the currently
--    handled event with 'GI.Gtk.Functions.getCurrentEvent'.
-- 
-- 4. The event is sent to a widget. If a grab is active all events for widgets
--    that are not in the contained in the grab widget are sent to the latter
--    with a few exceptions:
--    - Deletion and destruction events are still sent to the event widget for
--      obvious reasons.
--    - Events which directly relate to the visual representation of the event
--      widget.
--    - Leave events are delivered to the event widget if there was an enter
--      event delivered to it before without the paired leave event.
--    - Drag events are not redirected because it is unclear what the semantics
--      of that would be.
--    Another point of interest might be that all key events are first passed
--    through the key snooper functions if there are any. Read the description
--    of @/gtk_key_snooper_install()/@ if you need this feature.
-- 
-- 5. After finishing the delivery the event is popped from the event stack.
mainDoEvent ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    Gdk.Event.Event
    -- ^ /@event@/: An event to process (normally passed by GDK)
    -> m ()
mainDoEvent event = liftIO $ do
    event' <- unsafeManagedPtrGetPtr event
    gtk_main_do_event event'
    touchManagedPtr event
    return ()


-- function main
-- Args: []
-- Lengths: []
-- returnType: Nothing
-- throws : False
-- Skip return : False

foreign import ccall "gtk_main" gtk_main :: 
    IO ()

-- | Runs the main loop until 'GI.Gtk.Functions.mainQuit' is called.
-- 
-- You can nest calls to 'GI.Gtk.Functions.main'. In that case 'GI.Gtk.Functions.mainQuit'
-- will make the innermost invocation of the main loop return.
main ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m ()
main  = liftIO $ do
    gtk_main
    return ()


-- function key_snooper_remove
-- Args: [ Arg
--           { argCName = "snooper_handler_id"
--           , argType = TBasicType TUInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Identifies the key snooper to remove"
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

foreign import ccall "gtk_key_snooper_remove" gtk_key_snooper_remove :: 
    Word32 ->                               -- snooper_handler_id : TBasicType TUInt
    IO ()

{-# DEPRECATED keySnooperRemove ["(Since version 3.4)","Key snooping should not be done. Events should","    be handled by widgets."] #-}
-- | Removes the key snooper function with the given id.
keySnooperRemove ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    Word32
    -- ^ /@snooperHandlerId@/: Identifies the key snooper to remove
    -> m ()
keySnooperRemove snooperHandlerId = liftIO $ do
    gtk_key_snooper_remove snooperHandlerId
    return ()


-- function init_with_args
-- Args: [ Arg
--           { argCName = "argc"
--           , argType = TBasicType TInt
--           , direction = DirectionInout
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "Address of the `argc` parameter of\n    your main() function (or 0 if @argv is %NULL). This will be changed if\n    any arguments were handled."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       , Arg
--           { argCName = "argv"
--           , argType = TCArray False (-1) 0 (TBasicType TUTF8)
--           , direction = DirectionInout
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "Address of the\n    `argv` parameter of main(), or %NULL. Any options\n    understood by GTK+ are stripped before return."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       , Arg
--           { argCName = "parameter_string"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "a string which is displayed in\n   the first line of `--help` output, after\n   `programname [OPTION...]`"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "entries"
--           , argType =
--               TCArray
--                 True
--                 (-1)
--                 (-1)
--                 (TInterface Name { namespace = "GLib" , name = "OptionEntry" })
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "a %NULL-terminated array\n   of #GOptionEntrys describing the options of your program"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "translation_domain"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "a translation domain to use for translating\n   the `--help` output for the options in @entries\n   and the @parameter_string with gettext(), or %NULL"
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
--              { argCName = "argc"
--              , argType = TBasicType TInt
--              , direction = DirectionInout
--              , mayBeNull = False
--              , argDoc =
--                  Documentation
--                    { rawDocText =
--                        Just
--                          "Address of the `argc` parameter of\n    your main() function (or 0 if @argv is %NULL). This will be changed if\n    any arguments were handled."
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
-- throws : True
-- Skip return : False

foreign import ccall "gtk_init_with_args" gtk_init_with_args :: 
    Ptr Int32 ->                            -- argc : TBasicType TInt
    Ptr (Ptr CString) ->                    -- argv : TCArray False (-1) 0 (TBasicType TUTF8)
    CString ->                              -- parameter_string : TBasicType TUTF8
    Ptr (Ptr GLib.OptionEntry.OptionEntry) -> -- entries : TCArray True (-1) (-1) (TInterface (Name {namespace = "GLib", name = "OptionEntry"}))
    CString ->                              -- translation_domain : TBasicType TUTF8
    Ptr (Ptr GError) ->                     -- error
    IO CInt

-- | This function does the same work as 'GI.Gtk.Functions.initCheck'.
-- Additionally, it allows you to add your own commandline options,
-- and it automatically generates nicely formatted
-- @--help@ output. Note that your program will
-- be terminated after writing out the help output.
-- 
-- /Since: 2.6/
initWithArgs ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    Maybe ([T.Text])
    -- ^ /@argv@/: Address of the
    --     @argv@ parameter of @/main()/@, or 'P.Nothing'. Any options
    --     understood by GTK+ are stripped before return.
    -> Maybe (T.Text)
    -- ^ /@parameterString@/: a string which is displayed in
    --    the first line of @--help@ output, after
    --    @programname [OPTION...]@
    -> [GLib.OptionEntry.OptionEntry]
    -- ^ /@entries@/: a 'P.Nothing'-terminated array
    --    of @/GOptionEntrys/@ describing the options of your program
    -> Maybe (T.Text)
    -- ^ /@translationDomain@/: a translation domain to use for translating
    --    the @--help@ output for the options in /@entries@/
    --    and the /@parameterString@/ with @/gettext()/@, or 'P.Nothing'
    -> m ((Maybe [T.Text]))
    -- ^ /(Can throw 'Data.GI.Base.GError.GError')/
initWithArgs argv parameterString entries translationDomain = liftIO $ do
    let argc = case argv of
            Nothing -> 0
            Just jArgv -> fromIntegral $ P.length jArgv
    argc' <- allocMem :: IO (Ptr Int32)
    poke argc' argc
    maybeArgv <- case argv of
        Nothing -> return nullPtr
        Just jArgv -> do
            jArgv' <- packUTF8CArray jArgv
            return jArgv'
    maybeArgv' <- allocMem :: IO (Ptr (Ptr CString))
    poke maybeArgv' maybeArgv
    maybeParameterString <- case parameterString of
        Nothing -> return nullPtr
        Just jParameterString -> do
            jParameterString' <- textToCString jParameterString
            return jParameterString'
    entries' <- mapM unsafeManagedPtrGetPtr entries
    entries'' <- packZeroTerminatedPtrArray entries'
    maybeTranslationDomain <- case translationDomain of
        Nothing -> return nullPtr
        Just jTranslationDomain -> do
            jTranslationDomain' <- textToCString jTranslationDomain
            return jTranslationDomain'
    onException (do
        _ <- propagateGError $ gtk_init_with_args argc' maybeArgv' maybeParameterString entries'' maybeTranslationDomain
        argc'' <- peek argc'
        maybeArgv'' <- peek maybeArgv'
        maybeMaybeArgv'' <- convertIfNonNull maybeArgv'' $ \maybeArgv''' -> do
            maybeArgv'''' <- (unpackUTF8CArrayWithLength argc'') maybeArgv'''
            (mapCArrayWithLength argc'') freeMem maybeArgv'''
            freeMem maybeArgv'''
            return maybeArgv''''
        mapM_ touchManagedPtr entries
        freeMem argc'
        freeMem maybeArgv'
        freeMem maybeParameterString
        freeMem entries''
        freeMem maybeTranslationDomain
        return maybeMaybeArgv''
     ) (do
        freeMem argc'
        freeMem maybeArgv'
        freeMem maybeParameterString
        freeMem entries''
        freeMem maybeTranslationDomain
     )


-- function init_check
-- Args: [ Arg
--           { argCName = "argc"
--           , argType = TBasicType TInt
--           , direction = DirectionInout
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "Address of the `argc` parameter of\n    your main() function (or 0 if @argv is %NULL). This will be changed if\n    any arguments were handled."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       , Arg
--           { argCName = "argv"
--           , argType = TCArray False (-1) 0 (TBasicType TUTF8)
--           , direction = DirectionInout
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "Address of the\n    `argv` parameter of main(), or %NULL. Any options\n    understood by GTK+ are stripped before return."
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
--              { argCName = "argc"
--              , argType = TBasicType TInt
--              , direction = DirectionInout
--              , mayBeNull = False
--              , argDoc =
--                  Documentation
--                    { rawDocText =
--                        Just
--                          "Address of the `argc` parameter of\n    your main() function (or 0 if @argv is %NULL). This will be changed if\n    any arguments were handled."
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

foreign import ccall "gtk_init_check" gtk_init_check :: 
    Ptr Int32 ->                            -- argc : TBasicType TInt
    Ptr (Ptr CString) ->                    -- argv : TCArray False (-1) 0 (TBasicType TUTF8)
    IO CInt

-- | This function does the same work as 'GI.Gtk.Functions.init' with only a single
-- change: It does not terminate the program if the commandline
-- arguments couldn’t be parsed or the windowing system can’t be
-- initialized. Instead it returns 'P.False' on failure.
-- 
-- This way the application can fall back to some other means of
-- communication with the user - for example a curses or command line
-- interface.
-- 
-- Note that calling any GTK function or instantiating any GTK type after
-- this function returns 'P.False' results in undefined behavior.
initCheck ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    Maybe ([T.Text])
    -- ^ /@argv@/: Address of the
    --     @argv@ parameter of @/main()/@, or 'P.Nothing'. Any options
    --     understood by GTK+ are stripped before return.
    -> m ((Bool, Maybe [T.Text]))
    -- ^ __Returns:__ 'P.True' if the commandline arguments (if any) were valid and
    --     the windowing system has been successfully initialized, 'P.False'
    --     otherwise
initCheck argv = liftIO $ do
    let argc = case argv of
            Nothing -> 0
            Just jArgv -> fromIntegral $ P.length jArgv
    argc' <- allocMem :: IO (Ptr Int32)
    poke argc' argc
    maybeArgv <- case argv of
        Nothing -> return nullPtr
        Just jArgv -> do
            jArgv' <- packUTF8CArray jArgv
            return jArgv'
    maybeArgv' <- allocMem :: IO (Ptr (Ptr CString))
    poke maybeArgv' maybeArgv
    result <- gtk_init_check argc' maybeArgv'
    argc'' <- peek argc'
    let result' = (/= 0) result
    maybeArgv'' <- peek maybeArgv'
    maybeMaybeArgv'' <- convertIfNonNull maybeArgv'' $ \maybeArgv''' -> do
        maybeArgv'''' <- (unpackUTF8CArrayWithLength argc'') maybeArgv'''
        (mapCArrayWithLength argc'') freeMem maybeArgv'''
        freeMem maybeArgv'''
        return maybeArgv''''
    freeMem argc'
    freeMem maybeArgv'
    return (result', maybeMaybeArgv'')


-- function init
-- Args: [ Arg
--           { argCName = "argc"
--           , argType = TBasicType TInt
--           , direction = DirectionInout
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "Address of the `argc` parameter of\n    your main() function (or 0 if @argv is %NULL). This will be changed if\n    any arguments were handled."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       , Arg
--           { argCName = "argv"
--           , argType = TCArray False (-1) 0 (TBasicType TUTF8)
--           , direction = DirectionInout
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "Address of the\n    `argv` parameter of main(), or %NULL. Any options\n    understood by GTK+ are stripped before return."
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
--              { argCName = "argc"
--              , argType = TBasicType TInt
--              , direction = DirectionInout
--              , mayBeNull = False
--              , argDoc =
--                  Documentation
--                    { rawDocText =
--                        Just
--                          "Address of the `argc` parameter of\n    your main() function (or 0 if @argv is %NULL). This will be changed if\n    any arguments were handled."
--                    , sinceVersion = Nothing
--                    }
--              , argScope = ScopeTypeInvalid
--              , argClosure = -1
--              , argDestroy = -1
--              , argCallerAllocates = False
--              , transfer = TransferEverything
--              }
--          ]
-- returnType: Nothing
-- throws : False
-- Skip return : False

foreign import ccall "gtk_init" gtk_init :: 
    Ptr Int32 ->                            -- argc : TBasicType TInt
    Ptr (Ptr CString) ->                    -- argv : TCArray False (-1) 0 (TBasicType TUTF8)
    IO ()

-- | Call this function before using any other GTK+ functions in your GUI
-- applications.  It will initialize everything needed to operate the
-- toolkit and parses some standard command line options.
-- 
-- Although you are expected to pass the /@argc@/, /@argv@/ parameters from @/main()/@ to
-- this function, it is possible to pass 'P.Nothing' if /@argv@/ is not available or
-- commandline handling is not required.
-- 
-- /@argc@/ and /@argv@/ are adjusted accordingly so your own code will
-- never see those standard arguments.
-- 
-- Note that there are some alternative ways to initialize GTK+:
-- if you are calling 'GI.Gtk.Functions.parseArgs', 'GI.Gtk.Functions.initCheck',
-- 'GI.Gtk.Functions.initWithArgs' or 'GI.GLib.Structs.OptionContext.optionContextParse' with
-- the option group returned by 'GI.Gtk.Functions.getOptionGroup',
-- you don’t have to call 'GI.Gtk.Functions.init'.
-- 
-- And if you are using t'GI.Gtk.Objects.Application.Application', you don\'t have to call any of the
-- initialization functions either; the t'GI.Gtk.Objects.Application.Application'::@/startup/@ handler
-- does it for you.
-- 
-- This function will terminate your program if it was unable to
-- initialize the windowing system for some reason. If you want
-- your program to fall back to a textual interface you want to
-- call 'GI.Gtk.Functions.initCheck' instead.
-- 
-- Since 2.18, GTK+ calls @signal (SIGPIPE, SIG_IGN)@
-- during initialization, to ignore SIGPIPE signals, since these are
-- almost never wanted in graphical applications. If you do need to
-- handle SIGPIPE for some reason, reset the handler after 'GI.Gtk.Functions.init',
-- but notice that other libraries (e.g. libdbus or gvfs) might do
-- similar things.
init ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    Maybe ([T.Text])
    -- ^ /@argv@/: Address of the
    --     @argv@ parameter of @/main()/@, or 'P.Nothing'. Any options
    --     understood by GTK+ are stripped before return.
    -> m ((Maybe [T.Text]))
init argv = liftIO $ do
    let argc = case argv of
            Nothing -> 0
            Just jArgv -> fromIntegral $ P.length jArgv
    argc' <- allocMem :: IO (Ptr Int32)
    poke argc' argc
    maybeArgv <- case argv of
        Nothing -> return nullPtr
        Just jArgv -> do
            jArgv' <- packUTF8CArray jArgv
            return jArgv'
    maybeArgv' <- allocMem :: IO (Ptr (Ptr CString))
    poke maybeArgv' maybeArgv
    gtk_init argc' maybeArgv'
    argc'' <- peek argc'
    maybeArgv'' <- peek maybeArgv'
    maybeMaybeArgv'' <- convertIfNonNull maybeArgv'' $ \maybeArgv''' -> do
        maybeArgv'''' <- (unpackUTF8CArrayWithLength argc'') maybeArgv'''
        (mapCArrayWithLength argc'') freeMem maybeArgv'''
        freeMem maybeArgv'''
        return maybeArgv''''
    freeMem argc'
    freeMem maybeArgv'
    return maybeMaybeArgv''


-- function grab_get_current
-- Args: []
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "Widget" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_grab_get_current" gtk_grab_get_current :: 
    IO (Ptr Gtk.Widget.Widget)

-- | Queries the current grab of the default window group.
grabGetCurrent ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m (Maybe Gtk.Widget.Widget)
    -- ^ __Returns:__ The widget which currently
    --     has the grab or 'P.Nothing' if no grab is active
grabGetCurrent  = liftIO $ do
    result <- gtk_grab_get_current
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (newObject Gtk.Widget.Widget) result'
        return result''
    return maybeResult


-- function get_option_group
-- Args: [ Arg
--           { argCName = "open_default_display"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "whether to open the default display\n    when parsing the commandline arguments"
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
--               (TInterface Name { namespace = "GLib" , name = "OptionGroup" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_get_option_group" gtk_get_option_group :: 
    CInt ->                                 -- open_default_display : TBasicType TBoolean
    IO (Ptr GLib.OptionGroup.OptionGroup)

-- | Returns a t'GI.GLib.Structs.OptionGroup.OptionGroup' for the commandline arguments recognized
-- by GTK+ and GDK.
-- 
-- You should add this group to your t'GI.GLib.Structs.OptionContext.OptionContext'
-- with 'GI.GLib.Structs.OptionContext.optionContextAddGroup', if you are using
-- 'GI.GLib.Structs.OptionContext.optionContextParse' to parse your commandline arguments.
-- 
-- /Since: 2.6/
getOptionGroup ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    Bool
    -- ^ /@openDefaultDisplay@/: whether to open the default display
    --     when parsing the commandline arguments
    -> m GLib.OptionGroup.OptionGroup
    -- ^ __Returns:__ a t'GI.GLib.Structs.OptionGroup.OptionGroup' for the commandline
    --     arguments recognized by GTK+
getOptionGroup openDefaultDisplay = liftIO $ do
    let openDefaultDisplay' = (fromIntegral . fromEnum) openDefaultDisplay
    result <- gtk_get_option_group openDefaultDisplay'
    checkUnexpectedReturnNULL "getOptionGroup" result
    result' <- (wrapBoxed GLib.OptionGroup.OptionGroup) result
    return result'


-- function get_minor_version
-- Args: []
-- Lengths: []
-- returnType: Just (TBasicType TUInt)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_get_minor_version" gtk_get_minor_version :: 
    IO Word32

-- | Returns the minor version number of the GTK+ library.
-- (e.g. in GTK+ version 3.1.5 this is 1.)
-- 
-- This function is in the library, so it represents the GTK+ library
-- your code is are running against. Contrast with the
-- 'GI.Gtk.Constants.MINOR_VERSION' macro, which represents the minor version of the
-- GTK+ headers you have included when compiling your code.
-- 
-- /Since: 3.0/
getMinorVersion ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m Word32
    -- ^ __Returns:__ the minor version number of the GTK+ library
getMinorVersion  = liftIO $ do
    result <- gtk_get_minor_version
    return result


-- function get_micro_version
-- Args: []
-- Lengths: []
-- returnType: Just (TBasicType TUInt)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_get_micro_version" gtk_get_micro_version :: 
    IO Word32

-- | Returns the micro version number of the GTK+ library.
-- (e.g. in GTK+ version 3.1.5 this is 5.)
-- 
-- This function is in the library, so it represents the GTK+ library
-- your code is are running against. Contrast with the
-- 'GI.Gtk.Constants.MICRO_VERSION' macro, which represents the micro version of the
-- GTK+ headers you have included when compiling your code.
-- 
-- /Since: 3.0/
getMicroVersion ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m Word32
    -- ^ __Returns:__ the micro version number of the GTK+ library
getMicroVersion  = liftIO $ do
    result <- gtk_get_micro_version
    return result


-- function get_major_version
-- Args: []
-- Lengths: []
-- returnType: Just (TBasicType TUInt)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_get_major_version" gtk_get_major_version :: 
    IO Word32

-- | Returns the major version number of the GTK+ library.
-- (e.g. in GTK+ version 3.1.5 this is 3.)
-- 
-- This function is in the library, so it represents the GTK+ library
-- your code is running against. Contrast with the 'GI.Gtk.Constants.MAJOR_VERSION'
-- macro, which represents the major version of the GTK+ headers you
-- have included when compiling your code.
-- 
-- /Since: 3.0/
getMajorVersion ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m Word32
    -- ^ __Returns:__ the major version number of the GTK+ library
getMajorVersion  = liftIO $ do
    result <- gtk_get_major_version
    return result


-- function get_locale_direction
-- Args: []
-- Lengths: []
-- returnType: Just
--               (TInterface Name { namespace = "Gtk" , name = "TextDirection" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_get_locale_direction" gtk_get_locale_direction :: 
    IO CUInt

-- | Get the direction of the current locale. This is the expected
-- reading direction for text and UI.
-- 
-- This function depends on the current locale being set with
-- @/setlocale()/@ and will default to setting the 'GI.Gtk.Enums.TextDirectionLtr'
-- direction otherwise. 'GI.Gtk.Enums.TextDirectionNone' will never be returned.
-- 
-- GTK+ sets the default text direction according to the locale
-- during 'GI.Gtk.Functions.init', and you should normally use
-- 'GI.Gtk.Objects.Widget.widgetGetDirection' or 'GI.Gtk.Objects.Widget.widgetGetDefaultDirection'
-- to obtain the current direcion.
-- 
-- This function is only needed rare cases when the locale is
-- changed after GTK+ has already been initialized. In this case,
-- you can use it to update the default text direction as follows:
-- 
-- 
-- === /C code/
-- >
-- >setlocale (LC_ALL, new_locale);
-- >direction = gtk_get_locale_direction ();
-- >gtk_widget_set_default_direction (direction);
-- 
-- 
-- /Since: 3.12/
getLocaleDirection ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m Gtk.Enums.TextDirection
    -- ^ __Returns:__ the t'GI.Gtk.Enums.TextDirection' of the current locale
getLocaleDirection  = liftIO $ do
    result <- gtk_get_locale_direction
    let result' = (toEnum . fromIntegral) result
    return result'


-- function get_interface_age
-- Args: []
-- Lengths: []
-- returnType: Just (TBasicType TUInt)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_get_interface_age" gtk_get_interface_age :: 
    IO Word32

-- | Returns the interface age as passed to @libtool@
-- when building the GTK+ library the process is running against.
-- If @libtool@ means nothing to you, don\'t
-- worry about it.
-- 
-- /Since: 3.0/
getInterfaceAge ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m Word32
    -- ^ __Returns:__ the interface age of the GTK+ library
getInterfaceAge  = liftIO $ do
    result <- gtk_get_interface_age
    return result


-- function get_event_widget
-- Args: [ Arg
--           { argCName = "event"
--           , argType = TInterface Name { namespace = "Gdk" , name = "Event" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GdkEvent" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "Widget" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_get_event_widget" gtk_get_event_widget :: 
    Ptr Gdk.Event.Event ->                  -- event : TInterface (Name {namespace = "Gdk", name = "Event"})
    IO (Ptr Gtk.Widget.Widget)

-- | If /@event@/ is 'P.Nothing' or the event was not associated with any widget,
-- returns 'P.Nothing', otherwise returns the widget that received the event
-- originally.
getEventWidget ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    Gdk.Event.Event
    -- ^ /@event@/: a t'GI.Gdk.Unions.Event.Event'
    -> m (Maybe Gtk.Widget.Widget)
    -- ^ __Returns:__ the widget that originally
    --     received /@event@/, or 'P.Nothing'
getEventWidget event = liftIO $ do
    event' <- unsafeManagedPtrGetPtr event
    result <- gtk_get_event_widget event'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (newObject Gtk.Widget.Widget) result'
        return result''
    touchManagedPtr event
    return maybeResult


-- function get_default_language
-- Args: []
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Pango" , name = "Language" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_get_default_language" gtk_get_default_language :: 
    IO (Ptr Pango.Language.Language)

-- | Returns the t'GI.Pango.Structs.Language.Language' for the default language currently in
-- effect. (Note that this can change over the life of an
-- application.) The default language is derived from the current
-- locale. It determines, for example, whether GTK+ uses the
-- right-to-left or left-to-right text direction.
-- 
-- This function is equivalent to 'GI.Pango.Functions.languageGetDefault'.
-- See that function for details.
getDefaultLanguage ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m Pango.Language.Language
    -- ^ __Returns:__ the default language as a t'GI.Pango.Structs.Language.Language',
    --     must not be freed
getDefaultLanguage  = liftIO $ do
    result <- gtk_get_default_language
    checkUnexpectedReturnNULL "getDefaultLanguage" result
    result' <- (newBoxed Pango.Language.Language) result
    return result'


-- function get_debug_flags
-- Args: []
-- Lengths: []
-- returnType: Just (TBasicType TUInt)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_get_debug_flags" gtk_get_debug_flags :: 
    IO Word32

-- | Returns the GTK+ debug flags.
-- 
-- This function is intended for GTK+ modules that want
-- to adjust their debug output based on GTK+ debug flags.
getDebugFlags ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m Word32
    -- ^ __Returns:__ the GTK+ debug flags.
getDebugFlags  = liftIO $ do
    result <- gtk_get_debug_flags
    return result


-- function get_current_event_time
-- Args: []
-- Lengths: []
-- returnType: Just (TBasicType TUInt32)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_get_current_event_time" gtk_get_current_event_time :: 
    IO Word32

-- | If there is a current event and it has a timestamp,
-- return that timestamp, otherwise return 'GI.Gdk.Constants.CURRENT_TIME'.
getCurrentEventTime ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m Word32
    -- ^ __Returns:__ the timestamp from the current event,
    --     or 'GI.Gdk.Constants.CURRENT_TIME'.
getCurrentEventTime  = liftIO $ do
    result <- gtk_get_current_event_time
    return result


-- function get_current_event_state
-- Args: [ Arg
--           { argCName = "state"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "ModifierType" }
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "a location to store the state of the current event"
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

foreign import ccall "gtk_get_current_event_state" gtk_get_current_event_state :: 
    Ptr CUInt ->                            -- state : TInterface (Name {namespace = "Gdk", name = "ModifierType"})
    IO CInt

-- | If there is a current event and it has a state field, place
-- that state field in /@state@/ and return 'P.True', otherwise return
-- 'P.False'.
getCurrentEventState ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m ((Bool, [Gdk.Flags.ModifierType]))
    -- ^ __Returns:__ 'P.True' if there was a current event and it
    --     had a state field
getCurrentEventState  = liftIO $ do
    state <- allocMem :: IO (Ptr CUInt)
    result <- gtk_get_current_event_state state
    let result' = (/= 0) result
    state' <- peek state
    let state'' = wordToGFlags state'
    freeMem state
    return (result', state'')


-- function get_current_event_device
-- Args: []
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gdk" , name = "Device" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_get_current_event_device" gtk_get_current_event_device :: 
    IO (Ptr Gdk.Device.Device)

-- | If there is a current event and it has a device, return that
-- device, otherwise return 'P.Nothing'.
getCurrentEventDevice ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m (Maybe Gdk.Device.Device)
    -- ^ __Returns:__ a t'GI.Gdk.Objects.Device.Device', or 'P.Nothing'
getCurrentEventDevice  = liftIO $ do
    result <- gtk_get_current_event_device
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (newObject Gdk.Device.Device) result'
        return result''
    return maybeResult


-- function get_current_event
-- Args: []
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gdk" , name = "Event" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_get_current_event" gtk_get_current_event :: 
    IO (Ptr Gdk.Event.Event)

-- | Obtains a copy of the event currently being processed by GTK+.
-- 
-- For example, if you are handling a [Button::clicked]("GI.Gtk.Objects.Button#g:signal:clicked") signal,
-- the current event will be the t'GI.Gdk.Structs.EventButton.EventButton' that triggered
-- the [clicked](#g:signal:clicked) signal.
getCurrentEvent ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m (Maybe Gdk.Event.Event)
    -- ^ __Returns:__ a copy of the current event, or
    --     'P.Nothing' if there is no current event. The returned event must be
    --     freed with 'GI.Gdk.Unions.Event.eventFree'.
getCurrentEvent  = liftIO $ do
    result <- gtk_get_current_event
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (wrapBoxed Gdk.Event.Event) result'
        return result''
    return maybeResult


-- function get_binary_age
-- Args: []
-- Lengths: []
-- returnType: Just (TBasicType TUInt)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_get_binary_age" gtk_get_binary_age :: 
    IO Word32

-- | Returns the binary age as passed to @libtool@
-- when building the GTK+ library the process is running against.
-- If @libtool@ means nothing to you, don\'t
-- worry about it.
-- 
-- /Since: 3.0/
getBinaryAge ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m Word32
    -- ^ __Returns:__ the binary age of the GTK+ library
getBinaryAge  = liftIO $ do
    result <- gtk_get_binary_age
    return result


-- function false
-- Args: []
-- Lengths: []
-- returnType: Just (TBasicType TBoolean)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_false" gtk_false :: 
    IO CInt

-- | Analogical to 'GI.Gtk.Functions.true', this function does nothing
-- but always returns 'P.False'.
false ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m Bool
    -- ^ __Returns:__ 'P.False'
false  = liftIO $ do
    result <- gtk_false
    let result' = (/= 0) result
    return result'


-- function events_pending
-- Args: []
-- Lengths: []
-- returnType: Just (TBasicType TBoolean)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_events_pending" gtk_events_pending :: 
    IO CInt

-- | Checks if any events are pending.
-- 
-- This can be used to update the UI and invoke timeouts etc.
-- while doing some time intensive computation.
-- 
-- == Updating the UI during a long computation
-- 
-- 
-- === /C code/
-- >
-- > // computation going on...
-- >
-- > while (gtk_events_pending ())
-- >   gtk_main_iteration ();
-- >
-- > // ...computation continued
eventsPending ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m Bool
    -- ^ __Returns:__ 'P.True' if any events are pending, 'P.False' otherwise
eventsPending  = liftIO $ do
    result <- gtk_events_pending
    let result' = (/= 0) result
    return result'


-- function draw_insertion_cursor
-- Args: [ Arg
--           { argCName = "widget"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Widget" }
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
--           { argCName = "cr"
--           , argType =
--               TInterface Name { namespace = "cairo" , name = "Context" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "cairo context to draw to"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "location"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "Rectangle" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "location where to draw the cursor (@location->width is ignored)"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "is_primary"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "if the cursor should be the primary cursor color."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "direction"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextDirection" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "whether the cursor is left-to-right or\n            right-to-left. Should never be #GTK_TEXT_DIR_NONE"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "draw_arrow"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "%TRUE to draw a directional arrow on the\n       cursor. Should be %FALSE unless the cursor is split."
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

foreign import ccall "gtk_draw_insertion_cursor" gtk_draw_insertion_cursor :: 
    Ptr Gtk.Widget.Widget ->                -- widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    Ptr Cairo.Context.Context ->            -- cr : TInterface (Name {namespace = "cairo", name = "Context"})
    Ptr Gdk.Rectangle.Rectangle ->          -- location : TInterface (Name {namespace = "Gdk", name = "Rectangle"})
    CInt ->                                 -- is_primary : TBasicType TBoolean
    CUInt ->                                -- direction : TInterface (Name {namespace = "Gtk", name = "TextDirection"})
    CInt ->                                 -- draw_arrow : TBasicType TBoolean
    IO ()

{-# DEPRECATED drawInsertionCursor ["(Since version 3.4)","Use 'GI.Gtk.Functions.renderInsertionCursor' instead."] #-}
-- | Draws a text caret on /@cr@/ at /@location@/. This is not a style function
-- but merely a convenience function for drawing the standard cursor shape.
-- 
-- /Since: 3.0/
drawInsertionCursor ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.Widget.IsWidget a) =>
    a
    -- ^ /@widget@/: a t'GI.Gtk.Objects.Widget.Widget'
    -> Cairo.Context.Context
    -- ^ /@cr@/: cairo context to draw to
    -> Gdk.Rectangle.Rectangle
    -- ^ /@location@/: location where to draw the cursor (/@location@/->width is ignored)
    -> Bool
    -- ^ /@isPrimary@/: if the cursor should be the primary cursor color.
    -> Gtk.Enums.TextDirection
    -- ^ /@direction@/: whether the cursor is left-to-right or
    --             right-to-left. Should never be @/GTK_TEXT_DIR_NONE/@
    -> Bool
    -- ^ /@drawArrow@/: 'P.True' to draw a directional arrow on the
    --        cursor. Should be 'P.False' unless the cursor is split.
    -> m ()
drawInsertionCursor widget cr location isPrimary direction drawArrow = liftIO $ do
    widget' <- unsafeManagedPtrCastPtr widget
    cr' <- unsafeManagedPtrGetPtr cr
    location' <- unsafeManagedPtrGetPtr location
    let isPrimary' = (fromIntegral . fromEnum) isPrimary
    let direction' = (fromIntegral . fromEnum) direction
    let drawArrow' = (fromIntegral . fromEnum) drawArrow
    gtk_draw_insertion_cursor widget' cr' location' isPrimary' direction' drawArrow'
    touchManagedPtr widget
    touchManagedPtr cr
    touchManagedPtr location
    return ()


-- function drag_set_icon_widget
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "DragContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the context for a drag. (This must be called\n          with a  context for the source side of a drag)"
--                 , sinceVersion = Nothing
--                 }
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
--                 { rawDocText = Just "a toplevel window to use as an icon."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "hot_x"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the X offset within @widget of the hotspot."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "hot_y"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the Y offset within @widget of the hotspot."
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

foreign import ccall "gtk_drag_set_icon_widget" gtk_drag_set_icon_widget :: 
    Ptr Gdk.DragContext.DragContext ->      -- context : TInterface (Name {namespace = "Gdk", name = "DragContext"})
    Ptr Gtk.Widget.Widget ->                -- widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    Int32 ->                                -- hot_x : TBasicType TInt
    Int32 ->                                -- hot_y : TBasicType TInt
    IO ()

-- | Changes the icon for a widget to a given widget. GTK+
-- will not destroy the icon, so if you don’t want
-- it to persist, you should connect to the “drag-end”
-- signal and destroy it yourself.
dragSetIconWidget ::
    (B.CallStack.HasCallStack, MonadIO m, Gdk.DragContext.IsDragContext a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@context@/: the context for a drag. (This must be called
    --           with a  context for the source side of a drag)
    -> b
    -- ^ /@widget@/: a toplevel window to use as an icon.
    -> Int32
    -- ^ /@hotX@/: the X offset within /@widget@/ of the hotspot.
    -> Int32
    -- ^ /@hotY@/: the Y offset within /@widget@/ of the hotspot.
    -> m ()
dragSetIconWidget context widget hotX hotY = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    widget' <- unsafeManagedPtrCastPtr widget
    gtk_drag_set_icon_widget context' widget' hotX hotY
    touchManagedPtr context
    touchManagedPtr widget
    return ()


-- function drag_set_icon_surface
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "DragContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the context for a drag. (This must be called\n           with a context for the source side of a drag)"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "surface"
--           , argType =
--               TInterface Name { namespace = "cairo" , name = "Surface" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the surface to use as icon"
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

foreign import ccall "gtk_drag_set_icon_surface" gtk_drag_set_icon_surface :: 
    Ptr Gdk.DragContext.DragContext ->      -- context : TInterface (Name {namespace = "Gdk", name = "DragContext"})
    Ptr Cairo.Surface.Surface ->            -- surface : TInterface (Name {namespace = "cairo", name = "Surface"})
    IO ()

-- | Sets /@surface@/ as the icon for a given drag. GTK+ retains
-- references for the arguments, and will release them when
-- they are no longer needed.
-- 
-- To position the surface relative to the mouse, use
-- @/cairo_surface_set_device_offset()/@ on /@surface@/. The mouse
-- cursor will be positioned at the (0,0) coordinate of the
-- surface.
dragSetIconSurface ::
    (B.CallStack.HasCallStack, MonadIO m, Gdk.DragContext.IsDragContext a) =>
    a
    -- ^ /@context@/: the context for a drag. (This must be called
    --            with a context for the source side of a drag)
    -> Cairo.Surface.Surface
    -- ^ /@surface@/: the surface to use as icon
    -> m ()
dragSetIconSurface context surface = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    surface' <- unsafeManagedPtrGetPtr surface
    gtk_drag_set_icon_surface context' surface'
    touchManagedPtr context
    touchManagedPtr surface
    return ()


-- function drag_set_icon_stock
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "DragContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the context for a drag. (This must be called\n           with a  context for the source side of a drag)"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "stock_id"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the ID of the stock icon to use for the drag."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "hot_x"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the X offset within the icon of the hotspot."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "hot_y"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the Y offset within the icon of the hotspot."
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

foreign import ccall "gtk_drag_set_icon_stock" gtk_drag_set_icon_stock :: 
    Ptr Gdk.DragContext.DragContext ->      -- context : TInterface (Name {namespace = "Gdk", name = "DragContext"})
    CString ->                              -- stock_id : TBasicType TUTF8
    Int32 ->                                -- hot_x : TBasicType TInt
    Int32 ->                                -- hot_y : TBasicType TInt
    IO ()

-- | Sets the icon for a given drag from a stock ID.
dragSetIconStock ::
    (B.CallStack.HasCallStack, MonadIO m, Gdk.DragContext.IsDragContext a) =>
    a
    -- ^ /@context@/: the context for a drag. (This must be called
    --            with a  context for the source side of a drag)
    -> T.Text
    -- ^ /@stockId@/: the ID of the stock icon to use for the drag.
    -> Int32
    -- ^ /@hotX@/: the X offset within the icon of the hotspot.
    -> Int32
    -- ^ /@hotY@/: the Y offset within the icon of the hotspot.
    -> m ()
dragSetIconStock context stockId hotX hotY = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    stockId' <- textToCString stockId
    gtk_drag_set_icon_stock context' stockId' hotX hotY
    touchManagedPtr context
    freeMem stockId'
    return ()


-- function drag_set_icon_pixbuf
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "DragContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the context for a drag. (This must be called\n           with a  context for the source side of a drag)"
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
--                 { rawDocText = Just "the #GdkPixbuf to use as the drag icon."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "hot_x"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the X offset within @widget of the hotspot."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "hot_y"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the Y offset within @widget of the hotspot."
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

foreign import ccall "gtk_drag_set_icon_pixbuf" gtk_drag_set_icon_pixbuf :: 
    Ptr Gdk.DragContext.DragContext ->      -- context : TInterface (Name {namespace = "Gdk", name = "DragContext"})
    Ptr GdkPixbuf.Pixbuf.Pixbuf ->          -- pixbuf : TInterface (Name {namespace = "GdkPixbuf", name = "Pixbuf"})
    Int32 ->                                -- hot_x : TBasicType TInt
    Int32 ->                                -- hot_y : TBasicType TInt
    IO ()

-- | Sets /@pixbuf@/ as the icon for a given drag.
dragSetIconPixbuf ::
    (B.CallStack.HasCallStack, MonadIO m, Gdk.DragContext.IsDragContext a, GdkPixbuf.Pixbuf.IsPixbuf b) =>
    a
    -- ^ /@context@/: the context for a drag. (This must be called
    --            with a  context for the source side of a drag)
    -> b
    -- ^ /@pixbuf@/: the t'GI.GdkPixbuf.Objects.Pixbuf.Pixbuf' to use as the drag icon.
    -> Int32
    -- ^ /@hotX@/: the X offset within /@widget@/ of the hotspot.
    -> Int32
    -- ^ /@hotY@/: the Y offset within /@widget@/ of the hotspot.
    -> m ()
dragSetIconPixbuf context pixbuf hotX hotY = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    pixbuf' <- unsafeManagedPtrCastPtr pixbuf
    gtk_drag_set_icon_pixbuf context' pixbuf' hotX hotY
    touchManagedPtr context
    touchManagedPtr pixbuf
    return ()


-- function drag_set_icon_name
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "DragContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the context for a drag. (This must be called\n           with a context for the source side of a drag)"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "icon_name"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "name of icon to use"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "hot_x"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the X offset of the hotspot within the icon"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "hot_y"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the Y offset of the hotspot within the icon"
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

foreign import ccall "gtk_drag_set_icon_name" gtk_drag_set_icon_name :: 
    Ptr Gdk.DragContext.DragContext ->      -- context : TInterface (Name {namespace = "Gdk", name = "DragContext"})
    CString ->                              -- icon_name : TBasicType TUTF8
    Int32 ->                                -- hot_x : TBasicType TInt
    Int32 ->                                -- hot_y : TBasicType TInt
    IO ()

-- | Sets the icon for a given drag from a named themed icon. See
-- the docs for t'GI.Gtk.Objects.IconTheme.IconTheme' for more details. Note that the
-- size of the icon depends on the icon theme (the icon is
-- loaded at the symbolic size @/GTK_ICON_SIZE_DND/@), thus
-- /@hotX@/ and /@hotY@/ have to be used with care.
-- 
-- /Since: 2.8/
dragSetIconName ::
    (B.CallStack.HasCallStack, MonadIO m, Gdk.DragContext.IsDragContext a) =>
    a
    -- ^ /@context@/: the context for a drag. (This must be called
    --            with a context for the source side of a drag)
    -> T.Text
    -- ^ /@iconName@/: name of icon to use
    -> Int32
    -- ^ /@hotX@/: the X offset of the hotspot within the icon
    -> Int32
    -- ^ /@hotY@/: the Y offset of the hotspot within the icon
    -> m ()
dragSetIconName context iconName hotX hotY = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    iconName' <- textToCString iconName
    gtk_drag_set_icon_name context' iconName' hotX hotY
    touchManagedPtr context
    freeMem iconName'
    return ()


-- function drag_set_icon_default
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "DragContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the context for a drag. (This must be called\n             with a  context for the source side of a drag)"
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

foreign import ccall "gtk_drag_set_icon_default" gtk_drag_set_icon_default :: 
    Ptr Gdk.DragContext.DragContext ->      -- context : TInterface (Name {namespace = "Gdk", name = "DragContext"})
    IO ()

-- | Sets the icon for a particular drag to the default
-- icon.
dragSetIconDefault ::
    (B.CallStack.HasCallStack, MonadIO m, Gdk.DragContext.IsDragContext a) =>
    a
    -- ^ /@context@/: the context for a drag. (This must be called
    --              with a  context for the source side of a drag)
    -> m ()
dragSetIconDefault context = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    gtk_drag_set_icon_default context'
    touchManagedPtr context
    return ()


-- function drag_get_source_widget
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "DragContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a (destination side) drag context"
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "Widget" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_drag_get_source_widget" gtk_drag_get_source_widget :: 
    Ptr Gdk.DragContext.DragContext ->      -- context : TInterface (Name {namespace = "Gdk", name = "DragContext"})
    IO (Ptr Gtk.Widget.Widget)

-- | /No description available in the introspection data./
dragGetSourceWidget ::
    (B.CallStack.HasCallStack, MonadIO m, Gdk.DragContext.IsDragContext a) =>
    a
    -- ^ /@context@/: a (destination side) drag context
    -> m Gtk.Widget.Widget
dragGetSourceWidget context = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    result <- gtk_drag_get_source_widget context'
    checkUnexpectedReturnNULL "dragGetSourceWidget" result
    result' <- (newObject Gtk.Widget.Widget) result
    touchManagedPtr context
    return result'


-- function drag_finish
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "DragContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the drag context." , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "success"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "a flag indicating whether the drop was successful"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "del"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "a flag indicating whether the source should delete the\n  original data. (This should be %TRUE for a move)"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "time_"
--           , argType = TBasicType TUInt32
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "the timestamp from the #GtkWidget::drag-drop signal."
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

foreign import ccall "gtk_drag_finish" gtk_drag_finish :: 
    Ptr Gdk.DragContext.DragContext ->      -- context : TInterface (Name {namespace = "Gdk", name = "DragContext"})
    CInt ->                                 -- success : TBasicType TBoolean
    CInt ->                                 -- del : TBasicType TBoolean
    Word32 ->                               -- time_ : TBasicType TUInt32
    IO ()

-- | /No description available in the introspection data./
dragFinish ::
    (B.CallStack.HasCallStack, MonadIO m, Gdk.DragContext.IsDragContext a) =>
    a
    -- ^ /@context@/: the drag context.
    -> Bool
    -- ^ /@success@/: a flag indicating whether the drop was successful
    -> Bool
    -- ^ /@del@/: a flag indicating whether the source should delete the
    --   original data. (This should be 'P.True' for a move)
    -> Word32
    -- ^ /@time_@/: the timestamp from the [Widget::dragDrop]("GI.Gtk.Objects.Widget#g:signal:dragDrop") signal.
    -> m ()
dragFinish context success del time_ = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    let success' = (fromIntegral . fromEnum) success
    let del' = (fromIntegral . fromEnum) del
    gtk_drag_finish context' success' del' time_
    touchManagedPtr context
    return ()


-- function drag_cancel
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "DragContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "a #GdkDragContext, as e.g. returned by gtk_drag_begin_with_coordinates()"
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

foreign import ccall "gtk_drag_cancel" gtk_drag_cancel :: 
    Ptr Gdk.DragContext.DragContext ->      -- context : TInterface (Name {namespace = "Gdk", name = "DragContext"})
    IO ()

-- | /No description available in the introspection data./
dragCancel ::
    (B.CallStack.HasCallStack, MonadIO m, Gdk.DragContext.IsDragContext a) =>
    a
    -- ^ /@context@/: a t'GI.Gdk.Objects.DragContext.DragContext', as e.g. returned by 'GI.Gtk.Objects.Widget.widgetDragBeginWithCoordinates'
    -> m ()
dragCancel context = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    gtk_drag_cancel context'
    touchManagedPtr context
    return ()


-- function distribute_natural_allocation
-- Args: [ Arg
--           { argCName = "extra_space"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "Extra space to redistribute among children after subtracting\n              minimum sizes and any child padding from the overall allocation"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "n_requested_sizes"
--           , argType = TBasicType TUInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Number of requests to fit into the allocation"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "sizes"
--           , argType =
--               TCArray
--                 False
--                 (-1)
--                 1
--                 (TInterface Name { namespace = "Gtk" , name = "RequestedSize" })
--           , direction = DirectionInout
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "An array of structs with a client pointer and a minimum/natural size\n        in the orientation of the allocation."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = True
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: [ Arg
--              { argCName = "n_requested_sizes"
--              , argType = TBasicType TUInt
--              , direction = DirectionIn
--              , mayBeNull = False
--              , argDoc =
--                  Documentation
--                    { rawDocText = Just "Number of requests to fit into the allocation"
--                    , sinceVersion = Nothing
--                    }
--              , argScope = ScopeTypeInvalid
--              , argClosure = -1
--              , argDestroy = -1
--              , argCallerAllocates = False
--              , transfer = TransferNothing
--              }
--          ]
-- returnType: Just (TBasicType TInt)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_distribute_natural_allocation" gtk_distribute_natural_allocation :: 
    Int32 ->                                -- extra_space : TBasicType TInt
    Word32 ->                               -- n_requested_sizes : TBasicType TUInt
    Ptr Gtk.RequestedSize.RequestedSize ->  -- sizes : TCArray False (-1) 1 (TInterface (Name {namespace = "Gtk", name = "RequestedSize"}))
    IO Int32

-- | Distributes /@extraSpace@/ to child /@sizes@/ by bringing smaller
-- children up to natural size first.
-- 
-- The remaining space will be added to the /@minimumSize@/ member of the
-- GtkRequestedSize struct. If all sizes reach their natural size then
-- the remaining space is returned.
distributeNaturalAllocation ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    Int32
    -- ^ /@extraSpace@/: Extra space to redistribute among children after subtracting
    --               minimum sizes and any child padding from the overall allocation
    -> [Gtk.RequestedSize.RequestedSize]
    -- ^ /@sizes@/: An array of structs with a client pointer and a minimum\/natural size
    --         in the orientation of the allocation.
    -> m ((Int32, [Gtk.RequestedSize.RequestedSize]))
    -- ^ __Returns:__ The remainder of /@extraSpace@/ after redistributing space
    -- to /@sizes@/.
distributeNaturalAllocation extraSpace sizes = liftIO $ do
    let nRequestedSizes = fromIntegral $ P.length sizes
    sizes' <- mapM unsafeManagedPtrGetPtr sizes
    sizes'' <- packBlockArray 16 sizes'
    result <- gtk_distribute_natural_allocation extraSpace nRequestedSizes sizes''
    sizes''' <- (unpackBlockArrayWithLength 16 nRequestedSizes) sizes''
    sizes'''' <- mapM (wrapPtr Gtk.RequestedSize.RequestedSize) sizes'''
    freeMem sizes''
    mapM_ touchManagedPtr sizes
    return (result, sizes'''')


-- function disable_setlocale
-- Args: []
-- Lengths: []
-- returnType: Nothing
-- throws : False
-- Skip return : False

foreign import ccall "gtk_disable_setlocale" gtk_disable_setlocale :: 
    IO ()

-- | Prevents 'GI.Gtk.Functions.init', 'GI.Gtk.Functions.initCheck', 'GI.Gtk.Functions.initWithArgs' and
-- 'GI.Gtk.Functions.parseArgs' from automatically
-- calling @setlocale (LC_ALL, \"\")@. You would
-- want to use this function if you wanted to set the locale for
-- your program to something other than the user’s locale, or if
-- you wanted to set different values for different locale categories.
-- 
-- Most programs should not need to call this function.
disableSetlocale ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m ()
disableSetlocale  = liftIO $ do
    gtk_disable_setlocale
    return ()


-- function device_grab_remove
-- Args: [ Arg
--           { argCName = "widget"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Widget" }
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
--           { argCName = "device"
--           , argType = TInterface Name { namespace = "Gdk" , name = "Device" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GdkDevice" , sinceVersion = Nothing }
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

foreign import ccall "gtk_device_grab_remove" gtk_device_grab_remove :: 
    Ptr Gtk.Widget.Widget ->                -- widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    Ptr Gdk.Device.Device ->                -- device : TInterface (Name {namespace = "Gdk", name = "Device"})
    IO ()

-- | Removes a device grab from the given widget.
-- 
-- You have to pair calls to 'GI.Gtk.Functions.deviceGrabAdd' and
-- 'GI.Gtk.Functions.deviceGrabRemove'.
-- 
-- /Since: 3.0/
deviceGrabRemove ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.Widget.IsWidget a, Gdk.Device.IsDevice b) =>
    a
    -- ^ /@widget@/: a t'GI.Gtk.Objects.Widget.Widget'
    -> b
    -- ^ /@device@/: a t'GI.Gdk.Objects.Device.Device'
    -> m ()
deviceGrabRemove widget device = liftIO $ do
    widget' <- unsafeManagedPtrCastPtr widget
    device' <- unsafeManagedPtrCastPtr device
    gtk_device_grab_remove widget' device'
    touchManagedPtr widget
    touchManagedPtr device
    return ()


-- function device_grab_add
-- Args: [ Arg
--           { argCName = "widget"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Widget" }
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
--           { argCName = "device"
--           , argType = TInterface Name { namespace = "Gdk" , name = "Device" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GdkDevice to grab on."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "block_others"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "%TRUE to prevent other devices to interact with @widget."
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

foreign import ccall "gtk_device_grab_add" gtk_device_grab_add :: 
    Ptr Gtk.Widget.Widget ->                -- widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    Ptr Gdk.Device.Device ->                -- device : TInterface (Name {namespace = "Gdk", name = "Device"})
    CInt ->                                 -- block_others : TBasicType TBoolean
    IO ()

-- | Adds a GTK+ grab on /@device@/, so all the events on /@device@/ and its
-- associated pointer or keyboard (if any) are delivered to /@widget@/.
-- If the /@blockOthers@/ parameter is 'P.True', any other devices will be
-- unable to interact with /@widget@/ during the grab.
-- 
-- /Since: 3.0/
deviceGrabAdd ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.Widget.IsWidget a, Gdk.Device.IsDevice b) =>
    a
    -- ^ /@widget@/: a t'GI.Gtk.Objects.Widget.Widget'
    -> b
    -- ^ /@device@/: a t'GI.Gdk.Objects.Device.Device' to grab on.
    -> Bool
    -- ^ /@blockOthers@/: 'P.True' to prevent other devices to interact with /@widget@/.
    -> m ()
deviceGrabAdd widget device blockOthers = liftIO $ do
    widget' <- unsafeManagedPtrCastPtr widget
    device' <- unsafeManagedPtrCastPtr device
    let blockOthers' = (fromIntegral . fromEnum) blockOthers
    gtk_device_grab_add widget' device' blockOthers'
    touchManagedPtr widget
    touchManagedPtr device
    return ()


-- function check_version
-- Args: [ Arg
--           { argCName = "required_major"
--           , argType = TBasicType TUInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the required major version"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "required_minor"
--           , argType = TBasicType TUInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the required minor version"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "required_micro"
--           , argType = TBasicType TUInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the required micro version"
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

foreign import ccall "gtk_check_version" gtk_check_version :: 
    Word32 ->                               -- required_major : TBasicType TUInt
    Word32 ->                               -- required_minor : TBasicType TUInt
    Word32 ->                               -- required_micro : TBasicType TUInt
    IO CString

-- | Checks that the GTK+ library in use is compatible with the
-- given version. Generally you would pass in the constants
-- 'GI.Gtk.Constants.MAJOR_VERSION', 'GI.Gtk.Constants.MINOR_VERSION', 'GI.Gtk.Constants.MICRO_VERSION'
-- as the three arguments to this function; that produces
-- a check that the library in use is compatible with
-- the version of GTK+ the application or module was compiled
-- against.
-- 
-- Compatibility is defined by two things: first the version
-- of the running library is newer than the version
-- /@requiredMajor@/.required_minor./@requiredMicro@/. Second
-- the running library must be binary compatible with the
-- version /@requiredMajor@/.required_minor./@requiredMicro@/
-- (same major version.)
-- 
-- This function is primarily for GTK+ modules; the module
-- can call this function to check that it wasn’t loaded
-- into an incompatible version of GTK+. However, such a
-- check isn’t completely reliable, since the module may be
-- linked against an old version of GTK+ and calling the
-- old version of 'GI.Gtk.Functions.checkVersion', but still get loaded
-- into an application using a newer version of GTK+.
checkVersion ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    Word32
    -- ^ /@requiredMajor@/: the required major version
    -> Word32
    -- ^ /@requiredMinor@/: the required minor version
    -> Word32
    -- ^ /@requiredMicro@/: the required micro version
    -> m (Maybe T.Text)
    -- ^ __Returns:__ 'P.Nothing' if the GTK+ library is compatible with the
    --   given version, or a string describing the version mismatch.
    --   The returned string is owned by GTK+ and should not be modified
    --   or freed.
checkVersion requiredMajor requiredMinor requiredMicro = liftIO $ do
    result <- gtk_check_version requiredMajor requiredMinor requiredMicro
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- cstringToText result'
        return result''
    return maybeResult


-- function cairo_transform_to_window
-- Args: [ Arg
--           { argCName = "cr"
--           , argType =
--               TInterface Name { namespace = "cairo" , name = "Context" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the cairo context to transform"
--                 , sinceVersion = Nothing
--                 }
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
--                 { rawDocText =
--                     Just "the widget the context is currently centered for"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gdk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the window to transform the context to"
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

foreign import ccall "gtk_cairo_transform_to_window" gtk_cairo_transform_to_window :: 
    Ptr Cairo.Context.Context ->            -- cr : TInterface (Name {namespace = "cairo", name = "Context"})
    Ptr Gtk.Widget.Widget ->                -- widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    Ptr Gdk.Window.Window ->                -- window : TInterface (Name {namespace = "Gdk", name = "Window"})
    IO ()

-- | Transforms the given cairo context /@cr@/ that from /@widget@/-relative
-- coordinates to /@window@/-relative coordinates.
-- If the /@widget@/’s window is not an ancestor of /@window@/, no
-- modification will be applied.
-- 
-- This is the inverse to the transformation GTK applies when
-- preparing an expose event to be emitted with the [Widget::draw]("GI.Gtk.Objects.Widget#g:signal:draw")
-- signal. It is intended to help porting multiwindow widgets from
-- GTK+ 2 to the rendering architecture of GTK+ 3.
-- 
-- /Since: 3.0/
cairoTransformToWindow ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.Widget.IsWidget a, Gdk.Window.IsWindow b) =>
    Cairo.Context.Context
    -- ^ /@cr@/: the cairo context to transform
    -> a
    -- ^ /@widget@/: the widget the context is currently centered for
    -> b
    -- ^ /@window@/: the window to transform the context to
    -> m ()
cairoTransformToWindow cr widget window = liftIO $ do
    cr' <- unsafeManagedPtrGetPtr cr
    widget' <- unsafeManagedPtrCastPtr widget
    window' <- unsafeManagedPtrCastPtr window
    gtk_cairo_transform_to_window cr' widget' window'
    touchManagedPtr cr
    touchManagedPtr widget
    touchManagedPtr window
    return ()


-- function cairo_should_draw_window
-- Args: [ Arg
--           { argCName = "cr"
--           , argType =
--               TInterface Name { namespace = "cairo" , name = "Context" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a cairo context" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gdk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the window to check. @window may not be an input-only\n         window."
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

foreign import ccall "gtk_cairo_should_draw_window" gtk_cairo_should_draw_window :: 
    Ptr Cairo.Context.Context ->            -- cr : TInterface (Name {namespace = "cairo", name = "Context"})
    Ptr Gdk.Window.Window ->                -- window : TInterface (Name {namespace = "Gdk", name = "Window"})
    IO CInt

-- | This function is supposed to be called in [Widget::draw]("GI.Gtk.Objects.Widget#g:signal:draw")
-- implementations for widgets that support multiple windows.
-- /@cr@/ must be untransformed from invoking of the draw function.
-- This function will return 'P.True' if the contents of the given
-- /@window@/ are supposed to be drawn and 'P.False' otherwise. Note
-- that when the drawing was not initiated by the windowing
-- system this function will return 'P.True' for all windows, so
-- you need to draw the bottommost window first. Also, do not
-- use “else if” statements to check which window should be drawn.
-- 
-- /Since: 3.0/
cairoShouldDrawWindow ::
    (B.CallStack.HasCallStack, MonadIO m, Gdk.Window.IsWindow a) =>
    Cairo.Context.Context
    -- ^ /@cr@/: a cairo context
    -> a
    -- ^ /@window@/: the window to check. /@window@/ may not be an input-only
    --          window.
    -> m Bool
    -- ^ __Returns:__ 'P.True' if /@window@/ should be drawn
cairoShouldDrawWindow cr window = liftIO $ do
    cr' <- unsafeManagedPtrGetPtr cr
    window' <- unsafeManagedPtrCastPtr window
    result <- gtk_cairo_should_draw_window cr' window'
    let result' = (/= 0) result
    touchManagedPtr cr
    touchManagedPtr window
    return result'


-- function bindings_activate_event
-- Args: [ Arg
--           { argCName = "object"
--           , argType =
--               TInterface Name { namespace = "GObject" , name = "Object" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GObject (generally must be a widget)"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "event"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "EventKey" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GdkEventKey" , sinceVersion = Nothing }
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

foreign import ccall "gtk_bindings_activate_event" gtk_bindings_activate_event :: 
    Ptr GObject.Object.Object ->            -- object : TInterface (Name {namespace = "GObject", name = "Object"})
    Ptr Gdk.EventKey.EventKey ->            -- event : TInterface (Name {namespace = "Gdk", name = "EventKey"})
    IO CInt

-- | Looks up key bindings for /@object@/ to find one matching
-- /@event@/, and if one was found, activate it.
-- 
-- /Since: 2.4/
bindingsActivateEvent ::
    (B.CallStack.HasCallStack, MonadIO m, GObject.Object.IsObject a) =>
    a
    -- ^ /@object@/: a t'GI.GObject.Objects.Object.Object' (generally must be a widget)
    -> Gdk.EventKey.EventKey
    -- ^ /@event@/: a t'GI.Gdk.Structs.EventKey.EventKey'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if a matching key binding was found
bindingsActivateEvent object event = liftIO $ do
    object' <- unsafeManagedPtrCastPtr object
    event' <- unsafeManagedPtrGetPtr event
    result <- gtk_bindings_activate_event object' event'
    let result' = (/= 0) result
    touchManagedPtr object
    touchManagedPtr event
    return result'


-- function bindings_activate
-- Args: [ Arg
--           { argCName = "object"
--           , argType =
--               TInterface Name { namespace = "GObject" , name = "Object" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "object to activate when binding found"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "keyval"
--           , argType = TBasicType TUInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "key value of the binding"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "modifiers"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "ModifierType" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "key modifier of the binding"
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

foreign import ccall "gtk_bindings_activate" gtk_bindings_activate :: 
    Ptr GObject.Object.Object ->            -- object : TInterface (Name {namespace = "GObject", name = "Object"})
    Word32 ->                               -- keyval : TBasicType TUInt
    CUInt ->                                -- modifiers : TInterface (Name {namespace = "Gdk", name = "ModifierType"})
    IO CInt

-- | Find a key binding matching /@keyval@/ and /@modifiers@/ and activate the
-- binding on /@object@/.
bindingsActivate ::
    (B.CallStack.HasCallStack, MonadIO m, GObject.Object.IsObject a) =>
    a
    -- ^ /@object@/: object to activate when binding found
    -> Word32
    -- ^ /@keyval@/: key value of the binding
    -> [Gdk.Flags.ModifierType]
    -- ^ /@modifiers@/: key modifier of the binding
    -> m Bool
    -- ^ __Returns:__ 'P.True' if a binding was found and activated
bindingsActivate object keyval modifiers = liftIO $ do
    object' <- unsafeManagedPtrCastPtr object
    let modifiers' = gflagsToWord modifiers
    result <- gtk_bindings_activate object' keyval modifiers'
    let result' = (/= 0) result
    touchManagedPtr object
    return result'


-- function alternative_dialog_button_order
-- Args: [ Arg
--           { argCName = "screen"
--           , argType = TInterface Name { namespace = "Gdk" , name = "Screen" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "a #GdkScreen, or %NULL to use the default screen"
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

foreign import ccall "gtk_alternative_dialog_button_order" gtk_alternative_dialog_button_order :: 
    Ptr Gdk.Screen.Screen ->                -- screen : TInterface (Name {namespace = "Gdk", name = "Screen"})
    IO CInt

{-# DEPRECATED alternativeDialogButtonOrder ["(Since version 3.10)","Deprecated"] #-}
-- | Returns 'P.True' if dialogs are expected to use an alternative
-- button order on the screen /@screen@/. See
-- @/gtk_dialog_set_alternative_button_order()/@ for more details
-- about alternative button order.
-- 
-- If you need to use this function, you should probably connect
-- to the [notify](#g:signal:notify):gtk-alternative-button-order signal on the
-- t'GI.Gtk.Objects.Settings.Settings' object associated to /@screen@/, in order to be
-- notified if the button order setting changes.
-- 
-- /Since: 2.6/
alternativeDialogButtonOrder ::
    (B.CallStack.HasCallStack, MonadIO m, Gdk.Screen.IsScreen a) =>
    Maybe (a)
    -- ^ /@screen@/: a t'GI.Gdk.Objects.Screen.Screen', or 'P.Nothing' to use the default screen
    -> m Bool
    -- ^ __Returns:__ Whether the alternative button order should be used
alternativeDialogButtonOrder screen = liftIO $ do
    maybeScreen <- case screen of
        Nothing -> return nullPtr
        Just jScreen -> do
            jScreen' <- unsafeManagedPtrCastPtr jScreen
            return jScreen'
    result <- gtk_alternative_dialog_button_order maybeScreen
    let result' = (/= 0) result
    whenJust screen touchManagedPtr
    return result'


-- function accelerator_valid
-- Args: [ Arg
--           { argCName = "keyval"
--           , argType = TBasicType TUInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a GDK keyval" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "modifiers"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "ModifierType" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "modifier mask" , sinceVersion = Nothing }
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

foreign import ccall "gtk_accelerator_valid" gtk_accelerator_valid :: 
    Word32 ->                               -- keyval : TBasicType TUInt
    CUInt ->                                -- modifiers : TInterface (Name {namespace = "Gdk", name = "ModifierType"})
    IO CInt

-- | Determines whether a given keyval and modifier mask constitute
-- a valid keyboard accelerator. For example, the 'GI.Gdk.Constants.KEY_a' keyval
-- plus @/GDK_CONTROL_MASK/@ is valid - this is a “Ctrl+a” accelerator.
-- But, you can\'t, for instance, use the 'GI.Gdk.Constants.KEY_Control_L' keyval
-- as an accelerator.
acceleratorValid ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    Word32
    -- ^ /@keyval@/: a GDK keyval
    -> [Gdk.Flags.ModifierType]
    -- ^ /@modifiers@/: modifier mask
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the accelerator is valid
acceleratorValid keyval modifiers = liftIO $ do
    let modifiers' = gflagsToWord modifiers
    result <- gtk_accelerator_valid keyval modifiers'
    let result' = (/= 0) result
    return result'


-- function accelerator_set_default_mod_mask
-- Args: [ Arg
--           { argCName = "default_mod_mask"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "ModifierType" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "accelerator modifier mask"
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

foreign import ccall "gtk_accelerator_set_default_mod_mask" gtk_accelerator_set_default_mod_mask :: 
    CUInt ->                                -- default_mod_mask : TInterface (Name {namespace = "Gdk", name = "ModifierType"})
    IO ()

-- | Sets the modifiers that will be considered significant for keyboard
-- accelerators. The default mod mask depends on the GDK backend in use,
-- but will typically include @/GDK_CONTROL_MASK/@ | @/GDK_SHIFT_MASK/@ |
-- @/GDK_MOD1_MASK/@ | @/GDK_SUPER_MASK/@ | @/GDK_HYPER_MASK/@ | @/GDK_META_MASK/@.
-- In other words, Control, Shift, Alt, Super, Hyper and Meta. Other
-- modifiers will by default be ignored by t'GI.Gtk.Objects.AccelGroup.AccelGroup'.
-- 
-- You must include at least the three modifiers Control, Shift
-- and Alt in any value you pass to this function.
-- 
-- The default mod mask should be changed on application startup,
-- before using any accelerator groups.
acceleratorSetDefaultModMask ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    [Gdk.Flags.ModifierType]
    -- ^ /@defaultModMask@/: accelerator modifier mask
    -> m ()
acceleratorSetDefaultModMask defaultModMask = liftIO $ do
    let defaultModMask' = gflagsToWord defaultModMask
    gtk_accelerator_set_default_mod_mask defaultModMask'
    return ()


-- function accelerator_parse_with_keycode
-- Args: [ Arg
--           { argCName = "accelerator"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "string representing an accelerator"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "accelerator_key"
--           , argType = TBasicType TUInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "return location for accelerator\n    keyval, or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       , Arg
--           { argCName = "accelerator_codes"
--           , argType = TCArray True (-1) (-1) (TBasicType TUInt)
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "\n    return location for accelerator keycodes, or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       , Arg
--           { argCName = "accelerator_mods"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "ModifierType" }
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "return location for accelerator\n    modifier mask, %NULL"
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
-- returnType: Nothing
-- throws : False
-- Skip return : False

foreign import ccall "gtk_accelerator_parse_with_keycode" gtk_accelerator_parse_with_keycode :: 
    CString ->                              -- accelerator : TBasicType TUTF8
    Ptr Word32 ->                           -- accelerator_key : TBasicType TUInt
    Ptr (Ptr Word32) ->                     -- accelerator_codes : TCArray True (-1) (-1) (TBasicType TUInt)
    Ptr CUInt ->                            -- accelerator_mods : TInterface (Name {namespace = "Gdk", name = "ModifierType"})
    IO ()

-- | Parses a string representing an accelerator, similarly to
-- 'GI.Gtk.Functions.acceleratorParse' but handles keycodes as well. This is only
-- useful for system-level components, applications should use
-- 'GI.Gtk.Functions.acceleratorParse' instead.
-- 
-- If /@acceleratorCodes@/ is given and the result stored in it is non-'P.Nothing',
-- the result must be freed with 'GI.GLib.Functions.free'.
-- 
-- If a keycode is present in the accelerator and no /@acceleratorCodes@/
-- is given, the parse will fail.
-- 
-- If the parse fails, /@acceleratorKey@/, /@acceleratorMods@/ and
-- /@acceleratorCodes@/ will be set to 0 (zero).
-- 
-- /Since: 3.4/
acceleratorParseWithKeycode ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    T.Text
    -- ^ /@accelerator@/: string representing an accelerator
    -> m ((Word32, [Word32], [Gdk.Flags.ModifierType]))
acceleratorParseWithKeycode accelerator = liftIO $ do
    accelerator' <- textToCString accelerator
    acceleratorKey <- allocMem :: IO (Ptr Word32)
    acceleratorCodes <- callocMem :: IO (Ptr (Ptr Word32))
    acceleratorMods <- allocMem :: IO (Ptr CUInt)
    gtk_accelerator_parse_with_keycode accelerator' acceleratorKey acceleratorCodes acceleratorMods
    acceleratorKey' <- peek acceleratorKey
    acceleratorCodes' <- peek acceleratorCodes
    acceleratorCodes'' <- unpackZeroTerminatedStorableArray acceleratorCodes'
    freeMem acceleratorCodes'
    acceleratorMods' <- peek acceleratorMods
    let acceleratorMods'' = wordToGFlags acceleratorMods'
    freeMem accelerator'
    freeMem acceleratorKey
    freeMem acceleratorCodes
    freeMem acceleratorMods
    return (acceleratorKey', acceleratorCodes'', acceleratorMods'')


-- function accelerator_parse
-- Args: [ Arg
--           { argCName = "accelerator"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "string representing an accelerator"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "accelerator_key"
--           , argType = TBasicType TUInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "return location for accelerator\n    keyval, or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       , Arg
--           { argCName = "accelerator_mods"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "ModifierType" }
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "return location for accelerator\n    modifier mask, %NULL"
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
-- returnType: Nothing
-- throws : False
-- Skip return : False

foreign import ccall "gtk_accelerator_parse" gtk_accelerator_parse :: 
    CString ->                              -- accelerator : TBasicType TUTF8
    Ptr Word32 ->                           -- accelerator_key : TBasicType TUInt
    Ptr CUInt ->                            -- accelerator_mods : TInterface (Name {namespace = "Gdk", name = "ModifierType"})
    IO ()

-- | Parses a string representing an accelerator. The format looks like
-- “\<Control>a” or “\<Shift>\<Alt>F1” or “\<Release>z” (the last one is
-- for key release).
-- 
-- The parser is fairly liberal and allows lower or upper case, and also
-- abbreviations such as “\<Ctl>” and “\<Ctrl>”. Key names are parsed using
-- 'GI.Gdk.Functions.keyvalFromName'. For character keys the name is not the symbol,
-- but the lowercase name, e.g. one would use “\<Ctrl>minus” instead of
-- “\<Ctrl>-”.
-- 
-- If the parse fails, /@acceleratorKey@/ and /@acceleratorMods@/ will
-- be set to 0 (zero).
acceleratorParse ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    T.Text
    -- ^ /@accelerator@/: string representing an accelerator
    -> m ((Word32, [Gdk.Flags.ModifierType]))
acceleratorParse accelerator = liftIO $ do
    accelerator' <- textToCString accelerator
    acceleratorKey <- allocMem :: IO (Ptr Word32)
    acceleratorMods <- allocMem :: IO (Ptr CUInt)
    gtk_accelerator_parse accelerator' acceleratorKey acceleratorMods
    acceleratorKey' <- peek acceleratorKey
    acceleratorMods' <- peek acceleratorMods
    let acceleratorMods'' = wordToGFlags acceleratorMods'
    freeMem accelerator'
    freeMem acceleratorKey
    freeMem acceleratorMods
    return (acceleratorKey', acceleratorMods'')


-- function accelerator_name_with_keycode
-- Args: [ Arg
--           { argCName = "display"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "Display" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "a #GdkDisplay or %NULL to use the default display"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "accelerator_key"
--           , argType = TBasicType TUInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "accelerator keyval" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "keycode"
--           , argType = TBasicType TUInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "accelerator keycode"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "accelerator_mods"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "ModifierType" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "accelerator modifier mask"
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

foreign import ccall "gtk_accelerator_name_with_keycode" gtk_accelerator_name_with_keycode :: 
    Ptr Gdk.Display.Display ->              -- display : TInterface (Name {namespace = "Gdk", name = "Display"})
    Word32 ->                               -- accelerator_key : TBasicType TUInt
    Word32 ->                               -- keycode : TBasicType TUInt
    CUInt ->                                -- accelerator_mods : TInterface (Name {namespace = "Gdk", name = "ModifierType"})
    IO CString

-- | Converts an accelerator keyval and modifier mask
-- into a string parseable by 'GI.Gtk.Functions.acceleratorParseWithKeycode',
-- similarly to 'GI.Gtk.Functions.acceleratorName' but handling keycodes.
-- This is only useful for system-level components, applications
-- should use 'GI.Gtk.Functions.acceleratorParse' instead.
-- 
-- /Since: 3.4/
acceleratorNameWithKeycode ::
    (B.CallStack.HasCallStack, MonadIO m, Gdk.Display.IsDisplay a) =>
    Maybe (a)
    -- ^ /@display@/: a t'GI.Gdk.Objects.Display.Display' or 'P.Nothing' to use the default display
    -> Word32
    -- ^ /@acceleratorKey@/: accelerator keyval
    -> Word32
    -- ^ /@keycode@/: accelerator keycode
    -> [Gdk.Flags.ModifierType]
    -- ^ /@acceleratorMods@/: accelerator modifier mask
    -> m T.Text
    -- ^ __Returns:__ a newly allocated accelerator name.
acceleratorNameWithKeycode display acceleratorKey keycode acceleratorMods = liftIO $ do
    maybeDisplay <- case display of
        Nothing -> return nullPtr
        Just jDisplay -> do
            jDisplay' <- unsafeManagedPtrCastPtr jDisplay
            return jDisplay'
    let acceleratorMods' = gflagsToWord acceleratorMods
    result <- gtk_accelerator_name_with_keycode maybeDisplay acceleratorKey keycode acceleratorMods'
    checkUnexpectedReturnNULL "acceleratorNameWithKeycode" result
    result' <- cstringToText result
    freeMem result
    whenJust display touchManagedPtr
    return result'


-- function accelerator_name
-- Args: [ Arg
--           { argCName = "accelerator_key"
--           , argType = TBasicType TUInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "accelerator keyval" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "accelerator_mods"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "ModifierType" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "accelerator modifier mask"
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

foreign import ccall "gtk_accelerator_name" gtk_accelerator_name :: 
    Word32 ->                               -- accelerator_key : TBasicType TUInt
    CUInt ->                                -- accelerator_mods : TInterface (Name {namespace = "Gdk", name = "ModifierType"})
    IO CString

-- | Converts an accelerator keyval and modifier mask into a string
-- parseable by 'GI.Gtk.Functions.acceleratorParse'. For example, if you pass in
-- 'GI.Gdk.Constants.KEY_q' and @/GDK_CONTROL_MASK/@, this function returns “\<Control>q”.
-- 
-- If you need to display accelerators in the user interface,
-- see 'GI.Gtk.Functions.acceleratorGetLabel'.
acceleratorName ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    Word32
    -- ^ /@acceleratorKey@/: accelerator keyval
    -> [Gdk.Flags.ModifierType]
    -- ^ /@acceleratorMods@/: accelerator modifier mask
    -> m T.Text
    -- ^ __Returns:__ a newly-allocated accelerator name
acceleratorName acceleratorKey acceleratorMods = liftIO $ do
    let acceleratorMods' = gflagsToWord acceleratorMods
    result <- gtk_accelerator_name acceleratorKey acceleratorMods'
    checkUnexpectedReturnNULL "acceleratorName" result
    result' <- cstringToText result
    freeMem result
    return result'


-- function accelerator_get_label_with_keycode
-- Args: [ Arg
--           { argCName = "display"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "Display" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "a #GdkDisplay or %NULL to use the default display"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "accelerator_key"
--           , argType = TBasicType TUInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "accelerator keyval" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "keycode"
--           , argType = TBasicType TUInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "accelerator keycode"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "accelerator_mods"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "ModifierType" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "accelerator modifier mask"
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

foreign import ccall "gtk_accelerator_get_label_with_keycode" gtk_accelerator_get_label_with_keycode :: 
    Ptr Gdk.Display.Display ->              -- display : TInterface (Name {namespace = "Gdk", name = "Display"})
    Word32 ->                               -- accelerator_key : TBasicType TUInt
    Word32 ->                               -- keycode : TBasicType TUInt
    CUInt ->                                -- accelerator_mods : TInterface (Name {namespace = "Gdk", name = "ModifierType"})
    IO CString

-- | Converts an accelerator keyval and modifier mask
-- into a (possibly translated) string that can be displayed to
-- a user, similarly to 'GI.Gtk.Functions.acceleratorGetLabel', but handling
-- keycodes.
-- 
-- This is only useful for system-level components, applications
-- should use 'GI.Gtk.Functions.acceleratorParse' instead.
-- 
-- /Since: 3.4/
acceleratorGetLabelWithKeycode ::
    (B.CallStack.HasCallStack, MonadIO m, Gdk.Display.IsDisplay a) =>
    Maybe (a)
    -- ^ /@display@/: a t'GI.Gdk.Objects.Display.Display' or 'P.Nothing' to use the default display
    -> Word32
    -- ^ /@acceleratorKey@/: accelerator keyval
    -> Word32
    -- ^ /@keycode@/: accelerator keycode
    -> [Gdk.Flags.ModifierType]
    -- ^ /@acceleratorMods@/: accelerator modifier mask
    -> m T.Text
    -- ^ __Returns:__ a newly-allocated string representing the accelerator.
acceleratorGetLabelWithKeycode display acceleratorKey keycode acceleratorMods = liftIO $ do
    maybeDisplay <- case display of
        Nothing -> return nullPtr
        Just jDisplay -> do
            jDisplay' <- unsafeManagedPtrCastPtr jDisplay
            return jDisplay'
    let acceleratorMods' = gflagsToWord acceleratorMods
    result <- gtk_accelerator_get_label_with_keycode maybeDisplay acceleratorKey keycode acceleratorMods'
    checkUnexpectedReturnNULL "acceleratorGetLabelWithKeycode" result
    result' <- cstringToText result
    freeMem result
    whenJust display touchManagedPtr
    return result'


-- function accelerator_get_label
-- Args: [ Arg
--           { argCName = "accelerator_key"
--           , argType = TBasicType TUInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "accelerator keyval" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "accelerator_mods"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "ModifierType" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "accelerator modifier mask"
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

foreign import ccall "gtk_accelerator_get_label" gtk_accelerator_get_label :: 
    Word32 ->                               -- accelerator_key : TBasicType TUInt
    CUInt ->                                -- accelerator_mods : TInterface (Name {namespace = "Gdk", name = "ModifierType"})
    IO CString

-- | Converts an accelerator keyval and modifier mask into a string
-- which can be used to represent the accelerator to the user.
-- 
-- /Since: 2.6/
acceleratorGetLabel ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    Word32
    -- ^ /@acceleratorKey@/: accelerator keyval
    -> [Gdk.Flags.ModifierType]
    -- ^ /@acceleratorMods@/: accelerator modifier mask
    -> m T.Text
    -- ^ __Returns:__ a newly-allocated string representing the accelerator.
acceleratorGetLabel acceleratorKey acceleratorMods = liftIO $ do
    let acceleratorMods' = gflagsToWord acceleratorMods
    result <- gtk_accelerator_get_label acceleratorKey acceleratorMods'
    checkUnexpectedReturnNULL "acceleratorGetLabel" result
    result' <- cstringToText result
    freeMem result
    return result'


-- function accelerator_get_default_mod_mask
-- Args: []
-- Lengths: []
-- returnType: Just
--               (TInterface Name { namespace = "Gdk" , name = "ModifierType" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_accelerator_get_default_mod_mask" gtk_accelerator_get_default_mod_mask :: 
    IO CUInt

-- | Gets the modifier mask.
-- 
-- The modifier mask determines which modifiers are considered significant
-- for keyboard accelerators. See 'GI.Gtk.Functions.acceleratorSetDefaultModMask'.
acceleratorGetDefaultModMask ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m [Gdk.Flags.ModifierType]
    -- ^ __Returns:__ the default accelerator modifier mask
acceleratorGetDefaultModMask  = liftIO $ do
    result <- gtk_accelerator_get_default_mod_mask
    let result' = wordToGFlags result
    return result'


-- function accel_groups_from_object
-- Args: [ Arg
--           { argCName = "object"
--           , argType =
--               TInterface Name { namespace = "GObject" , name = "Object" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GObject, usually a #GtkWindow"
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
--               (TGSList
--                  (TInterface Name { namespace = "Gtk" , name = "AccelGroup" }))
-- throws : False
-- Skip return : False

foreign import ccall "gtk_accel_groups_from_object" gtk_accel_groups_from_object :: 
    Ptr GObject.Object.Object ->            -- object : TInterface (Name {namespace = "GObject", name = "Object"})
    IO (Ptr (GSList (Ptr Gtk.AccelGroup.AccelGroup)))

-- | Gets a list of all accel groups which are attached to /@object@/.
accelGroupsFromObject ::
    (B.CallStack.HasCallStack, MonadIO m, GObject.Object.IsObject a) =>
    a
    -- ^ /@object@/: a t'GI.GObject.Objects.Object.Object', usually a t'GI.Gtk.Objects.Window.Window'
    -> m [Gtk.AccelGroup.AccelGroup]
    -- ^ __Returns:__ a list of
    --     all accel groups which are attached to /@object@/
accelGroupsFromObject object = liftIO $ do
    object' <- unsafeManagedPtrCastPtr object
    result <- gtk_accel_groups_from_object object'
    result' <- unpackGSList result
    result'' <- mapM (newObject Gtk.AccelGroup.AccelGroup) result'
    touchManagedPtr object
    return result''


-- function accel_groups_activate
-- Args: [ Arg
--           { argCName = "object"
--           , argType =
--               TInterface Name { namespace = "GObject" , name = "Object" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the #GObject, usually a #GtkWindow, on which\n    to activate the accelerator"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "accel_key"
--           , argType = TBasicType TUInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "accelerator keyval from a key event"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "accel_mods"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "ModifierType" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "keyboard state mask from a key event"
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

foreign import ccall "gtk_accel_groups_activate" gtk_accel_groups_activate :: 
    Ptr GObject.Object.Object ->            -- object : TInterface (Name {namespace = "GObject", name = "Object"})
    Word32 ->                               -- accel_key : TBasicType TUInt
    CUInt ->                                -- accel_mods : TInterface (Name {namespace = "Gdk", name = "ModifierType"})
    IO CInt

-- | Finds the first accelerator in any t'GI.Gtk.Objects.AccelGroup.AccelGroup' attached
-- to /@object@/ that matches /@accelKey@/ and /@accelMods@/, and
-- activates that accelerator.
accelGroupsActivate ::
    (B.CallStack.HasCallStack, MonadIO m, GObject.Object.IsObject a) =>
    a
    -- ^ /@object@/: the t'GI.GObject.Objects.Object.Object', usually a t'GI.Gtk.Objects.Window.Window', on which
    --     to activate the accelerator
    -> Word32
    -- ^ /@accelKey@/: accelerator keyval from a key event
    -> [Gdk.Flags.ModifierType]
    -- ^ /@accelMods@/: keyboard state mask from a key event
    -> m Bool
    -- ^ __Returns:__ 'P.True' if an accelerator was activated and handled
    --     this keypress
accelGroupsActivate object accelKey accelMods = liftIO $ do
    object' <- unsafeManagedPtrCastPtr object
    let accelMods' = gflagsToWord accelMods
    result <- gtk_accel_groups_activate object' accelKey accelMods'
    let result' = (/= 0) result
    touchManagedPtr object
    return result'



