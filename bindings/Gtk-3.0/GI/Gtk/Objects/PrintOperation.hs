{-# LANGUAGE ImplicitParams, RankNTypes, TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- GtkPrintOperation is the high-level, portable printing API.
-- It looks a bit different than other GTK+ dialogs such as the
-- t'GI.Gtk.Interfaces.FileChooser.FileChooser', since some platforms don’t expose enough
-- infrastructure to implement a good print dialog. On such
-- platforms, GtkPrintOperation uses the native print dialog.
-- On platforms which do not provide a native print dialog, GTK+
-- uses its own, see @/GtkPrintUnixDialog/@.
-- 
-- The typical way to use the high-level printing API is to create
-- a GtkPrintOperation object with 'GI.Gtk.Objects.PrintOperation.printOperationNew' when
-- the user selects to print. Then you set some properties on it,
-- e.g. the page size, any t'GI.Gtk.Objects.PrintSettings.PrintSettings' from previous print
-- operations, the number of pages, the current page, etc.
-- 
-- Then you start the print operation by calling 'GI.Gtk.Objects.PrintOperation.printOperationRun'.
-- It will then show a dialog, let the user select a printer and
-- options. When the user finished the dialog various signals will
-- be emitted on the t'GI.Gtk.Objects.PrintOperation.PrintOperation', the main one being
-- [PrintOperation::drawPage]("GI.Gtk.Objects.PrintOperation#g:signal:drawPage"), which you are supposed to catch
-- and render the page on the provided t'GI.Gtk.Objects.PrintContext.PrintContext' using Cairo.
-- 
-- = The high-level printing API
-- 
-- 
-- === /C code/
-- >
-- >static GtkPrintSettings *settings = NULL;
-- >
-- >static void
-- >do_print (void)
-- >{
-- >  GtkPrintOperation *print;
-- >  GtkPrintOperationResult res;
-- >
-- >  print = gtk_print_operation_new ();
-- >
-- >  if (settings != NULL)
-- >    gtk_print_operation_set_print_settings (print, settings);
-- >
-- >  g_signal_connect (print, "begin_print", G_CALLBACK (begin_print), NULL);
-- >  g_signal_connect (print, "draw_page", G_CALLBACK (draw_page), NULL);
-- >
-- >  res = gtk_print_operation_run (print, GTK_PRINT_OPERATION_ACTION_PRINT_DIALOG,
-- >                                 GTK_WINDOW (main_window), NULL);
-- >
-- >  if (res == GTK_PRINT_OPERATION_RESULT_APPLY)
-- >    {
-- >      if (settings != NULL)
-- >        g_object_unref (settings);
-- >      settings = g_object_ref (gtk_print_operation_get_print_settings (print));
-- >    }
-- >
-- >  g_object_unref (print);
-- >}
-- 
-- 
-- By default GtkPrintOperation uses an external application to do
-- print preview. To implement a custom print preview, an application
-- must connect to the preview signal. The functions
-- 'GI.Gtk.Interfaces.PrintOperationPreview.printOperationPreviewRenderPage',
-- 'GI.Gtk.Interfaces.PrintOperationPreview.printOperationPreviewEndPreview' and
-- 'GI.Gtk.Interfaces.PrintOperationPreview.printOperationPreviewIsSelected'
-- are useful when implementing a print preview.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.PrintOperation
    ( 

-- * Exported types
    PrintOperation(..)                      ,
    IsPrintOperation                        ,
    toPrintOperation                        ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [cancel]("GI.Gtk.Objects.PrintOperation#g:method:cancel"), [drawPageFinish]("GI.Gtk.Objects.PrintOperation#g:method:drawPageFinish"), [endPreview]("GI.Gtk.Interfaces.PrintOperationPreview#g:method:endPreview"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [isFinished]("GI.Gtk.Objects.PrintOperation#g:method:isFinished"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [isSelected]("GI.Gtk.Interfaces.PrintOperationPreview#g:method:isSelected"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [renderPage]("GI.Gtk.Interfaces.PrintOperationPreview#g:method:renderPage"), [run]("GI.Gtk.Objects.PrintOperation#g:method:run"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getData]("GI.GObject.Objects.Object#g:method:getData"), [getDefaultPageSetup]("GI.Gtk.Objects.PrintOperation#g:method:getDefaultPageSetup"), [getEmbedPageSetup]("GI.Gtk.Objects.PrintOperation#g:method:getEmbedPageSetup"), [getError]("GI.Gtk.Objects.PrintOperation#g:method:getError"), [getHasSelection]("GI.Gtk.Objects.PrintOperation#g:method:getHasSelection"), [getNPagesToPrint]("GI.Gtk.Objects.PrintOperation#g:method:getNPagesToPrint"), [getPrintSettings]("GI.Gtk.Objects.PrintOperation#g:method:getPrintSettings"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getStatus]("GI.Gtk.Objects.PrintOperation#g:method:getStatus"), [getStatusString]("GI.Gtk.Objects.PrintOperation#g:method:getStatusString"), [getSupportSelection]("GI.Gtk.Objects.PrintOperation#g:method:getSupportSelection").
-- 
-- ==== Setters
-- [setAllowAsync]("GI.Gtk.Objects.PrintOperation#g:method:setAllowAsync"), [setCurrentPage]("GI.Gtk.Objects.PrintOperation#g:method:setCurrentPage"), [setCustomTabLabel]("GI.Gtk.Objects.PrintOperation#g:method:setCustomTabLabel"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setDefaultPageSetup]("GI.Gtk.Objects.PrintOperation#g:method:setDefaultPageSetup"), [setDeferDrawing]("GI.Gtk.Objects.PrintOperation#g:method:setDeferDrawing"), [setEmbedPageSetup]("GI.Gtk.Objects.PrintOperation#g:method:setEmbedPageSetup"), [setExportFilename]("GI.Gtk.Objects.PrintOperation#g:method:setExportFilename"), [setHasSelection]("GI.Gtk.Objects.PrintOperation#g:method:setHasSelection"), [setJobName]("GI.Gtk.Objects.PrintOperation#g:method:setJobName"), [setNPages]("GI.Gtk.Objects.PrintOperation#g:method:setNPages"), [setPrintSettings]("GI.Gtk.Objects.PrintOperation#g:method:setPrintSettings"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setShowProgress]("GI.Gtk.Objects.PrintOperation#g:method:setShowProgress"), [setSupportSelection]("GI.Gtk.Objects.PrintOperation#g:method:setSupportSelection"), [setTrackPrintStatus]("GI.Gtk.Objects.PrintOperation#g:method:setTrackPrintStatus"), [setUnit]("GI.Gtk.Objects.PrintOperation#g:method:setUnit"), [setUseFullPage]("GI.Gtk.Objects.PrintOperation#g:method:setUseFullPage").

#if defined(ENABLE_OVERLOADING)
    ResolvePrintOperationMethod             ,
#endif

-- ** cancel #method:cancel#

#if defined(ENABLE_OVERLOADING)
    PrintOperationCancelMethodInfo          ,
#endif
    printOperationCancel                    ,


-- ** drawPageFinish #method:drawPageFinish#

#if defined(ENABLE_OVERLOADING)
    PrintOperationDrawPageFinishMethodInfo  ,
#endif
    printOperationDrawPageFinish            ,


-- ** getDefaultPageSetup #method:getDefaultPageSetup#

#if defined(ENABLE_OVERLOADING)
    PrintOperationGetDefaultPageSetupMethodInfo,
#endif
    printOperationGetDefaultPageSetup       ,


-- ** getEmbedPageSetup #method:getEmbedPageSetup#

#if defined(ENABLE_OVERLOADING)
    PrintOperationGetEmbedPageSetupMethodInfo,
#endif
    printOperationGetEmbedPageSetup         ,


-- ** getError #method:getError#

#if defined(ENABLE_OVERLOADING)
    PrintOperationGetErrorMethodInfo        ,
#endif
    printOperationGetError                  ,


-- ** getHasSelection #method:getHasSelection#

#if defined(ENABLE_OVERLOADING)
    PrintOperationGetHasSelectionMethodInfo ,
#endif
    printOperationGetHasSelection           ,


-- ** getNPagesToPrint #method:getNPagesToPrint#

#if defined(ENABLE_OVERLOADING)
    PrintOperationGetNPagesToPrintMethodInfo,
#endif
    printOperationGetNPagesToPrint          ,


-- ** getPrintSettings #method:getPrintSettings#

#if defined(ENABLE_OVERLOADING)
    PrintOperationGetPrintSettingsMethodInfo,
#endif
    printOperationGetPrintSettings          ,


-- ** getStatus #method:getStatus#

#if defined(ENABLE_OVERLOADING)
    PrintOperationGetStatusMethodInfo       ,
#endif
    printOperationGetStatus                 ,


-- ** getStatusString #method:getStatusString#

#if defined(ENABLE_OVERLOADING)
    PrintOperationGetStatusStringMethodInfo ,
#endif
    printOperationGetStatusString           ,


-- ** getSupportSelection #method:getSupportSelection#

#if defined(ENABLE_OVERLOADING)
    PrintOperationGetSupportSelectionMethodInfo,
#endif
    printOperationGetSupportSelection       ,


-- ** isFinished #method:isFinished#

#if defined(ENABLE_OVERLOADING)
    PrintOperationIsFinishedMethodInfo      ,
#endif
    printOperationIsFinished                ,


-- ** new #method:new#

    printOperationNew                       ,


-- ** run #method:run#

#if defined(ENABLE_OVERLOADING)
    PrintOperationRunMethodInfo             ,
#endif
    printOperationRun                       ,


-- ** setAllowAsync #method:setAllowAsync#

#if defined(ENABLE_OVERLOADING)
    PrintOperationSetAllowAsyncMethodInfo   ,
#endif
    printOperationSetAllowAsync             ,


-- ** setCurrentPage #method:setCurrentPage#

#if defined(ENABLE_OVERLOADING)
    PrintOperationSetCurrentPageMethodInfo  ,
#endif
    printOperationSetCurrentPage            ,


-- ** setCustomTabLabel #method:setCustomTabLabel#

#if defined(ENABLE_OVERLOADING)
    PrintOperationSetCustomTabLabelMethodInfo,
#endif
    printOperationSetCustomTabLabel         ,


-- ** setDefaultPageSetup #method:setDefaultPageSetup#

#if defined(ENABLE_OVERLOADING)
    PrintOperationSetDefaultPageSetupMethodInfo,
#endif
    printOperationSetDefaultPageSetup       ,


-- ** setDeferDrawing #method:setDeferDrawing#

#if defined(ENABLE_OVERLOADING)
    PrintOperationSetDeferDrawingMethodInfo ,
#endif
    printOperationSetDeferDrawing           ,


-- ** setEmbedPageSetup #method:setEmbedPageSetup#

#if defined(ENABLE_OVERLOADING)
    PrintOperationSetEmbedPageSetupMethodInfo,
#endif
    printOperationSetEmbedPageSetup         ,


-- ** setExportFilename #method:setExportFilename#

#if defined(ENABLE_OVERLOADING)
    PrintOperationSetExportFilenameMethodInfo,
#endif
    printOperationSetExportFilename         ,


-- ** setHasSelection #method:setHasSelection#

#if defined(ENABLE_OVERLOADING)
    PrintOperationSetHasSelectionMethodInfo ,
#endif
    printOperationSetHasSelection           ,


-- ** setJobName #method:setJobName#

#if defined(ENABLE_OVERLOADING)
    PrintOperationSetJobNameMethodInfo      ,
#endif
    printOperationSetJobName                ,


-- ** setNPages #method:setNPages#

#if defined(ENABLE_OVERLOADING)
    PrintOperationSetNPagesMethodInfo       ,
#endif
    printOperationSetNPages                 ,


-- ** setPrintSettings #method:setPrintSettings#

#if defined(ENABLE_OVERLOADING)
    PrintOperationSetPrintSettingsMethodInfo,
#endif
    printOperationSetPrintSettings          ,


-- ** setShowProgress #method:setShowProgress#

#if defined(ENABLE_OVERLOADING)
    PrintOperationSetShowProgressMethodInfo ,
#endif
    printOperationSetShowProgress           ,


-- ** setSupportSelection #method:setSupportSelection#

#if defined(ENABLE_OVERLOADING)
    PrintOperationSetSupportSelectionMethodInfo,
#endif
    printOperationSetSupportSelection       ,


-- ** setTrackPrintStatus #method:setTrackPrintStatus#

#if defined(ENABLE_OVERLOADING)
    PrintOperationSetTrackPrintStatusMethodInfo,
#endif
    printOperationSetTrackPrintStatus       ,


-- ** setUnit #method:setUnit#

#if defined(ENABLE_OVERLOADING)
    PrintOperationSetUnitMethodInfo         ,
#endif
    printOperationSetUnit                   ,


-- ** setUseFullPage #method:setUseFullPage#

#if defined(ENABLE_OVERLOADING)
    PrintOperationSetUseFullPageMethodInfo  ,
#endif
    printOperationSetUseFullPage            ,




 -- * Properties


-- ** allowAsync #attr:allowAsync#
-- | Determines whether the print operation may run asynchronously or not.
-- 
-- Some systems don\'t support asynchronous printing, but those that do
-- will return 'GI.Gtk.Enums.PrintOperationResultInProgress' as the status, and
-- emit the [PrintOperation::done]("GI.Gtk.Objects.PrintOperation#g:signal:done") signal when the operation is actually
-- done.
-- 
-- The Windows port does not support asynchronous operation at all (this
-- is unlikely to change). On other platforms, all actions except for
-- 'GI.Gtk.Enums.PrintOperationActionExport' support asynchronous operation.
-- 
-- /Since: 2.10/

#if defined(ENABLE_OVERLOADING)
    PrintOperationAllowAsyncPropertyInfo    ,
#endif
    constructPrintOperationAllowAsync       ,
    getPrintOperationAllowAsync             ,
#if defined(ENABLE_OVERLOADING)
    printOperationAllowAsync                ,
#endif
    setPrintOperationAllowAsync             ,


-- ** currentPage #attr:currentPage#
-- | The current page in the document.
-- 
-- If this is set before 'GI.Gtk.Objects.PrintOperation.printOperationRun',
-- the user will be able to select to print only the current page.
-- 
-- Note that this only makes sense for pre-paginated documents.
-- 
-- /Since: 2.10/

#if defined(ENABLE_OVERLOADING)
    PrintOperationCurrentPagePropertyInfo   ,
#endif
    constructPrintOperationCurrentPage      ,
    getPrintOperationCurrentPage            ,
#if defined(ENABLE_OVERLOADING)
    printOperationCurrentPage               ,
#endif
    setPrintOperationCurrentPage            ,


-- ** customTabLabel #attr:customTabLabel#
-- | Used as the label of the tab containing custom widgets.
-- Note that this property may be ignored on some platforms.
-- 
-- If this is 'P.Nothing', GTK+ uses a default label.
-- 
-- /Since: 2.10/

#if defined(ENABLE_OVERLOADING)
    PrintOperationCustomTabLabelPropertyInfo,
#endif
    clearPrintOperationCustomTabLabel       ,
    constructPrintOperationCustomTabLabel   ,
    getPrintOperationCustomTabLabel         ,
#if defined(ENABLE_OVERLOADING)
    printOperationCustomTabLabel            ,
#endif
    setPrintOperationCustomTabLabel         ,


-- ** defaultPageSetup #attr:defaultPageSetup#
-- | The t'GI.Gtk.Objects.PageSetup.PageSetup' used by default.
-- 
-- This page setup will be used by 'GI.Gtk.Objects.PrintOperation.printOperationRun',
-- but it can be overridden on a per-page basis by connecting
-- to the [PrintOperation::requestPageSetup]("GI.Gtk.Objects.PrintOperation#g:signal:requestPageSetup") signal.
-- 
-- /Since: 2.10/

#if defined(ENABLE_OVERLOADING)
    PrintOperationDefaultPageSetupPropertyInfo,
#endif
    clearPrintOperationDefaultPageSetup     ,
    constructPrintOperationDefaultPageSetup ,
    getPrintOperationDefaultPageSetup       ,
#if defined(ENABLE_OVERLOADING)
    printOperationDefaultPageSetup          ,
#endif
    setPrintOperationDefaultPageSetup       ,


-- ** embedPageSetup #attr:embedPageSetup#
-- | If 'P.True', page size combo box and orientation combo box are embedded into page setup page.
-- 
-- /Since: 2.18/

#if defined(ENABLE_OVERLOADING)
    PrintOperationEmbedPageSetupPropertyInfo,
#endif
    constructPrintOperationEmbedPageSetup   ,
    getPrintOperationEmbedPageSetup         ,
#if defined(ENABLE_OVERLOADING)
    printOperationEmbedPageSetup            ,
#endif
    setPrintOperationEmbedPageSetup         ,


-- ** exportFilename #attr:exportFilename#
-- | The name of a file to generate instead of showing the print dialog.
-- Currently, PDF is the only supported format.
-- 
-- The intended use of this property is for implementing
-- “Export to PDF” actions.
-- 
-- “Print to PDF” support is independent of this and is done
-- by letting the user pick the “Print to PDF” item from the
-- list of printers in the print dialog.
-- 
-- /Since: 2.10/

#if defined(ENABLE_OVERLOADING)
    PrintOperationExportFilenamePropertyInfo,
#endif
    clearPrintOperationExportFilename       ,
    constructPrintOperationExportFilename   ,
    getPrintOperationExportFilename         ,
#if defined(ENABLE_OVERLOADING)
    printOperationExportFilename            ,
#endif
    setPrintOperationExportFilename         ,


-- ** hasSelection #attr:hasSelection#
-- | Determines whether there is a selection in your application.
-- This can allow your application to print the selection.
-- This is typically used to make a \"Selection\" button sensitive.
-- 
-- /Since: 2.18/

#if defined(ENABLE_OVERLOADING)
    PrintOperationHasSelectionPropertyInfo  ,
#endif
    constructPrintOperationHasSelection     ,
    getPrintOperationHasSelection           ,
#if defined(ENABLE_OVERLOADING)
    printOperationHasSelection              ,
#endif
    setPrintOperationHasSelection           ,


-- ** jobName #attr:jobName#
-- | A string used to identify the job (e.g. in monitoring
-- applications like eggcups).
-- 
-- If you don\'t set a job name, GTK+ picks a default one
-- by numbering successive print jobs.
-- 
-- /Since: 2.10/

#if defined(ENABLE_OVERLOADING)
    PrintOperationJobNamePropertyInfo       ,
#endif
    constructPrintOperationJobName          ,
    getPrintOperationJobName                ,
#if defined(ENABLE_OVERLOADING)
    printOperationJobName                   ,
#endif
    setPrintOperationJobName                ,


-- ** nPages #attr:nPages#
-- | The number of pages in the document.
-- 
-- This must be set to a positive number
-- before the rendering starts. It may be set in a
-- [PrintOperation::beginPrint]("GI.Gtk.Objects.PrintOperation#g:signal:beginPrint") signal hander.
-- 
-- Note that the page numbers passed to the
-- [PrintOperation::requestPageSetup]("GI.Gtk.Objects.PrintOperation#g:signal:requestPageSetup") and
-- [PrintOperation::drawPage]("GI.Gtk.Objects.PrintOperation#g:signal:drawPage") signals are 0-based, i.e. if
-- the user chooses to print all pages, the last [drawPage](#g:signal:drawPage) signal
-- will be for page /@nPages@/ - 1.
-- 
-- /Since: 2.10/

#if defined(ENABLE_OVERLOADING)
    PrintOperationNPagesPropertyInfo        ,
#endif
    constructPrintOperationNPages           ,
    getPrintOperationNPages                 ,
#if defined(ENABLE_OVERLOADING)
    printOperationNPages                    ,
#endif
    setPrintOperationNPages                 ,


-- ** nPagesToPrint #attr:nPagesToPrint#
-- | The number of pages that will be printed.
-- 
-- Note that this value is set during print preparation phase
-- ('GI.Gtk.Enums.PrintStatusPreparing'), so this value should never be
-- get before the data generation phase ('GI.Gtk.Enums.PrintStatusGeneratingData').
-- You can connect to the [PrintOperation::statusChanged]("GI.Gtk.Objects.PrintOperation#g:signal:statusChanged") signal
-- and call 'GI.Gtk.Objects.PrintOperation.printOperationGetNPagesToPrint' when
-- print status is 'GI.Gtk.Enums.PrintStatusGeneratingData'.
-- This is typically used to track the progress of print operation.
-- 
-- /Since: 2.18/

#if defined(ENABLE_OVERLOADING)
    PrintOperationNPagesToPrintPropertyInfo ,
#endif
    getPrintOperationNPagesToPrint          ,
#if defined(ENABLE_OVERLOADING)
    printOperationNPagesToPrint             ,
#endif


-- ** printSettings #attr:printSettings#
-- | The t'GI.Gtk.Objects.PrintSettings.PrintSettings' used for initializing the dialog.
-- 
-- Setting this property is typically used to re-establish
-- print settings from a previous print operation, see
-- 'GI.Gtk.Objects.PrintOperation.printOperationRun'.
-- 
-- /Since: 2.10/

#if defined(ENABLE_OVERLOADING)
    PrintOperationPrintSettingsPropertyInfo ,
#endif
    clearPrintOperationPrintSettings        ,
    constructPrintOperationPrintSettings    ,
    getPrintOperationPrintSettings          ,
#if defined(ENABLE_OVERLOADING)
    printOperationPrintSettings             ,
#endif
    setPrintOperationPrintSettings          ,


-- ** showProgress #attr:showProgress#
-- | Determines whether to show a progress dialog during the
-- print operation.
-- 
-- /Since: 2.10/

#if defined(ENABLE_OVERLOADING)
    PrintOperationShowProgressPropertyInfo  ,
#endif
    constructPrintOperationShowProgress     ,
    getPrintOperationShowProgress           ,
#if defined(ENABLE_OVERLOADING)
    printOperationShowProgress              ,
#endif
    setPrintOperationShowProgress           ,


-- ** status #attr:status#
-- | The status of the print operation.
-- 
-- /Since: 2.10/

#if defined(ENABLE_OVERLOADING)
    PrintOperationStatusPropertyInfo        ,
#endif
    getPrintOperationStatus                 ,
#if defined(ENABLE_OVERLOADING)
    printOperationStatus                    ,
#endif


-- ** statusString #attr:statusString#
-- | A string representation of the status of the print operation.
-- The string is translated and suitable for displaying the print
-- status e.g. in a t'GI.Gtk.Objects.Statusbar.Statusbar'.
-- 
-- See the [PrintOperation:status]("GI.Gtk.Objects.PrintOperation#g:attr:status") property for a status value that
-- is suitable for programmatic use.
-- 
-- /Since: 2.10/

#if defined(ENABLE_OVERLOADING)
    PrintOperationStatusStringPropertyInfo  ,
#endif
    getPrintOperationStatusString           ,
#if defined(ENABLE_OVERLOADING)
    printOperationStatusString              ,
#endif


-- ** supportSelection #attr:supportSelection#
-- | If 'P.True', the print operation will support print of selection.
-- This allows the print dialog to show a \"Selection\" button.
-- 
-- /Since: 2.18/

#if defined(ENABLE_OVERLOADING)
    PrintOperationSupportSelectionPropertyInfo,
#endif
    constructPrintOperationSupportSelection ,
    getPrintOperationSupportSelection       ,
#if defined(ENABLE_OVERLOADING)
    printOperationSupportSelection          ,
#endif
    setPrintOperationSupportSelection       ,


-- ** trackPrintStatus #attr:trackPrintStatus#
-- | If 'P.True', the print operation will try to continue report on
-- the status of the print job in the printer queues and printer.
-- This can allow your application to show things like “out of paper”
-- issues, and when the print job actually reaches the printer.
-- However, this is often implemented using polling, and should
-- not be enabled unless needed.
-- 
-- /Since: 2.10/

#if defined(ENABLE_OVERLOADING)
    PrintOperationTrackPrintStatusPropertyInfo,
#endif
    constructPrintOperationTrackPrintStatus ,
    getPrintOperationTrackPrintStatus       ,
#if defined(ENABLE_OVERLOADING)
    printOperationTrackPrintStatus          ,
#endif
    setPrintOperationTrackPrintStatus       ,


-- ** unit #attr:unit#
-- | The transformation for the cairo context obtained from
-- t'GI.Gtk.Objects.PrintContext.PrintContext' is set up in such a way that distances
-- are measured in units of /@unit@/.
-- 
-- /Since: 2.10/

#if defined(ENABLE_OVERLOADING)
    PrintOperationUnitPropertyInfo          ,
#endif
    constructPrintOperationUnit             ,
    getPrintOperationUnit                   ,
#if defined(ENABLE_OVERLOADING)
    printOperationUnit                      ,
#endif
    setPrintOperationUnit                   ,


-- ** useFullPage #attr:useFullPage#
-- | If 'P.True', the transformation for the cairo context obtained
-- from t'GI.Gtk.Objects.PrintContext.PrintContext' puts the origin at the top left corner
-- of the page (which may not be the top left corner of the sheet,
-- depending on page orientation and the number of pages per sheet).
-- Otherwise, the origin is at the top left corner of the imageable
-- area (i.e. inside the margins).
-- 
-- /Since: 2.10/

#if defined(ENABLE_OVERLOADING)
    PrintOperationUseFullPagePropertyInfo   ,
#endif
    constructPrintOperationUseFullPage      ,
    getPrintOperationUseFullPage            ,
#if defined(ENABLE_OVERLOADING)
    printOperationUseFullPage               ,
#endif
    setPrintOperationUseFullPage            ,




 -- * Signals


-- ** beginPrint #signal:beginPrint#

    PrintOperationBeginPrintCallback        ,
#if defined(ENABLE_OVERLOADING)
    PrintOperationBeginPrintSignalInfo      ,
#endif
    afterPrintOperationBeginPrint           ,
    onPrintOperationBeginPrint              ,


-- ** createCustomWidget #signal:createCustomWidget#

    PrintOperationCreateCustomWidgetCallback,
#if defined(ENABLE_OVERLOADING)
    PrintOperationCreateCustomWidgetSignalInfo,
#endif
    afterPrintOperationCreateCustomWidget   ,
    onPrintOperationCreateCustomWidget      ,


-- ** customWidgetApply #signal:customWidgetApply#

    PrintOperationCustomWidgetApplyCallback ,
#if defined(ENABLE_OVERLOADING)
    PrintOperationCustomWidgetApplySignalInfo,
#endif
    afterPrintOperationCustomWidgetApply    ,
    onPrintOperationCustomWidgetApply       ,


-- ** done #signal:done#

    PrintOperationDoneCallback              ,
#if defined(ENABLE_OVERLOADING)
    PrintOperationDoneSignalInfo            ,
#endif
    afterPrintOperationDone                 ,
    onPrintOperationDone                    ,


-- ** drawPage #signal:drawPage#

    PrintOperationDrawPageCallback          ,
#if defined(ENABLE_OVERLOADING)
    PrintOperationDrawPageSignalInfo        ,
#endif
    afterPrintOperationDrawPage             ,
    onPrintOperationDrawPage                ,


-- ** endPrint #signal:endPrint#

    PrintOperationEndPrintCallback          ,
#if defined(ENABLE_OVERLOADING)
    PrintOperationEndPrintSignalInfo        ,
#endif
    afterPrintOperationEndPrint             ,
    onPrintOperationEndPrint                ,


-- ** paginate #signal:paginate#

    PrintOperationPaginateCallback          ,
#if defined(ENABLE_OVERLOADING)
    PrintOperationPaginateSignalInfo        ,
#endif
    afterPrintOperationPaginate             ,
    onPrintOperationPaginate                ,


-- ** preview #signal:preview#

    PrintOperationPreviewCallback           ,
#if defined(ENABLE_OVERLOADING)
    PrintOperationPreviewSignalInfo         ,
#endif
    afterPrintOperationPreview              ,
    onPrintOperationPreview                 ,


-- ** requestPageSetup #signal:requestPageSetup#

    PrintOperationRequestPageSetupCallback  ,
#if defined(ENABLE_OVERLOADING)
    PrintOperationRequestPageSetupSignalInfo,
#endif
    afterPrintOperationRequestPageSetup     ,
    onPrintOperationRequestPageSetup        ,


-- ** statusChanged #signal:statusChanged#

    PrintOperationStatusChangedCallback     ,
#if defined(ENABLE_OVERLOADING)
    PrintOperationStatusChangedSignalInfo   ,
#endif
    afterPrintOperationStatusChanged        ,
    onPrintOperationStatusChanged           ,


-- ** updateCustomWidget #signal:updateCustomWidget#

    PrintOperationUpdateCustomWidgetCallback,
#if defined(ENABLE_OVERLOADING)
    PrintOperationUpdateCustomWidgetSignalInfo,
#endif
    afterPrintOperationUpdateCustomWidget   ,
    onPrintOperationUpdateCustomWidget      ,




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
import {-# SOURCE #-} qualified GI.Gtk.Enums as Gtk.Enums
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.PrintOperationPreview as Gtk.PrintOperationPreview
import {-# SOURCE #-} qualified GI.Gtk.Objects.PageSetup as Gtk.PageSetup
import {-# SOURCE #-} qualified GI.Gtk.Objects.PrintContext as Gtk.PrintContext
import {-# SOURCE #-} qualified GI.Gtk.Objects.PrintSettings as Gtk.PrintSettings
import {-# SOURCE #-} qualified GI.Gtk.Objects.Widget as Gtk.Widget
import {-# SOURCE #-} qualified GI.Gtk.Objects.Window as Gtk.Window

-- | Memory-managed wrapper type.
newtype PrintOperation = PrintOperation (SP.ManagedPtr PrintOperation)
    deriving (Eq)

instance SP.ManagedPtrNewtype PrintOperation where
    toManagedPtr (PrintOperation p) = p

foreign import ccall "gtk_print_operation_get_type"
    c_gtk_print_operation_get_type :: IO B.Types.GType

instance B.Types.TypedObject PrintOperation where
    glibType = c_gtk_print_operation_get_type

instance B.Types.GObject PrintOperation

-- | Type class for types which can be safely cast to `PrintOperation`, for instance with `toPrintOperation`.
class (SP.GObject o, O.IsDescendantOf PrintOperation o) => IsPrintOperation o
instance (SP.GObject o, O.IsDescendantOf PrintOperation o) => IsPrintOperation o

instance O.HasParentTypes PrintOperation
type instance O.ParentTypes PrintOperation = '[GObject.Object.Object, Gtk.PrintOperationPreview.PrintOperationPreview]

-- | Cast to `PrintOperation`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toPrintOperation :: (MIO.MonadIO m, IsPrintOperation o) => o -> m PrintOperation
toPrintOperation = MIO.liftIO . B.ManagedPtr.unsafeCastTo PrintOperation

-- | Convert 'PrintOperation' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe PrintOperation) where
    gvalueGType_ = c_gtk_print_operation_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr PrintOperation)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr PrintOperation)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject PrintOperation ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolvePrintOperationMethod (t :: Symbol) (o :: *) :: * where
    ResolvePrintOperationMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolvePrintOperationMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolvePrintOperationMethod "cancel" o = PrintOperationCancelMethodInfo
    ResolvePrintOperationMethod "drawPageFinish" o = PrintOperationDrawPageFinishMethodInfo
    ResolvePrintOperationMethod "endPreview" o = Gtk.PrintOperationPreview.PrintOperationPreviewEndPreviewMethodInfo
    ResolvePrintOperationMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolvePrintOperationMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolvePrintOperationMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolvePrintOperationMethod "isFinished" o = PrintOperationIsFinishedMethodInfo
    ResolvePrintOperationMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolvePrintOperationMethod "isSelected" o = Gtk.PrintOperationPreview.PrintOperationPreviewIsSelectedMethodInfo
    ResolvePrintOperationMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolvePrintOperationMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolvePrintOperationMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolvePrintOperationMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolvePrintOperationMethod "renderPage" o = Gtk.PrintOperationPreview.PrintOperationPreviewRenderPageMethodInfo
    ResolvePrintOperationMethod "run" o = PrintOperationRunMethodInfo
    ResolvePrintOperationMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolvePrintOperationMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolvePrintOperationMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolvePrintOperationMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolvePrintOperationMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolvePrintOperationMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolvePrintOperationMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolvePrintOperationMethod "getDefaultPageSetup" o = PrintOperationGetDefaultPageSetupMethodInfo
    ResolvePrintOperationMethod "getEmbedPageSetup" o = PrintOperationGetEmbedPageSetupMethodInfo
    ResolvePrintOperationMethod "getError" o = PrintOperationGetErrorMethodInfo
    ResolvePrintOperationMethod "getHasSelection" o = PrintOperationGetHasSelectionMethodInfo
    ResolvePrintOperationMethod "getNPagesToPrint" o = PrintOperationGetNPagesToPrintMethodInfo
    ResolvePrintOperationMethod "getPrintSettings" o = PrintOperationGetPrintSettingsMethodInfo
    ResolvePrintOperationMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolvePrintOperationMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolvePrintOperationMethod "getStatus" o = PrintOperationGetStatusMethodInfo
    ResolvePrintOperationMethod "getStatusString" o = PrintOperationGetStatusStringMethodInfo
    ResolvePrintOperationMethod "getSupportSelection" o = PrintOperationGetSupportSelectionMethodInfo
    ResolvePrintOperationMethod "setAllowAsync" o = PrintOperationSetAllowAsyncMethodInfo
    ResolvePrintOperationMethod "setCurrentPage" o = PrintOperationSetCurrentPageMethodInfo
    ResolvePrintOperationMethod "setCustomTabLabel" o = PrintOperationSetCustomTabLabelMethodInfo
    ResolvePrintOperationMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolvePrintOperationMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolvePrintOperationMethod "setDefaultPageSetup" o = PrintOperationSetDefaultPageSetupMethodInfo
    ResolvePrintOperationMethod "setDeferDrawing" o = PrintOperationSetDeferDrawingMethodInfo
    ResolvePrintOperationMethod "setEmbedPageSetup" o = PrintOperationSetEmbedPageSetupMethodInfo
    ResolvePrintOperationMethod "setExportFilename" o = PrintOperationSetExportFilenameMethodInfo
    ResolvePrintOperationMethod "setHasSelection" o = PrintOperationSetHasSelectionMethodInfo
    ResolvePrintOperationMethod "setJobName" o = PrintOperationSetJobNameMethodInfo
    ResolvePrintOperationMethod "setNPages" o = PrintOperationSetNPagesMethodInfo
    ResolvePrintOperationMethod "setPrintSettings" o = PrintOperationSetPrintSettingsMethodInfo
    ResolvePrintOperationMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolvePrintOperationMethod "setShowProgress" o = PrintOperationSetShowProgressMethodInfo
    ResolvePrintOperationMethod "setSupportSelection" o = PrintOperationSetSupportSelectionMethodInfo
    ResolvePrintOperationMethod "setTrackPrintStatus" o = PrintOperationSetTrackPrintStatusMethodInfo
    ResolvePrintOperationMethod "setUnit" o = PrintOperationSetUnitMethodInfo
    ResolvePrintOperationMethod "setUseFullPage" o = PrintOperationSetUseFullPageMethodInfo
    ResolvePrintOperationMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolvePrintOperationMethod t PrintOperation, O.OverloadedMethod info PrintOperation p) => OL.IsLabel t (PrintOperation -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolvePrintOperationMethod t PrintOperation, O.OverloadedMethod info PrintOperation p, R.HasField t PrintOperation p) => R.HasField t PrintOperation p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolvePrintOperationMethod t PrintOperation, O.OverloadedMethodInfo info PrintOperation) => OL.IsLabel t (O.MethodProxy info PrintOperation) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- signal PrintOperation::begin-print
-- | Emitted after the user has finished changing print settings
-- in the dialog, before the actual rendering starts.
-- 
-- A typical use for [beginPrint](#g:signal:beginPrint) is to use the parameters from the
-- t'GI.Gtk.Objects.PrintContext.PrintContext' and paginate the document accordingly, and then
-- set the number of pages with 'GI.Gtk.Objects.PrintOperation.printOperationSetNPages'.
-- 
-- /Since: 2.10/
type PrintOperationBeginPrintCallback =
    Gtk.PrintContext.PrintContext
    -- ^ /@context@/: the t'GI.Gtk.Objects.PrintContext.PrintContext' for the current operation
    -> IO ()

type C_PrintOperationBeginPrintCallback =
    Ptr PrintOperation ->                   -- object
    Ptr Gtk.PrintContext.PrintContext ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_PrintOperationBeginPrintCallback`.
foreign import ccall "wrapper"
    mk_PrintOperationBeginPrintCallback :: C_PrintOperationBeginPrintCallback -> IO (FunPtr C_PrintOperationBeginPrintCallback)

wrap_PrintOperationBeginPrintCallback :: 
    GObject a => (a -> PrintOperationBeginPrintCallback) ->
    C_PrintOperationBeginPrintCallback
wrap_PrintOperationBeginPrintCallback gi'cb gi'selfPtr context _ = do
    context' <- (newObject Gtk.PrintContext.PrintContext) context
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  context'


-- | Connect a signal handler for the [beginPrint](#signal:beginPrint) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' printOperation #beginPrint callback
-- @
-- 
-- 
onPrintOperationBeginPrint :: (IsPrintOperation a, MonadIO m) => a -> ((?self :: a) => PrintOperationBeginPrintCallback) -> m SignalHandlerId
onPrintOperationBeginPrint obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_PrintOperationBeginPrintCallback wrapped
    wrapped'' <- mk_PrintOperationBeginPrintCallback wrapped'
    connectSignalFunPtr obj "begin-print" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [beginPrint](#signal:beginPrint) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' printOperation #beginPrint callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterPrintOperationBeginPrint :: (IsPrintOperation a, MonadIO m) => a -> ((?self :: a) => PrintOperationBeginPrintCallback) -> m SignalHandlerId
afterPrintOperationBeginPrint obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_PrintOperationBeginPrintCallback wrapped
    wrapped'' <- mk_PrintOperationBeginPrintCallback wrapped'
    connectSignalFunPtr obj "begin-print" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data PrintOperationBeginPrintSignalInfo
instance SignalInfo PrintOperationBeginPrintSignalInfo where
    type HaskellCallbackType PrintOperationBeginPrintSignalInfo = PrintOperationBeginPrintCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_PrintOperationBeginPrintCallback cb
        cb'' <- mk_PrintOperationBeginPrintCallback cb'
        connectSignalFunPtr obj "begin-print" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintOperation::begin-print"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintOperation.html#g:signal:beginPrint"})

#endif

-- signal PrintOperation::create-custom-widget
-- | Emitted when displaying the print dialog. If you return a
-- widget in a handler for this signal it will be added to a custom
-- tab in the print dialog. You typically return a container widget
-- with multiple widgets in it.
-- 
-- The print dialog owns the returned widget, and its lifetime is not
-- controlled by the application. However, the widget is guaranteed
-- to stay around until the [PrintOperation::customWidgetApply]("GI.Gtk.Objects.PrintOperation#g:signal:customWidgetApply")
-- signal is emitted on the operation. Then you can read out any
-- information you need from the widgets.
-- 
-- /Since: 2.10/
type PrintOperationCreateCustomWidgetCallback =
    IO GObject.Object.Object
    -- ^ __Returns:__ A custom widget that gets embedded in
    --          the print dialog, or 'P.Nothing'

type C_PrintOperationCreateCustomWidgetCallback =
    Ptr PrintOperation ->                   -- object
    Ptr () ->                               -- user_data
    IO (Ptr GObject.Object.Object)

-- | Generate a function pointer callable from C code, from a `C_PrintOperationCreateCustomWidgetCallback`.
foreign import ccall "wrapper"
    mk_PrintOperationCreateCustomWidgetCallback :: C_PrintOperationCreateCustomWidgetCallback -> IO (FunPtr C_PrintOperationCreateCustomWidgetCallback)

wrap_PrintOperationCreateCustomWidgetCallback :: 
    GObject a => (a -> PrintOperationCreateCustomWidgetCallback) ->
    C_PrintOperationCreateCustomWidgetCallback
wrap_PrintOperationCreateCustomWidgetCallback gi'cb gi'selfPtr _ = do
    result <- B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 
    result' <- B.ManagedPtr.disownObject result
    return result'


-- | Connect a signal handler for the [createCustomWidget](#signal:createCustomWidget) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' printOperation #createCustomWidget callback
-- @
-- 
-- 
onPrintOperationCreateCustomWidget :: (IsPrintOperation a, MonadIO m) => a -> ((?self :: a) => PrintOperationCreateCustomWidgetCallback) -> m SignalHandlerId
onPrintOperationCreateCustomWidget obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_PrintOperationCreateCustomWidgetCallback wrapped
    wrapped'' <- mk_PrintOperationCreateCustomWidgetCallback wrapped'
    connectSignalFunPtr obj "create-custom-widget" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [createCustomWidget](#signal:createCustomWidget) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' printOperation #createCustomWidget callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterPrintOperationCreateCustomWidget :: (IsPrintOperation a, MonadIO m) => a -> ((?self :: a) => PrintOperationCreateCustomWidgetCallback) -> m SignalHandlerId
afterPrintOperationCreateCustomWidget obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_PrintOperationCreateCustomWidgetCallback wrapped
    wrapped'' <- mk_PrintOperationCreateCustomWidgetCallback wrapped'
    connectSignalFunPtr obj "create-custom-widget" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data PrintOperationCreateCustomWidgetSignalInfo
instance SignalInfo PrintOperationCreateCustomWidgetSignalInfo where
    type HaskellCallbackType PrintOperationCreateCustomWidgetSignalInfo = PrintOperationCreateCustomWidgetCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_PrintOperationCreateCustomWidgetCallback cb
        cb'' <- mk_PrintOperationCreateCustomWidgetCallback cb'
        connectSignalFunPtr obj "create-custom-widget" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintOperation::create-custom-widget"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintOperation.html#g:signal:createCustomWidget"})

#endif

-- signal PrintOperation::custom-widget-apply
-- | Emitted right before [PrintOperation::beginPrint]("GI.Gtk.Objects.PrintOperation#g:signal:beginPrint") if you added
-- a custom widget in the [PrintOperation::createCustomWidget]("GI.Gtk.Objects.PrintOperation#g:signal:createCustomWidget") handler.
-- When you get this signal you should read the information from the
-- custom widgets, as the widgets are not guaraneed to be around at a
-- later time.
-- 
-- /Since: 2.10/
type PrintOperationCustomWidgetApplyCallback =
    Gtk.Widget.Widget
    -- ^ /@widget@/: the custom widget added in create-custom-widget
    -> IO ()

type C_PrintOperationCustomWidgetApplyCallback =
    Ptr PrintOperation ->                   -- object
    Ptr Gtk.Widget.Widget ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_PrintOperationCustomWidgetApplyCallback`.
foreign import ccall "wrapper"
    mk_PrintOperationCustomWidgetApplyCallback :: C_PrintOperationCustomWidgetApplyCallback -> IO (FunPtr C_PrintOperationCustomWidgetApplyCallback)

wrap_PrintOperationCustomWidgetApplyCallback :: 
    GObject a => (a -> PrintOperationCustomWidgetApplyCallback) ->
    C_PrintOperationCustomWidgetApplyCallback
wrap_PrintOperationCustomWidgetApplyCallback gi'cb gi'selfPtr widget _ = do
    widget' <- (newObject Gtk.Widget.Widget) widget
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  widget'


-- | Connect a signal handler for the [customWidgetApply](#signal:customWidgetApply) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' printOperation #customWidgetApply callback
-- @
-- 
-- 
onPrintOperationCustomWidgetApply :: (IsPrintOperation a, MonadIO m) => a -> ((?self :: a) => PrintOperationCustomWidgetApplyCallback) -> m SignalHandlerId
onPrintOperationCustomWidgetApply obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_PrintOperationCustomWidgetApplyCallback wrapped
    wrapped'' <- mk_PrintOperationCustomWidgetApplyCallback wrapped'
    connectSignalFunPtr obj "custom-widget-apply" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [customWidgetApply](#signal:customWidgetApply) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' printOperation #customWidgetApply callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterPrintOperationCustomWidgetApply :: (IsPrintOperation a, MonadIO m) => a -> ((?self :: a) => PrintOperationCustomWidgetApplyCallback) -> m SignalHandlerId
afterPrintOperationCustomWidgetApply obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_PrintOperationCustomWidgetApplyCallback wrapped
    wrapped'' <- mk_PrintOperationCustomWidgetApplyCallback wrapped'
    connectSignalFunPtr obj "custom-widget-apply" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data PrintOperationCustomWidgetApplySignalInfo
instance SignalInfo PrintOperationCustomWidgetApplySignalInfo where
    type HaskellCallbackType PrintOperationCustomWidgetApplySignalInfo = PrintOperationCustomWidgetApplyCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_PrintOperationCustomWidgetApplyCallback cb
        cb'' <- mk_PrintOperationCustomWidgetApplyCallback cb'
        connectSignalFunPtr obj "custom-widget-apply" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintOperation::custom-widget-apply"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintOperation.html#g:signal:customWidgetApply"})

#endif

-- signal PrintOperation::done
-- | Emitted when the print operation run has finished doing
-- everything required for printing.
-- 
-- /@result@/ gives you information about what happened during the run.
-- If /@result@/ is 'GI.Gtk.Enums.PrintOperationResultError' then you can call
-- 'GI.Gtk.Objects.PrintOperation.printOperationGetError' for more information.
-- 
-- If you enabled print status tracking then
-- 'GI.Gtk.Objects.PrintOperation.printOperationIsFinished' may still return 'P.False'
-- after [PrintOperation::done]("GI.Gtk.Objects.PrintOperation#g:signal:done") was emitted.
-- 
-- /Since: 2.10/
type PrintOperationDoneCallback =
    Gtk.Enums.PrintOperationResult
    -- ^ /@result@/: the result of the print operation
    -> IO ()

type C_PrintOperationDoneCallback =
    Ptr PrintOperation ->                   -- object
    CUInt ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_PrintOperationDoneCallback`.
foreign import ccall "wrapper"
    mk_PrintOperationDoneCallback :: C_PrintOperationDoneCallback -> IO (FunPtr C_PrintOperationDoneCallback)

wrap_PrintOperationDoneCallback :: 
    GObject a => (a -> PrintOperationDoneCallback) ->
    C_PrintOperationDoneCallback
wrap_PrintOperationDoneCallback gi'cb gi'selfPtr result_ _ = do
    let result_' = (toEnum . fromIntegral) result_
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  result_'


-- | Connect a signal handler for the [done](#signal:done) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' printOperation #done callback
-- @
-- 
-- 
onPrintOperationDone :: (IsPrintOperation a, MonadIO m) => a -> ((?self :: a) => PrintOperationDoneCallback) -> m SignalHandlerId
onPrintOperationDone obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_PrintOperationDoneCallback wrapped
    wrapped'' <- mk_PrintOperationDoneCallback wrapped'
    connectSignalFunPtr obj "done" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [done](#signal:done) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' printOperation #done callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterPrintOperationDone :: (IsPrintOperation a, MonadIO m) => a -> ((?self :: a) => PrintOperationDoneCallback) -> m SignalHandlerId
afterPrintOperationDone obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_PrintOperationDoneCallback wrapped
    wrapped'' <- mk_PrintOperationDoneCallback wrapped'
    connectSignalFunPtr obj "done" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data PrintOperationDoneSignalInfo
instance SignalInfo PrintOperationDoneSignalInfo where
    type HaskellCallbackType PrintOperationDoneSignalInfo = PrintOperationDoneCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_PrintOperationDoneCallback cb
        cb'' <- mk_PrintOperationDoneCallback cb'
        connectSignalFunPtr obj "done" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintOperation::done"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintOperation.html#g:signal:done"})

#endif

-- signal PrintOperation::draw-page
-- | Emitted for every page that is printed. The signal handler
-- must render the /@pageNr@/\'s page onto the cairo context obtained
-- from /@context@/ using 'GI.Gtk.Objects.PrintContext.printContextGetCairoContext'.
-- 
-- === /C code/
-- >
-- >static void
-- >draw_page (GtkPrintOperation *operation,
-- >           GtkPrintContext   *context,
-- >           gint               page_nr,
-- >           gpointer           user_data)
-- >{
-- >  cairo_t *cr;
-- >  PangoLayout *layout;
-- >  gdouble width, text_height;
-- >  gint layout_height;
-- >  PangoFontDescription *desc;
-- >  
-- >  cr = gtk_print_context_get_cairo_context (context);
-- >  width = gtk_print_context_get_width (context);
-- >  
-- >  cairo_rectangle (cr, 0, 0, width, HEADER_HEIGHT);
-- >  
-- >  cairo_set_source_rgb (cr, 0.8, 0.8, 0.8);
-- >  cairo_fill (cr);
-- >  
-- >  layout = gtk_print_context_create_pango_layout (context);
-- >  
-- >  desc = pango_font_description_from_string ("sans 14");
-- >  pango_layout_set_font_description (layout, desc);
-- >  pango_font_description_free (desc);
-- >  
-- >  pango_layout_set_text (layout, "some text", -1);
-- >  pango_layout_set_width (layout, width * PANGO_SCALE);
-- >  pango_layout_set_alignment (layout, PANGO_ALIGN_CENTER);
-- >     		      
-- >  pango_layout_get_size (layout, NULL, &layout_height);
-- >  text_height = (gdouble)layout_height / PANGO_SCALE;
-- >  
-- >  cairo_move_to (cr, width / 2,  (HEADER_HEIGHT - text_height) / 2);
-- >  pango_cairo_show_layout (cr, layout);
-- >  
-- >  g_object_unref (layout);
-- >}
-- 
-- 
-- Use 'GI.Gtk.Objects.PrintOperation.printOperationSetUseFullPage' and
-- 'GI.Gtk.Objects.PrintOperation.printOperationSetUnit' before starting the print operation
-- to set up the transformation of the cairo context according to your
-- needs.
-- 
-- /Since: 2.10/
type PrintOperationDrawPageCallback =
    Gtk.PrintContext.PrintContext
    -- ^ /@context@/: the t'GI.Gtk.Objects.PrintContext.PrintContext' for the current operation
    -> Int32
    -- ^ /@pageNr@/: the number of the currently printed page (0-based)
    -> IO ()

type C_PrintOperationDrawPageCallback =
    Ptr PrintOperation ->                   -- object
    Ptr Gtk.PrintContext.PrintContext ->
    Int32 ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_PrintOperationDrawPageCallback`.
foreign import ccall "wrapper"
    mk_PrintOperationDrawPageCallback :: C_PrintOperationDrawPageCallback -> IO (FunPtr C_PrintOperationDrawPageCallback)

wrap_PrintOperationDrawPageCallback :: 
    GObject a => (a -> PrintOperationDrawPageCallback) ->
    C_PrintOperationDrawPageCallback
wrap_PrintOperationDrawPageCallback gi'cb gi'selfPtr context pageNr _ = do
    context' <- (newObject Gtk.PrintContext.PrintContext) context
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  context' pageNr


-- | Connect a signal handler for the [drawPage](#signal:drawPage) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' printOperation #drawPage callback
-- @
-- 
-- 
onPrintOperationDrawPage :: (IsPrintOperation a, MonadIO m) => a -> ((?self :: a) => PrintOperationDrawPageCallback) -> m SignalHandlerId
onPrintOperationDrawPage obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_PrintOperationDrawPageCallback wrapped
    wrapped'' <- mk_PrintOperationDrawPageCallback wrapped'
    connectSignalFunPtr obj "draw-page" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [drawPage](#signal:drawPage) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' printOperation #drawPage callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterPrintOperationDrawPage :: (IsPrintOperation a, MonadIO m) => a -> ((?self :: a) => PrintOperationDrawPageCallback) -> m SignalHandlerId
afterPrintOperationDrawPage obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_PrintOperationDrawPageCallback wrapped
    wrapped'' <- mk_PrintOperationDrawPageCallback wrapped'
    connectSignalFunPtr obj "draw-page" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data PrintOperationDrawPageSignalInfo
instance SignalInfo PrintOperationDrawPageSignalInfo where
    type HaskellCallbackType PrintOperationDrawPageSignalInfo = PrintOperationDrawPageCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_PrintOperationDrawPageCallback cb
        cb'' <- mk_PrintOperationDrawPageCallback cb'
        connectSignalFunPtr obj "draw-page" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintOperation::draw-page"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintOperation.html#g:signal:drawPage"})

#endif

-- signal PrintOperation::end-print
-- | Emitted after all pages have been rendered.
-- A handler for this signal can clean up any resources that have
-- been allocated in the [PrintOperation::beginPrint]("GI.Gtk.Objects.PrintOperation#g:signal:beginPrint") handler.
-- 
-- /Since: 2.10/
type PrintOperationEndPrintCallback =
    Gtk.PrintContext.PrintContext
    -- ^ /@context@/: the t'GI.Gtk.Objects.PrintContext.PrintContext' for the current operation
    -> IO ()

type C_PrintOperationEndPrintCallback =
    Ptr PrintOperation ->                   -- object
    Ptr Gtk.PrintContext.PrintContext ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_PrintOperationEndPrintCallback`.
foreign import ccall "wrapper"
    mk_PrintOperationEndPrintCallback :: C_PrintOperationEndPrintCallback -> IO (FunPtr C_PrintOperationEndPrintCallback)

wrap_PrintOperationEndPrintCallback :: 
    GObject a => (a -> PrintOperationEndPrintCallback) ->
    C_PrintOperationEndPrintCallback
wrap_PrintOperationEndPrintCallback gi'cb gi'selfPtr context _ = do
    context' <- (newObject Gtk.PrintContext.PrintContext) context
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  context'


-- | Connect a signal handler for the [endPrint](#signal:endPrint) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' printOperation #endPrint callback
-- @
-- 
-- 
onPrintOperationEndPrint :: (IsPrintOperation a, MonadIO m) => a -> ((?self :: a) => PrintOperationEndPrintCallback) -> m SignalHandlerId
onPrintOperationEndPrint obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_PrintOperationEndPrintCallback wrapped
    wrapped'' <- mk_PrintOperationEndPrintCallback wrapped'
    connectSignalFunPtr obj "end-print" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [endPrint](#signal:endPrint) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' printOperation #endPrint callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterPrintOperationEndPrint :: (IsPrintOperation a, MonadIO m) => a -> ((?self :: a) => PrintOperationEndPrintCallback) -> m SignalHandlerId
afterPrintOperationEndPrint obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_PrintOperationEndPrintCallback wrapped
    wrapped'' <- mk_PrintOperationEndPrintCallback wrapped'
    connectSignalFunPtr obj "end-print" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data PrintOperationEndPrintSignalInfo
instance SignalInfo PrintOperationEndPrintSignalInfo where
    type HaskellCallbackType PrintOperationEndPrintSignalInfo = PrintOperationEndPrintCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_PrintOperationEndPrintCallback cb
        cb'' <- mk_PrintOperationEndPrintCallback cb'
        connectSignalFunPtr obj "end-print" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintOperation::end-print"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintOperation.html#g:signal:endPrint"})

#endif

-- signal PrintOperation::paginate
-- | Emitted after the [PrintOperation::beginPrint]("GI.Gtk.Objects.PrintOperation#g:signal:beginPrint") signal, but before
-- the actual rendering starts. It keeps getting emitted until a connected
-- signal handler returns 'P.True'.
-- 
-- The [paginate](#g:signal:paginate) signal is intended to be used for paginating a document
-- in small chunks, to avoid blocking the user interface for a long
-- time. The signal handler should update the number of pages using
-- 'GI.Gtk.Objects.PrintOperation.printOperationSetNPages', and return 'P.True' if the document
-- has been completely paginated.
-- 
-- If you don\'t need to do pagination in chunks, you can simply do
-- it all in the [beginPrint](#g:signal:beginPrint) handler, and set the number of pages
-- from there.
-- 
-- /Since: 2.10/
type PrintOperationPaginateCallback =
    Gtk.PrintContext.PrintContext
    -- ^ /@context@/: the t'GI.Gtk.Objects.PrintContext.PrintContext' for the current operation
    -> IO Bool
    -- ^ __Returns:__ 'P.True' if pagination is complete

type C_PrintOperationPaginateCallback =
    Ptr PrintOperation ->                   -- object
    Ptr Gtk.PrintContext.PrintContext ->
    Ptr () ->                               -- user_data
    IO CInt

-- | Generate a function pointer callable from C code, from a `C_PrintOperationPaginateCallback`.
foreign import ccall "wrapper"
    mk_PrintOperationPaginateCallback :: C_PrintOperationPaginateCallback -> IO (FunPtr C_PrintOperationPaginateCallback)

wrap_PrintOperationPaginateCallback :: 
    GObject a => (a -> PrintOperationPaginateCallback) ->
    C_PrintOperationPaginateCallback
wrap_PrintOperationPaginateCallback gi'cb gi'selfPtr context _ = do
    context' <- (newObject Gtk.PrintContext.PrintContext) context
    result <- B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  context'
    let result' = (fromIntegral . fromEnum) result
    return result'


-- | Connect a signal handler for the [paginate](#signal:paginate) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' printOperation #paginate callback
-- @
-- 
-- 
onPrintOperationPaginate :: (IsPrintOperation a, MonadIO m) => a -> ((?self :: a) => PrintOperationPaginateCallback) -> m SignalHandlerId
onPrintOperationPaginate obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_PrintOperationPaginateCallback wrapped
    wrapped'' <- mk_PrintOperationPaginateCallback wrapped'
    connectSignalFunPtr obj "paginate" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [paginate](#signal:paginate) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' printOperation #paginate callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterPrintOperationPaginate :: (IsPrintOperation a, MonadIO m) => a -> ((?self :: a) => PrintOperationPaginateCallback) -> m SignalHandlerId
afterPrintOperationPaginate obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_PrintOperationPaginateCallback wrapped
    wrapped'' <- mk_PrintOperationPaginateCallback wrapped'
    connectSignalFunPtr obj "paginate" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data PrintOperationPaginateSignalInfo
instance SignalInfo PrintOperationPaginateSignalInfo where
    type HaskellCallbackType PrintOperationPaginateSignalInfo = PrintOperationPaginateCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_PrintOperationPaginateCallback cb
        cb'' <- mk_PrintOperationPaginateCallback cb'
        connectSignalFunPtr obj "paginate" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintOperation::paginate"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintOperation.html#g:signal:paginate"})

#endif

-- signal PrintOperation::preview
-- | Gets emitted when a preview is requested from the native dialog.
-- 
-- The default handler for this signal uses an external viewer
-- application to preview.
-- 
-- To implement a custom print preview, an application must return
-- 'P.True' from its handler for this signal. In order to use the
-- provided /@context@/ for the preview implementation, it must be
-- given a suitable cairo context with 'GI.Gtk.Objects.PrintContext.printContextSetCairoContext'.
-- 
-- The custom preview implementation can use
-- 'GI.Gtk.Interfaces.PrintOperationPreview.printOperationPreviewIsSelected' and
-- 'GI.Gtk.Interfaces.PrintOperationPreview.printOperationPreviewRenderPage' to find pages which
-- are selected for print and render them. The preview must be
-- finished by calling 'GI.Gtk.Interfaces.PrintOperationPreview.printOperationPreviewEndPreview'
-- (typically in response to the user clicking a close button).
-- 
-- /Since: 2.10/
type PrintOperationPreviewCallback =
    Gtk.PrintOperationPreview.PrintOperationPreview
    -- ^ /@preview@/: the t'GI.Gtk.Interfaces.PrintOperationPreview.PrintOperationPreview' for the current operation
    -> Gtk.PrintContext.PrintContext
    -- ^ /@context@/: the t'GI.Gtk.Objects.PrintContext.PrintContext' that will be used
    -> Maybe Gtk.Window.Window
    -- ^ /@parent@/: the t'GI.Gtk.Objects.Window.Window' to use as window parent, or 'P.Nothing'
    -> IO Bool
    -- ^ __Returns:__ 'P.True' if the listener wants to take over control of the preview

type C_PrintOperationPreviewCallback =
    Ptr PrintOperation ->                   -- object
    Ptr Gtk.PrintOperationPreview.PrintOperationPreview ->
    Ptr Gtk.PrintContext.PrintContext ->
    Ptr Gtk.Window.Window ->
    Ptr () ->                               -- user_data
    IO CInt

-- | Generate a function pointer callable from C code, from a `C_PrintOperationPreviewCallback`.
foreign import ccall "wrapper"
    mk_PrintOperationPreviewCallback :: C_PrintOperationPreviewCallback -> IO (FunPtr C_PrintOperationPreviewCallback)

wrap_PrintOperationPreviewCallback :: 
    GObject a => (a -> PrintOperationPreviewCallback) ->
    C_PrintOperationPreviewCallback
wrap_PrintOperationPreviewCallback gi'cb gi'selfPtr preview context parent _ = do
    preview' <- (newObject Gtk.PrintOperationPreview.PrintOperationPreview) preview
    context' <- (newObject Gtk.PrintContext.PrintContext) context
    maybeParent <-
        if parent == nullPtr
        then return Nothing
        else do
            parent' <- (newObject Gtk.Window.Window) parent
            return $ Just parent'
    result <- B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  preview' context' maybeParent
    let result' = (fromIntegral . fromEnum) result
    return result'


-- | Connect a signal handler for the [preview](#signal:preview) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' printOperation #preview callback
-- @
-- 
-- 
onPrintOperationPreview :: (IsPrintOperation a, MonadIO m) => a -> ((?self :: a) => PrintOperationPreviewCallback) -> m SignalHandlerId
onPrintOperationPreview obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_PrintOperationPreviewCallback wrapped
    wrapped'' <- mk_PrintOperationPreviewCallback wrapped'
    connectSignalFunPtr obj "preview" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [preview](#signal:preview) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' printOperation #preview callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterPrintOperationPreview :: (IsPrintOperation a, MonadIO m) => a -> ((?self :: a) => PrintOperationPreviewCallback) -> m SignalHandlerId
afterPrintOperationPreview obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_PrintOperationPreviewCallback wrapped
    wrapped'' <- mk_PrintOperationPreviewCallback wrapped'
    connectSignalFunPtr obj "preview" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data PrintOperationPreviewSignalInfo
instance SignalInfo PrintOperationPreviewSignalInfo where
    type HaskellCallbackType PrintOperationPreviewSignalInfo = PrintOperationPreviewCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_PrintOperationPreviewCallback cb
        cb'' <- mk_PrintOperationPreviewCallback cb'
        connectSignalFunPtr obj "preview" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintOperation::preview"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintOperation.html#g:signal:preview"})

#endif

-- signal PrintOperation::request-page-setup
-- | Emitted once for every page that is printed, to give
-- the application a chance to modify the page setup. Any changes
-- done to /@setup@/ will be in force only for printing this page.
-- 
-- /Since: 2.10/
type PrintOperationRequestPageSetupCallback =
    Gtk.PrintContext.PrintContext
    -- ^ /@context@/: the t'GI.Gtk.Objects.PrintContext.PrintContext' for the current operation
    -> Int32
    -- ^ /@pageNr@/: the number of the currently printed page (0-based)
    -> Gtk.PageSetup.PageSetup
    -- ^ /@setup@/: the t'GI.Gtk.Objects.PageSetup.PageSetup'
    -> IO ()

type C_PrintOperationRequestPageSetupCallback =
    Ptr PrintOperation ->                   -- object
    Ptr Gtk.PrintContext.PrintContext ->
    Int32 ->
    Ptr Gtk.PageSetup.PageSetup ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_PrintOperationRequestPageSetupCallback`.
foreign import ccall "wrapper"
    mk_PrintOperationRequestPageSetupCallback :: C_PrintOperationRequestPageSetupCallback -> IO (FunPtr C_PrintOperationRequestPageSetupCallback)

wrap_PrintOperationRequestPageSetupCallback :: 
    GObject a => (a -> PrintOperationRequestPageSetupCallback) ->
    C_PrintOperationRequestPageSetupCallback
wrap_PrintOperationRequestPageSetupCallback gi'cb gi'selfPtr context pageNr setup _ = do
    context' <- (newObject Gtk.PrintContext.PrintContext) context
    setup' <- (newObject Gtk.PageSetup.PageSetup) setup
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  context' pageNr setup'


-- | Connect a signal handler for the [requestPageSetup](#signal:requestPageSetup) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' printOperation #requestPageSetup callback
-- @
-- 
-- 
onPrintOperationRequestPageSetup :: (IsPrintOperation a, MonadIO m) => a -> ((?self :: a) => PrintOperationRequestPageSetupCallback) -> m SignalHandlerId
onPrintOperationRequestPageSetup obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_PrintOperationRequestPageSetupCallback wrapped
    wrapped'' <- mk_PrintOperationRequestPageSetupCallback wrapped'
    connectSignalFunPtr obj "request-page-setup" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [requestPageSetup](#signal:requestPageSetup) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' printOperation #requestPageSetup callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterPrintOperationRequestPageSetup :: (IsPrintOperation a, MonadIO m) => a -> ((?self :: a) => PrintOperationRequestPageSetupCallback) -> m SignalHandlerId
afterPrintOperationRequestPageSetup obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_PrintOperationRequestPageSetupCallback wrapped
    wrapped'' <- mk_PrintOperationRequestPageSetupCallback wrapped'
    connectSignalFunPtr obj "request-page-setup" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data PrintOperationRequestPageSetupSignalInfo
instance SignalInfo PrintOperationRequestPageSetupSignalInfo where
    type HaskellCallbackType PrintOperationRequestPageSetupSignalInfo = PrintOperationRequestPageSetupCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_PrintOperationRequestPageSetupCallback cb
        cb'' <- mk_PrintOperationRequestPageSetupCallback cb'
        connectSignalFunPtr obj "request-page-setup" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintOperation::request-page-setup"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintOperation.html#g:signal:requestPageSetup"})

#endif

-- signal PrintOperation::status-changed
-- | Emitted at between the various phases of the print operation.
-- See t'GI.Gtk.Enums.PrintStatus' for the phases that are being discriminated.
-- Use 'GI.Gtk.Objects.PrintOperation.printOperationGetStatus' to find out the current
-- status.
-- 
-- /Since: 2.10/
type PrintOperationStatusChangedCallback =
    IO ()

type C_PrintOperationStatusChangedCallback =
    Ptr PrintOperation ->                   -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_PrintOperationStatusChangedCallback`.
foreign import ccall "wrapper"
    mk_PrintOperationStatusChangedCallback :: C_PrintOperationStatusChangedCallback -> IO (FunPtr C_PrintOperationStatusChangedCallback)

wrap_PrintOperationStatusChangedCallback :: 
    GObject a => (a -> PrintOperationStatusChangedCallback) ->
    C_PrintOperationStatusChangedCallback
wrap_PrintOperationStatusChangedCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [statusChanged](#signal:statusChanged) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' printOperation #statusChanged callback
-- @
-- 
-- 
onPrintOperationStatusChanged :: (IsPrintOperation a, MonadIO m) => a -> ((?self :: a) => PrintOperationStatusChangedCallback) -> m SignalHandlerId
onPrintOperationStatusChanged obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_PrintOperationStatusChangedCallback wrapped
    wrapped'' <- mk_PrintOperationStatusChangedCallback wrapped'
    connectSignalFunPtr obj "status-changed" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [statusChanged](#signal:statusChanged) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' printOperation #statusChanged callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterPrintOperationStatusChanged :: (IsPrintOperation a, MonadIO m) => a -> ((?self :: a) => PrintOperationStatusChangedCallback) -> m SignalHandlerId
afterPrintOperationStatusChanged obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_PrintOperationStatusChangedCallback wrapped
    wrapped'' <- mk_PrintOperationStatusChangedCallback wrapped'
    connectSignalFunPtr obj "status-changed" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data PrintOperationStatusChangedSignalInfo
instance SignalInfo PrintOperationStatusChangedSignalInfo where
    type HaskellCallbackType PrintOperationStatusChangedSignalInfo = PrintOperationStatusChangedCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_PrintOperationStatusChangedCallback cb
        cb'' <- mk_PrintOperationStatusChangedCallback cb'
        connectSignalFunPtr obj "status-changed" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintOperation::status-changed"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintOperation.html#g:signal:statusChanged"})

#endif

-- signal PrintOperation::update-custom-widget
-- | Emitted after change of selected printer. The actual page setup and
-- print settings are passed to the custom widget, which can actualize
-- itself according to this change.
-- 
-- /Since: 2.18/
type PrintOperationUpdateCustomWidgetCallback =
    Gtk.Widget.Widget
    -- ^ /@widget@/: the custom widget added in create-custom-widget
    -> Gtk.PageSetup.PageSetup
    -- ^ /@setup@/: actual page setup
    -> Gtk.PrintSettings.PrintSettings
    -- ^ /@settings@/: actual print settings
    -> IO ()

type C_PrintOperationUpdateCustomWidgetCallback =
    Ptr PrintOperation ->                   -- object
    Ptr Gtk.Widget.Widget ->
    Ptr Gtk.PageSetup.PageSetup ->
    Ptr Gtk.PrintSettings.PrintSettings ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_PrintOperationUpdateCustomWidgetCallback`.
foreign import ccall "wrapper"
    mk_PrintOperationUpdateCustomWidgetCallback :: C_PrintOperationUpdateCustomWidgetCallback -> IO (FunPtr C_PrintOperationUpdateCustomWidgetCallback)

wrap_PrintOperationUpdateCustomWidgetCallback :: 
    GObject a => (a -> PrintOperationUpdateCustomWidgetCallback) ->
    C_PrintOperationUpdateCustomWidgetCallback
wrap_PrintOperationUpdateCustomWidgetCallback gi'cb gi'selfPtr widget setup settings _ = do
    widget' <- (newObject Gtk.Widget.Widget) widget
    setup' <- (newObject Gtk.PageSetup.PageSetup) setup
    settings' <- (newObject Gtk.PrintSettings.PrintSettings) settings
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  widget' setup' settings'


-- | Connect a signal handler for the [updateCustomWidget](#signal:updateCustomWidget) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' printOperation #updateCustomWidget callback
-- @
-- 
-- 
onPrintOperationUpdateCustomWidget :: (IsPrintOperation a, MonadIO m) => a -> ((?self :: a) => PrintOperationUpdateCustomWidgetCallback) -> m SignalHandlerId
onPrintOperationUpdateCustomWidget obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_PrintOperationUpdateCustomWidgetCallback wrapped
    wrapped'' <- mk_PrintOperationUpdateCustomWidgetCallback wrapped'
    connectSignalFunPtr obj "update-custom-widget" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [updateCustomWidget](#signal:updateCustomWidget) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' printOperation #updateCustomWidget callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterPrintOperationUpdateCustomWidget :: (IsPrintOperation a, MonadIO m) => a -> ((?self :: a) => PrintOperationUpdateCustomWidgetCallback) -> m SignalHandlerId
afterPrintOperationUpdateCustomWidget obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_PrintOperationUpdateCustomWidgetCallback wrapped
    wrapped'' <- mk_PrintOperationUpdateCustomWidgetCallback wrapped'
    connectSignalFunPtr obj "update-custom-widget" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data PrintOperationUpdateCustomWidgetSignalInfo
instance SignalInfo PrintOperationUpdateCustomWidgetSignalInfo where
    type HaskellCallbackType PrintOperationUpdateCustomWidgetSignalInfo = PrintOperationUpdateCustomWidgetCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_PrintOperationUpdateCustomWidgetCallback cb
        cb'' <- mk_PrintOperationUpdateCustomWidgetCallback cb'
        connectSignalFunPtr obj "update-custom-widget" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintOperation::update-custom-widget"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintOperation.html#g:signal:updateCustomWidget"})

#endif

-- VVV Prop "allow-async"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Just False)

-- | Get the value of the “@allow-async@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' printOperation #allowAsync
-- @
getPrintOperationAllowAsync :: (MonadIO m, IsPrintOperation o) => o -> m Bool
getPrintOperationAllowAsync obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "allow-async"

-- | Set the value of the “@allow-async@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' printOperation [ #allowAsync 'Data.GI.Base.Attributes.:=' value ]
-- @
setPrintOperationAllowAsync :: (MonadIO m, IsPrintOperation o) => o -> Bool -> m ()
setPrintOperationAllowAsync obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "allow-async" val

-- | Construct a `GValueConstruct` with valid value for the “@allow-async@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructPrintOperationAllowAsync :: (IsPrintOperation o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructPrintOperationAllowAsync val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "allow-async" val

#if defined(ENABLE_OVERLOADING)
data PrintOperationAllowAsyncPropertyInfo
instance AttrInfo PrintOperationAllowAsyncPropertyInfo where
    type AttrAllowedOps PrintOperationAllowAsyncPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint PrintOperationAllowAsyncPropertyInfo = IsPrintOperation
    type AttrSetTypeConstraint PrintOperationAllowAsyncPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint PrintOperationAllowAsyncPropertyInfo = (~) Bool
    type AttrTransferType PrintOperationAllowAsyncPropertyInfo = Bool
    type AttrGetType PrintOperationAllowAsyncPropertyInfo = Bool
    type AttrLabel PrintOperationAllowAsyncPropertyInfo = "allow-async"
    type AttrOrigin PrintOperationAllowAsyncPropertyInfo = PrintOperation
    attrGet = getPrintOperationAllowAsync
    attrSet = setPrintOperationAllowAsync
    attrTransfer _ v = do
        return v
    attrConstruct = constructPrintOperationAllowAsync
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintOperation.allowAsync"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintOperation.html#g:attr:allowAsync"
        })
#endif

-- VVV Prop "current-page"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Just False)

-- | Get the value of the “@current-page@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' printOperation #currentPage
-- @
getPrintOperationCurrentPage :: (MonadIO m, IsPrintOperation o) => o -> m Int32
getPrintOperationCurrentPage obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "current-page"

-- | Set the value of the “@current-page@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' printOperation [ #currentPage 'Data.GI.Base.Attributes.:=' value ]
-- @
setPrintOperationCurrentPage :: (MonadIO m, IsPrintOperation o) => o -> Int32 -> m ()
setPrintOperationCurrentPage obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyInt32 obj "current-page" val

-- | Construct a `GValueConstruct` with valid value for the “@current-page@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructPrintOperationCurrentPage :: (IsPrintOperation o, MIO.MonadIO m) => Int32 -> m (GValueConstruct o)
constructPrintOperationCurrentPage val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyInt32 "current-page" val

#if defined(ENABLE_OVERLOADING)
data PrintOperationCurrentPagePropertyInfo
instance AttrInfo PrintOperationCurrentPagePropertyInfo where
    type AttrAllowedOps PrintOperationCurrentPagePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint PrintOperationCurrentPagePropertyInfo = IsPrintOperation
    type AttrSetTypeConstraint PrintOperationCurrentPagePropertyInfo = (~) Int32
    type AttrTransferTypeConstraint PrintOperationCurrentPagePropertyInfo = (~) Int32
    type AttrTransferType PrintOperationCurrentPagePropertyInfo = Int32
    type AttrGetType PrintOperationCurrentPagePropertyInfo = Int32
    type AttrLabel PrintOperationCurrentPagePropertyInfo = "current-page"
    type AttrOrigin PrintOperationCurrentPagePropertyInfo = PrintOperation
    attrGet = getPrintOperationCurrentPage
    attrSet = setPrintOperationCurrentPage
    attrTransfer _ v = do
        return v
    attrConstruct = constructPrintOperationCurrentPage
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintOperation.currentPage"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintOperation.html#g:attr:currentPage"
        })
#endif

-- VVV Prop "custom-tab-label"
   -- Type: TBasicType TUTF8
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Just True)

-- | Get the value of the “@custom-tab-label@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' printOperation #customTabLabel
-- @
getPrintOperationCustomTabLabel :: (MonadIO m, IsPrintOperation o) => o -> m (Maybe T.Text)
getPrintOperationCustomTabLabel obj = MIO.liftIO $ B.Properties.getObjectPropertyString obj "custom-tab-label"

-- | Set the value of the “@custom-tab-label@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' printOperation [ #customTabLabel 'Data.GI.Base.Attributes.:=' value ]
-- @
setPrintOperationCustomTabLabel :: (MonadIO m, IsPrintOperation o) => o -> T.Text -> m ()
setPrintOperationCustomTabLabel obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "custom-tab-label" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@custom-tab-label@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructPrintOperationCustomTabLabel :: (IsPrintOperation o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructPrintOperationCustomTabLabel val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "custom-tab-label" (P.Just val)

-- | Set the value of the “@custom-tab-label@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #customTabLabel
-- @
clearPrintOperationCustomTabLabel :: (MonadIO m, IsPrintOperation o) => o -> m ()
clearPrintOperationCustomTabLabel obj = liftIO $ B.Properties.setObjectPropertyString obj "custom-tab-label" (Nothing :: Maybe T.Text)

#if defined(ENABLE_OVERLOADING)
data PrintOperationCustomTabLabelPropertyInfo
instance AttrInfo PrintOperationCustomTabLabelPropertyInfo where
    type AttrAllowedOps PrintOperationCustomTabLabelPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint PrintOperationCustomTabLabelPropertyInfo = IsPrintOperation
    type AttrSetTypeConstraint PrintOperationCustomTabLabelPropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint PrintOperationCustomTabLabelPropertyInfo = (~) T.Text
    type AttrTransferType PrintOperationCustomTabLabelPropertyInfo = T.Text
    type AttrGetType PrintOperationCustomTabLabelPropertyInfo = (Maybe T.Text)
    type AttrLabel PrintOperationCustomTabLabelPropertyInfo = "custom-tab-label"
    type AttrOrigin PrintOperationCustomTabLabelPropertyInfo = PrintOperation
    attrGet = getPrintOperationCustomTabLabel
    attrSet = setPrintOperationCustomTabLabel
    attrTransfer _ v = do
        return v
    attrConstruct = constructPrintOperationCustomTabLabel
    attrClear = clearPrintOperationCustomTabLabel
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintOperation.customTabLabel"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintOperation.html#g:attr:customTabLabel"
        })
#endif

-- VVV Prop "default-page-setup"
   -- Type: TInterface (Name {namespace = "Gtk", name = "PageSetup"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just True)

-- | Get the value of the “@default-page-setup@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' printOperation #defaultPageSetup
-- @
getPrintOperationDefaultPageSetup :: (MonadIO m, IsPrintOperation o) => o -> m Gtk.PageSetup.PageSetup
getPrintOperationDefaultPageSetup obj = MIO.liftIO $ checkUnexpectedNothing "getPrintOperationDefaultPageSetup" $ B.Properties.getObjectPropertyObject obj "default-page-setup" Gtk.PageSetup.PageSetup

-- | Set the value of the “@default-page-setup@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' printOperation [ #defaultPageSetup 'Data.GI.Base.Attributes.:=' value ]
-- @
setPrintOperationDefaultPageSetup :: (MonadIO m, IsPrintOperation o, Gtk.PageSetup.IsPageSetup a) => o -> a -> m ()
setPrintOperationDefaultPageSetup obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyObject obj "default-page-setup" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@default-page-setup@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructPrintOperationDefaultPageSetup :: (IsPrintOperation o, MIO.MonadIO m, Gtk.PageSetup.IsPageSetup a) => a -> m (GValueConstruct o)
constructPrintOperationDefaultPageSetup val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyObject "default-page-setup" (P.Just val)

-- | Set the value of the “@default-page-setup@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #defaultPageSetup
-- @
clearPrintOperationDefaultPageSetup :: (MonadIO m, IsPrintOperation o) => o -> m ()
clearPrintOperationDefaultPageSetup obj = liftIO $ B.Properties.setObjectPropertyObject obj "default-page-setup" (Nothing :: Maybe Gtk.PageSetup.PageSetup)

#if defined(ENABLE_OVERLOADING)
data PrintOperationDefaultPageSetupPropertyInfo
instance AttrInfo PrintOperationDefaultPageSetupPropertyInfo where
    type AttrAllowedOps PrintOperationDefaultPageSetupPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint PrintOperationDefaultPageSetupPropertyInfo = IsPrintOperation
    type AttrSetTypeConstraint PrintOperationDefaultPageSetupPropertyInfo = Gtk.PageSetup.IsPageSetup
    type AttrTransferTypeConstraint PrintOperationDefaultPageSetupPropertyInfo = Gtk.PageSetup.IsPageSetup
    type AttrTransferType PrintOperationDefaultPageSetupPropertyInfo = Gtk.PageSetup.PageSetup
    type AttrGetType PrintOperationDefaultPageSetupPropertyInfo = Gtk.PageSetup.PageSetup
    type AttrLabel PrintOperationDefaultPageSetupPropertyInfo = "default-page-setup"
    type AttrOrigin PrintOperationDefaultPageSetupPropertyInfo = PrintOperation
    attrGet = getPrintOperationDefaultPageSetup
    attrSet = setPrintOperationDefaultPageSetup
    attrTransfer _ v = do
        unsafeCastTo Gtk.PageSetup.PageSetup v
    attrConstruct = constructPrintOperationDefaultPageSetup
    attrClear = clearPrintOperationDefaultPageSetup
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintOperation.defaultPageSetup"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintOperation.html#g:attr:defaultPageSetup"
        })
#endif

-- VVV Prop "embed-page-setup"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@embed-page-setup@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' printOperation #embedPageSetup
-- @
getPrintOperationEmbedPageSetup :: (MonadIO m, IsPrintOperation o) => o -> m Bool
getPrintOperationEmbedPageSetup obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "embed-page-setup"

-- | Set the value of the “@embed-page-setup@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' printOperation [ #embedPageSetup 'Data.GI.Base.Attributes.:=' value ]
-- @
setPrintOperationEmbedPageSetup :: (MonadIO m, IsPrintOperation o) => o -> Bool -> m ()
setPrintOperationEmbedPageSetup obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "embed-page-setup" val

-- | Construct a `GValueConstruct` with valid value for the “@embed-page-setup@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructPrintOperationEmbedPageSetup :: (IsPrintOperation o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructPrintOperationEmbedPageSetup val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "embed-page-setup" val

#if defined(ENABLE_OVERLOADING)
data PrintOperationEmbedPageSetupPropertyInfo
instance AttrInfo PrintOperationEmbedPageSetupPropertyInfo where
    type AttrAllowedOps PrintOperationEmbedPageSetupPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint PrintOperationEmbedPageSetupPropertyInfo = IsPrintOperation
    type AttrSetTypeConstraint PrintOperationEmbedPageSetupPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint PrintOperationEmbedPageSetupPropertyInfo = (~) Bool
    type AttrTransferType PrintOperationEmbedPageSetupPropertyInfo = Bool
    type AttrGetType PrintOperationEmbedPageSetupPropertyInfo = Bool
    type AttrLabel PrintOperationEmbedPageSetupPropertyInfo = "embed-page-setup"
    type AttrOrigin PrintOperationEmbedPageSetupPropertyInfo = PrintOperation
    attrGet = getPrintOperationEmbedPageSetup
    attrSet = setPrintOperationEmbedPageSetup
    attrTransfer _ v = do
        return v
    attrConstruct = constructPrintOperationEmbedPageSetup
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintOperation.embedPageSetup"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintOperation.html#g:attr:embedPageSetup"
        })
#endif

-- VVV Prop "export-filename"
   -- Type: TBasicType TUTF8
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@export-filename@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' printOperation #exportFilename
-- @
getPrintOperationExportFilename :: (MonadIO m, IsPrintOperation o) => o -> m (Maybe T.Text)
getPrintOperationExportFilename obj = MIO.liftIO $ B.Properties.getObjectPropertyString obj "export-filename"

-- | Set the value of the “@export-filename@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' printOperation [ #exportFilename 'Data.GI.Base.Attributes.:=' value ]
-- @
setPrintOperationExportFilename :: (MonadIO m, IsPrintOperation o) => o -> T.Text -> m ()
setPrintOperationExportFilename obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "export-filename" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@export-filename@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructPrintOperationExportFilename :: (IsPrintOperation o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructPrintOperationExportFilename val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "export-filename" (P.Just val)

-- | Set the value of the “@export-filename@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #exportFilename
-- @
clearPrintOperationExportFilename :: (MonadIO m, IsPrintOperation o) => o -> m ()
clearPrintOperationExportFilename obj = liftIO $ B.Properties.setObjectPropertyString obj "export-filename" (Nothing :: Maybe T.Text)

#if defined(ENABLE_OVERLOADING)
data PrintOperationExportFilenamePropertyInfo
instance AttrInfo PrintOperationExportFilenamePropertyInfo where
    type AttrAllowedOps PrintOperationExportFilenamePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint PrintOperationExportFilenamePropertyInfo = IsPrintOperation
    type AttrSetTypeConstraint PrintOperationExportFilenamePropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint PrintOperationExportFilenamePropertyInfo = (~) T.Text
    type AttrTransferType PrintOperationExportFilenamePropertyInfo = T.Text
    type AttrGetType PrintOperationExportFilenamePropertyInfo = (Maybe T.Text)
    type AttrLabel PrintOperationExportFilenamePropertyInfo = "export-filename"
    type AttrOrigin PrintOperationExportFilenamePropertyInfo = PrintOperation
    attrGet = getPrintOperationExportFilename
    attrSet = setPrintOperationExportFilename
    attrTransfer _ v = do
        return v
    attrConstruct = constructPrintOperationExportFilename
    attrClear = clearPrintOperationExportFilename
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintOperation.exportFilename"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintOperation.html#g:attr:exportFilename"
        })
#endif

-- VVV Prop "has-selection"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@has-selection@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' printOperation #hasSelection
-- @
getPrintOperationHasSelection :: (MonadIO m, IsPrintOperation o) => o -> m Bool
getPrintOperationHasSelection obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "has-selection"

-- | Set the value of the “@has-selection@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' printOperation [ #hasSelection 'Data.GI.Base.Attributes.:=' value ]
-- @
setPrintOperationHasSelection :: (MonadIO m, IsPrintOperation o) => o -> Bool -> m ()
setPrintOperationHasSelection obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "has-selection" val

-- | Construct a `GValueConstruct` with valid value for the “@has-selection@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructPrintOperationHasSelection :: (IsPrintOperation o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructPrintOperationHasSelection val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "has-selection" val

#if defined(ENABLE_OVERLOADING)
data PrintOperationHasSelectionPropertyInfo
instance AttrInfo PrintOperationHasSelectionPropertyInfo where
    type AttrAllowedOps PrintOperationHasSelectionPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint PrintOperationHasSelectionPropertyInfo = IsPrintOperation
    type AttrSetTypeConstraint PrintOperationHasSelectionPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint PrintOperationHasSelectionPropertyInfo = (~) Bool
    type AttrTransferType PrintOperationHasSelectionPropertyInfo = Bool
    type AttrGetType PrintOperationHasSelectionPropertyInfo = Bool
    type AttrLabel PrintOperationHasSelectionPropertyInfo = "has-selection"
    type AttrOrigin PrintOperationHasSelectionPropertyInfo = PrintOperation
    attrGet = getPrintOperationHasSelection
    attrSet = setPrintOperationHasSelection
    attrTransfer _ v = do
        return v
    attrConstruct = constructPrintOperationHasSelection
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintOperation.hasSelection"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintOperation.html#g:attr:hasSelection"
        })
#endif

-- VVV Prop "job-name"
   -- Type: TBasicType TUTF8
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Just False)

-- | Get the value of the “@job-name@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' printOperation #jobName
-- @
getPrintOperationJobName :: (MonadIO m, IsPrintOperation o) => o -> m (Maybe T.Text)
getPrintOperationJobName obj = MIO.liftIO $ B.Properties.getObjectPropertyString obj "job-name"

-- | Set the value of the “@job-name@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' printOperation [ #jobName 'Data.GI.Base.Attributes.:=' value ]
-- @
setPrintOperationJobName :: (MonadIO m, IsPrintOperation o) => o -> T.Text -> m ()
setPrintOperationJobName obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "job-name" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@job-name@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructPrintOperationJobName :: (IsPrintOperation o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructPrintOperationJobName val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "job-name" (P.Just val)

#if defined(ENABLE_OVERLOADING)
data PrintOperationJobNamePropertyInfo
instance AttrInfo PrintOperationJobNamePropertyInfo where
    type AttrAllowedOps PrintOperationJobNamePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint PrintOperationJobNamePropertyInfo = IsPrintOperation
    type AttrSetTypeConstraint PrintOperationJobNamePropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint PrintOperationJobNamePropertyInfo = (~) T.Text
    type AttrTransferType PrintOperationJobNamePropertyInfo = T.Text
    type AttrGetType PrintOperationJobNamePropertyInfo = (Maybe T.Text)
    type AttrLabel PrintOperationJobNamePropertyInfo = "job-name"
    type AttrOrigin PrintOperationJobNamePropertyInfo = PrintOperation
    attrGet = getPrintOperationJobName
    attrSet = setPrintOperationJobName
    attrTransfer _ v = do
        return v
    attrConstruct = constructPrintOperationJobName
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintOperation.jobName"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintOperation.html#g:attr:jobName"
        })
#endif

-- VVV Prop "n-pages"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Just False)

-- | Get the value of the “@n-pages@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' printOperation #nPages
-- @
getPrintOperationNPages :: (MonadIO m, IsPrintOperation o) => o -> m Int32
getPrintOperationNPages obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "n-pages"

-- | Set the value of the “@n-pages@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' printOperation [ #nPages 'Data.GI.Base.Attributes.:=' value ]
-- @
setPrintOperationNPages :: (MonadIO m, IsPrintOperation o) => o -> Int32 -> m ()
setPrintOperationNPages obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyInt32 obj "n-pages" val

-- | Construct a `GValueConstruct` with valid value for the “@n-pages@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructPrintOperationNPages :: (IsPrintOperation o, MIO.MonadIO m) => Int32 -> m (GValueConstruct o)
constructPrintOperationNPages val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyInt32 "n-pages" val

#if defined(ENABLE_OVERLOADING)
data PrintOperationNPagesPropertyInfo
instance AttrInfo PrintOperationNPagesPropertyInfo where
    type AttrAllowedOps PrintOperationNPagesPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint PrintOperationNPagesPropertyInfo = IsPrintOperation
    type AttrSetTypeConstraint PrintOperationNPagesPropertyInfo = (~) Int32
    type AttrTransferTypeConstraint PrintOperationNPagesPropertyInfo = (~) Int32
    type AttrTransferType PrintOperationNPagesPropertyInfo = Int32
    type AttrGetType PrintOperationNPagesPropertyInfo = Int32
    type AttrLabel PrintOperationNPagesPropertyInfo = "n-pages"
    type AttrOrigin PrintOperationNPagesPropertyInfo = PrintOperation
    attrGet = getPrintOperationNPages
    attrSet = setPrintOperationNPages
    attrTransfer _ v = do
        return v
    attrConstruct = constructPrintOperationNPages
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintOperation.nPages"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintOperation.html#g:attr:nPages"
        })
#endif

-- VVV Prop "n-pages-to-print"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable]
   -- Nullable: (Just False,Nothing)

-- | Get the value of the “@n-pages-to-print@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' printOperation #nPagesToPrint
-- @
getPrintOperationNPagesToPrint :: (MonadIO m, IsPrintOperation o) => o -> m Int32
getPrintOperationNPagesToPrint obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "n-pages-to-print"

#if defined(ENABLE_OVERLOADING)
data PrintOperationNPagesToPrintPropertyInfo
instance AttrInfo PrintOperationNPagesToPrintPropertyInfo where
    type AttrAllowedOps PrintOperationNPagesToPrintPropertyInfo = '[ 'AttrGet]
    type AttrBaseTypeConstraint PrintOperationNPagesToPrintPropertyInfo = IsPrintOperation
    type AttrSetTypeConstraint PrintOperationNPagesToPrintPropertyInfo = (~) ()
    type AttrTransferTypeConstraint PrintOperationNPagesToPrintPropertyInfo = (~) ()
    type AttrTransferType PrintOperationNPagesToPrintPropertyInfo = ()
    type AttrGetType PrintOperationNPagesToPrintPropertyInfo = Int32
    type AttrLabel PrintOperationNPagesToPrintPropertyInfo = "n-pages-to-print"
    type AttrOrigin PrintOperationNPagesToPrintPropertyInfo = PrintOperation
    attrGet = getPrintOperationNPagesToPrint
    attrSet = undefined
    attrTransfer _ = undefined
    attrConstruct = undefined
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintOperation.nPagesToPrint"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintOperation.html#g:attr:nPagesToPrint"
        })
#endif

-- VVV Prop "print-settings"
   -- Type: TInterface (Name {namespace = "Gtk", name = "PrintSettings"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just True)

-- | Get the value of the “@print-settings@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' printOperation #printSettings
-- @
getPrintOperationPrintSettings :: (MonadIO m, IsPrintOperation o) => o -> m Gtk.PrintSettings.PrintSettings
getPrintOperationPrintSettings obj = MIO.liftIO $ checkUnexpectedNothing "getPrintOperationPrintSettings" $ B.Properties.getObjectPropertyObject obj "print-settings" Gtk.PrintSettings.PrintSettings

-- | Set the value of the “@print-settings@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' printOperation [ #printSettings 'Data.GI.Base.Attributes.:=' value ]
-- @
setPrintOperationPrintSettings :: (MonadIO m, IsPrintOperation o, Gtk.PrintSettings.IsPrintSettings a) => o -> a -> m ()
setPrintOperationPrintSettings obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyObject obj "print-settings" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@print-settings@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructPrintOperationPrintSettings :: (IsPrintOperation o, MIO.MonadIO m, Gtk.PrintSettings.IsPrintSettings a) => a -> m (GValueConstruct o)
constructPrintOperationPrintSettings val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyObject "print-settings" (P.Just val)

-- | Set the value of the “@print-settings@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #printSettings
-- @
clearPrintOperationPrintSettings :: (MonadIO m, IsPrintOperation o) => o -> m ()
clearPrintOperationPrintSettings obj = liftIO $ B.Properties.setObjectPropertyObject obj "print-settings" (Nothing :: Maybe Gtk.PrintSettings.PrintSettings)

#if defined(ENABLE_OVERLOADING)
data PrintOperationPrintSettingsPropertyInfo
instance AttrInfo PrintOperationPrintSettingsPropertyInfo where
    type AttrAllowedOps PrintOperationPrintSettingsPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint PrintOperationPrintSettingsPropertyInfo = IsPrintOperation
    type AttrSetTypeConstraint PrintOperationPrintSettingsPropertyInfo = Gtk.PrintSettings.IsPrintSettings
    type AttrTransferTypeConstraint PrintOperationPrintSettingsPropertyInfo = Gtk.PrintSettings.IsPrintSettings
    type AttrTransferType PrintOperationPrintSettingsPropertyInfo = Gtk.PrintSettings.PrintSettings
    type AttrGetType PrintOperationPrintSettingsPropertyInfo = Gtk.PrintSettings.PrintSettings
    type AttrLabel PrintOperationPrintSettingsPropertyInfo = "print-settings"
    type AttrOrigin PrintOperationPrintSettingsPropertyInfo = PrintOperation
    attrGet = getPrintOperationPrintSettings
    attrSet = setPrintOperationPrintSettings
    attrTransfer _ v = do
        unsafeCastTo Gtk.PrintSettings.PrintSettings v
    attrConstruct = constructPrintOperationPrintSettings
    attrClear = clearPrintOperationPrintSettings
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintOperation.printSettings"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintOperation.html#g:attr:printSettings"
        })
#endif

-- VVV Prop "show-progress"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Just False)

-- | Get the value of the “@show-progress@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' printOperation #showProgress
-- @
getPrintOperationShowProgress :: (MonadIO m, IsPrintOperation o) => o -> m Bool
getPrintOperationShowProgress obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "show-progress"

-- | Set the value of the “@show-progress@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' printOperation [ #showProgress 'Data.GI.Base.Attributes.:=' value ]
-- @
setPrintOperationShowProgress :: (MonadIO m, IsPrintOperation o) => o -> Bool -> m ()
setPrintOperationShowProgress obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "show-progress" val

-- | Construct a `GValueConstruct` with valid value for the “@show-progress@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructPrintOperationShowProgress :: (IsPrintOperation o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructPrintOperationShowProgress val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "show-progress" val

#if defined(ENABLE_OVERLOADING)
data PrintOperationShowProgressPropertyInfo
instance AttrInfo PrintOperationShowProgressPropertyInfo where
    type AttrAllowedOps PrintOperationShowProgressPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint PrintOperationShowProgressPropertyInfo = IsPrintOperation
    type AttrSetTypeConstraint PrintOperationShowProgressPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint PrintOperationShowProgressPropertyInfo = (~) Bool
    type AttrTransferType PrintOperationShowProgressPropertyInfo = Bool
    type AttrGetType PrintOperationShowProgressPropertyInfo = Bool
    type AttrLabel PrintOperationShowProgressPropertyInfo = "show-progress"
    type AttrOrigin PrintOperationShowProgressPropertyInfo = PrintOperation
    attrGet = getPrintOperationShowProgress
    attrSet = setPrintOperationShowProgress
    attrTransfer _ v = do
        return v
    attrConstruct = constructPrintOperationShowProgress
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintOperation.showProgress"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintOperation.html#g:attr:showProgress"
        })
#endif

-- VVV Prop "status"
   -- Type: TInterface (Name {namespace = "Gtk", name = "PrintStatus"})
   -- Flags: [PropertyReadable]
   -- Nullable: (Just False,Nothing)

-- | Get the value of the “@status@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' printOperation #status
-- @
getPrintOperationStatus :: (MonadIO m, IsPrintOperation o) => o -> m Gtk.Enums.PrintStatus
getPrintOperationStatus obj = MIO.liftIO $ B.Properties.getObjectPropertyEnum obj "status"

#if defined(ENABLE_OVERLOADING)
data PrintOperationStatusPropertyInfo
instance AttrInfo PrintOperationStatusPropertyInfo where
    type AttrAllowedOps PrintOperationStatusPropertyInfo = '[ 'AttrGet]
    type AttrBaseTypeConstraint PrintOperationStatusPropertyInfo = IsPrintOperation
    type AttrSetTypeConstraint PrintOperationStatusPropertyInfo = (~) ()
    type AttrTransferTypeConstraint PrintOperationStatusPropertyInfo = (~) ()
    type AttrTransferType PrintOperationStatusPropertyInfo = ()
    type AttrGetType PrintOperationStatusPropertyInfo = Gtk.Enums.PrintStatus
    type AttrLabel PrintOperationStatusPropertyInfo = "status"
    type AttrOrigin PrintOperationStatusPropertyInfo = PrintOperation
    attrGet = getPrintOperationStatus
    attrSet = undefined
    attrTransfer _ = undefined
    attrConstruct = undefined
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintOperation.status"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintOperation.html#g:attr:status"
        })
#endif

-- VVV Prop "status-string"
   -- Type: TBasicType TUTF8
   -- Flags: [PropertyReadable]
   -- Nullable: (Just False,Nothing)

-- | Get the value of the “@status-string@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' printOperation #statusString
-- @
getPrintOperationStatusString :: (MonadIO m, IsPrintOperation o) => o -> m T.Text
getPrintOperationStatusString obj = MIO.liftIO $ checkUnexpectedNothing "getPrintOperationStatusString" $ B.Properties.getObjectPropertyString obj "status-string"

#if defined(ENABLE_OVERLOADING)
data PrintOperationStatusStringPropertyInfo
instance AttrInfo PrintOperationStatusStringPropertyInfo where
    type AttrAllowedOps PrintOperationStatusStringPropertyInfo = '[ 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint PrintOperationStatusStringPropertyInfo = IsPrintOperation
    type AttrSetTypeConstraint PrintOperationStatusStringPropertyInfo = (~) ()
    type AttrTransferTypeConstraint PrintOperationStatusStringPropertyInfo = (~) ()
    type AttrTransferType PrintOperationStatusStringPropertyInfo = ()
    type AttrGetType PrintOperationStatusStringPropertyInfo = T.Text
    type AttrLabel PrintOperationStatusStringPropertyInfo = "status-string"
    type AttrOrigin PrintOperationStatusStringPropertyInfo = PrintOperation
    attrGet = getPrintOperationStatusString
    attrSet = undefined
    attrTransfer _ = undefined
    attrConstruct = undefined
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintOperation.statusString"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintOperation.html#g:attr:statusString"
        })
#endif

-- VVV Prop "support-selection"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@support-selection@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' printOperation #supportSelection
-- @
getPrintOperationSupportSelection :: (MonadIO m, IsPrintOperation o) => o -> m Bool
getPrintOperationSupportSelection obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "support-selection"

-- | Set the value of the “@support-selection@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' printOperation [ #supportSelection 'Data.GI.Base.Attributes.:=' value ]
-- @
setPrintOperationSupportSelection :: (MonadIO m, IsPrintOperation o) => o -> Bool -> m ()
setPrintOperationSupportSelection obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "support-selection" val

-- | Construct a `GValueConstruct` with valid value for the “@support-selection@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructPrintOperationSupportSelection :: (IsPrintOperation o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructPrintOperationSupportSelection val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "support-selection" val

#if defined(ENABLE_OVERLOADING)
data PrintOperationSupportSelectionPropertyInfo
instance AttrInfo PrintOperationSupportSelectionPropertyInfo where
    type AttrAllowedOps PrintOperationSupportSelectionPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint PrintOperationSupportSelectionPropertyInfo = IsPrintOperation
    type AttrSetTypeConstraint PrintOperationSupportSelectionPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint PrintOperationSupportSelectionPropertyInfo = (~) Bool
    type AttrTransferType PrintOperationSupportSelectionPropertyInfo = Bool
    type AttrGetType PrintOperationSupportSelectionPropertyInfo = Bool
    type AttrLabel PrintOperationSupportSelectionPropertyInfo = "support-selection"
    type AttrOrigin PrintOperationSupportSelectionPropertyInfo = PrintOperation
    attrGet = getPrintOperationSupportSelection
    attrSet = setPrintOperationSupportSelection
    attrTransfer _ v = do
        return v
    attrConstruct = constructPrintOperationSupportSelection
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintOperation.supportSelection"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintOperation.html#g:attr:supportSelection"
        })
#endif

-- VVV Prop "track-print-status"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Just False)

-- | Get the value of the “@track-print-status@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' printOperation #trackPrintStatus
-- @
getPrintOperationTrackPrintStatus :: (MonadIO m, IsPrintOperation o) => o -> m Bool
getPrintOperationTrackPrintStatus obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "track-print-status"

-- | Set the value of the “@track-print-status@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' printOperation [ #trackPrintStatus 'Data.GI.Base.Attributes.:=' value ]
-- @
setPrintOperationTrackPrintStatus :: (MonadIO m, IsPrintOperation o) => o -> Bool -> m ()
setPrintOperationTrackPrintStatus obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "track-print-status" val

-- | Construct a `GValueConstruct` with valid value for the “@track-print-status@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructPrintOperationTrackPrintStatus :: (IsPrintOperation o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructPrintOperationTrackPrintStatus val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "track-print-status" val

#if defined(ENABLE_OVERLOADING)
data PrintOperationTrackPrintStatusPropertyInfo
instance AttrInfo PrintOperationTrackPrintStatusPropertyInfo where
    type AttrAllowedOps PrintOperationTrackPrintStatusPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint PrintOperationTrackPrintStatusPropertyInfo = IsPrintOperation
    type AttrSetTypeConstraint PrintOperationTrackPrintStatusPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint PrintOperationTrackPrintStatusPropertyInfo = (~) Bool
    type AttrTransferType PrintOperationTrackPrintStatusPropertyInfo = Bool
    type AttrGetType PrintOperationTrackPrintStatusPropertyInfo = Bool
    type AttrLabel PrintOperationTrackPrintStatusPropertyInfo = "track-print-status"
    type AttrOrigin PrintOperationTrackPrintStatusPropertyInfo = PrintOperation
    attrGet = getPrintOperationTrackPrintStatus
    attrSet = setPrintOperationTrackPrintStatus
    attrTransfer _ v = do
        return v
    attrConstruct = constructPrintOperationTrackPrintStatus
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintOperation.trackPrintStatus"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintOperation.html#g:attr:trackPrintStatus"
        })
#endif

-- VVV Prop "unit"
   -- Type: TInterface (Name {namespace = "Gtk", name = "Unit"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Just False)

-- | Get the value of the “@unit@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' printOperation #unit
-- @
getPrintOperationUnit :: (MonadIO m, IsPrintOperation o) => o -> m Gtk.Enums.Unit
getPrintOperationUnit obj = MIO.liftIO $ B.Properties.getObjectPropertyEnum obj "unit"

-- | Set the value of the “@unit@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' printOperation [ #unit 'Data.GI.Base.Attributes.:=' value ]
-- @
setPrintOperationUnit :: (MonadIO m, IsPrintOperation o) => o -> Gtk.Enums.Unit -> m ()
setPrintOperationUnit obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyEnum obj "unit" val

-- | Construct a `GValueConstruct` with valid value for the “@unit@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructPrintOperationUnit :: (IsPrintOperation o, MIO.MonadIO m) => Gtk.Enums.Unit -> m (GValueConstruct o)
constructPrintOperationUnit val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyEnum "unit" val

#if defined(ENABLE_OVERLOADING)
data PrintOperationUnitPropertyInfo
instance AttrInfo PrintOperationUnitPropertyInfo where
    type AttrAllowedOps PrintOperationUnitPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint PrintOperationUnitPropertyInfo = IsPrintOperation
    type AttrSetTypeConstraint PrintOperationUnitPropertyInfo = (~) Gtk.Enums.Unit
    type AttrTransferTypeConstraint PrintOperationUnitPropertyInfo = (~) Gtk.Enums.Unit
    type AttrTransferType PrintOperationUnitPropertyInfo = Gtk.Enums.Unit
    type AttrGetType PrintOperationUnitPropertyInfo = Gtk.Enums.Unit
    type AttrLabel PrintOperationUnitPropertyInfo = "unit"
    type AttrOrigin PrintOperationUnitPropertyInfo = PrintOperation
    attrGet = getPrintOperationUnit
    attrSet = setPrintOperationUnit
    attrTransfer _ v = do
        return v
    attrConstruct = constructPrintOperationUnit
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintOperation.unit"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintOperation.html#g:attr:unit"
        })
#endif

-- VVV Prop "use-full-page"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Just False)

-- | Get the value of the “@use-full-page@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' printOperation #useFullPage
-- @
getPrintOperationUseFullPage :: (MonadIO m, IsPrintOperation o) => o -> m Bool
getPrintOperationUseFullPage obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "use-full-page"

-- | Set the value of the “@use-full-page@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' printOperation [ #useFullPage 'Data.GI.Base.Attributes.:=' value ]
-- @
setPrintOperationUseFullPage :: (MonadIO m, IsPrintOperation o) => o -> Bool -> m ()
setPrintOperationUseFullPage obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "use-full-page" val

-- | Construct a `GValueConstruct` with valid value for the “@use-full-page@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructPrintOperationUseFullPage :: (IsPrintOperation o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructPrintOperationUseFullPage val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "use-full-page" val

#if defined(ENABLE_OVERLOADING)
data PrintOperationUseFullPagePropertyInfo
instance AttrInfo PrintOperationUseFullPagePropertyInfo where
    type AttrAllowedOps PrintOperationUseFullPagePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint PrintOperationUseFullPagePropertyInfo = IsPrintOperation
    type AttrSetTypeConstraint PrintOperationUseFullPagePropertyInfo = (~) Bool
    type AttrTransferTypeConstraint PrintOperationUseFullPagePropertyInfo = (~) Bool
    type AttrTransferType PrintOperationUseFullPagePropertyInfo = Bool
    type AttrGetType PrintOperationUseFullPagePropertyInfo = Bool
    type AttrLabel PrintOperationUseFullPagePropertyInfo = "use-full-page"
    type AttrOrigin PrintOperationUseFullPagePropertyInfo = PrintOperation
    attrGet = getPrintOperationUseFullPage
    attrSet = setPrintOperationUseFullPage
    attrTransfer _ v = do
        return v
    attrConstruct = constructPrintOperationUseFullPage
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintOperation.useFullPage"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintOperation.html#g:attr:useFullPage"
        })
#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList PrintOperation
type instance O.AttributeList PrintOperation = PrintOperationAttributeList
type PrintOperationAttributeList = ('[ '("allowAsync", PrintOperationAllowAsyncPropertyInfo), '("currentPage", PrintOperationCurrentPagePropertyInfo), '("customTabLabel", PrintOperationCustomTabLabelPropertyInfo), '("defaultPageSetup", PrintOperationDefaultPageSetupPropertyInfo), '("embedPageSetup", PrintOperationEmbedPageSetupPropertyInfo), '("exportFilename", PrintOperationExportFilenamePropertyInfo), '("hasSelection", PrintOperationHasSelectionPropertyInfo), '("jobName", PrintOperationJobNamePropertyInfo), '("nPages", PrintOperationNPagesPropertyInfo), '("nPagesToPrint", PrintOperationNPagesToPrintPropertyInfo), '("printSettings", PrintOperationPrintSettingsPropertyInfo), '("showProgress", PrintOperationShowProgressPropertyInfo), '("status", PrintOperationStatusPropertyInfo), '("statusString", PrintOperationStatusStringPropertyInfo), '("supportSelection", PrintOperationSupportSelectionPropertyInfo), '("trackPrintStatus", PrintOperationTrackPrintStatusPropertyInfo), '("unit", PrintOperationUnitPropertyInfo), '("useFullPage", PrintOperationUseFullPagePropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
printOperationAllowAsync :: AttrLabelProxy "allowAsync"
printOperationAllowAsync = AttrLabelProxy

printOperationCurrentPage :: AttrLabelProxy "currentPage"
printOperationCurrentPage = AttrLabelProxy

printOperationCustomTabLabel :: AttrLabelProxy "customTabLabel"
printOperationCustomTabLabel = AttrLabelProxy

printOperationDefaultPageSetup :: AttrLabelProxy "defaultPageSetup"
printOperationDefaultPageSetup = AttrLabelProxy

printOperationEmbedPageSetup :: AttrLabelProxy "embedPageSetup"
printOperationEmbedPageSetup = AttrLabelProxy

printOperationExportFilename :: AttrLabelProxy "exportFilename"
printOperationExportFilename = AttrLabelProxy

printOperationHasSelection :: AttrLabelProxy "hasSelection"
printOperationHasSelection = AttrLabelProxy

printOperationJobName :: AttrLabelProxy "jobName"
printOperationJobName = AttrLabelProxy

printOperationNPages :: AttrLabelProxy "nPages"
printOperationNPages = AttrLabelProxy

printOperationNPagesToPrint :: AttrLabelProxy "nPagesToPrint"
printOperationNPagesToPrint = AttrLabelProxy

printOperationPrintSettings :: AttrLabelProxy "printSettings"
printOperationPrintSettings = AttrLabelProxy

printOperationShowProgress :: AttrLabelProxy "showProgress"
printOperationShowProgress = AttrLabelProxy

printOperationStatus :: AttrLabelProxy "status"
printOperationStatus = AttrLabelProxy

printOperationStatusString :: AttrLabelProxy "statusString"
printOperationStatusString = AttrLabelProxy

printOperationSupportSelection :: AttrLabelProxy "supportSelection"
printOperationSupportSelection = AttrLabelProxy

printOperationTrackPrintStatus :: AttrLabelProxy "trackPrintStatus"
printOperationTrackPrintStatus = AttrLabelProxy

printOperationUnit :: AttrLabelProxy "unit"
printOperationUnit = AttrLabelProxy

printOperationUseFullPage :: AttrLabelProxy "useFullPage"
printOperationUseFullPage = AttrLabelProxy

#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList PrintOperation = PrintOperationSignalList
type PrintOperationSignalList = ('[ '("beginPrint", PrintOperationBeginPrintSignalInfo), '("createCustomWidget", PrintOperationCreateCustomWidgetSignalInfo), '("customWidgetApply", PrintOperationCustomWidgetApplySignalInfo), '("done", PrintOperationDoneSignalInfo), '("drawPage", PrintOperationDrawPageSignalInfo), '("endPrint", PrintOperationEndPrintSignalInfo), '("gotPageSize", Gtk.PrintOperationPreview.PrintOperationPreviewGotPageSizeSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo), '("paginate", PrintOperationPaginateSignalInfo), '("preview", PrintOperationPreviewSignalInfo), '("ready", Gtk.PrintOperationPreview.PrintOperationPreviewReadySignalInfo), '("requestPageSetup", PrintOperationRequestPageSetupSignalInfo), '("statusChanged", PrintOperationStatusChangedSignalInfo), '("updateCustomWidget", PrintOperationUpdateCustomWidgetSignalInfo)] :: [(Symbol, *)])

#endif

-- method PrintOperation::new
-- method type : Constructor
-- Args: []
-- Lengths: []
-- returnType: Just
--               (TInterface Name { namespace = "Gtk" , name = "PrintOperation" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_print_operation_new" gtk_print_operation_new :: 
    IO (Ptr PrintOperation)

-- | Creates a new t'GI.Gtk.Objects.PrintOperation.PrintOperation'.
-- 
-- /Since: 2.10/
printOperationNew ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m PrintOperation
    -- ^ __Returns:__ a new t'GI.Gtk.Objects.PrintOperation.PrintOperation'
printOperationNew  = liftIO $ do
    result <- gtk_print_operation_new
    checkUnexpectedReturnNULL "printOperationNew" result
    result' <- (wrapObject PrintOperation) result
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method PrintOperation::cancel
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "op"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintOperation" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintOperation"
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

foreign import ccall "gtk_print_operation_cancel" gtk_print_operation_cancel :: 
    Ptr PrintOperation ->                   -- op : TInterface (Name {namespace = "Gtk", name = "PrintOperation"})
    IO ()

-- | Cancels a running print operation. This function may
-- be called from a [PrintOperation::beginPrint]("GI.Gtk.Objects.PrintOperation#g:signal:beginPrint"),
-- [PrintOperation::paginate]("GI.Gtk.Objects.PrintOperation#g:signal:paginate") or [PrintOperation::drawPage]("GI.Gtk.Objects.PrintOperation#g:signal:drawPage")
-- signal handler to stop the currently running print
-- operation.
-- 
-- /Since: 2.10/
printOperationCancel ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintOperation a) =>
    a
    -- ^ /@op@/: a t'GI.Gtk.Objects.PrintOperation.PrintOperation'
    -> m ()
printOperationCancel op = liftIO $ do
    op' <- unsafeManagedPtrCastPtr op
    gtk_print_operation_cancel op'
    touchManagedPtr op
    return ()

#if defined(ENABLE_OVERLOADING)
data PrintOperationCancelMethodInfo
instance (signature ~ (m ()), MonadIO m, IsPrintOperation a) => O.OverloadedMethod PrintOperationCancelMethodInfo a signature where
    overloadedMethod = printOperationCancel

instance O.OverloadedMethodInfo PrintOperationCancelMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintOperation.printOperationCancel",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintOperation.html#v:printOperationCancel"
        })


#endif

-- method PrintOperation::draw_page_finish
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "op"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintOperation" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintOperation"
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

foreign import ccall "gtk_print_operation_draw_page_finish" gtk_print_operation_draw_page_finish :: 
    Ptr PrintOperation ->                   -- op : TInterface (Name {namespace = "Gtk", name = "PrintOperation"})
    IO ()

-- | Signalize that drawing of particular page is complete.
-- 
-- It is called after completion of page drawing (e.g. drawing in another
-- thread).
-- If 'GI.Gtk.Objects.PrintOperation.printOperationSetDeferDrawing' was called before, then this function
-- has to be called by application. In another case it is called by the library
-- itself.
-- 
-- /Since: 2.16/
printOperationDrawPageFinish ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintOperation a) =>
    a
    -- ^ /@op@/: a t'GI.Gtk.Objects.PrintOperation.PrintOperation'
    -> m ()
printOperationDrawPageFinish op = liftIO $ do
    op' <- unsafeManagedPtrCastPtr op
    gtk_print_operation_draw_page_finish op'
    touchManagedPtr op
    return ()

#if defined(ENABLE_OVERLOADING)
data PrintOperationDrawPageFinishMethodInfo
instance (signature ~ (m ()), MonadIO m, IsPrintOperation a) => O.OverloadedMethod PrintOperationDrawPageFinishMethodInfo a signature where
    overloadedMethod = printOperationDrawPageFinish

instance O.OverloadedMethodInfo PrintOperationDrawPageFinishMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintOperation.printOperationDrawPageFinish",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintOperation.html#v:printOperationDrawPageFinish"
        })


#endif

-- method PrintOperation::get_default_page_setup
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "op"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintOperation" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintOperation"
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

foreign import ccall "gtk_print_operation_get_default_page_setup" gtk_print_operation_get_default_page_setup :: 
    Ptr PrintOperation ->                   -- op : TInterface (Name {namespace = "Gtk", name = "PrintOperation"})
    IO (Ptr Gtk.PageSetup.PageSetup)

-- | Returns the default page setup, see
-- 'GI.Gtk.Objects.PrintOperation.printOperationSetDefaultPageSetup'.
-- 
-- /Since: 2.10/
printOperationGetDefaultPageSetup ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintOperation a) =>
    a
    -- ^ /@op@/: a t'GI.Gtk.Objects.PrintOperation.PrintOperation'
    -> m Gtk.PageSetup.PageSetup
    -- ^ __Returns:__ the default page setup
printOperationGetDefaultPageSetup op = liftIO $ do
    op' <- unsafeManagedPtrCastPtr op
    result <- gtk_print_operation_get_default_page_setup op'
    checkUnexpectedReturnNULL "printOperationGetDefaultPageSetup" result
    result' <- (newObject Gtk.PageSetup.PageSetup) result
    touchManagedPtr op
    return result'

#if defined(ENABLE_OVERLOADING)
data PrintOperationGetDefaultPageSetupMethodInfo
instance (signature ~ (m Gtk.PageSetup.PageSetup), MonadIO m, IsPrintOperation a) => O.OverloadedMethod PrintOperationGetDefaultPageSetupMethodInfo a signature where
    overloadedMethod = printOperationGetDefaultPageSetup

instance O.OverloadedMethodInfo PrintOperationGetDefaultPageSetupMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintOperation.printOperationGetDefaultPageSetup",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintOperation.html#v:printOperationGetDefaultPageSetup"
        })


#endif

-- method PrintOperation::get_embed_page_setup
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "op"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintOperation" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintOperation"
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

foreign import ccall "gtk_print_operation_get_embed_page_setup" gtk_print_operation_get_embed_page_setup :: 
    Ptr PrintOperation ->                   -- op : TInterface (Name {namespace = "Gtk", name = "PrintOperation"})
    IO CInt

-- | Gets the value of [PrintOperation:embedPageSetup]("GI.Gtk.Objects.PrintOperation#g:attr:embedPageSetup") property.
-- 
-- /Since: 2.18/
printOperationGetEmbedPageSetup ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintOperation a) =>
    a
    -- ^ /@op@/: a t'GI.Gtk.Objects.PrintOperation.PrintOperation'
    -> m Bool
    -- ^ __Returns:__ whether page setup selection combos are embedded
printOperationGetEmbedPageSetup op = liftIO $ do
    op' <- unsafeManagedPtrCastPtr op
    result <- gtk_print_operation_get_embed_page_setup op'
    let result' = (/= 0) result
    touchManagedPtr op
    return result'

#if defined(ENABLE_OVERLOADING)
data PrintOperationGetEmbedPageSetupMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsPrintOperation a) => O.OverloadedMethod PrintOperationGetEmbedPageSetupMethodInfo a signature where
    overloadedMethod = printOperationGetEmbedPageSetup

instance O.OverloadedMethodInfo PrintOperationGetEmbedPageSetupMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintOperation.printOperationGetEmbedPageSetup",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintOperation.html#v:printOperationGetEmbedPageSetup"
        })


#endif

-- method PrintOperation::get_error
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "op"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintOperation" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintOperation"
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
-- throws : True
-- Skip return : False

foreign import ccall "gtk_print_operation_get_error" gtk_print_operation_get_error :: 
    Ptr PrintOperation ->                   -- op : TInterface (Name {namespace = "Gtk", name = "PrintOperation"})
    Ptr (Ptr GError) ->                     -- error
    IO ()

-- | Call this when the result of a print operation is
-- 'GI.Gtk.Enums.PrintOperationResultError', either as returned by
-- 'GI.Gtk.Objects.PrintOperation.printOperationRun', or in the [PrintOperation::done]("GI.Gtk.Objects.PrintOperation#g:signal:done") signal
-- handler. The returned t'GError' will contain more details on what went wrong.
-- 
-- /Since: 2.10/
printOperationGetError ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintOperation a) =>
    a
    -- ^ /@op@/: a t'GI.Gtk.Objects.PrintOperation.PrintOperation'
    -> m ()
    -- ^ /(Can throw 'Data.GI.Base.GError.GError')/
printOperationGetError op = liftIO $ do
    op' <- unsafeManagedPtrCastPtr op
    onException (do
        propagateGError $ gtk_print_operation_get_error op'
        touchManagedPtr op
        return ()
     ) (do
        return ()
     )

#if defined(ENABLE_OVERLOADING)
data PrintOperationGetErrorMethodInfo
instance (signature ~ (m ()), MonadIO m, IsPrintOperation a) => O.OverloadedMethod PrintOperationGetErrorMethodInfo a signature where
    overloadedMethod = printOperationGetError

instance O.OverloadedMethodInfo PrintOperationGetErrorMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintOperation.printOperationGetError",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintOperation.html#v:printOperationGetError"
        })


#endif

-- method PrintOperation::get_has_selection
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "op"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintOperation" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintOperation"
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

foreign import ccall "gtk_print_operation_get_has_selection" gtk_print_operation_get_has_selection :: 
    Ptr PrintOperation ->                   -- op : TInterface (Name {namespace = "Gtk", name = "PrintOperation"})
    IO CInt

-- | Gets the value of [PrintOperation:hasSelection]("GI.Gtk.Objects.PrintOperation#g:attr:hasSelection") property.
-- 
-- /Since: 2.18/
printOperationGetHasSelection ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintOperation a) =>
    a
    -- ^ /@op@/: a t'GI.Gtk.Objects.PrintOperation.PrintOperation'
    -> m Bool
    -- ^ __Returns:__ whether there is a selection
printOperationGetHasSelection op = liftIO $ do
    op' <- unsafeManagedPtrCastPtr op
    result <- gtk_print_operation_get_has_selection op'
    let result' = (/= 0) result
    touchManagedPtr op
    return result'

#if defined(ENABLE_OVERLOADING)
data PrintOperationGetHasSelectionMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsPrintOperation a) => O.OverloadedMethod PrintOperationGetHasSelectionMethodInfo a signature where
    overloadedMethod = printOperationGetHasSelection

instance O.OverloadedMethodInfo PrintOperationGetHasSelectionMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintOperation.printOperationGetHasSelection",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintOperation.html#v:printOperationGetHasSelection"
        })


#endif

-- method PrintOperation::get_n_pages_to_print
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "op"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintOperation" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintOperation"
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

foreign import ccall "gtk_print_operation_get_n_pages_to_print" gtk_print_operation_get_n_pages_to_print :: 
    Ptr PrintOperation ->                   -- op : TInterface (Name {namespace = "Gtk", name = "PrintOperation"})
    IO Int32

-- | Returns the number of pages that will be printed.
-- 
-- Note that this value is set during print preparation phase
-- ('GI.Gtk.Enums.PrintStatusPreparing'), so this function should never be
-- called before the data generation phase ('GI.Gtk.Enums.PrintStatusGeneratingData').
-- You can connect to the [PrintOperation::statusChanged]("GI.Gtk.Objects.PrintOperation#g:signal:statusChanged") signal
-- and call 'GI.Gtk.Objects.PrintOperation.printOperationGetNPagesToPrint' when
-- print status is 'GI.Gtk.Enums.PrintStatusGeneratingData'.
-- This is typically used to track the progress of print operation.
-- 
-- /Since: 2.18/
printOperationGetNPagesToPrint ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintOperation a) =>
    a
    -- ^ /@op@/: a t'GI.Gtk.Objects.PrintOperation.PrintOperation'
    -> m Int32
    -- ^ __Returns:__ the number of pages that will be printed
printOperationGetNPagesToPrint op = liftIO $ do
    op' <- unsafeManagedPtrCastPtr op
    result <- gtk_print_operation_get_n_pages_to_print op'
    touchManagedPtr op
    return result

#if defined(ENABLE_OVERLOADING)
data PrintOperationGetNPagesToPrintMethodInfo
instance (signature ~ (m Int32), MonadIO m, IsPrintOperation a) => O.OverloadedMethod PrintOperationGetNPagesToPrintMethodInfo a signature where
    overloadedMethod = printOperationGetNPagesToPrint

instance O.OverloadedMethodInfo PrintOperationGetNPagesToPrintMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintOperation.printOperationGetNPagesToPrint",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintOperation.html#v:printOperationGetNPagesToPrint"
        })


#endif

-- method PrintOperation::get_print_settings
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "op"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintOperation" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintOperation"
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

foreign import ccall "gtk_print_operation_get_print_settings" gtk_print_operation_get_print_settings :: 
    Ptr PrintOperation ->                   -- op : TInterface (Name {namespace = "Gtk", name = "PrintOperation"})
    IO (Ptr Gtk.PrintSettings.PrintSettings)

-- | Returns the current print settings.
-- 
-- Note that the return value is 'P.Nothing' until either
-- 'GI.Gtk.Objects.PrintOperation.printOperationSetPrintSettings' or
-- 'GI.Gtk.Objects.PrintOperation.printOperationRun' have been called.
-- 
-- /Since: 2.10/
printOperationGetPrintSettings ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintOperation a) =>
    a
    -- ^ /@op@/: a t'GI.Gtk.Objects.PrintOperation.PrintOperation'
    -> m Gtk.PrintSettings.PrintSettings
    -- ^ __Returns:__ the current print settings of /@op@/.
printOperationGetPrintSettings op = liftIO $ do
    op' <- unsafeManagedPtrCastPtr op
    result <- gtk_print_operation_get_print_settings op'
    checkUnexpectedReturnNULL "printOperationGetPrintSettings" result
    result' <- (newObject Gtk.PrintSettings.PrintSettings) result
    touchManagedPtr op
    return result'

#if defined(ENABLE_OVERLOADING)
data PrintOperationGetPrintSettingsMethodInfo
instance (signature ~ (m Gtk.PrintSettings.PrintSettings), MonadIO m, IsPrintOperation a) => O.OverloadedMethod PrintOperationGetPrintSettingsMethodInfo a signature where
    overloadedMethod = printOperationGetPrintSettings

instance O.OverloadedMethodInfo PrintOperationGetPrintSettingsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintOperation.printOperationGetPrintSettings",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintOperation.html#v:printOperationGetPrintSettings"
        })


#endif

-- method PrintOperation::get_status
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "op"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintOperation" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintOperation"
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "PrintStatus" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_print_operation_get_status" gtk_print_operation_get_status :: 
    Ptr PrintOperation ->                   -- op : TInterface (Name {namespace = "Gtk", name = "PrintOperation"})
    IO CUInt

-- | Returns the status of the print operation.
-- Also see 'GI.Gtk.Objects.PrintOperation.printOperationGetStatusString'.
-- 
-- /Since: 2.10/
printOperationGetStatus ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintOperation a) =>
    a
    -- ^ /@op@/: a t'GI.Gtk.Objects.PrintOperation.PrintOperation'
    -> m Gtk.Enums.PrintStatus
    -- ^ __Returns:__ the status of the print operation
printOperationGetStatus op = liftIO $ do
    op' <- unsafeManagedPtrCastPtr op
    result <- gtk_print_operation_get_status op'
    let result' = (toEnum . fromIntegral) result
    touchManagedPtr op
    return result'

#if defined(ENABLE_OVERLOADING)
data PrintOperationGetStatusMethodInfo
instance (signature ~ (m Gtk.Enums.PrintStatus), MonadIO m, IsPrintOperation a) => O.OverloadedMethod PrintOperationGetStatusMethodInfo a signature where
    overloadedMethod = printOperationGetStatus

instance O.OverloadedMethodInfo PrintOperationGetStatusMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintOperation.printOperationGetStatus",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintOperation.html#v:printOperationGetStatus"
        })


#endif

-- method PrintOperation::get_status_string
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "op"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintOperation" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintOperation"
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

foreign import ccall "gtk_print_operation_get_status_string" gtk_print_operation_get_status_string :: 
    Ptr PrintOperation ->                   -- op : TInterface (Name {namespace = "Gtk", name = "PrintOperation"})
    IO CString

-- | Returns a string representation of the status of the
-- print operation. The string is translated and suitable
-- for displaying the print status e.g. in a t'GI.Gtk.Objects.Statusbar.Statusbar'.
-- 
-- Use 'GI.Gtk.Objects.PrintOperation.printOperationGetStatus' to obtain a status
-- value that is suitable for programmatic use.
-- 
-- /Since: 2.10/
printOperationGetStatusString ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintOperation a) =>
    a
    -- ^ /@op@/: a t'GI.Gtk.Objects.PrintOperation.PrintOperation'
    -> m T.Text
    -- ^ __Returns:__ a string representation of the status
    --    of the print operation
printOperationGetStatusString op = liftIO $ do
    op' <- unsafeManagedPtrCastPtr op
    result <- gtk_print_operation_get_status_string op'
    checkUnexpectedReturnNULL "printOperationGetStatusString" result
    result' <- cstringToText result
    touchManagedPtr op
    return result'

#if defined(ENABLE_OVERLOADING)
data PrintOperationGetStatusStringMethodInfo
instance (signature ~ (m T.Text), MonadIO m, IsPrintOperation a) => O.OverloadedMethod PrintOperationGetStatusStringMethodInfo a signature where
    overloadedMethod = printOperationGetStatusString

instance O.OverloadedMethodInfo PrintOperationGetStatusStringMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintOperation.printOperationGetStatusString",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintOperation.html#v:printOperationGetStatusString"
        })


#endif

-- method PrintOperation::get_support_selection
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "op"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintOperation" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintOperation"
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

foreign import ccall "gtk_print_operation_get_support_selection" gtk_print_operation_get_support_selection :: 
    Ptr PrintOperation ->                   -- op : TInterface (Name {namespace = "Gtk", name = "PrintOperation"})
    IO CInt

-- | Gets the value of [PrintOperation:supportSelection]("GI.Gtk.Objects.PrintOperation#g:attr:supportSelection") property.
-- 
-- /Since: 2.18/
printOperationGetSupportSelection ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintOperation a) =>
    a
    -- ^ /@op@/: a t'GI.Gtk.Objects.PrintOperation.PrintOperation'
    -> m Bool
    -- ^ __Returns:__ whether the application supports print of selection
printOperationGetSupportSelection op = liftIO $ do
    op' <- unsafeManagedPtrCastPtr op
    result <- gtk_print_operation_get_support_selection op'
    let result' = (/= 0) result
    touchManagedPtr op
    return result'

#if defined(ENABLE_OVERLOADING)
data PrintOperationGetSupportSelectionMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsPrintOperation a) => O.OverloadedMethod PrintOperationGetSupportSelectionMethodInfo a signature where
    overloadedMethod = printOperationGetSupportSelection

instance O.OverloadedMethodInfo PrintOperationGetSupportSelectionMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintOperation.printOperationGetSupportSelection",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintOperation.html#v:printOperationGetSupportSelection"
        })


#endif

-- method PrintOperation::is_finished
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "op"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintOperation" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintOperation"
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

foreign import ccall "gtk_print_operation_is_finished" gtk_print_operation_is_finished :: 
    Ptr PrintOperation ->                   -- op : TInterface (Name {namespace = "Gtk", name = "PrintOperation"})
    IO CInt

-- | A convenience function to find out if the print operation
-- is finished, either successfully ('GI.Gtk.Enums.PrintStatusFinished')
-- or unsuccessfully ('GI.Gtk.Enums.PrintStatusFinishedAborted').
-- 
-- Note: when you enable print status tracking the print operation
-- can be in a non-finished state even after done has been called, as
-- the operation status then tracks the print job status on the printer.
-- 
-- /Since: 2.10/
printOperationIsFinished ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintOperation a) =>
    a
    -- ^ /@op@/: a t'GI.Gtk.Objects.PrintOperation.PrintOperation'
    -> m Bool
    -- ^ __Returns:__ 'P.True', if the print operation is finished.
printOperationIsFinished op = liftIO $ do
    op' <- unsafeManagedPtrCastPtr op
    result <- gtk_print_operation_is_finished op'
    let result' = (/= 0) result
    touchManagedPtr op
    return result'

#if defined(ENABLE_OVERLOADING)
data PrintOperationIsFinishedMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsPrintOperation a) => O.OverloadedMethod PrintOperationIsFinishedMethodInfo a signature where
    overloadedMethod = printOperationIsFinished

instance O.OverloadedMethodInfo PrintOperationIsFinishedMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintOperation.printOperationIsFinished",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintOperation.html#v:printOperationIsFinished"
        })


#endif

-- method PrintOperation::run
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "op"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintOperation" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintOperation"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "action"
--           , argType =
--               TInterface
--                 Name { namespace = "Gtk" , name = "PrintOperationAction" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the action to start"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "parent"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Transient parent of the dialog"
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
--               (TInterface
--                  Name { namespace = "Gtk" , name = "PrintOperationResult" })
-- throws : True
-- Skip return : False

foreign import ccall "gtk_print_operation_run" gtk_print_operation_run :: 
    Ptr PrintOperation ->                   -- op : TInterface (Name {namespace = "Gtk", name = "PrintOperation"})
    CUInt ->                                -- action : TInterface (Name {namespace = "Gtk", name = "PrintOperationAction"})
    Ptr Gtk.Window.Window ->                -- parent : TInterface (Name {namespace = "Gtk", name = "Window"})
    Ptr (Ptr GError) ->                     -- error
    IO CUInt

-- | Runs the print operation, by first letting the user modify
-- print settings in the print dialog, and then print the document.
-- 
-- Normally that this function does not return until the rendering of all
-- pages is complete. You can connect to the
-- [PrintOperation::statusChanged]("GI.Gtk.Objects.PrintOperation#g:signal:statusChanged") signal on /@op@/ to obtain some
-- information about the progress of the print operation.
-- Furthermore, it may use a recursive mainloop to show the print dialog.
-- 
-- If you call 'GI.Gtk.Objects.PrintOperation.printOperationSetAllowAsync' or set the
-- [PrintOperation:allowAsync]("GI.Gtk.Objects.PrintOperation#g:attr:allowAsync") property the operation will run
-- asynchronously if this is supported on the platform. The
-- [PrintOperation::done]("GI.Gtk.Objects.PrintOperation#g:signal:done") signal will be emitted with the result of the
-- operation when the it is done (i.e. when the dialog is canceled, or when
-- the print succeeds or fails).
-- 
-- === /C code/
-- >
-- >if (settings != NULL)
-- >  gtk_print_operation_set_print_settings (print, settings);
-- >  
-- >if (page_setup != NULL)
-- >  gtk_print_operation_set_default_page_setup (print, page_setup);
-- >  
-- >g_signal_connect (print, "begin-print",
-- >                  G_CALLBACK (begin_print), &data);
-- >g_signal_connect (print, "draw-page",
-- >                  G_CALLBACK (draw_page), &data);
-- > 
-- >res = gtk_print_operation_run (print,
-- >                               GTK_PRINT_OPERATION_ACTION_PRINT_DIALOG,
-- >                               parent,
-- >                               &error);
-- > 
-- >if (res == GTK_PRINT_OPERATION_RESULT_ERROR)
-- > {
-- >   error_dialog = gtk_message_dialog_new (GTK_WINDOW (parent),
-- >  			                     GTK_DIALOG_DESTROY_WITH_PARENT,
-- >					     GTK_MESSAGE_ERROR,
-- >					     GTK_BUTTONS_CLOSE,
-- >					     "Error printing file:\n%s",
-- >					     error->message);
-- >   g_signal_connect (error_dialog, "response",
-- >                     G_CALLBACK (gtk_widget_destroy), NULL);
-- >   gtk_widget_show (error_dialog);
-- >   g_error_free (error);
-- > }
-- >else if (res == GTK_PRINT_OPERATION_RESULT_APPLY)
-- > {
-- >   if (settings != NULL)
-- >g_object_unref (settings);
-- >   settings = g_object_ref (gtk_print_operation_get_print_settings (print));
-- > }
-- 
-- 
-- Note that 'GI.Gtk.Objects.PrintOperation.printOperationRun' can only be called once on a
-- given t'GI.Gtk.Objects.PrintOperation.PrintOperation'.
-- 
-- /Since: 2.10/
printOperationRun ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintOperation a, Gtk.Window.IsWindow b) =>
    a
    -- ^ /@op@/: a t'GI.Gtk.Objects.PrintOperation.PrintOperation'
    -> Gtk.Enums.PrintOperationAction
    -- ^ /@action@/: the action to start
    -> Maybe (b)
    -- ^ /@parent@/: Transient parent of the dialog
    -> m Gtk.Enums.PrintOperationResult
    -- ^ __Returns:__ the result of the print operation. A return value of
    --   'GI.Gtk.Enums.PrintOperationResultApply' indicates that the printing was
    --   completed successfully. In this case, it is a good idea to obtain
    --   the used print settings with 'GI.Gtk.Objects.PrintOperation.printOperationGetPrintSettings'
    --   and store them for reuse with the next print operation. A value of
    --   'GI.Gtk.Enums.PrintOperationResultInProgress' means the operation is running
    --   asynchronously, and will emit the [PrintOperation::done]("GI.Gtk.Objects.PrintOperation#g:signal:done") signal when
    --   done. /(Can throw 'Data.GI.Base.GError.GError')/
printOperationRun op action parent = liftIO $ do
    op' <- unsafeManagedPtrCastPtr op
    let action' = (fromIntegral . fromEnum) action
    maybeParent <- case parent of
        Nothing -> return nullPtr
        Just jParent -> do
            jParent' <- unsafeManagedPtrCastPtr jParent
            return jParent'
    onException (do
        result <- propagateGError $ gtk_print_operation_run op' action' maybeParent
        let result' = (toEnum . fromIntegral) result
        touchManagedPtr op
        whenJust parent touchManagedPtr
        return result'
     ) (do
        return ()
     )

#if defined(ENABLE_OVERLOADING)
data PrintOperationRunMethodInfo
instance (signature ~ (Gtk.Enums.PrintOperationAction -> Maybe (b) -> m Gtk.Enums.PrintOperationResult), MonadIO m, IsPrintOperation a, Gtk.Window.IsWindow b) => O.OverloadedMethod PrintOperationRunMethodInfo a signature where
    overloadedMethod = printOperationRun

instance O.OverloadedMethodInfo PrintOperationRunMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintOperation.printOperationRun",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintOperation.html#v:printOperationRun"
        })


#endif

-- method PrintOperation::set_allow_async
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "op"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintOperation" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintOperation"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "allow_async"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "%TRUE to allow asynchronous operation"
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

foreign import ccall "gtk_print_operation_set_allow_async" gtk_print_operation_set_allow_async :: 
    Ptr PrintOperation ->                   -- op : TInterface (Name {namespace = "Gtk", name = "PrintOperation"})
    CInt ->                                 -- allow_async : TBasicType TBoolean
    IO ()

-- | Sets whether the 'GI.Gtk.Objects.PrintOperation.printOperationRun' may return
-- before the print operation is completed. Note that
-- some platforms may not allow asynchronous operation.
-- 
-- /Since: 2.10/
printOperationSetAllowAsync ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintOperation a) =>
    a
    -- ^ /@op@/: a t'GI.Gtk.Objects.PrintOperation.PrintOperation'
    -> Bool
    -- ^ /@allowAsync@/: 'P.True' to allow asynchronous operation
    -> m ()
printOperationSetAllowAsync op allowAsync = liftIO $ do
    op' <- unsafeManagedPtrCastPtr op
    let allowAsync' = (fromIntegral . fromEnum) allowAsync
    gtk_print_operation_set_allow_async op' allowAsync'
    touchManagedPtr op
    return ()

#if defined(ENABLE_OVERLOADING)
data PrintOperationSetAllowAsyncMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsPrintOperation a) => O.OverloadedMethod PrintOperationSetAllowAsyncMethodInfo a signature where
    overloadedMethod = printOperationSetAllowAsync

instance O.OverloadedMethodInfo PrintOperationSetAllowAsyncMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintOperation.printOperationSetAllowAsync",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintOperation.html#v:printOperationSetAllowAsync"
        })


#endif

-- method PrintOperation::set_current_page
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "op"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintOperation" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintOperation"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "current_page"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the current page, 0-based"
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

foreign import ccall "gtk_print_operation_set_current_page" gtk_print_operation_set_current_page :: 
    Ptr PrintOperation ->                   -- op : TInterface (Name {namespace = "Gtk", name = "PrintOperation"})
    Int32 ->                                -- current_page : TBasicType TInt
    IO ()

-- | Sets the current page.
-- 
-- If this is called before 'GI.Gtk.Objects.PrintOperation.printOperationRun',
-- the user will be able to select to print only the current page.
-- 
-- Note that this only makes sense for pre-paginated documents.
-- 
-- /Since: 2.10/
printOperationSetCurrentPage ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintOperation a) =>
    a
    -- ^ /@op@/: a t'GI.Gtk.Objects.PrintOperation.PrintOperation'
    -> Int32
    -- ^ /@currentPage@/: the current page, 0-based
    -> m ()
printOperationSetCurrentPage op currentPage = liftIO $ do
    op' <- unsafeManagedPtrCastPtr op
    gtk_print_operation_set_current_page op' currentPage
    touchManagedPtr op
    return ()

#if defined(ENABLE_OVERLOADING)
data PrintOperationSetCurrentPageMethodInfo
instance (signature ~ (Int32 -> m ()), MonadIO m, IsPrintOperation a) => O.OverloadedMethod PrintOperationSetCurrentPageMethodInfo a signature where
    overloadedMethod = printOperationSetCurrentPage

instance O.OverloadedMethodInfo PrintOperationSetCurrentPageMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintOperation.printOperationSetCurrentPage",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintOperation.html#v:printOperationSetCurrentPage"
        })


#endif

-- method PrintOperation::set_custom_tab_label
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "op"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintOperation" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintOperation"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "label"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "the label to use, or %NULL to use the default label"
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

foreign import ccall "gtk_print_operation_set_custom_tab_label" gtk_print_operation_set_custom_tab_label :: 
    Ptr PrintOperation ->                   -- op : TInterface (Name {namespace = "Gtk", name = "PrintOperation"})
    CString ->                              -- label : TBasicType TUTF8
    IO ()

-- | Sets the label for the tab holding custom widgets.
-- 
-- /Since: 2.10/
printOperationSetCustomTabLabel ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintOperation a) =>
    a
    -- ^ /@op@/: a t'GI.Gtk.Objects.PrintOperation.PrintOperation'
    -> Maybe (T.Text)
    -- ^ /@label@/: the label to use, or 'P.Nothing' to use the default label
    -> m ()
printOperationSetCustomTabLabel op label = liftIO $ do
    op' <- unsafeManagedPtrCastPtr op
    maybeLabel <- case label of
        Nothing -> return nullPtr
        Just jLabel -> do
            jLabel' <- textToCString jLabel
            return jLabel'
    gtk_print_operation_set_custom_tab_label op' maybeLabel
    touchManagedPtr op
    freeMem maybeLabel
    return ()

#if defined(ENABLE_OVERLOADING)
data PrintOperationSetCustomTabLabelMethodInfo
instance (signature ~ (Maybe (T.Text) -> m ()), MonadIO m, IsPrintOperation a) => O.OverloadedMethod PrintOperationSetCustomTabLabelMethodInfo a signature where
    overloadedMethod = printOperationSetCustomTabLabel

instance O.OverloadedMethodInfo PrintOperationSetCustomTabLabelMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintOperation.printOperationSetCustomTabLabel",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintOperation.html#v:printOperationSetCustomTabLabel"
        })


#endif

-- method PrintOperation::set_default_page_setup
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "op"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintOperation" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintOperation"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "default_page_setup"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PageSetup" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPageSetup, or %NULL"
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

foreign import ccall "gtk_print_operation_set_default_page_setup" gtk_print_operation_set_default_page_setup :: 
    Ptr PrintOperation ->                   -- op : TInterface (Name {namespace = "Gtk", name = "PrintOperation"})
    Ptr Gtk.PageSetup.PageSetup ->          -- default_page_setup : TInterface (Name {namespace = "Gtk", name = "PageSetup"})
    IO ()

-- | Makes /@defaultPageSetup@/ the default page setup for /@op@/.
-- 
-- This page setup will be used by 'GI.Gtk.Objects.PrintOperation.printOperationRun',
-- but it can be overridden on a per-page basis by connecting
-- to the [PrintOperation::requestPageSetup]("GI.Gtk.Objects.PrintOperation#g:signal:requestPageSetup") signal.
-- 
-- /Since: 2.10/
printOperationSetDefaultPageSetup ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintOperation a, Gtk.PageSetup.IsPageSetup b) =>
    a
    -- ^ /@op@/: a t'GI.Gtk.Objects.PrintOperation.PrintOperation'
    -> Maybe (b)
    -- ^ /@defaultPageSetup@/: a t'GI.Gtk.Objects.PageSetup.PageSetup', or 'P.Nothing'
    -> m ()
printOperationSetDefaultPageSetup op defaultPageSetup = liftIO $ do
    op' <- unsafeManagedPtrCastPtr op
    maybeDefaultPageSetup <- case defaultPageSetup of
        Nothing -> return nullPtr
        Just jDefaultPageSetup -> do
            jDefaultPageSetup' <- unsafeManagedPtrCastPtr jDefaultPageSetup
            return jDefaultPageSetup'
    gtk_print_operation_set_default_page_setup op' maybeDefaultPageSetup
    touchManagedPtr op
    whenJust defaultPageSetup touchManagedPtr
    return ()

#if defined(ENABLE_OVERLOADING)
data PrintOperationSetDefaultPageSetupMethodInfo
instance (signature ~ (Maybe (b) -> m ()), MonadIO m, IsPrintOperation a, Gtk.PageSetup.IsPageSetup b) => O.OverloadedMethod PrintOperationSetDefaultPageSetupMethodInfo a signature where
    overloadedMethod = printOperationSetDefaultPageSetup

instance O.OverloadedMethodInfo PrintOperationSetDefaultPageSetupMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintOperation.printOperationSetDefaultPageSetup",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintOperation.html#v:printOperationSetDefaultPageSetup"
        })


#endif

-- method PrintOperation::set_defer_drawing
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "op"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintOperation" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintOperation"
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

foreign import ccall "gtk_print_operation_set_defer_drawing" gtk_print_operation_set_defer_drawing :: 
    Ptr PrintOperation ->                   -- op : TInterface (Name {namespace = "Gtk", name = "PrintOperation"})
    IO ()

-- | Sets up the t'GI.Gtk.Objects.PrintOperation.PrintOperation' to wait for calling of
-- 'GI.Gtk.Objects.PrintOperation.printOperationDrawPageFinish' from application. It can
-- be used for drawing page in another thread.
-- 
-- This function must be called in the callback of “draw-page” signal.
-- 
-- /Since: 2.16/
printOperationSetDeferDrawing ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintOperation a) =>
    a
    -- ^ /@op@/: a t'GI.Gtk.Objects.PrintOperation.PrintOperation'
    -> m ()
printOperationSetDeferDrawing op = liftIO $ do
    op' <- unsafeManagedPtrCastPtr op
    gtk_print_operation_set_defer_drawing op'
    touchManagedPtr op
    return ()

#if defined(ENABLE_OVERLOADING)
data PrintOperationSetDeferDrawingMethodInfo
instance (signature ~ (m ()), MonadIO m, IsPrintOperation a) => O.OverloadedMethod PrintOperationSetDeferDrawingMethodInfo a signature where
    overloadedMethod = printOperationSetDeferDrawing

instance O.OverloadedMethodInfo PrintOperationSetDeferDrawingMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintOperation.printOperationSetDeferDrawing",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintOperation.html#v:printOperationSetDeferDrawing"
        })


#endif

-- method PrintOperation::set_embed_page_setup
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "op"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintOperation" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintOperation"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "embed"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "%TRUE to embed page setup selection in the #GtkPrintUnixDialog"
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

foreign import ccall "gtk_print_operation_set_embed_page_setup" gtk_print_operation_set_embed_page_setup :: 
    Ptr PrintOperation ->                   -- op : TInterface (Name {namespace = "Gtk", name = "PrintOperation"})
    CInt ->                                 -- embed : TBasicType TBoolean
    IO ()

-- | Embed page size combo box and orientation combo box into page setup page.
-- Selected page setup is stored as default page setup in t'GI.Gtk.Objects.PrintOperation.PrintOperation'.
-- 
-- /Since: 2.18/
printOperationSetEmbedPageSetup ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintOperation a) =>
    a
    -- ^ /@op@/: a t'GI.Gtk.Objects.PrintOperation.PrintOperation'
    -> Bool
    -- ^ /@embed@/: 'P.True' to embed page setup selection in the @/GtkPrintUnixDialog/@
    -> m ()
printOperationSetEmbedPageSetup op embed = liftIO $ do
    op' <- unsafeManagedPtrCastPtr op
    let embed' = (fromIntegral . fromEnum) embed
    gtk_print_operation_set_embed_page_setup op' embed'
    touchManagedPtr op
    return ()

#if defined(ENABLE_OVERLOADING)
data PrintOperationSetEmbedPageSetupMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsPrintOperation a) => O.OverloadedMethod PrintOperationSetEmbedPageSetupMethodInfo a signature where
    overloadedMethod = printOperationSetEmbedPageSetup

instance O.OverloadedMethodInfo PrintOperationSetEmbedPageSetupMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintOperation.printOperationSetEmbedPageSetup",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintOperation.html#v:printOperationSetEmbedPageSetup"
        })


#endif

-- method PrintOperation::set_export_filename
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "op"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintOperation" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintOperation"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "filename"
--           , argType = TBasicType TFileName
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the filename for the exported file"
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

foreign import ccall "gtk_print_operation_set_export_filename" gtk_print_operation_set_export_filename :: 
    Ptr PrintOperation ->                   -- op : TInterface (Name {namespace = "Gtk", name = "PrintOperation"})
    CString ->                              -- filename : TBasicType TFileName
    IO ()

-- | Sets up the t'GI.Gtk.Objects.PrintOperation.PrintOperation' to generate a file instead
-- of showing the print dialog. The indended use of this function
-- is for implementing “Export to PDF” actions. Currently, PDF
-- is the only supported format.
-- 
-- “Print to PDF” support is independent of this and is done
-- by letting the user pick the “Print to PDF” item from the list
-- of printers in the print dialog.
-- 
-- /Since: 2.10/
printOperationSetExportFilename ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintOperation a) =>
    a
    -- ^ /@op@/: a t'GI.Gtk.Objects.PrintOperation.PrintOperation'
    -> [Char]
    -- ^ /@filename@/: the filename for the exported file
    -> m ()
printOperationSetExportFilename op filename = liftIO $ do
    op' <- unsafeManagedPtrCastPtr op
    filename' <- stringToCString filename
    gtk_print_operation_set_export_filename op' filename'
    touchManagedPtr op
    freeMem filename'
    return ()

#if defined(ENABLE_OVERLOADING)
data PrintOperationSetExportFilenameMethodInfo
instance (signature ~ ([Char] -> m ()), MonadIO m, IsPrintOperation a) => O.OverloadedMethod PrintOperationSetExportFilenameMethodInfo a signature where
    overloadedMethod = printOperationSetExportFilename

instance O.OverloadedMethodInfo PrintOperationSetExportFilenameMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintOperation.printOperationSetExportFilename",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintOperation.html#v:printOperationSetExportFilename"
        })


#endif

-- method PrintOperation::set_has_selection
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "op"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintOperation" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintOperation"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "has_selection"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "%TRUE indicates that a selection exists"
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

foreign import ccall "gtk_print_operation_set_has_selection" gtk_print_operation_set_has_selection :: 
    Ptr PrintOperation ->                   -- op : TInterface (Name {namespace = "Gtk", name = "PrintOperation"})
    CInt ->                                 -- has_selection : TBasicType TBoolean
    IO ()

-- | Sets whether there is a selection to print.
-- 
-- Application has to set number of pages to which the selection
-- will draw by 'GI.Gtk.Objects.PrintOperation.printOperationSetNPages' in a callback of
-- [PrintOperation::beginPrint]("GI.Gtk.Objects.PrintOperation#g:signal:beginPrint").
-- 
-- /Since: 2.18/
printOperationSetHasSelection ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintOperation a) =>
    a
    -- ^ /@op@/: a t'GI.Gtk.Objects.PrintOperation.PrintOperation'
    -> Bool
    -- ^ /@hasSelection@/: 'P.True' indicates that a selection exists
    -> m ()
printOperationSetHasSelection op hasSelection = liftIO $ do
    op' <- unsafeManagedPtrCastPtr op
    let hasSelection' = (fromIntegral . fromEnum) hasSelection
    gtk_print_operation_set_has_selection op' hasSelection'
    touchManagedPtr op
    return ()

#if defined(ENABLE_OVERLOADING)
data PrintOperationSetHasSelectionMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsPrintOperation a) => O.OverloadedMethod PrintOperationSetHasSelectionMethodInfo a signature where
    overloadedMethod = printOperationSetHasSelection

instance O.OverloadedMethodInfo PrintOperationSetHasSelectionMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintOperation.printOperationSetHasSelection",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintOperation.html#v:printOperationSetHasSelection"
        })


#endif

-- method PrintOperation::set_job_name
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "op"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintOperation" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintOperation"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "job_name"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a string that identifies the print job"
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

foreign import ccall "gtk_print_operation_set_job_name" gtk_print_operation_set_job_name :: 
    Ptr PrintOperation ->                   -- op : TInterface (Name {namespace = "Gtk", name = "PrintOperation"})
    CString ->                              -- job_name : TBasicType TUTF8
    IO ()

-- | Sets the name of the print job. The name is used to identify
-- the job (e.g. in monitoring applications like eggcups).
-- 
-- If you don’t set a job name, GTK+ picks a default one by
-- numbering successive print jobs.
-- 
-- /Since: 2.10/
printOperationSetJobName ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintOperation a) =>
    a
    -- ^ /@op@/: a t'GI.Gtk.Objects.PrintOperation.PrintOperation'
    -> T.Text
    -- ^ /@jobName@/: a string that identifies the print job
    -> m ()
printOperationSetJobName op jobName = liftIO $ do
    op' <- unsafeManagedPtrCastPtr op
    jobName' <- textToCString jobName
    gtk_print_operation_set_job_name op' jobName'
    touchManagedPtr op
    freeMem jobName'
    return ()

#if defined(ENABLE_OVERLOADING)
data PrintOperationSetJobNameMethodInfo
instance (signature ~ (T.Text -> m ()), MonadIO m, IsPrintOperation a) => O.OverloadedMethod PrintOperationSetJobNameMethodInfo a signature where
    overloadedMethod = printOperationSetJobName

instance O.OverloadedMethodInfo PrintOperationSetJobNameMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintOperation.printOperationSetJobName",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintOperation.html#v:printOperationSetJobName"
        })


#endif

-- method PrintOperation::set_n_pages
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "op"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintOperation" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintOperation"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "n_pages"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the number of pages"
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

foreign import ccall "gtk_print_operation_set_n_pages" gtk_print_operation_set_n_pages :: 
    Ptr PrintOperation ->                   -- op : TInterface (Name {namespace = "Gtk", name = "PrintOperation"})
    Int32 ->                                -- n_pages : TBasicType TInt
    IO ()

-- | Sets the number of pages in the document.
-- 
-- This must be set to a positive number
-- before the rendering starts. It may be set in a
-- [PrintOperation::beginPrint]("GI.Gtk.Objects.PrintOperation#g:signal:beginPrint") signal hander.
-- 
-- Note that the page numbers passed to the
-- [PrintOperation::requestPageSetup]("GI.Gtk.Objects.PrintOperation#g:signal:requestPageSetup")
-- and [PrintOperation::drawPage]("GI.Gtk.Objects.PrintOperation#g:signal:drawPage") signals are 0-based, i.e. if
-- the user chooses to print all pages, the last [drawPage](#g:signal:drawPage) signal
-- will be for page /@nPages@/ - 1.
-- 
-- /Since: 2.10/
printOperationSetNPages ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintOperation a) =>
    a
    -- ^ /@op@/: a t'GI.Gtk.Objects.PrintOperation.PrintOperation'
    -> Int32
    -- ^ /@nPages@/: the number of pages
    -> m ()
printOperationSetNPages op nPages = liftIO $ do
    op' <- unsafeManagedPtrCastPtr op
    gtk_print_operation_set_n_pages op' nPages
    touchManagedPtr op
    return ()

#if defined(ENABLE_OVERLOADING)
data PrintOperationSetNPagesMethodInfo
instance (signature ~ (Int32 -> m ()), MonadIO m, IsPrintOperation a) => O.OverloadedMethod PrintOperationSetNPagesMethodInfo a signature where
    overloadedMethod = printOperationSetNPages

instance O.OverloadedMethodInfo PrintOperationSetNPagesMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintOperation.printOperationSetNPages",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintOperation.html#v:printOperationSetNPages"
        })


#endif

-- method PrintOperation::set_print_settings
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "op"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintOperation" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintOperation"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "print_settings"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintSettings" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "#GtkPrintSettings" , sinceVersion = Nothing }
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

foreign import ccall "gtk_print_operation_set_print_settings" gtk_print_operation_set_print_settings :: 
    Ptr PrintOperation ->                   -- op : TInterface (Name {namespace = "Gtk", name = "PrintOperation"})
    Ptr Gtk.PrintSettings.PrintSettings ->  -- print_settings : TInterface (Name {namespace = "Gtk", name = "PrintSettings"})
    IO ()

-- | Sets the print settings for /@op@/. This is typically used to
-- re-establish print settings from a previous print operation,
-- see 'GI.Gtk.Objects.PrintOperation.printOperationRun'.
-- 
-- /Since: 2.10/
printOperationSetPrintSettings ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintOperation a, Gtk.PrintSettings.IsPrintSettings b) =>
    a
    -- ^ /@op@/: a t'GI.Gtk.Objects.PrintOperation.PrintOperation'
    -> Maybe (b)
    -- ^ /@printSettings@/: t'GI.Gtk.Objects.PrintSettings.PrintSettings'
    -> m ()
printOperationSetPrintSettings op printSettings = liftIO $ do
    op' <- unsafeManagedPtrCastPtr op
    maybePrintSettings <- case printSettings of
        Nothing -> return nullPtr
        Just jPrintSettings -> do
            jPrintSettings' <- unsafeManagedPtrCastPtr jPrintSettings
            return jPrintSettings'
    gtk_print_operation_set_print_settings op' maybePrintSettings
    touchManagedPtr op
    whenJust printSettings touchManagedPtr
    return ()

#if defined(ENABLE_OVERLOADING)
data PrintOperationSetPrintSettingsMethodInfo
instance (signature ~ (Maybe (b) -> m ()), MonadIO m, IsPrintOperation a, Gtk.PrintSettings.IsPrintSettings b) => O.OverloadedMethod PrintOperationSetPrintSettingsMethodInfo a signature where
    overloadedMethod = printOperationSetPrintSettings

instance O.OverloadedMethodInfo PrintOperationSetPrintSettingsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintOperation.printOperationSetPrintSettings",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintOperation.html#v:printOperationSetPrintSettings"
        })


#endif

-- method PrintOperation::set_show_progress
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "op"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintOperation" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintOperation"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "show_progress"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "%TRUE to show a progress dialog"
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

foreign import ccall "gtk_print_operation_set_show_progress" gtk_print_operation_set_show_progress :: 
    Ptr PrintOperation ->                   -- op : TInterface (Name {namespace = "Gtk", name = "PrintOperation"})
    CInt ->                                 -- show_progress : TBasicType TBoolean
    IO ()

-- | If /@showProgress@/ is 'P.True', the print operation will show a
-- progress dialog during the print operation.
-- 
-- /Since: 2.10/
printOperationSetShowProgress ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintOperation a) =>
    a
    -- ^ /@op@/: a t'GI.Gtk.Objects.PrintOperation.PrintOperation'
    -> Bool
    -- ^ /@showProgress@/: 'P.True' to show a progress dialog
    -> m ()
printOperationSetShowProgress op showProgress = liftIO $ do
    op' <- unsafeManagedPtrCastPtr op
    let showProgress' = (fromIntegral . fromEnum) showProgress
    gtk_print_operation_set_show_progress op' showProgress'
    touchManagedPtr op
    return ()

#if defined(ENABLE_OVERLOADING)
data PrintOperationSetShowProgressMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsPrintOperation a) => O.OverloadedMethod PrintOperationSetShowProgressMethodInfo a signature where
    overloadedMethod = printOperationSetShowProgress

instance O.OverloadedMethodInfo PrintOperationSetShowProgressMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintOperation.printOperationSetShowProgress",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintOperation.html#v:printOperationSetShowProgress"
        })


#endif

-- method PrintOperation::set_support_selection
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "op"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintOperation" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintOperation"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "support_selection"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "%TRUE to support selection"
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

foreign import ccall "gtk_print_operation_set_support_selection" gtk_print_operation_set_support_selection :: 
    Ptr PrintOperation ->                   -- op : TInterface (Name {namespace = "Gtk", name = "PrintOperation"})
    CInt ->                                 -- support_selection : TBasicType TBoolean
    IO ()

-- | Sets whether selection is supported by t'GI.Gtk.Objects.PrintOperation.PrintOperation'.
-- 
-- /Since: 2.18/
printOperationSetSupportSelection ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintOperation a) =>
    a
    -- ^ /@op@/: a t'GI.Gtk.Objects.PrintOperation.PrintOperation'
    -> Bool
    -- ^ /@supportSelection@/: 'P.True' to support selection
    -> m ()
printOperationSetSupportSelection op supportSelection = liftIO $ do
    op' <- unsafeManagedPtrCastPtr op
    let supportSelection' = (fromIntegral . fromEnum) supportSelection
    gtk_print_operation_set_support_selection op' supportSelection'
    touchManagedPtr op
    return ()

#if defined(ENABLE_OVERLOADING)
data PrintOperationSetSupportSelectionMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsPrintOperation a) => O.OverloadedMethod PrintOperationSetSupportSelectionMethodInfo a signature where
    overloadedMethod = printOperationSetSupportSelection

instance O.OverloadedMethodInfo PrintOperationSetSupportSelectionMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintOperation.printOperationSetSupportSelection",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintOperation.html#v:printOperationSetSupportSelection"
        })


#endif

-- method PrintOperation::set_track_print_status
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "op"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintOperation" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintOperation"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "track_status"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "%TRUE to track status after printing"
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

foreign import ccall "gtk_print_operation_set_track_print_status" gtk_print_operation_set_track_print_status :: 
    Ptr PrintOperation ->                   -- op : TInterface (Name {namespace = "Gtk", name = "PrintOperation"})
    CInt ->                                 -- track_status : TBasicType TBoolean
    IO ()

-- | If track_status is 'P.True', the print operation will try to continue report
-- on the status of the print job in the printer queues and printer. This
-- can allow your application to show things like “out of paper” issues,
-- and when the print job actually reaches the printer.
-- 
-- This function is often implemented using some form of polling, so it should
-- not be enabled unless needed.
-- 
-- /Since: 2.10/
printOperationSetTrackPrintStatus ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintOperation a) =>
    a
    -- ^ /@op@/: a t'GI.Gtk.Objects.PrintOperation.PrintOperation'
    -> Bool
    -- ^ /@trackStatus@/: 'P.True' to track status after printing
    -> m ()
printOperationSetTrackPrintStatus op trackStatus = liftIO $ do
    op' <- unsafeManagedPtrCastPtr op
    let trackStatus' = (fromIntegral . fromEnum) trackStatus
    gtk_print_operation_set_track_print_status op' trackStatus'
    touchManagedPtr op
    return ()

#if defined(ENABLE_OVERLOADING)
data PrintOperationSetTrackPrintStatusMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsPrintOperation a) => O.OverloadedMethod PrintOperationSetTrackPrintStatusMethodInfo a signature where
    overloadedMethod = printOperationSetTrackPrintStatus

instance O.OverloadedMethodInfo PrintOperationSetTrackPrintStatusMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintOperation.printOperationSetTrackPrintStatus",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintOperation.html#v:printOperationSetTrackPrintStatus"
        })


#endif

-- method PrintOperation::set_unit
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "op"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintOperation" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintOperation"
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
--                 { rawDocText = Just "the unit to use" , sinceVersion = Nothing }
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

foreign import ccall "gtk_print_operation_set_unit" gtk_print_operation_set_unit :: 
    Ptr PrintOperation ->                   -- op : TInterface (Name {namespace = "Gtk", name = "PrintOperation"})
    CUInt ->                                -- unit : TInterface (Name {namespace = "Gtk", name = "Unit"})
    IO ()

-- | Sets up the transformation for the cairo context obtained from
-- t'GI.Gtk.Objects.PrintContext.PrintContext' in such a way that distances are measured in
-- units of /@unit@/.
-- 
-- /Since: 2.10/
printOperationSetUnit ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintOperation a) =>
    a
    -- ^ /@op@/: a t'GI.Gtk.Objects.PrintOperation.PrintOperation'
    -> Gtk.Enums.Unit
    -- ^ /@unit@/: the unit to use
    -> m ()
printOperationSetUnit op unit = liftIO $ do
    op' <- unsafeManagedPtrCastPtr op
    let unit' = (fromIntegral . fromEnum) unit
    gtk_print_operation_set_unit op' unit'
    touchManagedPtr op
    return ()

#if defined(ENABLE_OVERLOADING)
data PrintOperationSetUnitMethodInfo
instance (signature ~ (Gtk.Enums.Unit -> m ()), MonadIO m, IsPrintOperation a) => O.OverloadedMethod PrintOperationSetUnitMethodInfo a signature where
    overloadedMethod = printOperationSetUnit

instance O.OverloadedMethodInfo PrintOperationSetUnitMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintOperation.printOperationSetUnit",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintOperation.html#v:printOperationSetUnit"
        })


#endif

-- method PrintOperation::set_use_full_page
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "op"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintOperation" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintOperation"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "full_page"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "%TRUE to set up the #GtkPrintContext for the full page"
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

foreign import ccall "gtk_print_operation_set_use_full_page" gtk_print_operation_set_use_full_page :: 
    Ptr PrintOperation ->                   -- op : TInterface (Name {namespace = "Gtk", name = "PrintOperation"})
    CInt ->                                 -- full_page : TBasicType TBoolean
    IO ()

-- | If /@fullPage@/ is 'P.True', the transformation for the cairo context
-- obtained from t'GI.Gtk.Objects.PrintContext.PrintContext' puts the origin at the top left
-- corner of the page (which may not be the top left corner of the
-- sheet, depending on page orientation and the number of pages per
-- sheet). Otherwise, the origin is at the top left corner of the
-- imageable area (i.e. inside the margins).
-- 
-- /Since: 2.10/
printOperationSetUseFullPage ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintOperation a) =>
    a
    -- ^ /@op@/: a t'GI.Gtk.Objects.PrintOperation.PrintOperation'
    -> Bool
    -- ^ /@fullPage@/: 'P.True' to set up the t'GI.Gtk.Objects.PrintContext.PrintContext' for the full page
    -> m ()
printOperationSetUseFullPage op fullPage = liftIO $ do
    op' <- unsafeManagedPtrCastPtr op
    let fullPage' = (fromIntegral . fromEnum) fullPage
    gtk_print_operation_set_use_full_page op' fullPage'
    touchManagedPtr op
    return ()

#if defined(ENABLE_OVERLOADING)
data PrintOperationSetUseFullPageMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsPrintOperation a) => O.OverloadedMethod PrintOperationSetUseFullPageMethodInfo a signature where
    overloadedMethod = printOperationSetUseFullPage

instance O.OverloadedMethodInfo PrintOperationSetUseFullPageMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintOperation.printOperationSetUseFullPage",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintOperation.html#v:printOperationSetUseFullPage"
        })


#endif


