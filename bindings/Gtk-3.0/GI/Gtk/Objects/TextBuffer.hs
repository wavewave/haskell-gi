{-# LANGUAGE ImplicitParams, RankNTypes, TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- You may wish to begin by reading the
-- [text widget conceptual overview][TextWidget]
-- which gives an overview of all the objects and data
-- types related to the text widget and how they work together.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.TextBuffer
    ( 

-- * Exported types
    TextBuffer(..)                          ,
    IsTextBuffer                            ,
    toTextBuffer                            ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [addMark]("GI.Gtk.Objects.TextBuffer#g:method:addMark"), [addSelectionClipboard]("GI.Gtk.Objects.TextBuffer#g:method:addSelectionClipboard"), [applyTag]("GI.Gtk.Objects.TextBuffer#g:method:applyTag"), [applyTagByName]("GI.Gtk.Objects.TextBuffer#g:method:applyTagByName"), [backspace]("GI.Gtk.Objects.TextBuffer#g:method:backspace"), [beginUserAction]("GI.Gtk.Objects.TextBuffer#g:method:beginUserAction"), [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [copyClipboard]("GI.Gtk.Objects.TextBuffer#g:method:copyClipboard"), [createChildAnchor]("GI.Gtk.Objects.TextBuffer#g:method:createChildAnchor"), [createMark]("GI.Gtk.Objects.TextBuffer#g:method:createMark"), [cutClipboard]("GI.Gtk.Objects.TextBuffer#g:method:cutClipboard"), [delete]("GI.Gtk.Objects.TextBuffer#g:method:delete"), [deleteInteractive]("GI.Gtk.Objects.TextBuffer#g:method:deleteInteractive"), [deleteMark]("GI.Gtk.Objects.TextBuffer#g:method:deleteMark"), [deleteMarkByName]("GI.Gtk.Objects.TextBuffer#g:method:deleteMarkByName"), [deleteSelection]("GI.Gtk.Objects.TextBuffer#g:method:deleteSelection"), [deserialize]("GI.Gtk.Objects.TextBuffer#g:method:deserialize"), [deserializeGetCanCreateTags]("GI.Gtk.Objects.TextBuffer#g:method:deserializeGetCanCreateTags"), [deserializeSetCanCreateTags]("GI.Gtk.Objects.TextBuffer#g:method:deserializeSetCanCreateTags"), [endUserAction]("GI.Gtk.Objects.TextBuffer#g:method:endUserAction"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [insert]("GI.Gtk.Objects.TextBuffer#g:method:insert"), [insertAtCursor]("GI.Gtk.Objects.TextBuffer#g:method:insertAtCursor"), [insertChildAnchor]("GI.Gtk.Objects.TextBuffer#g:method:insertChildAnchor"), [insertInteractive]("GI.Gtk.Objects.TextBuffer#g:method:insertInteractive"), [insertInteractiveAtCursor]("GI.Gtk.Objects.TextBuffer#g:method:insertInteractiveAtCursor"), [insertMarkup]("GI.Gtk.Objects.TextBuffer#g:method:insertMarkup"), [insertPixbuf]("GI.Gtk.Objects.TextBuffer#g:method:insertPixbuf"), [insertRange]("GI.Gtk.Objects.TextBuffer#g:method:insertRange"), [insertRangeInteractive]("GI.Gtk.Objects.TextBuffer#g:method:insertRangeInteractive"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [moveMark]("GI.Gtk.Objects.TextBuffer#g:method:moveMark"), [moveMarkByName]("GI.Gtk.Objects.TextBuffer#g:method:moveMarkByName"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [pasteClipboard]("GI.Gtk.Objects.TextBuffer#g:method:pasteClipboard"), [placeCursor]("GI.Gtk.Objects.TextBuffer#g:method:placeCursor"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [registerDeserializeFormat]("GI.Gtk.Objects.TextBuffer#g:method:registerDeserializeFormat"), [registerDeserializeTagset]("GI.Gtk.Objects.TextBuffer#g:method:registerDeserializeTagset"), [registerSerializeFormat]("GI.Gtk.Objects.TextBuffer#g:method:registerSerializeFormat"), [registerSerializeTagset]("GI.Gtk.Objects.TextBuffer#g:method:registerSerializeTagset"), [removeAllTags]("GI.Gtk.Objects.TextBuffer#g:method:removeAllTags"), [removeSelectionClipboard]("GI.Gtk.Objects.TextBuffer#g:method:removeSelectionClipboard"), [removeTag]("GI.Gtk.Objects.TextBuffer#g:method:removeTag"), [removeTagByName]("GI.Gtk.Objects.TextBuffer#g:method:removeTagByName"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [selectRange]("GI.Gtk.Objects.TextBuffer#g:method:selectRange"), [serialize]("GI.Gtk.Objects.TextBuffer#g:method:serialize"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [unregisterDeserializeFormat]("GI.Gtk.Objects.TextBuffer#g:method:unregisterDeserializeFormat"), [unregisterSerializeFormat]("GI.Gtk.Objects.TextBuffer#g:method:unregisterSerializeFormat"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getBounds]("GI.Gtk.Objects.TextBuffer#g:method:getBounds"), [getCharCount]("GI.Gtk.Objects.TextBuffer#g:method:getCharCount"), [getCopyTargetList]("GI.Gtk.Objects.TextBuffer#g:method:getCopyTargetList"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getDeserializeFormats]("GI.Gtk.Objects.TextBuffer#g:method:getDeserializeFormats"), [getEndIter]("GI.Gtk.Objects.TextBuffer#g:method:getEndIter"), [getHasSelection]("GI.Gtk.Objects.TextBuffer#g:method:getHasSelection"), [getInsert]("GI.Gtk.Objects.TextBuffer#g:method:getInsert"), [getIterAtChildAnchor]("GI.Gtk.Objects.TextBuffer#g:method:getIterAtChildAnchor"), [getIterAtLine]("GI.Gtk.Objects.TextBuffer#g:method:getIterAtLine"), [getIterAtLineIndex]("GI.Gtk.Objects.TextBuffer#g:method:getIterAtLineIndex"), [getIterAtLineOffset]("GI.Gtk.Objects.TextBuffer#g:method:getIterAtLineOffset"), [getIterAtMark]("GI.Gtk.Objects.TextBuffer#g:method:getIterAtMark"), [getIterAtOffset]("GI.Gtk.Objects.TextBuffer#g:method:getIterAtOffset"), [getLineCount]("GI.Gtk.Objects.TextBuffer#g:method:getLineCount"), [getMark]("GI.Gtk.Objects.TextBuffer#g:method:getMark"), [getModified]("GI.Gtk.Objects.TextBuffer#g:method:getModified"), [getPasteTargetList]("GI.Gtk.Objects.TextBuffer#g:method:getPasteTargetList"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getSelectionBound]("GI.Gtk.Objects.TextBuffer#g:method:getSelectionBound"), [getSelectionBounds]("GI.Gtk.Objects.TextBuffer#g:method:getSelectionBounds"), [getSerializeFormats]("GI.Gtk.Objects.TextBuffer#g:method:getSerializeFormats"), [getSlice]("GI.Gtk.Objects.TextBuffer#g:method:getSlice"), [getStartIter]("GI.Gtk.Objects.TextBuffer#g:method:getStartIter"), [getTagTable]("GI.Gtk.Objects.TextBuffer#g:method:getTagTable"), [getText]("GI.Gtk.Objects.TextBuffer#g:method:getText").
-- 
-- ==== Setters
-- [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setModified]("GI.Gtk.Objects.TextBuffer#g:method:setModified"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setText]("GI.Gtk.Objects.TextBuffer#g:method:setText").

#if defined(ENABLE_OVERLOADING)
    ResolveTextBufferMethod                 ,
#endif

-- ** addMark #method:addMark#

#if defined(ENABLE_OVERLOADING)
    TextBufferAddMarkMethodInfo             ,
#endif
    textBufferAddMark                       ,


-- ** addSelectionClipboard #method:addSelectionClipboard#

#if defined(ENABLE_OVERLOADING)
    TextBufferAddSelectionClipboardMethodInfo,
#endif
    textBufferAddSelectionClipboard         ,


-- ** applyTag #method:applyTag#

#if defined(ENABLE_OVERLOADING)
    TextBufferApplyTagMethodInfo            ,
#endif
    textBufferApplyTag                      ,


-- ** applyTagByName #method:applyTagByName#

#if defined(ENABLE_OVERLOADING)
    TextBufferApplyTagByNameMethodInfo      ,
#endif
    textBufferApplyTagByName                ,


-- ** backspace #method:backspace#

#if defined(ENABLE_OVERLOADING)
    TextBufferBackspaceMethodInfo           ,
#endif
    textBufferBackspace                     ,


-- ** beginUserAction #method:beginUserAction#

#if defined(ENABLE_OVERLOADING)
    TextBufferBeginUserActionMethodInfo     ,
#endif
    textBufferBeginUserAction               ,


-- ** copyClipboard #method:copyClipboard#

#if defined(ENABLE_OVERLOADING)
    TextBufferCopyClipboardMethodInfo       ,
#endif
    textBufferCopyClipboard                 ,


-- ** createChildAnchor #method:createChildAnchor#

#if defined(ENABLE_OVERLOADING)
    TextBufferCreateChildAnchorMethodInfo   ,
#endif
    textBufferCreateChildAnchor             ,


-- ** createMark #method:createMark#

#if defined(ENABLE_OVERLOADING)
    TextBufferCreateMarkMethodInfo          ,
#endif
    textBufferCreateMark                    ,


-- ** cutClipboard #method:cutClipboard#

#if defined(ENABLE_OVERLOADING)
    TextBufferCutClipboardMethodInfo        ,
#endif
    textBufferCutClipboard                  ,


-- ** delete #method:delete#

#if defined(ENABLE_OVERLOADING)
    TextBufferDeleteMethodInfo              ,
#endif
    textBufferDelete                        ,


-- ** deleteInteractive #method:deleteInteractive#

#if defined(ENABLE_OVERLOADING)
    TextBufferDeleteInteractiveMethodInfo   ,
#endif
    textBufferDeleteInteractive             ,


-- ** deleteMark #method:deleteMark#

#if defined(ENABLE_OVERLOADING)
    TextBufferDeleteMarkMethodInfo          ,
#endif
    textBufferDeleteMark                    ,


-- ** deleteMarkByName #method:deleteMarkByName#

#if defined(ENABLE_OVERLOADING)
    TextBufferDeleteMarkByNameMethodInfo    ,
#endif
    textBufferDeleteMarkByName              ,


-- ** deleteSelection #method:deleteSelection#

#if defined(ENABLE_OVERLOADING)
    TextBufferDeleteSelectionMethodInfo     ,
#endif
    textBufferDeleteSelection               ,


-- ** deserialize #method:deserialize#

#if defined(ENABLE_OVERLOADING)
    TextBufferDeserializeMethodInfo         ,
#endif
    textBufferDeserialize                   ,


-- ** deserializeGetCanCreateTags #method:deserializeGetCanCreateTags#

#if defined(ENABLE_OVERLOADING)
    TextBufferDeserializeGetCanCreateTagsMethodInfo,
#endif
    textBufferDeserializeGetCanCreateTags   ,


-- ** deserializeSetCanCreateTags #method:deserializeSetCanCreateTags#

#if defined(ENABLE_OVERLOADING)
    TextBufferDeserializeSetCanCreateTagsMethodInfo,
#endif
    textBufferDeserializeSetCanCreateTags   ,


-- ** endUserAction #method:endUserAction#

#if defined(ENABLE_OVERLOADING)
    TextBufferEndUserActionMethodInfo       ,
#endif
    textBufferEndUserAction                 ,


-- ** getBounds #method:getBounds#

#if defined(ENABLE_OVERLOADING)
    TextBufferGetBoundsMethodInfo           ,
#endif
    textBufferGetBounds                     ,


-- ** getCharCount #method:getCharCount#

#if defined(ENABLE_OVERLOADING)
    TextBufferGetCharCountMethodInfo        ,
#endif
    textBufferGetCharCount                  ,


-- ** getCopyTargetList #method:getCopyTargetList#

#if defined(ENABLE_OVERLOADING)
    TextBufferGetCopyTargetListMethodInfo   ,
#endif
    textBufferGetCopyTargetList             ,


-- ** getDeserializeFormats #method:getDeserializeFormats#

#if defined(ENABLE_OVERLOADING)
    TextBufferGetDeserializeFormatsMethodInfo,
#endif
    textBufferGetDeserializeFormats         ,


-- ** getEndIter #method:getEndIter#

#if defined(ENABLE_OVERLOADING)
    TextBufferGetEndIterMethodInfo          ,
#endif
    textBufferGetEndIter                    ,


-- ** getHasSelection #method:getHasSelection#

#if defined(ENABLE_OVERLOADING)
    TextBufferGetHasSelectionMethodInfo     ,
#endif
    textBufferGetHasSelection               ,


-- ** getInsert #method:getInsert#

#if defined(ENABLE_OVERLOADING)
    TextBufferGetInsertMethodInfo           ,
#endif
    textBufferGetInsert                     ,


-- ** getIterAtChildAnchor #method:getIterAtChildAnchor#

#if defined(ENABLE_OVERLOADING)
    TextBufferGetIterAtChildAnchorMethodInfo,
#endif
    textBufferGetIterAtChildAnchor          ,


-- ** getIterAtLine #method:getIterAtLine#

#if defined(ENABLE_OVERLOADING)
    TextBufferGetIterAtLineMethodInfo       ,
#endif
    textBufferGetIterAtLine                 ,


-- ** getIterAtLineIndex #method:getIterAtLineIndex#

#if defined(ENABLE_OVERLOADING)
    TextBufferGetIterAtLineIndexMethodInfo  ,
#endif
    textBufferGetIterAtLineIndex            ,


-- ** getIterAtLineOffset #method:getIterAtLineOffset#

#if defined(ENABLE_OVERLOADING)
    TextBufferGetIterAtLineOffsetMethodInfo ,
#endif
    textBufferGetIterAtLineOffset           ,


-- ** getIterAtMark #method:getIterAtMark#

#if defined(ENABLE_OVERLOADING)
    TextBufferGetIterAtMarkMethodInfo       ,
#endif
    textBufferGetIterAtMark                 ,


-- ** getIterAtOffset #method:getIterAtOffset#

#if defined(ENABLE_OVERLOADING)
    TextBufferGetIterAtOffsetMethodInfo     ,
#endif
    textBufferGetIterAtOffset               ,


-- ** getLineCount #method:getLineCount#

#if defined(ENABLE_OVERLOADING)
    TextBufferGetLineCountMethodInfo        ,
#endif
    textBufferGetLineCount                  ,


-- ** getMark #method:getMark#

#if defined(ENABLE_OVERLOADING)
    TextBufferGetMarkMethodInfo             ,
#endif
    textBufferGetMark                       ,


-- ** getModified #method:getModified#

#if defined(ENABLE_OVERLOADING)
    TextBufferGetModifiedMethodInfo         ,
#endif
    textBufferGetModified                   ,


-- ** getPasteTargetList #method:getPasteTargetList#

#if defined(ENABLE_OVERLOADING)
    TextBufferGetPasteTargetListMethodInfo  ,
#endif
    textBufferGetPasteTargetList            ,


-- ** getSelectionBound #method:getSelectionBound#

#if defined(ENABLE_OVERLOADING)
    TextBufferGetSelectionBoundMethodInfo   ,
#endif
    textBufferGetSelectionBound             ,


-- ** getSelectionBounds #method:getSelectionBounds#

#if defined(ENABLE_OVERLOADING)
    TextBufferGetSelectionBoundsMethodInfo  ,
#endif
    textBufferGetSelectionBounds            ,


-- ** getSerializeFormats #method:getSerializeFormats#

#if defined(ENABLE_OVERLOADING)
    TextBufferGetSerializeFormatsMethodInfo ,
#endif
    textBufferGetSerializeFormats           ,


-- ** getSlice #method:getSlice#

#if defined(ENABLE_OVERLOADING)
    TextBufferGetSliceMethodInfo            ,
#endif
    textBufferGetSlice                      ,


-- ** getStartIter #method:getStartIter#

#if defined(ENABLE_OVERLOADING)
    TextBufferGetStartIterMethodInfo        ,
#endif
    textBufferGetStartIter                  ,


-- ** getTagTable #method:getTagTable#

#if defined(ENABLE_OVERLOADING)
    TextBufferGetTagTableMethodInfo         ,
#endif
    textBufferGetTagTable                   ,


-- ** getText #method:getText#

#if defined(ENABLE_OVERLOADING)
    TextBufferGetTextMethodInfo             ,
#endif
    textBufferGetText                       ,


-- ** insert #method:insert#

#if defined(ENABLE_OVERLOADING)
    TextBufferInsertMethodInfo              ,
#endif
    textBufferInsert                        ,


-- ** insertAtCursor #method:insertAtCursor#

#if defined(ENABLE_OVERLOADING)
    TextBufferInsertAtCursorMethodInfo      ,
#endif
    textBufferInsertAtCursor                ,


-- ** insertChildAnchor #method:insertChildAnchor#

#if defined(ENABLE_OVERLOADING)
    TextBufferInsertChildAnchorMethodInfo   ,
#endif
    textBufferInsertChildAnchor             ,


-- ** insertInteractive #method:insertInteractive#

#if defined(ENABLE_OVERLOADING)
    TextBufferInsertInteractiveMethodInfo   ,
#endif
    textBufferInsertInteractive             ,


-- ** insertInteractiveAtCursor #method:insertInteractiveAtCursor#

#if defined(ENABLE_OVERLOADING)
    TextBufferInsertInteractiveAtCursorMethodInfo,
#endif
    textBufferInsertInteractiveAtCursor     ,


-- ** insertMarkup #method:insertMarkup#

#if defined(ENABLE_OVERLOADING)
    TextBufferInsertMarkupMethodInfo        ,
#endif
    textBufferInsertMarkup                  ,


-- ** insertPixbuf #method:insertPixbuf#

#if defined(ENABLE_OVERLOADING)
    TextBufferInsertPixbufMethodInfo        ,
#endif
    textBufferInsertPixbuf                  ,


-- ** insertRange #method:insertRange#

#if defined(ENABLE_OVERLOADING)
    TextBufferInsertRangeMethodInfo         ,
#endif
    textBufferInsertRange                   ,


-- ** insertRangeInteractive #method:insertRangeInteractive#

#if defined(ENABLE_OVERLOADING)
    TextBufferInsertRangeInteractiveMethodInfo,
#endif
    textBufferInsertRangeInteractive        ,


-- ** moveMark #method:moveMark#

#if defined(ENABLE_OVERLOADING)
    TextBufferMoveMarkMethodInfo            ,
#endif
    textBufferMoveMark                      ,


-- ** moveMarkByName #method:moveMarkByName#

#if defined(ENABLE_OVERLOADING)
    TextBufferMoveMarkByNameMethodInfo      ,
#endif
    textBufferMoveMarkByName                ,


-- ** new #method:new#

    textBufferNew                           ,


-- ** pasteClipboard #method:pasteClipboard#

#if defined(ENABLE_OVERLOADING)
    TextBufferPasteClipboardMethodInfo      ,
#endif
    textBufferPasteClipboard                ,


-- ** placeCursor #method:placeCursor#

#if defined(ENABLE_OVERLOADING)
    TextBufferPlaceCursorMethodInfo         ,
#endif
    textBufferPlaceCursor                   ,


-- ** registerDeserializeFormat #method:registerDeserializeFormat#

#if defined(ENABLE_OVERLOADING)
    TextBufferRegisterDeserializeFormatMethodInfo,
#endif
    textBufferRegisterDeserializeFormat     ,


-- ** registerDeserializeTagset #method:registerDeserializeTagset#

#if defined(ENABLE_OVERLOADING)
    TextBufferRegisterDeserializeTagsetMethodInfo,
#endif
    textBufferRegisterDeserializeTagset     ,


-- ** registerSerializeFormat #method:registerSerializeFormat#

#if defined(ENABLE_OVERLOADING)
    TextBufferRegisterSerializeFormatMethodInfo,
#endif
    textBufferRegisterSerializeFormat       ,


-- ** registerSerializeTagset #method:registerSerializeTagset#

#if defined(ENABLE_OVERLOADING)
    TextBufferRegisterSerializeTagsetMethodInfo,
#endif
    textBufferRegisterSerializeTagset       ,


-- ** removeAllTags #method:removeAllTags#

#if defined(ENABLE_OVERLOADING)
    TextBufferRemoveAllTagsMethodInfo       ,
#endif
    textBufferRemoveAllTags                 ,


-- ** removeSelectionClipboard #method:removeSelectionClipboard#

#if defined(ENABLE_OVERLOADING)
    TextBufferRemoveSelectionClipboardMethodInfo,
#endif
    textBufferRemoveSelectionClipboard      ,


-- ** removeTag #method:removeTag#

#if defined(ENABLE_OVERLOADING)
    TextBufferRemoveTagMethodInfo           ,
#endif
    textBufferRemoveTag                     ,


-- ** removeTagByName #method:removeTagByName#

#if defined(ENABLE_OVERLOADING)
    TextBufferRemoveTagByNameMethodInfo     ,
#endif
    textBufferRemoveTagByName               ,


-- ** selectRange #method:selectRange#

#if defined(ENABLE_OVERLOADING)
    TextBufferSelectRangeMethodInfo         ,
#endif
    textBufferSelectRange                   ,


-- ** serialize #method:serialize#

#if defined(ENABLE_OVERLOADING)
    TextBufferSerializeMethodInfo           ,
#endif
    textBufferSerialize                     ,


-- ** setModified #method:setModified#

#if defined(ENABLE_OVERLOADING)
    TextBufferSetModifiedMethodInfo         ,
#endif
    textBufferSetModified                   ,


-- ** setText #method:setText#

#if defined(ENABLE_OVERLOADING)
    TextBufferSetTextMethodInfo             ,
#endif
    textBufferSetText                       ,


-- ** unregisterDeserializeFormat #method:unregisterDeserializeFormat#

#if defined(ENABLE_OVERLOADING)
    TextBufferUnregisterDeserializeFormatMethodInfo,
#endif
    textBufferUnregisterDeserializeFormat   ,


-- ** unregisterSerializeFormat #method:unregisterSerializeFormat#

#if defined(ENABLE_OVERLOADING)
    TextBufferUnregisterSerializeFormatMethodInfo,
#endif
    textBufferUnregisterSerializeFormat     ,




 -- * Properties


-- ** copyTargetList #attr:copyTargetList#
-- | The list of targets this buffer supports for clipboard copying
-- and as DND source.
-- 
-- /Since: 2.10/

#if defined(ENABLE_OVERLOADING)
    TextBufferCopyTargetListPropertyInfo    ,
#endif
    getTextBufferCopyTargetList             ,
#if defined(ENABLE_OVERLOADING)
    textBufferCopyTargetList                ,
#endif


-- ** cursorPosition #attr:cursorPosition#
-- | The position of the insert mark (as offset from the beginning
-- of the buffer). It is useful for getting notified when the
-- cursor moves.
-- 
-- /Since: 2.10/

#if defined(ENABLE_OVERLOADING)
    TextBufferCursorPositionPropertyInfo    ,
#endif
    getTextBufferCursorPosition             ,
#if defined(ENABLE_OVERLOADING)
    textBufferCursorPosition                ,
#endif


-- ** hasSelection #attr:hasSelection#
-- | Whether the buffer has some text currently selected.
-- 
-- /Since: 2.10/

#if defined(ENABLE_OVERLOADING)
    TextBufferHasSelectionPropertyInfo      ,
#endif
    getTextBufferHasSelection               ,
#if defined(ENABLE_OVERLOADING)
    textBufferHasSelection                  ,
#endif


-- ** pasteTargetList #attr:pasteTargetList#
-- | The list of targets this buffer supports for clipboard pasting
-- and as DND destination.
-- 
-- /Since: 2.10/

#if defined(ENABLE_OVERLOADING)
    TextBufferPasteTargetListPropertyInfo   ,
#endif
    getTextBufferPasteTargetList            ,
#if defined(ENABLE_OVERLOADING)
    textBufferPasteTargetList               ,
#endif


-- ** tagTable #attr:tagTable#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TextBufferTagTablePropertyInfo          ,
#endif
    constructTextBufferTagTable             ,
    getTextBufferTagTable                   ,
#if defined(ENABLE_OVERLOADING)
    textBufferTagTable                      ,
#endif


-- ** text #attr:text#
-- | The text content of the buffer. Without child widgets and images,
-- see 'GI.Gtk.Objects.TextBuffer.textBufferGetText' for more information.
-- 
-- /Since: 2.8/

#if defined(ENABLE_OVERLOADING)
    TextBufferTextPropertyInfo              ,
#endif
    clearTextBufferText                     ,
    constructTextBufferText                 ,
    getTextBufferText                       ,
    setTextBufferText                       ,
#if defined(ENABLE_OVERLOADING)
    textBufferText                          ,
#endif




 -- * Signals


-- ** applyTag #signal:applyTag#

    TextBufferApplyTagCallback              ,
#if defined(ENABLE_OVERLOADING)
    TextBufferApplyTagSignalInfo            ,
#endif
    afterTextBufferApplyTag                 ,
    onTextBufferApplyTag                    ,


-- ** beginUserAction #signal:beginUserAction#

    TextBufferBeginUserActionCallback       ,
#if defined(ENABLE_OVERLOADING)
    TextBufferBeginUserActionSignalInfo     ,
#endif
    afterTextBufferBeginUserAction          ,
    onTextBufferBeginUserAction             ,


-- ** changed #signal:changed#

    TextBufferChangedCallback               ,
#if defined(ENABLE_OVERLOADING)
    TextBufferChangedSignalInfo             ,
#endif
    afterTextBufferChanged                  ,
    onTextBufferChanged                     ,


-- ** deleteRange #signal:deleteRange#

    TextBufferDeleteRangeCallback           ,
#if defined(ENABLE_OVERLOADING)
    TextBufferDeleteRangeSignalInfo         ,
#endif
    afterTextBufferDeleteRange              ,
    onTextBufferDeleteRange                 ,


-- ** endUserAction #signal:endUserAction#

    TextBufferEndUserActionCallback         ,
#if defined(ENABLE_OVERLOADING)
    TextBufferEndUserActionSignalInfo       ,
#endif
    afterTextBufferEndUserAction            ,
    onTextBufferEndUserAction               ,


-- ** insertChildAnchor #signal:insertChildAnchor#

    TextBufferInsertChildAnchorCallback     ,
#if defined(ENABLE_OVERLOADING)
    TextBufferInsertChildAnchorSignalInfo   ,
#endif
    afterTextBufferInsertChildAnchor        ,
    onTextBufferInsertChildAnchor           ,


-- ** insertPixbuf #signal:insertPixbuf#

    TextBufferInsertPixbufCallback          ,
#if defined(ENABLE_OVERLOADING)
    TextBufferInsertPixbufSignalInfo        ,
#endif
    afterTextBufferInsertPixbuf             ,
    onTextBufferInsertPixbuf                ,


-- ** insertText #signal:insertText#

    TextBufferInsertTextCallback            ,
#if defined(ENABLE_OVERLOADING)
    TextBufferInsertTextSignalInfo          ,
#endif
    afterTextBufferInsertText               ,
    onTextBufferInsertText                  ,


-- ** markDeleted #signal:markDeleted#

    TextBufferMarkDeletedCallback           ,
#if defined(ENABLE_OVERLOADING)
    TextBufferMarkDeletedSignalInfo         ,
#endif
    afterTextBufferMarkDeleted              ,
    onTextBufferMarkDeleted                 ,


-- ** markSet #signal:markSet#

    TextBufferMarkSetCallback               ,
#if defined(ENABLE_OVERLOADING)
    TextBufferMarkSetSignalInfo             ,
#endif
    afterTextBufferMarkSet                  ,
    onTextBufferMarkSet                     ,


-- ** modifiedChanged #signal:modifiedChanged#

    TextBufferModifiedChangedCallback       ,
#if defined(ENABLE_OVERLOADING)
    TextBufferModifiedChangedSignalInfo     ,
#endif
    afterTextBufferModifiedChanged          ,
    onTextBufferModifiedChanged             ,


-- ** pasteDone #signal:pasteDone#

    TextBufferPasteDoneCallback             ,
#if defined(ENABLE_OVERLOADING)
    TextBufferPasteDoneSignalInfo           ,
#endif
    afterTextBufferPasteDone                ,
    onTextBufferPasteDone                   ,


-- ** removeTag #signal:removeTag#

    TextBufferRemoveTagCallback             ,
#if defined(ENABLE_OVERLOADING)
    TextBufferRemoveTagSignalInfo           ,
#endif
    afterTextBufferRemoveTag                ,
    onTextBufferRemoveTag                   ,




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

import qualified GI.GLib.Callbacks as GLib.Callbacks
import qualified GI.GObject.Objects.Object as GObject.Object
import qualified GI.Gdk.Structs.Atom as Gdk.Atom
import qualified GI.GdkPixbuf.Objects.Pixbuf as GdkPixbuf.Pixbuf
import qualified GI.Gtk.Callbacks as Gtk.Callbacks
import {-# SOURCE #-} qualified GI.Gtk.Objects.Clipboard as Gtk.Clipboard
import {-# SOURCE #-} qualified GI.Gtk.Objects.TextChildAnchor as Gtk.TextChildAnchor
import {-# SOURCE #-} qualified GI.Gtk.Objects.TextMark as Gtk.TextMark
import {-# SOURCE #-} qualified GI.Gtk.Objects.TextTag as Gtk.TextTag
import {-# SOURCE #-} qualified GI.Gtk.Objects.TextTagTable as Gtk.TextTagTable
import {-# SOURCE #-} qualified GI.Gtk.Structs.TargetList as Gtk.TargetList
import {-# SOURCE #-} qualified GI.Gtk.Structs.TextIter as Gtk.TextIter

-- | Memory-managed wrapper type.
newtype TextBuffer = TextBuffer (SP.ManagedPtr TextBuffer)
    deriving (Eq)

instance SP.ManagedPtrNewtype TextBuffer where
    toManagedPtr (TextBuffer p) = p

foreign import ccall "gtk_text_buffer_get_type"
    c_gtk_text_buffer_get_type :: IO B.Types.GType

instance B.Types.TypedObject TextBuffer where
    glibType = c_gtk_text_buffer_get_type

instance B.Types.GObject TextBuffer

-- | Type class for types which can be safely cast to `TextBuffer`, for instance with `toTextBuffer`.
class (SP.GObject o, O.IsDescendantOf TextBuffer o) => IsTextBuffer o
instance (SP.GObject o, O.IsDescendantOf TextBuffer o) => IsTextBuffer o

instance O.HasParentTypes TextBuffer
type instance O.ParentTypes TextBuffer = '[GObject.Object.Object]

-- | Cast to `TextBuffer`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toTextBuffer :: (MIO.MonadIO m, IsTextBuffer o) => o -> m TextBuffer
toTextBuffer = MIO.liftIO . B.ManagedPtr.unsafeCastTo TextBuffer

-- | Convert 'TextBuffer' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe TextBuffer) where
    gvalueGType_ = c_gtk_text_buffer_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr TextBuffer)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr TextBuffer)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject TextBuffer ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveTextBufferMethod (t :: Symbol) (o :: *) :: * where
    ResolveTextBufferMethod "addMark" o = TextBufferAddMarkMethodInfo
    ResolveTextBufferMethod "addSelectionClipboard" o = TextBufferAddSelectionClipboardMethodInfo
    ResolveTextBufferMethod "applyTag" o = TextBufferApplyTagMethodInfo
    ResolveTextBufferMethod "applyTagByName" o = TextBufferApplyTagByNameMethodInfo
    ResolveTextBufferMethod "backspace" o = TextBufferBackspaceMethodInfo
    ResolveTextBufferMethod "beginUserAction" o = TextBufferBeginUserActionMethodInfo
    ResolveTextBufferMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveTextBufferMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveTextBufferMethod "copyClipboard" o = TextBufferCopyClipboardMethodInfo
    ResolveTextBufferMethod "createChildAnchor" o = TextBufferCreateChildAnchorMethodInfo
    ResolveTextBufferMethod "createMark" o = TextBufferCreateMarkMethodInfo
    ResolveTextBufferMethod "cutClipboard" o = TextBufferCutClipboardMethodInfo
    ResolveTextBufferMethod "delete" o = TextBufferDeleteMethodInfo
    ResolveTextBufferMethod "deleteInteractive" o = TextBufferDeleteInteractiveMethodInfo
    ResolveTextBufferMethod "deleteMark" o = TextBufferDeleteMarkMethodInfo
    ResolveTextBufferMethod "deleteMarkByName" o = TextBufferDeleteMarkByNameMethodInfo
    ResolveTextBufferMethod "deleteSelection" o = TextBufferDeleteSelectionMethodInfo
    ResolveTextBufferMethod "deserialize" o = TextBufferDeserializeMethodInfo
    ResolveTextBufferMethod "deserializeGetCanCreateTags" o = TextBufferDeserializeGetCanCreateTagsMethodInfo
    ResolveTextBufferMethod "deserializeSetCanCreateTags" o = TextBufferDeserializeSetCanCreateTagsMethodInfo
    ResolveTextBufferMethod "endUserAction" o = TextBufferEndUserActionMethodInfo
    ResolveTextBufferMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveTextBufferMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveTextBufferMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveTextBufferMethod "insert" o = TextBufferInsertMethodInfo
    ResolveTextBufferMethod "insertAtCursor" o = TextBufferInsertAtCursorMethodInfo
    ResolveTextBufferMethod "insertChildAnchor" o = TextBufferInsertChildAnchorMethodInfo
    ResolveTextBufferMethod "insertInteractive" o = TextBufferInsertInteractiveMethodInfo
    ResolveTextBufferMethod "insertInteractiveAtCursor" o = TextBufferInsertInteractiveAtCursorMethodInfo
    ResolveTextBufferMethod "insertMarkup" o = TextBufferInsertMarkupMethodInfo
    ResolveTextBufferMethod "insertPixbuf" o = TextBufferInsertPixbufMethodInfo
    ResolveTextBufferMethod "insertRange" o = TextBufferInsertRangeMethodInfo
    ResolveTextBufferMethod "insertRangeInteractive" o = TextBufferInsertRangeInteractiveMethodInfo
    ResolveTextBufferMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveTextBufferMethod "moveMark" o = TextBufferMoveMarkMethodInfo
    ResolveTextBufferMethod "moveMarkByName" o = TextBufferMoveMarkByNameMethodInfo
    ResolveTextBufferMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveTextBufferMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveTextBufferMethod "pasteClipboard" o = TextBufferPasteClipboardMethodInfo
    ResolveTextBufferMethod "placeCursor" o = TextBufferPlaceCursorMethodInfo
    ResolveTextBufferMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveTextBufferMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveTextBufferMethod "registerDeserializeFormat" o = TextBufferRegisterDeserializeFormatMethodInfo
    ResolveTextBufferMethod "registerDeserializeTagset" o = TextBufferRegisterDeserializeTagsetMethodInfo
    ResolveTextBufferMethod "registerSerializeFormat" o = TextBufferRegisterSerializeFormatMethodInfo
    ResolveTextBufferMethod "registerSerializeTagset" o = TextBufferRegisterSerializeTagsetMethodInfo
    ResolveTextBufferMethod "removeAllTags" o = TextBufferRemoveAllTagsMethodInfo
    ResolveTextBufferMethod "removeSelectionClipboard" o = TextBufferRemoveSelectionClipboardMethodInfo
    ResolveTextBufferMethod "removeTag" o = TextBufferRemoveTagMethodInfo
    ResolveTextBufferMethod "removeTagByName" o = TextBufferRemoveTagByNameMethodInfo
    ResolveTextBufferMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveTextBufferMethod "selectRange" o = TextBufferSelectRangeMethodInfo
    ResolveTextBufferMethod "serialize" o = TextBufferSerializeMethodInfo
    ResolveTextBufferMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveTextBufferMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveTextBufferMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveTextBufferMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveTextBufferMethod "unregisterDeserializeFormat" o = TextBufferUnregisterDeserializeFormatMethodInfo
    ResolveTextBufferMethod "unregisterSerializeFormat" o = TextBufferUnregisterSerializeFormatMethodInfo
    ResolveTextBufferMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveTextBufferMethod "getBounds" o = TextBufferGetBoundsMethodInfo
    ResolveTextBufferMethod "getCharCount" o = TextBufferGetCharCountMethodInfo
    ResolveTextBufferMethod "getCopyTargetList" o = TextBufferGetCopyTargetListMethodInfo
    ResolveTextBufferMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveTextBufferMethod "getDeserializeFormats" o = TextBufferGetDeserializeFormatsMethodInfo
    ResolveTextBufferMethod "getEndIter" o = TextBufferGetEndIterMethodInfo
    ResolveTextBufferMethod "getHasSelection" o = TextBufferGetHasSelectionMethodInfo
    ResolveTextBufferMethod "getInsert" o = TextBufferGetInsertMethodInfo
    ResolveTextBufferMethod "getIterAtChildAnchor" o = TextBufferGetIterAtChildAnchorMethodInfo
    ResolveTextBufferMethod "getIterAtLine" o = TextBufferGetIterAtLineMethodInfo
    ResolveTextBufferMethod "getIterAtLineIndex" o = TextBufferGetIterAtLineIndexMethodInfo
    ResolveTextBufferMethod "getIterAtLineOffset" o = TextBufferGetIterAtLineOffsetMethodInfo
    ResolveTextBufferMethod "getIterAtMark" o = TextBufferGetIterAtMarkMethodInfo
    ResolveTextBufferMethod "getIterAtOffset" o = TextBufferGetIterAtOffsetMethodInfo
    ResolveTextBufferMethod "getLineCount" o = TextBufferGetLineCountMethodInfo
    ResolveTextBufferMethod "getMark" o = TextBufferGetMarkMethodInfo
    ResolveTextBufferMethod "getModified" o = TextBufferGetModifiedMethodInfo
    ResolveTextBufferMethod "getPasteTargetList" o = TextBufferGetPasteTargetListMethodInfo
    ResolveTextBufferMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveTextBufferMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveTextBufferMethod "getSelectionBound" o = TextBufferGetSelectionBoundMethodInfo
    ResolveTextBufferMethod "getSelectionBounds" o = TextBufferGetSelectionBoundsMethodInfo
    ResolveTextBufferMethod "getSerializeFormats" o = TextBufferGetSerializeFormatsMethodInfo
    ResolveTextBufferMethod "getSlice" o = TextBufferGetSliceMethodInfo
    ResolveTextBufferMethod "getStartIter" o = TextBufferGetStartIterMethodInfo
    ResolveTextBufferMethod "getTagTable" o = TextBufferGetTagTableMethodInfo
    ResolveTextBufferMethod "getText" o = TextBufferGetTextMethodInfo
    ResolveTextBufferMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveTextBufferMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveTextBufferMethod "setModified" o = TextBufferSetModifiedMethodInfo
    ResolveTextBufferMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveTextBufferMethod "setText" o = TextBufferSetTextMethodInfo
    ResolveTextBufferMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveTextBufferMethod t TextBuffer, O.OverloadedMethod info TextBuffer p) => OL.IsLabel t (TextBuffer -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveTextBufferMethod t TextBuffer, O.OverloadedMethod info TextBuffer p, R.HasField t TextBuffer p) => R.HasField t TextBuffer p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveTextBufferMethod t TextBuffer, O.OverloadedMethodInfo info TextBuffer) => OL.IsLabel t (O.MethodProxy info TextBuffer) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- signal TextBuffer::apply-tag
-- | The [applyTag](#g:signal:applyTag) signal is emitted to apply a tag to a
-- range of text in a t'GI.Gtk.Objects.TextBuffer.TextBuffer'.
-- Applying actually occurs in the default handler.
-- 
-- Note that if your handler runs before the default handler it must not
-- invalidate the /@start@/ and /@end@/ iters (or has to revalidate them).
-- 
-- See also:
-- 'GI.Gtk.Objects.TextBuffer.textBufferApplyTag',
-- @/gtk_text_buffer_insert_with_tags()/@,
-- 'GI.Gtk.Objects.TextBuffer.textBufferInsertRange'.
type TextBufferApplyTagCallback =
    Gtk.TextTag.TextTag
    -- ^ /@tag@/: the applied tag
    -> Gtk.TextIter.TextIter
    -- ^ /@start@/: the start of the range the tag is applied to
    -> Gtk.TextIter.TextIter
    -- ^ /@end@/: the end of the range the tag is applied to
    -> IO ()

type C_TextBufferApplyTagCallback =
    Ptr TextBuffer ->                       -- object
    Ptr Gtk.TextTag.TextTag ->
    Ptr Gtk.TextIter.TextIter ->
    Ptr Gtk.TextIter.TextIter ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_TextBufferApplyTagCallback`.
foreign import ccall "wrapper"
    mk_TextBufferApplyTagCallback :: C_TextBufferApplyTagCallback -> IO (FunPtr C_TextBufferApplyTagCallback)

wrap_TextBufferApplyTagCallback :: 
    GObject a => (a -> TextBufferApplyTagCallback) ->
    C_TextBufferApplyTagCallback
wrap_TextBufferApplyTagCallback gi'cb gi'selfPtr tag start end _ = do
    tag' <- (newObject Gtk.TextTag.TextTag) tag
    B.ManagedPtr.withTransient  start $ \start' -> do
        B.ManagedPtr.withTransient  end $ \end' -> do
            B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  tag' start' end'


-- | Connect a signal handler for the [applyTag](#signal:applyTag) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' textBuffer #applyTag callback
-- @
-- 
-- 
onTextBufferApplyTag :: (IsTextBuffer a, MonadIO m) => a -> ((?self :: a) => TextBufferApplyTagCallback) -> m SignalHandlerId
onTextBufferApplyTag obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TextBufferApplyTagCallback wrapped
    wrapped'' <- mk_TextBufferApplyTagCallback wrapped'
    connectSignalFunPtr obj "apply-tag" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [applyTag](#signal:applyTag) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' textBuffer #applyTag callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterTextBufferApplyTag :: (IsTextBuffer a, MonadIO m) => a -> ((?self :: a) => TextBufferApplyTagCallback) -> m SignalHandlerId
afterTextBufferApplyTag obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TextBufferApplyTagCallback wrapped
    wrapped'' <- mk_TextBufferApplyTagCallback wrapped'
    connectSignalFunPtr obj "apply-tag" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data TextBufferApplyTagSignalInfo
instance SignalInfo TextBufferApplyTagSignalInfo where
    type HaskellCallbackType TextBufferApplyTagSignalInfo = TextBufferApplyTagCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_TextBufferApplyTagCallback cb
        cb'' <- mk_TextBufferApplyTagCallback cb'
        connectSignalFunPtr obj "apply-tag" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextBuffer::apply-tag"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextBuffer.html#g:signal:applyTag"})

#endif

-- signal TextBuffer::begin-user-action
-- | The [beginUserAction](#g:signal:beginUserAction) signal is emitted at the beginning of a single
-- user-visible operation on a t'GI.Gtk.Objects.TextBuffer.TextBuffer'.
-- 
-- See also:
-- 'GI.Gtk.Objects.TextBuffer.textBufferBeginUserAction',
-- 'GI.Gtk.Objects.TextBuffer.textBufferInsertInteractive',
-- 'GI.Gtk.Objects.TextBuffer.textBufferInsertRangeInteractive',
-- 'GI.Gtk.Objects.TextBuffer.textBufferDeleteInteractive',
-- 'GI.Gtk.Objects.TextBuffer.textBufferBackspace',
-- 'GI.Gtk.Objects.TextBuffer.textBufferDeleteSelection'.
type TextBufferBeginUserActionCallback =
    IO ()

type C_TextBufferBeginUserActionCallback =
    Ptr TextBuffer ->                       -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_TextBufferBeginUserActionCallback`.
foreign import ccall "wrapper"
    mk_TextBufferBeginUserActionCallback :: C_TextBufferBeginUserActionCallback -> IO (FunPtr C_TextBufferBeginUserActionCallback)

wrap_TextBufferBeginUserActionCallback :: 
    GObject a => (a -> TextBufferBeginUserActionCallback) ->
    C_TextBufferBeginUserActionCallback
wrap_TextBufferBeginUserActionCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [beginUserAction](#signal:beginUserAction) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' textBuffer #beginUserAction callback
-- @
-- 
-- 
onTextBufferBeginUserAction :: (IsTextBuffer a, MonadIO m) => a -> ((?self :: a) => TextBufferBeginUserActionCallback) -> m SignalHandlerId
onTextBufferBeginUserAction obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TextBufferBeginUserActionCallback wrapped
    wrapped'' <- mk_TextBufferBeginUserActionCallback wrapped'
    connectSignalFunPtr obj "begin-user-action" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [beginUserAction](#signal:beginUserAction) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' textBuffer #beginUserAction callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterTextBufferBeginUserAction :: (IsTextBuffer a, MonadIO m) => a -> ((?self :: a) => TextBufferBeginUserActionCallback) -> m SignalHandlerId
afterTextBufferBeginUserAction obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TextBufferBeginUserActionCallback wrapped
    wrapped'' <- mk_TextBufferBeginUserActionCallback wrapped'
    connectSignalFunPtr obj "begin-user-action" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data TextBufferBeginUserActionSignalInfo
instance SignalInfo TextBufferBeginUserActionSignalInfo where
    type HaskellCallbackType TextBufferBeginUserActionSignalInfo = TextBufferBeginUserActionCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_TextBufferBeginUserActionCallback cb
        cb'' <- mk_TextBufferBeginUserActionCallback cb'
        connectSignalFunPtr obj "begin-user-action" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextBuffer::begin-user-action"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextBuffer.html#g:signal:beginUserAction"})

#endif

-- signal TextBuffer::changed
-- | The [changed](#g:signal:changed) signal is emitted when the content of a t'GI.Gtk.Objects.TextBuffer.TextBuffer'
-- has changed.
type TextBufferChangedCallback =
    IO ()

type C_TextBufferChangedCallback =
    Ptr TextBuffer ->                       -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_TextBufferChangedCallback`.
foreign import ccall "wrapper"
    mk_TextBufferChangedCallback :: C_TextBufferChangedCallback -> IO (FunPtr C_TextBufferChangedCallback)

wrap_TextBufferChangedCallback :: 
    GObject a => (a -> TextBufferChangedCallback) ->
    C_TextBufferChangedCallback
wrap_TextBufferChangedCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [changed](#signal:changed) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' textBuffer #changed callback
-- @
-- 
-- 
onTextBufferChanged :: (IsTextBuffer a, MonadIO m) => a -> ((?self :: a) => TextBufferChangedCallback) -> m SignalHandlerId
onTextBufferChanged obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TextBufferChangedCallback wrapped
    wrapped'' <- mk_TextBufferChangedCallback wrapped'
    connectSignalFunPtr obj "changed" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [changed](#signal:changed) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' textBuffer #changed callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterTextBufferChanged :: (IsTextBuffer a, MonadIO m) => a -> ((?self :: a) => TextBufferChangedCallback) -> m SignalHandlerId
afterTextBufferChanged obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TextBufferChangedCallback wrapped
    wrapped'' <- mk_TextBufferChangedCallback wrapped'
    connectSignalFunPtr obj "changed" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data TextBufferChangedSignalInfo
instance SignalInfo TextBufferChangedSignalInfo where
    type HaskellCallbackType TextBufferChangedSignalInfo = TextBufferChangedCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_TextBufferChangedCallback cb
        cb'' <- mk_TextBufferChangedCallback cb'
        connectSignalFunPtr obj "changed" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextBuffer::changed"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextBuffer.html#g:signal:changed"})

#endif

-- signal TextBuffer::delete-range
-- | The [deleteRange](#g:signal:deleteRange) signal is emitted to delete a range
-- from a t'GI.Gtk.Objects.TextBuffer.TextBuffer'.
-- 
-- Note that if your handler runs before the default handler it must not
-- invalidate the /@start@/ and /@end@/ iters (or has to revalidate them).
-- The default signal handler revalidates the /@start@/ and /@end@/ iters to
-- both point to the location where text was deleted. Handlers
-- which run after the default handler (see @/g_signal_connect_after()/@)
-- do not have access to the deleted text.
-- 
-- See also: 'GI.Gtk.Objects.TextBuffer.textBufferDelete'.
type TextBufferDeleteRangeCallback =
    Gtk.TextIter.TextIter
    -- ^ /@start@/: the start of the range to be deleted
    -> Gtk.TextIter.TextIter
    -- ^ /@end@/: the end of the range to be deleted
    -> IO ()

type C_TextBufferDeleteRangeCallback =
    Ptr TextBuffer ->                       -- object
    Ptr Gtk.TextIter.TextIter ->
    Ptr Gtk.TextIter.TextIter ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_TextBufferDeleteRangeCallback`.
foreign import ccall "wrapper"
    mk_TextBufferDeleteRangeCallback :: C_TextBufferDeleteRangeCallback -> IO (FunPtr C_TextBufferDeleteRangeCallback)

wrap_TextBufferDeleteRangeCallback :: 
    GObject a => (a -> TextBufferDeleteRangeCallback) ->
    C_TextBufferDeleteRangeCallback
wrap_TextBufferDeleteRangeCallback gi'cb gi'selfPtr start end _ = do
    B.ManagedPtr.withTransient  start $ \start' -> do
        B.ManagedPtr.withTransient  end $ \end' -> do
            B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  start' end'


-- | Connect a signal handler for the [deleteRange](#signal:deleteRange) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' textBuffer #deleteRange callback
-- @
-- 
-- 
onTextBufferDeleteRange :: (IsTextBuffer a, MonadIO m) => a -> ((?self :: a) => TextBufferDeleteRangeCallback) -> m SignalHandlerId
onTextBufferDeleteRange obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TextBufferDeleteRangeCallback wrapped
    wrapped'' <- mk_TextBufferDeleteRangeCallback wrapped'
    connectSignalFunPtr obj "delete-range" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [deleteRange](#signal:deleteRange) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' textBuffer #deleteRange callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterTextBufferDeleteRange :: (IsTextBuffer a, MonadIO m) => a -> ((?self :: a) => TextBufferDeleteRangeCallback) -> m SignalHandlerId
afterTextBufferDeleteRange obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TextBufferDeleteRangeCallback wrapped
    wrapped'' <- mk_TextBufferDeleteRangeCallback wrapped'
    connectSignalFunPtr obj "delete-range" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data TextBufferDeleteRangeSignalInfo
instance SignalInfo TextBufferDeleteRangeSignalInfo where
    type HaskellCallbackType TextBufferDeleteRangeSignalInfo = TextBufferDeleteRangeCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_TextBufferDeleteRangeCallback cb
        cb'' <- mk_TextBufferDeleteRangeCallback cb'
        connectSignalFunPtr obj "delete-range" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextBuffer::delete-range"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextBuffer.html#g:signal:deleteRange"})

#endif

-- signal TextBuffer::end-user-action
-- | The [endUserAction](#g:signal:endUserAction) signal is emitted at the end of a single
-- user-visible operation on the t'GI.Gtk.Objects.TextBuffer.TextBuffer'.
-- 
-- See also:
-- 'GI.Gtk.Objects.TextBuffer.textBufferEndUserAction',
-- 'GI.Gtk.Objects.TextBuffer.textBufferInsertInteractive',
-- 'GI.Gtk.Objects.TextBuffer.textBufferInsertRangeInteractive',
-- 'GI.Gtk.Objects.TextBuffer.textBufferDeleteInteractive',
-- 'GI.Gtk.Objects.TextBuffer.textBufferBackspace',
-- 'GI.Gtk.Objects.TextBuffer.textBufferDeleteSelection',
-- 'GI.Gtk.Objects.TextBuffer.textBufferBackspace'.
type TextBufferEndUserActionCallback =
    IO ()

type C_TextBufferEndUserActionCallback =
    Ptr TextBuffer ->                       -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_TextBufferEndUserActionCallback`.
foreign import ccall "wrapper"
    mk_TextBufferEndUserActionCallback :: C_TextBufferEndUserActionCallback -> IO (FunPtr C_TextBufferEndUserActionCallback)

wrap_TextBufferEndUserActionCallback :: 
    GObject a => (a -> TextBufferEndUserActionCallback) ->
    C_TextBufferEndUserActionCallback
wrap_TextBufferEndUserActionCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [endUserAction](#signal:endUserAction) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' textBuffer #endUserAction callback
-- @
-- 
-- 
onTextBufferEndUserAction :: (IsTextBuffer a, MonadIO m) => a -> ((?self :: a) => TextBufferEndUserActionCallback) -> m SignalHandlerId
onTextBufferEndUserAction obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TextBufferEndUserActionCallback wrapped
    wrapped'' <- mk_TextBufferEndUserActionCallback wrapped'
    connectSignalFunPtr obj "end-user-action" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [endUserAction](#signal:endUserAction) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' textBuffer #endUserAction callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterTextBufferEndUserAction :: (IsTextBuffer a, MonadIO m) => a -> ((?self :: a) => TextBufferEndUserActionCallback) -> m SignalHandlerId
afterTextBufferEndUserAction obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TextBufferEndUserActionCallback wrapped
    wrapped'' <- mk_TextBufferEndUserActionCallback wrapped'
    connectSignalFunPtr obj "end-user-action" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data TextBufferEndUserActionSignalInfo
instance SignalInfo TextBufferEndUserActionSignalInfo where
    type HaskellCallbackType TextBufferEndUserActionSignalInfo = TextBufferEndUserActionCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_TextBufferEndUserActionCallback cb
        cb'' <- mk_TextBufferEndUserActionCallback cb'
        connectSignalFunPtr obj "end-user-action" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextBuffer::end-user-action"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextBuffer.html#g:signal:endUserAction"})

#endif

-- signal TextBuffer::insert-child-anchor
-- | The [insertChildAnchor](#g:signal:insertChildAnchor) signal is emitted to insert a
-- t'GI.Gtk.Objects.TextChildAnchor.TextChildAnchor' in a t'GI.Gtk.Objects.TextBuffer.TextBuffer'.
-- Insertion actually occurs in the default handler.
-- 
-- Note that if your handler runs before the default handler it must
-- not invalidate the /@location@/ iter (or has to revalidate it).
-- The default signal handler revalidates it to be placed after the
-- inserted /@anchor@/.
-- 
-- See also: 'GI.Gtk.Objects.TextBuffer.textBufferInsertChildAnchor'.
type TextBufferInsertChildAnchorCallback =
    Gtk.TextIter.TextIter
    -- ^ /@location@/: position to insert /@anchor@/ in /@textbuffer@/
    -> Gtk.TextChildAnchor.TextChildAnchor
    -- ^ /@anchor@/: the t'GI.Gtk.Objects.TextChildAnchor.TextChildAnchor' to be inserted
    -> IO ()

type C_TextBufferInsertChildAnchorCallback =
    Ptr TextBuffer ->                       -- object
    Ptr Gtk.TextIter.TextIter ->
    Ptr Gtk.TextChildAnchor.TextChildAnchor ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_TextBufferInsertChildAnchorCallback`.
foreign import ccall "wrapper"
    mk_TextBufferInsertChildAnchorCallback :: C_TextBufferInsertChildAnchorCallback -> IO (FunPtr C_TextBufferInsertChildAnchorCallback)

wrap_TextBufferInsertChildAnchorCallback :: 
    GObject a => (a -> TextBufferInsertChildAnchorCallback) ->
    C_TextBufferInsertChildAnchorCallback
wrap_TextBufferInsertChildAnchorCallback gi'cb gi'selfPtr location anchor _ = do
    B.ManagedPtr.withTransient  location $ \location' -> do
        anchor' <- (newObject Gtk.TextChildAnchor.TextChildAnchor) anchor
        B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  location' anchor'


-- | Connect a signal handler for the [insertChildAnchor](#signal:insertChildAnchor) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' textBuffer #insertChildAnchor callback
-- @
-- 
-- 
onTextBufferInsertChildAnchor :: (IsTextBuffer a, MonadIO m) => a -> ((?self :: a) => TextBufferInsertChildAnchorCallback) -> m SignalHandlerId
onTextBufferInsertChildAnchor obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TextBufferInsertChildAnchorCallback wrapped
    wrapped'' <- mk_TextBufferInsertChildAnchorCallback wrapped'
    connectSignalFunPtr obj "insert-child-anchor" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [insertChildAnchor](#signal:insertChildAnchor) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' textBuffer #insertChildAnchor callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterTextBufferInsertChildAnchor :: (IsTextBuffer a, MonadIO m) => a -> ((?self :: a) => TextBufferInsertChildAnchorCallback) -> m SignalHandlerId
afterTextBufferInsertChildAnchor obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TextBufferInsertChildAnchorCallback wrapped
    wrapped'' <- mk_TextBufferInsertChildAnchorCallback wrapped'
    connectSignalFunPtr obj "insert-child-anchor" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data TextBufferInsertChildAnchorSignalInfo
instance SignalInfo TextBufferInsertChildAnchorSignalInfo where
    type HaskellCallbackType TextBufferInsertChildAnchorSignalInfo = TextBufferInsertChildAnchorCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_TextBufferInsertChildAnchorCallback cb
        cb'' <- mk_TextBufferInsertChildAnchorCallback cb'
        connectSignalFunPtr obj "insert-child-anchor" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextBuffer::insert-child-anchor"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextBuffer.html#g:signal:insertChildAnchor"})

#endif

-- signal TextBuffer::insert-pixbuf
-- | The [insertPixbuf](#g:signal:insertPixbuf) signal is emitted to insert a t'GI.GdkPixbuf.Objects.Pixbuf.Pixbuf'
-- in a t'GI.Gtk.Objects.TextBuffer.TextBuffer'. Insertion actually occurs in the default handler.
-- 
-- Note that if your handler runs before the default handler it must not
-- invalidate the /@location@/ iter (or has to revalidate it).
-- The default signal handler revalidates it to be placed after the
-- inserted /@pixbuf@/.
-- 
-- See also: 'GI.Gtk.Objects.TextBuffer.textBufferInsertPixbuf'.
type TextBufferInsertPixbufCallback =
    Gtk.TextIter.TextIter
    -- ^ /@location@/: position to insert /@pixbuf@/ in /@textbuffer@/
    -> GdkPixbuf.Pixbuf.Pixbuf
    -- ^ /@pixbuf@/: the t'GI.GdkPixbuf.Objects.Pixbuf.Pixbuf' to be inserted
    -> IO ()

type C_TextBufferInsertPixbufCallback =
    Ptr TextBuffer ->                       -- object
    Ptr Gtk.TextIter.TextIter ->
    Ptr GdkPixbuf.Pixbuf.Pixbuf ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_TextBufferInsertPixbufCallback`.
foreign import ccall "wrapper"
    mk_TextBufferInsertPixbufCallback :: C_TextBufferInsertPixbufCallback -> IO (FunPtr C_TextBufferInsertPixbufCallback)

wrap_TextBufferInsertPixbufCallback :: 
    GObject a => (a -> TextBufferInsertPixbufCallback) ->
    C_TextBufferInsertPixbufCallback
wrap_TextBufferInsertPixbufCallback gi'cb gi'selfPtr location pixbuf _ = do
    B.ManagedPtr.withTransient  location $ \location' -> do
        pixbuf' <- (newObject GdkPixbuf.Pixbuf.Pixbuf) pixbuf
        B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  location' pixbuf'


-- | Connect a signal handler for the [insertPixbuf](#signal:insertPixbuf) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' textBuffer #insertPixbuf callback
-- @
-- 
-- 
onTextBufferInsertPixbuf :: (IsTextBuffer a, MonadIO m) => a -> ((?self :: a) => TextBufferInsertPixbufCallback) -> m SignalHandlerId
onTextBufferInsertPixbuf obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TextBufferInsertPixbufCallback wrapped
    wrapped'' <- mk_TextBufferInsertPixbufCallback wrapped'
    connectSignalFunPtr obj "insert-pixbuf" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [insertPixbuf](#signal:insertPixbuf) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' textBuffer #insertPixbuf callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterTextBufferInsertPixbuf :: (IsTextBuffer a, MonadIO m) => a -> ((?self :: a) => TextBufferInsertPixbufCallback) -> m SignalHandlerId
afterTextBufferInsertPixbuf obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TextBufferInsertPixbufCallback wrapped
    wrapped'' <- mk_TextBufferInsertPixbufCallback wrapped'
    connectSignalFunPtr obj "insert-pixbuf" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data TextBufferInsertPixbufSignalInfo
instance SignalInfo TextBufferInsertPixbufSignalInfo where
    type HaskellCallbackType TextBufferInsertPixbufSignalInfo = TextBufferInsertPixbufCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_TextBufferInsertPixbufCallback cb
        cb'' <- mk_TextBufferInsertPixbufCallback cb'
        connectSignalFunPtr obj "insert-pixbuf" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextBuffer::insert-pixbuf"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextBuffer.html#g:signal:insertPixbuf"})

#endif

-- signal TextBuffer::insert-text
-- | The [insertText](#g:signal:insertText) signal is emitted to insert text in a t'GI.Gtk.Objects.TextBuffer.TextBuffer'.
-- Insertion actually occurs in the default handler.
-- 
-- Note that if your handler runs before the default handler it must not
-- invalidate the /@location@/ iter (or has to revalidate it).
-- The default signal handler revalidates it to point to the end of the
-- inserted text.
-- 
-- See also:
-- 'GI.Gtk.Objects.TextBuffer.textBufferInsert',
-- 'GI.Gtk.Objects.TextBuffer.textBufferInsertRange'.
type TextBufferInsertTextCallback =
    Gtk.TextIter.TextIter
    -- ^ /@location@/: position to insert /@text@/ in /@textbuffer@/
    -> T.Text
    -- ^ /@text@/: the UTF-8 text to be inserted
    -> Int32
    -- ^ /@len@/: length of the inserted text in bytes
    -> IO ()

type C_TextBufferInsertTextCallback =
    Ptr TextBuffer ->                       -- object
    Ptr Gtk.TextIter.TextIter ->
    CString ->
    Int32 ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_TextBufferInsertTextCallback`.
foreign import ccall "wrapper"
    mk_TextBufferInsertTextCallback :: C_TextBufferInsertTextCallback -> IO (FunPtr C_TextBufferInsertTextCallback)

wrap_TextBufferInsertTextCallback :: 
    GObject a => (a -> TextBufferInsertTextCallback) ->
    C_TextBufferInsertTextCallback
wrap_TextBufferInsertTextCallback gi'cb gi'selfPtr location text len _ = do
    B.ManagedPtr.withTransient  location $ \location' -> do
        text' <- cstringToText text
        B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  location' text' len


-- | Connect a signal handler for the [insertText](#signal:insertText) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' textBuffer #insertText callback
-- @
-- 
-- 
onTextBufferInsertText :: (IsTextBuffer a, MonadIO m) => a -> ((?self :: a) => TextBufferInsertTextCallback) -> m SignalHandlerId
onTextBufferInsertText obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TextBufferInsertTextCallback wrapped
    wrapped'' <- mk_TextBufferInsertTextCallback wrapped'
    connectSignalFunPtr obj "insert-text" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [insertText](#signal:insertText) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' textBuffer #insertText callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterTextBufferInsertText :: (IsTextBuffer a, MonadIO m) => a -> ((?self :: a) => TextBufferInsertTextCallback) -> m SignalHandlerId
afterTextBufferInsertText obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TextBufferInsertTextCallback wrapped
    wrapped'' <- mk_TextBufferInsertTextCallback wrapped'
    connectSignalFunPtr obj "insert-text" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data TextBufferInsertTextSignalInfo
instance SignalInfo TextBufferInsertTextSignalInfo where
    type HaskellCallbackType TextBufferInsertTextSignalInfo = TextBufferInsertTextCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_TextBufferInsertTextCallback cb
        cb'' <- mk_TextBufferInsertTextCallback cb'
        connectSignalFunPtr obj "insert-text" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextBuffer::insert-text"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextBuffer.html#g:signal:insertText"})

#endif

-- signal TextBuffer::mark-deleted
-- | The [markDeleted](#g:signal:markDeleted) signal is emitted as notification
-- after a t'GI.Gtk.Objects.TextMark.TextMark' is deleted.
-- 
-- See also:
-- 'GI.Gtk.Objects.TextBuffer.textBufferDeleteMark'.
type TextBufferMarkDeletedCallback =
    Gtk.TextMark.TextMark
    -- ^ /@mark@/: The mark that was deleted
    -> IO ()

type C_TextBufferMarkDeletedCallback =
    Ptr TextBuffer ->                       -- object
    Ptr Gtk.TextMark.TextMark ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_TextBufferMarkDeletedCallback`.
foreign import ccall "wrapper"
    mk_TextBufferMarkDeletedCallback :: C_TextBufferMarkDeletedCallback -> IO (FunPtr C_TextBufferMarkDeletedCallback)

wrap_TextBufferMarkDeletedCallback :: 
    GObject a => (a -> TextBufferMarkDeletedCallback) ->
    C_TextBufferMarkDeletedCallback
wrap_TextBufferMarkDeletedCallback gi'cb gi'selfPtr mark _ = do
    mark' <- (newObject Gtk.TextMark.TextMark) mark
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  mark'


-- | Connect a signal handler for the [markDeleted](#signal:markDeleted) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' textBuffer #markDeleted callback
-- @
-- 
-- 
onTextBufferMarkDeleted :: (IsTextBuffer a, MonadIO m) => a -> ((?self :: a) => TextBufferMarkDeletedCallback) -> m SignalHandlerId
onTextBufferMarkDeleted obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TextBufferMarkDeletedCallback wrapped
    wrapped'' <- mk_TextBufferMarkDeletedCallback wrapped'
    connectSignalFunPtr obj "mark-deleted" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [markDeleted](#signal:markDeleted) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' textBuffer #markDeleted callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterTextBufferMarkDeleted :: (IsTextBuffer a, MonadIO m) => a -> ((?self :: a) => TextBufferMarkDeletedCallback) -> m SignalHandlerId
afterTextBufferMarkDeleted obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TextBufferMarkDeletedCallback wrapped
    wrapped'' <- mk_TextBufferMarkDeletedCallback wrapped'
    connectSignalFunPtr obj "mark-deleted" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data TextBufferMarkDeletedSignalInfo
instance SignalInfo TextBufferMarkDeletedSignalInfo where
    type HaskellCallbackType TextBufferMarkDeletedSignalInfo = TextBufferMarkDeletedCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_TextBufferMarkDeletedCallback cb
        cb'' <- mk_TextBufferMarkDeletedCallback cb'
        connectSignalFunPtr obj "mark-deleted" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextBuffer::mark-deleted"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextBuffer.html#g:signal:markDeleted"})

#endif

-- signal TextBuffer::mark-set
-- | The [markSet](#g:signal:markSet) signal is emitted as notification
-- after a t'GI.Gtk.Objects.TextMark.TextMark' is set.
-- 
-- See also:
-- 'GI.Gtk.Objects.TextBuffer.textBufferCreateMark',
-- 'GI.Gtk.Objects.TextBuffer.textBufferMoveMark'.
type TextBufferMarkSetCallback =
    Gtk.TextIter.TextIter
    -- ^ /@location@/: The location of /@mark@/ in /@textbuffer@/
    -> Gtk.TextMark.TextMark
    -- ^ /@mark@/: The mark that is set
    -> IO ()

type C_TextBufferMarkSetCallback =
    Ptr TextBuffer ->                       -- object
    Ptr Gtk.TextIter.TextIter ->
    Ptr Gtk.TextMark.TextMark ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_TextBufferMarkSetCallback`.
foreign import ccall "wrapper"
    mk_TextBufferMarkSetCallback :: C_TextBufferMarkSetCallback -> IO (FunPtr C_TextBufferMarkSetCallback)

wrap_TextBufferMarkSetCallback :: 
    GObject a => (a -> TextBufferMarkSetCallback) ->
    C_TextBufferMarkSetCallback
wrap_TextBufferMarkSetCallback gi'cb gi'selfPtr location mark _ = do
    B.ManagedPtr.withTransient  location $ \location' -> do
        mark' <- (newObject Gtk.TextMark.TextMark) mark
        B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  location' mark'


-- | Connect a signal handler for the [markSet](#signal:markSet) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' textBuffer #markSet callback
-- @
-- 
-- 
onTextBufferMarkSet :: (IsTextBuffer a, MonadIO m) => a -> ((?self :: a) => TextBufferMarkSetCallback) -> m SignalHandlerId
onTextBufferMarkSet obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TextBufferMarkSetCallback wrapped
    wrapped'' <- mk_TextBufferMarkSetCallback wrapped'
    connectSignalFunPtr obj "mark-set" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [markSet](#signal:markSet) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' textBuffer #markSet callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterTextBufferMarkSet :: (IsTextBuffer a, MonadIO m) => a -> ((?self :: a) => TextBufferMarkSetCallback) -> m SignalHandlerId
afterTextBufferMarkSet obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TextBufferMarkSetCallback wrapped
    wrapped'' <- mk_TextBufferMarkSetCallback wrapped'
    connectSignalFunPtr obj "mark-set" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data TextBufferMarkSetSignalInfo
instance SignalInfo TextBufferMarkSetSignalInfo where
    type HaskellCallbackType TextBufferMarkSetSignalInfo = TextBufferMarkSetCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_TextBufferMarkSetCallback cb
        cb'' <- mk_TextBufferMarkSetCallback cb'
        connectSignalFunPtr obj "mark-set" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextBuffer::mark-set"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextBuffer.html#g:signal:markSet"})

#endif

-- signal TextBuffer::modified-changed
-- | The [modifiedChanged](#g:signal:modifiedChanged) signal is emitted when the modified bit of a
-- t'GI.Gtk.Objects.TextBuffer.TextBuffer' flips.
-- 
-- See also:
-- 'GI.Gtk.Objects.TextBuffer.textBufferSetModified'.
type TextBufferModifiedChangedCallback =
    IO ()

type C_TextBufferModifiedChangedCallback =
    Ptr TextBuffer ->                       -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_TextBufferModifiedChangedCallback`.
foreign import ccall "wrapper"
    mk_TextBufferModifiedChangedCallback :: C_TextBufferModifiedChangedCallback -> IO (FunPtr C_TextBufferModifiedChangedCallback)

wrap_TextBufferModifiedChangedCallback :: 
    GObject a => (a -> TextBufferModifiedChangedCallback) ->
    C_TextBufferModifiedChangedCallback
wrap_TextBufferModifiedChangedCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [modifiedChanged](#signal:modifiedChanged) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' textBuffer #modifiedChanged callback
-- @
-- 
-- 
onTextBufferModifiedChanged :: (IsTextBuffer a, MonadIO m) => a -> ((?self :: a) => TextBufferModifiedChangedCallback) -> m SignalHandlerId
onTextBufferModifiedChanged obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TextBufferModifiedChangedCallback wrapped
    wrapped'' <- mk_TextBufferModifiedChangedCallback wrapped'
    connectSignalFunPtr obj "modified-changed" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [modifiedChanged](#signal:modifiedChanged) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' textBuffer #modifiedChanged callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterTextBufferModifiedChanged :: (IsTextBuffer a, MonadIO m) => a -> ((?self :: a) => TextBufferModifiedChangedCallback) -> m SignalHandlerId
afterTextBufferModifiedChanged obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TextBufferModifiedChangedCallback wrapped
    wrapped'' <- mk_TextBufferModifiedChangedCallback wrapped'
    connectSignalFunPtr obj "modified-changed" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data TextBufferModifiedChangedSignalInfo
instance SignalInfo TextBufferModifiedChangedSignalInfo where
    type HaskellCallbackType TextBufferModifiedChangedSignalInfo = TextBufferModifiedChangedCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_TextBufferModifiedChangedCallback cb
        cb'' <- mk_TextBufferModifiedChangedCallback cb'
        connectSignalFunPtr obj "modified-changed" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextBuffer::modified-changed"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextBuffer.html#g:signal:modifiedChanged"})

#endif

-- signal TextBuffer::paste-done
-- | The paste-done signal is emitted after paste operation has been completed.
-- This is useful to properly scroll the view to the end of the pasted text.
-- See 'GI.Gtk.Objects.TextBuffer.textBufferPasteClipboard' for more details.
-- 
-- /Since: 2.16/
type TextBufferPasteDoneCallback =
    Gtk.Clipboard.Clipboard
    -- ^ /@clipboard@/: the t'GI.Gtk.Objects.Clipboard.Clipboard' pasted from
    -> IO ()

type C_TextBufferPasteDoneCallback =
    Ptr TextBuffer ->                       -- object
    Ptr Gtk.Clipboard.Clipboard ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_TextBufferPasteDoneCallback`.
foreign import ccall "wrapper"
    mk_TextBufferPasteDoneCallback :: C_TextBufferPasteDoneCallback -> IO (FunPtr C_TextBufferPasteDoneCallback)

wrap_TextBufferPasteDoneCallback :: 
    GObject a => (a -> TextBufferPasteDoneCallback) ->
    C_TextBufferPasteDoneCallback
wrap_TextBufferPasteDoneCallback gi'cb gi'selfPtr clipboard _ = do
    clipboard' <- (newObject Gtk.Clipboard.Clipboard) clipboard
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  clipboard'


-- | Connect a signal handler for the [pasteDone](#signal:pasteDone) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' textBuffer #pasteDone callback
-- @
-- 
-- 
onTextBufferPasteDone :: (IsTextBuffer a, MonadIO m) => a -> ((?self :: a) => TextBufferPasteDoneCallback) -> m SignalHandlerId
onTextBufferPasteDone obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TextBufferPasteDoneCallback wrapped
    wrapped'' <- mk_TextBufferPasteDoneCallback wrapped'
    connectSignalFunPtr obj "paste-done" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [pasteDone](#signal:pasteDone) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' textBuffer #pasteDone callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterTextBufferPasteDone :: (IsTextBuffer a, MonadIO m) => a -> ((?self :: a) => TextBufferPasteDoneCallback) -> m SignalHandlerId
afterTextBufferPasteDone obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TextBufferPasteDoneCallback wrapped
    wrapped'' <- mk_TextBufferPasteDoneCallback wrapped'
    connectSignalFunPtr obj "paste-done" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data TextBufferPasteDoneSignalInfo
instance SignalInfo TextBufferPasteDoneSignalInfo where
    type HaskellCallbackType TextBufferPasteDoneSignalInfo = TextBufferPasteDoneCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_TextBufferPasteDoneCallback cb
        cb'' <- mk_TextBufferPasteDoneCallback cb'
        connectSignalFunPtr obj "paste-done" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextBuffer::paste-done"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextBuffer.html#g:signal:pasteDone"})

#endif

-- signal TextBuffer::remove-tag
-- | The [removeTag](#g:signal:removeTag) signal is emitted to remove all occurrences of /@tag@/ from
-- a range of text in a t'GI.Gtk.Objects.TextBuffer.TextBuffer'.
-- Removal actually occurs in the default handler.
-- 
-- Note that if your handler runs before the default handler it must not
-- invalidate the /@start@/ and /@end@/ iters (or has to revalidate them).
-- 
-- See also:
-- 'GI.Gtk.Objects.TextBuffer.textBufferRemoveTag'.
type TextBufferRemoveTagCallback =
    Gtk.TextTag.TextTag
    -- ^ /@tag@/: the tag to be removed
    -> Gtk.TextIter.TextIter
    -- ^ /@start@/: the start of the range the tag is removed from
    -> Gtk.TextIter.TextIter
    -- ^ /@end@/: the end of the range the tag is removed from
    -> IO ()

type C_TextBufferRemoveTagCallback =
    Ptr TextBuffer ->                       -- object
    Ptr Gtk.TextTag.TextTag ->
    Ptr Gtk.TextIter.TextIter ->
    Ptr Gtk.TextIter.TextIter ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_TextBufferRemoveTagCallback`.
foreign import ccall "wrapper"
    mk_TextBufferRemoveTagCallback :: C_TextBufferRemoveTagCallback -> IO (FunPtr C_TextBufferRemoveTagCallback)

wrap_TextBufferRemoveTagCallback :: 
    GObject a => (a -> TextBufferRemoveTagCallback) ->
    C_TextBufferRemoveTagCallback
wrap_TextBufferRemoveTagCallback gi'cb gi'selfPtr tag start end _ = do
    tag' <- (newObject Gtk.TextTag.TextTag) tag
    B.ManagedPtr.withTransient  start $ \start' -> do
        B.ManagedPtr.withTransient  end $ \end' -> do
            B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  tag' start' end'


-- | Connect a signal handler for the [removeTag](#signal:removeTag) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' textBuffer #removeTag callback
-- @
-- 
-- 
onTextBufferRemoveTag :: (IsTextBuffer a, MonadIO m) => a -> ((?self :: a) => TextBufferRemoveTagCallback) -> m SignalHandlerId
onTextBufferRemoveTag obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TextBufferRemoveTagCallback wrapped
    wrapped'' <- mk_TextBufferRemoveTagCallback wrapped'
    connectSignalFunPtr obj "remove-tag" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [removeTag](#signal:removeTag) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' textBuffer #removeTag callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterTextBufferRemoveTag :: (IsTextBuffer a, MonadIO m) => a -> ((?self :: a) => TextBufferRemoveTagCallback) -> m SignalHandlerId
afterTextBufferRemoveTag obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TextBufferRemoveTagCallback wrapped
    wrapped'' <- mk_TextBufferRemoveTagCallback wrapped'
    connectSignalFunPtr obj "remove-tag" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data TextBufferRemoveTagSignalInfo
instance SignalInfo TextBufferRemoveTagSignalInfo where
    type HaskellCallbackType TextBufferRemoveTagSignalInfo = TextBufferRemoveTagCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_TextBufferRemoveTagCallback cb
        cb'' <- mk_TextBufferRemoveTagCallback cb'
        connectSignalFunPtr obj "remove-tag" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextBuffer::remove-tag"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextBuffer.html#g:signal:removeTag"})

#endif

-- VVV Prop "copy-target-list"
   -- Type: TInterface (Name {namespace = "Gtk", name = "TargetList"})
   -- Flags: [PropertyReadable]
   -- Nullable: (Just False,Nothing)

-- | Get the value of the “@copy-target-list@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textBuffer #copyTargetList
-- @
getTextBufferCopyTargetList :: (MonadIO m, IsTextBuffer o) => o -> m Gtk.TargetList.TargetList
getTextBufferCopyTargetList obj = MIO.liftIO $ checkUnexpectedNothing "getTextBufferCopyTargetList" $ B.Properties.getObjectPropertyBoxed obj "copy-target-list" Gtk.TargetList.TargetList

#if defined(ENABLE_OVERLOADING)
data TextBufferCopyTargetListPropertyInfo
instance AttrInfo TextBufferCopyTargetListPropertyInfo where
    type AttrAllowedOps TextBufferCopyTargetListPropertyInfo = '[ 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint TextBufferCopyTargetListPropertyInfo = IsTextBuffer
    type AttrSetTypeConstraint TextBufferCopyTargetListPropertyInfo = (~) ()
    type AttrTransferTypeConstraint TextBufferCopyTargetListPropertyInfo = (~) ()
    type AttrTransferType TextBufferCopyTargetListPropertyInfo = ()
    type AttrGetType TextBufferCopyTargetListPropertyInfo = Gtk.TargetList.TargetList
    type AttrLabel TextBufferCopyTargetListPropertyInfo = "copy-target-list"
    type AttrOrigin TextBufferCopyTargetListPropertyInfo = TextBuffer
    attrGet = getTextBufferCopyTargetList
    attrSet = undefined
    attrTransfer _ = undefined
    attrConstruct = undefined
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextBuffer.copyTargetList"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextBuffer.html#g:attr:copyTargetList"
        })
#endif

-- VVV Prop "cursor-position"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@cursor-position@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textBuffer #cursorPosition
-- @
getTextBufferCursorPosition :: (MonadIO m, IsTextBuffer o) => o -> m Int32
getTextBufferCursorPosition obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "cursor-position"

#if defined(ENABLE_OVERLOADING)
data TextBufferCursorPositionPropertyInfo
instance AttrInfo TextBufferCursorPositionPropertyInfo where
    type AttrAllowedOps TextBufferCursorPositionPropertyInfo = '[ 'AttrGet]
    type AttrBaseTypeConstraint TextBufferCursorPositionPropertyInfo = IsTextBuffer
    type AttrSetTypeConstraint TextBufferCursorPositionPropertyInfo = (~) ()
    type AttrTransferTypeConstraint TextBufferCursorPositionPropertyInfo = (~) ()
    type AttrTransferType TextBufferCursorPositionPropertyInfo = ()
    type AttrGetType TextBufferCursorPositionPropertyInfo = Int32
    type AttrLabel TextBufferCursorPositionPropertyInfo = "cursor-position"
    type AttrOrigin TextBufferCursorPositionPropertyInfo = TextBuffer
    attrGet = getTextBufferCursorPosition
    attrSet = undefined
    attrTransfer _ = undefined
    attrConstruct = undefined
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextBuffer.cursorPosition"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextBuffer.html#g:attr:cursorPosition"
        })
#endif

-- VVV Prop "has-selection"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable]
   -- Nullable: (Just False,Nothing)

-- | Get the value of the “@has-selection@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textBuffer #hasSelection
-- @
getTextBufferHasSelection :: (MonadIO m, IsTextBuffer o) => o -> m Bool
getTextBufferHasSelection obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "has-selection"

#if defined(ENABLE_OVERLOADING)
data TextBufferHasSelectionPropertyInfo
instance AttrInfo TextBufferHasSelectionPropertyInfo where
    type AttrAllowedOps TextBufferHasSelectionPropertyInfo = '[ 'AttrGet]
    type AttrBaseTypeConstraint TextBufferHasSelectionPropertyInfo = IsTextBuffer
    type AttrSetTypeConstraint TextBufferHasSelectionPropertyInfo = (~) ()
    type AttrTransferTypeConstraint TextBufferHasSelectionPropertyInfo = (~) ()
    type AttrTransferType TextBufferHasSelectionPropertyInfo = ()
    type AttrGetType TextBufferHasSelectionPropertyInfo = Bool
    type AttrLabel TextBufferHasSelectionPropertyInfo = "has-selection"
    type AttrOrigin TextBufferHasSelectionPropertyInfo = TextBuffer
    attrGet = getTextBufferHasSelection
    attrSet = undefined
    attrTransfer _ = undefined
    attrConstruct = undefined
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextBuffer.hasSelection"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextBuffer.html#g:attr:hasSelection"
        })
#endif

-- VVV Prop "paste-target-list"
   -- Type: TInterface (Name {namespace = "Gtk", name = "TargetList"})
   -- Flags: [PropertyReadable]
   -- Nullable: (Just False,Nothing)

-- | Get the value of the “@paste-target-list@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textBuffer #pasteTargetList
-- @
getTextBufferPasteTargetList :: (MonadIO m, IsTextBuffer o) => o -> m Gtk.TargetList.TargetList
getTextBufferPasteTargetList obj = MIO.liftIO $ checkUnexpectedNothing "getTextBufferPasteTargetList" $ B.Properties.getObjectPropertyBoxed obj "paste-target-list" Gtk.TargetList.TargetList

#if defined(ENABLE_OVERLOADING)
data TextBufferPasteTargetListPropertyInfo
instance AttrInfo TextBufferPasteTargetListPropertyInfo where
    type AttrAllowedOps TextBufferPasteTargetListPropertyInfo = '[ 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint TextBufferPasteTargetListPropertyInfo = IsTextBuffer
    type AttrSetTypeConstraint TextBufferPasteTargetListPropertyInfo = (~) ()
    type AttrTransferTypeConstraint TextBufferPasteTargetListPropertyInfo = (~) ()
    type AttrTransferType TextBufferPasteTargetListPropertyInfo = ()
    type AttrGetType TextBufferPasteTargetListPropertyInfo = Gtk.TargetList.TargetList
    type AttrLabel TextBufferPasteTargetListPropertyInfo = "paste-target-list"
    type AttrOrigin TextBufferPasteTargetListPropertyInfo = TextBuffer
    attrGet = getTextBufferPasteTargetList
    attrSet = undefined
    attrTransfer _ = undefined
    attrConstruct = undefined
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextBuffer.pasteTargetList"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextBuffer.html#g:attr:pasteTargetList"
        })
#endif

-- VVV Prop "tag-table"
   -- Type: TInterface (Name {namespace = "Gtk", name = "TextTagTable"})
   -- Flags: [PropertyReadable,PropertyWritable,PropertyConstructOnly]
   -- Nullable: (Just False,Nothing)

-- | Get the value of the “@tag-table@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textBuffer #tagTable
-- @
getTextBufferTagTable :: (MonadIO m, IsTextBuffer o) => o -> m Gtk.TextTagTable.TextTagTable
getTextBufferTagTable obj = MIO.liftIO $ checkUnexpectedNothing "getTextBufferTagTable" $ B.Properties.getObjectPropertyObject obj "tag-table" Gtk.TextTagTable.TextTagTable

-- | Construct a `GValueConstruct` with valid value for the “@tag-table@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextBufferTagTable :: (IsTextBuffer o, MIO.MonadIO m, Gtk.TextTagTable.IsTextTagTable a) => a -> m (GValueConstruct o)
constructTextBufferTagTable val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyObject "tag-table" (P.Just val)

#if defined(ENABLE_OVERLOADING)
data TextBufferTagTablePropertyInfo
instance AttrInfo TextBufferTagTablePropertyInfo where
    type AttrAllowedOps TextBufferTagTablePropertyInfo = '[ 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint TextBufferTagTablePropertyInfo = IsTextBuffer
    type AttrSetTypeConstraint TextBufferTagTablePropertyInfo = Gtk.TextTagTable.IsTextTagTable
    type AttrTransferTypeConstraint TextBufferTagTablePropertyInfo = Gtk.TextTagTable.IsTextTagTable
    type AttrTransferType TextBufferTagTablePropertyInfo = Gtk.TextTagTable.TextTagTable
    type AttrGetType TextBufferTagTablePropertyInfo = Gtk.TextTagTable.TextTagTable
    type AttrLabel TextBufferTagTablePropertyInfo = "tag-table"
    type AttrOrigin TextBufferTagTablePropertyInfo = TextBuffer
    attrGet = getTextBufferTagTable
    attrSet = undefined
    attrTransfer _ v = do
        unsafeCastTo Gtk.TextTagTable.TextTagTable v
    attrConstruct = constructTextBufferTagTable
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextBuffer.tagTable"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextBuffer.html#g:attr:tagTable"
        })
#endif

-- VVV Prop "text"
   -- Type: TBasicType TUTF8
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@text@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textBuffer #text
-- @
getTextBufferText :: (MonadIO m, IsTextBuffer o) => o -> m (Maybe T.Text)
getTextBufferText obj = MIO.liftIO $ B.Properties.getObjectPropertyString obj "text"

-- | Set the value of the “@text@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textBuffer [ #text 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextBufferText :: (MonadIO m, IsTextBuffer o) => o -> T.Text -> m ()
setTextBufferText obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "text" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@text@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextBufferText :: (IsTextBuffer o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructTextBufferText val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "text" (P.Just val)

-- | Set the value of the “@text@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #text
-- @
clearTextBufferText :: (MonadIO m, IsTextBuffer o) => o -> m ()
clearTextBufferText obj = liftIO $ B.Properties.setObjectPropertyString obj "text" (Nothing :: Maybe T.Text)

#if defined(ENABLE_OVERLOADING)
data TextBufferTextPropertyInfo
instance AttrInfo TextBufferTextPropertyInfo where
    type AttrAllowedOps TextBufferTextPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint TextBufferTextPropertyInfo = IsTextBuffer
    type AttrSetTypeConstraint TextBufferTextPropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint TextBufferTextPropertyInfo = (~) T.Text
    type AttrTransferType TextBufferTextPropertyInfo = T.Text
    type AttrGetType TextBufferTextPropertyInfo = (Maybe T.Text)
    type AttrLabel TextBufferTextPropertyInfo = "text"
    type AttrOrigin TextBufferTextPropertyInfo = TextBuffer
    attrGet = getTextBufferText
    attrSet = setTextBufferText
    attrTransfer _ v = do
        return v
    attrConstruct = constructTextBufferText
    attrClear = clearTextBufferText
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextBuffer.text"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextBuffer.html#g:attr:text"
        })
#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList TextBuffer
type instance O.AttributeList TextBuffer = TextBufferAttributeList
type TextBufferAttributeList = ('[ '("copyTargetList", TextBufferCopyTargetListPropertyInfo), '("cursorPosition", TextBufferCursorPositionPropertyInfo), '("hasSelection", TextBufferHasSelectionPropertyInfo), '("pasteTargetList", TextBufferPasteTargetListPropertyInfo), '("tagTable", TextBufferTagTablePropertyInfo), '("text", TextBufferTextPropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
textBufferCopyTargetList :: AttrLabelProxy "copyTargetList"
textBufferCopyTargetList = AttrLabelProxy

textBufferCursorPosition :: AttrLabelProxy "cursorPosition"
textBufferCursorPosition = AttrLabelProxy

textBufferHasSelection :: AttrLabelProxy "hasSelection"
textBufferHasSelection = AttrLabelProxy

textBufferPasteTargetList :: AttrLabelProxy "pasteTargetList"
textBufferPasteTargetList = AttrLabelProxy

textBufferTagTable :: AttrLabelProxy "tagTable"
textBufferTagTable = AttrLabelProxy

textBufferText :: AttrLabelProxy "text"
textBufferText = AttrLabelProxy

#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList TextBuffer = TextBufferSignalList
type TextBufferSignalList = ('[ '("applyTag", TextBufferApplyTagSignalInfo), '("beginUserAction", TextBufferBeginUserActionSignalInfo), '("changed", TextBufferChangedSignalInfo), '("deleteRange", TextBufferDeleteRangeSignalInfo), '("endUserAction", TextBufferEndUserActionSignalInfo), '("insertChildAnchor", TextBufferInsertChildAnchorSignalInfo), '("insertPixbuf", TextBufferInsertPixbufSignalInfo), '("insertText", TextBufferInsertTextSignalInfo), '("markDeleted", TextBufferMarkDeletedSignalInfo), '("markSet", TextBufferMarkSetSignalInfo), '("modifiedChanged", TextBufferModifiedChangedSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo), '("pasteDone", TextBufferPasteDoneSignalInfo), '("removeTag", TextBufferRemoveTagSignalInfo)] :: [(Symbol, *)])

#endif

-- method TextBuffer::new
-- method type : Constructor
-- Args: [ Arg
--           { argCName = "table"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextTagTable" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a tag table, or %NULL to create a new one"
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "TextBuffer" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_text_buffer_new" gtk_text_buffer_new :: 
    Ptr Gtk.TextTagTable.TextTagTable ->    -- table : TInterface (Name {namespace = "Gtk", name = "TextTagTable"})
    IO (Ptr TextBuffer)

-- | Creates a new text buffer.
textBufferNew ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.TextTagTable.IsTextTagTable a) =>
    Maybe (a)
    -- ^ /@table@/: a tag table, or 'P.Nothing' to create a new one
    -> m TextBuffer
    -- ^ __Returns:__ a new text buffer
textBufferNew table = liftIO $ do
    maybeTable <- case table of
        Nothing -> return nullPtr
        Just jTable -> do
            jTable' <- unsafeManagedPtrCastPtr jTable
            return jTable'
    result <- gtk_text_buffer_new maybeTable
    checkUnexpectedReturnNULL "textBufferNew" result
    result' <- (wrapObject TextBuffer) result
    whenJust table touchManagedPtr
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method TextBuffer::add_mark
-- method type : OrdinaryMethod
-- Args: [ Arg
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
--       , Arg
--           { argCName = "mark"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextMark" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the mark to add" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "where"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "location to place mark"
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

foreign import ccall "gtk_text_buffer_add_mark" gtk_text_buffer_add_mark :: 
    Ptr TextBuffer ->                       -- buffer : TInterface (Name {namespace = "Gtk", name = "TextBuffer"})
    Ptr Gtk.TextMark.TextMark ->            -- mark : TInterface (Name {namespace = "Gtk", name = "TextMark"})
    Ptr Gtk.TextIter.TextIter ->            -- where : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    IO ()

-- | Adds the mark at position /@where@/. The mark must not be added to
-- another buffer, and if its name is not 'P.Nothing' then there must not
-- be another mark in the buffer with the same name.
-- 
-- Emits the [TextBuffer::markSet]("GI.Gtk.Objects.TextBuffer#g:signal:markSet") signal as notification of the mark\'s
-- initial placement.
-- 
-- /Since: 2.12/
textBufferAddMark ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextBuffer a, Gtk.TextMark.IsTextMark b) =>
    a
    -- ^ /@buffer@/: a t'GI.Gtk.Objects.TextBuffer.TextBuffer'
    -> b
    -- ^ /@mark@/: the mark to add
    -> Gtk.TextIter.TextIter
    -- ^ /@where@/: location to place mark
    -> m ()
textBufferAddMark buffer mark where_ = liftIO $ do
    buffer' <- unsafeManagedPtrCastPtr buffer
    mark' <- unsafeManagedPtrCastPtr mark
    where_' <- unsafeManagedPtrGetPtr where_
    gtk_text_buffer_add_mark buffer' mark' where_'
    touchManagedPtr buffer
    touchManagedPtr mark
    touchManagedPtr where_
    return ()

#if defined(ENABLE_OVERLOADING)
data TextBufferAddMarkMethodInfo
instance (signature ~ (b -> Gtk.TextIter.TextIter -> m ()), MonadIO m, IsTextBuffer a, Gtk.TextMark.IsTextMark b) => O.OverloadedMethod TextBufferAddMarkMethodInfo a signature where
    overloadedMethod = textBufferAddMark

instance O.OverloadedMethodInfo TextBufferAddMarkMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextBuffer.textBufferAddMark",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextBuffer.html#v:textBufferAddMark"
        })


#endif

-- method TextBuffer::add_selection_clipboard
-- method type : OrdinaryMethod
-- Args: [ Arg
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
--       , Arg
--           { argCName = "clipboard"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Clipboard" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkClipboard" , sinceVersion = Nothing }
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

foreign import ccall "gtk_text_buffer_add_selection_clipboard" gtk_text_buffer_add_selection_clipboard :: 
    Ptr TextBuffer ->                       -- buffer : TInterface (Name {namespace = "Gtk", name = "TextBuffer"})
    Ptr Gtk.Clipboard.Clipboard ->          -- clipboard : TInterface (Name {namespace = "Gtk", name = "Clipboard"})
    IO ()

-- | Adds /@clipboard@/ to the list of clipboards in which the selection
-- contents of /@buffer@/ are available. In most cases, /@clipboard@/ will be
-- the t'GI.Gtk.Objects.Clipboard.Clipboard' of type @/GDK_SELECTION_PRIMARY/@ for a view of /@buffer@/.
textBufferAddSelectionClipboard ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextBuffer a, Gtk.Clipboard.IsClipboard b) =>
    a
    -- ^ /@buffer@/: a t'GI.Gtk.Objects.TextBuffer.TextBuffer'
    -> b
    -- ^ /@clipboard@/: a t'GI.Gtk.Objects.Clipboard.Clipboard'
    -> m ()
textBufferAddSelectionClipboard buffer clipboard = liftIO $ do
    buffer' <- unsafeManagedPtrCastPtr buffer
    clipboard' <- unsafeManagedPtrCastPtr clipboard
    gtk_text_buffer_add_selection_clipboard buffer' clipboard'
    touchManagedPtr buffer
    touchManagedPtr clipboard
    return ()

#if defined(ENABLE_OVERLOADING)
data TextBufferAddSelectionClipboardMethodInfo
instance (signature ~ (b -> m ()), MonadIO m, IsTextBuffer a, Gtk.Clipboard.IsClipboard b) => O.OverloadedMethod TextBufferAddSelectionClipboardMethodInfo a signature where
    overloadedMethod = textBufferAddSelectionClipboard

instance O.OverloadedMethodInfo TextBufferAddSelectionClipboardMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextBuffer.textBufferAddSelectionClipboard",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextBuffer.html#v:textBufferAddSelectionClipboard"
        })


#endif

-- method TextBuffer::apply_tag
-- method type : OrdinaryMethod
-- Args: [ Arg
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
--       , Arg
--           { argCName = "start"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "one bound of range to be tagged"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "end"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "other bound of range to be tagged"
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

foreign import ccall "gtk_text_buffer_apply_tag" gtk_text_buffer_apply_tag :: 
    Ptr TextBuffer ->                       -- buffer : TInterface (Name {namespace = "Gtk", name = "TextBuffer"})
    Ptr Gtk.TextTag.TextTag ->              -- tag : TInterface (Name {namespace = "Gtk", name = "TextTag"})
    Ptr Gtk.TextIter.TextIter ->            -- start : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    Ptr Gtk.TextIter.TextIter ->            -- end : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    IO ()

-- | Emits the “apply-tag” signal on /@buffer@/. The default
-- handler for the signal applies /@tag@/ to the given range.
-- /@start@/ and /@end@/ do not have to be in order.
textBufferApplyTag ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextBuffer a, Gtk.TextTag.IsTextTag b) =>
    a
    -- ^ /@buffer@/: a t'GI.Gtk.Objects.TextBuffer.TextBuffer'
    -> b
    -- ^ /@tag@/: a t'GI.Gtk.Objects.TextTag.TextTag'
    -> Gtk.TextIter.TextIter
    -- ^ /@start@/: one bound of range to be tagged
    -> Gtk.TextIter.TextIter
    -- ^ /@end@/: other bound of range to be tagged
    -> m ()
textBufferApplyTag buffer tag start end = liftIO $ do
    buffer' <- unsafeManagedPtrCastPtr buffer
    tag' <- unsafeManagedPtrCastPtr tag
    start' <- unsafeManagedPtrGetPtr start
    end' <- unsafeManagedPtrGetPtr end
    gtk_text_buffer_apply_tag buffer' tag' start' end'
    touchManagedPtr buffer
    touchManagedPtr tag
    touchManagedPtr start
    touchManagedPtr end
    return ()

#if defined(ENABLE_OVERLOADING)
data TextBufferApplyTagMethodInfo
instance (signature ~ (b -> Gtk.TextIter.TextIter -> Gtk.TextIter.TextIter -> m ()), MonadIO m, IsTextBuffer a, Gtk.TextTag.IsTextTag b) => O.OverloadedMethod TextBufferApplyTagMethodInfo a signature where
    overloadedMethod = textBufferApplyTag

instance O.OverloadedMethodInfo TextBufferApplyTagMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextBuffer.textBufferApplyTag",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextBuffer.html#v:textBufferApplyTag"
        })


#endif

-- method TextBuffer::apply_tag_by_name
-- method type : OrdinaryMethod
-- Args: [ Arg
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
--       , Arg
--           { argCName = "name"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "name of a named #GtkTextTag"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "start"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "one bound of range to be tagged"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "end"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "other bound of range to be tagged"
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

foreign import ccall "gtk_text_buffer_apply_tag_by_name" gtk_text_buffer_apply_tag_by_name :: 
    Ptr TextBuffer ->                       -- buffer : TInterface (Name {namespace = "Gtk", name = "TextBuffer"})
    CString ->                              -- name : TBasicType TUTF8
    Ptr Gtk.TextIter.TextIter ->            -- start : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    Ptr Gtk.TextIter.TextIter ->            -- end : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    IO ()

-- | Calls 'GI.Gtk.Objects.TextTagTable.textTagTableLookup' on the buffer’s tag table to
-- get a t'GI.Gtk.Objects.TextTag.TextTag', then calls 'GI.Gtk.Objects.TextBuffer.textBufferApplyTag'.
textBufferApplyTagByName ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextBuffer a) =>
    a
    -- ^ /@buffer@/: a t'GI.Gtk.Objects.TextBuffer.TextBuffer'
    -> T.Text
    -- ^ /@name@/: name of a named t'GI.Gtk.Objects.TextTag.TextTag'
    -> Gtk.TextIter.TextIter
    -- ^ /@start@/: one bound of range to be tagged
    -> Gtk.TextIter.TextIter
    -- ^ /@end@/: other bound of range to be tagged
    -> m ()
textBufferApplyTagByName buffer name start end = liftIO $ do
    buffer' <- unsafeManagedPtrCastPtr buffer
    name' <- textToCString name
    start' <- unsafeManagedPtrGetPtr start
    end' <- unsafeManagedPtrGetPtr end
    gtk_text_buffer_apply_tag_by_name buffer' name' start' end'
    touchManagedPtr buffer
    touchManagedPtr start
    touchManagedPtr end
    freeMem name'
    return ()

#if defined(ENABLE_OVERLOADING)
data TextBufferApplyTagByNameMethodInfo
instance (signature ~ (T.Text -> Gtk.TextIter.TextIter -> Gtk.TextIter.TextIter -> m ()), MonadIO m, IsTextBuffer a) => O.OverloadedMethod TextBufferApplyTagByNameMethodInfo a signature where
    overloadedMethod = textBufferApplyTagByName

instance O.OverloadedMethodInfo TextBufferApplyTagByNameMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextBuffer.textBufferApplyTagByName",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextBuffer.html#v:textBufferApplyTagByName"
        })


#endif

-- method TextBuffer::backspace
-- method type : OrdinaryMethod
-- Args: [ Arg
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
--       , Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a position in @buffer"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "interactive"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "whether the deletion is caused by user interaction"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "default_editable"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "whether the buffer is editable by default"
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

foreign import ccall "gtk_text_buffer_backspace" gtk_text_buffer_backspace :: 
    Ptr TextBuffer ->                       -- buffer : TInterface (Name {namespace = "Gtk", name = "TextBuffer"})
    Ptr Gtk.TextIter.TextIter ->            -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    CInt ->                                 -- interactive : TBasicType TBoolean
    CInt ->                                 -- default_editable : TBasicType TBoolean
    IO CInt

-- | Performs the appropriate action as if the user hit the delete
-- key with the cursor at the position specified by /@iter@/. In the
-- normal case a single character will be deleted, but when
-- combining accents are involved, more than one character can
-- be deleted, and when precomposed character and accent combinations
-- are involved, less than one character will be deleted.
-- 
-- Because the buffer is modified, all outstanding iterators become
-- invalid after calling this function; however, the /@iter@/ will be
-- re-initialized to point to the location where text was deleted.
-- 
-- /Since: 2.6/
textBufferBackspace ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextBuffer a) =>
    a
    -- ^ /@buffer@/: a t'GI.Gtk.Objects.TextBuffer.TextBuffer'
    -> Gtk.TextIter.TextIter
    -- ^ /@iter@/: a position in /@buffer@/
    -> Bool
    -- ^ /@interactive@/: whether the deletion is caused by user interaction
    -> Bool
    -- ^ /@defaultEditable@/: whether the buffer is editable by default
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the buffer was modified
textBufferBackspace buffer iter interactive defaultEditable = liftIO $ do
    buffer' <- unsafeManagedPtrCastPtr buffer
    iter' <- unsafeManagedPtrGetPtr iter
    let interactive' = (fromIntegral . fromEnum) interactive
    let defaultEditable' = (fromIntegral . fromEnum) defaultEditable
    result <- gtk_text_buffer_backspace buffer' iter' interactive' defaultEditable'
    let result' = (/= 0) result
    touchManagedPtr buffer
    touchManagedPtr iter
    return result'

#if defined(ENABLE_OVERLOADING)
data TextBufferBackspaceMethodInfo
instance (signature ~ (Gtk.TextIter.TextIter -> Bool -> Bool -> m Bool), MonadIO m, IsTextBuffer a) => O.OverloadedMethod TextBufferBackspaceMethodInfo a signature where
    overloadedMethod = textBufferBackspace

instance O.OverloadedMethodInfo TextBufferBackspaceMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextBuffer.textBufferBackspace",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextBuffer.html#v:textBufferBackspace"
        })


#endif

-- method TextBuffer::begin_user_action
-- method type : OrdinaryMethod
-- Args: [ Arg
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
-- returnType: Nothing
-- throws : False
-- Skip return : False

foreign import ccall "gtk_text_buffer_begin_user_action" gtk_text_buffer_begin_user_action :: 
    Ptr TextBuffer ->                       -- buffer : TInterface (Name {namespace = "Gtk", name = "TextBuffer"})
    IO ()

-- | Called to indicate that the buffer operations between here and a
-- call to 'GI.Gtk.Objects.TextBuffer.textBufferEndUserAction' are part of a single
-- user-visible operation. The operations between
-- 'GI.Gtk.Objects.TextBuffer.textBufferBeginUserAction' and
-- 'GI.Gtk.Objects.TextBuffer.textBufferEndUserAction' can then be grouped when creating
-- an undo stack. t'GI.Gtk.Objects.TextBuffer.TextBuffer' maintains a count of calls to
-- 'GI.Gtk.Objects.TextBuffer.textBufferBeginUserAction' that have not been closed with
-- a call to 'GI.Gtk.Objects.TextBuffer.textBufferEndUserAction', and emits the
-- “begin-user-action” and “end-user-action” signals only for the
-- outermost pair of calls. This allows you to build user actions
-- from other user actions.
-- 
-- The “interactive” buffer mutation functions, such as
-- 'GI.Gtk.Objects.TextBuffer.textBufferInsertInteractive', automatically call begin\/end
-- user action around the buffer operations they perform, so there\'s
-- no need to add extra calls if you user action consists solely of a
-- single call to one of those functions.
textBufferBeginUserAction ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextBuffer a) =>
    a
    -- ^ /@buffer@/: a t'GI.Gtk.Objects.TextBuffer.TextBuffer'
    -> m ()
textBufferBeginUserAction buffer = liftIO $ do
    buffer' <- unsafeManagedPtrCastPtr buffer
    gtk_text_buffer_begin_user_action buffer'
    touchManagedPtr buffer
    return ()

#if defined(ENABLE_OVERLOADING)
data TextBufferBeginUserActionMethodInfo
instance (signature ~ (m ()), MonadIO m, IsTextBuffer a) => O.OverloadedMethod TextBufferBeginUserActionMethodInfo a signature where
    overloadedMethod = textBufferBeginUserAction

instance O.OverloadedMethodInfo TextBufferBeginUserActionMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextBuffer.textBufferBeginUserAction",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextBuffer.html#v:textBufferBeginUserAction"
        })


#endif

-- method TextBuffer::copy_clipboard
-- method type : OrdinaryMethod
-- Args: [ Arg
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
--       , Arg
--           { argCName = "clipboard"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Clipboard" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the #GtkClipboard object to copy to"
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

foreign import ccall "gtk_text_buffer_copy_clipboard" gtk_text_buffer_copy_clipboard :: 
    Ptr TextBuffer ->                       -- buffer : TInterface (Name {namespace = "Gtk", name = "TextBuffer"})
    Ptr Gtk.Clipboard.Clipboard ->          -- clipboard : TInterface (Name {namespace = "Gtk", name = "Clipboard"})
    IO ()

-- | Copies the currently-selected text to a clipboard.
textBufferCopyClipboard ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextBuffer a, Gtk.Clipboard.IsClipboard b) =>
    a
    -- ^ /@buffer@/: a t'GI.Gtk.Objects.TextBuffer.TextBuffer'
    -> b
    -- ^ /@clipboard@/: the t'GI.Gtk.Objects.Clipboard.Clipboard' object to copy to
    -> m ()
textBufferCopyClipboard buffer clipboard = liftIO $ do
    buffer' <- unsafeManagedPtrCastPtr buffer
    clipboard' <- unsafeManagedPtrCastPtr clipboard
    gtk_text_buffer_copy_clipboard buffer' clipboard'
    touchManagedPtr buffer
    touchManagedPtr clipboard
    return ()

#if defined(ENABLE_OVERLOADING)
data TextBufferCopyClipboardMethodInfo
instance (signature ~ (b -> m ()), MonadIO m, IsTextBuffer a, Gtk.Clipboard.IsClipboard b) => O.OverloadedMethod TextBufferCopyClipboardMethodInfo a signature where
    overloadedMethod = textBufferCopyClipboard

instance O.OverloadedMethodInfo TextBufferCopyClipboardMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextBuffer.textBufferCopyClipboard",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextBuffer.html#v:textBufferCopyClipboard"
        })


#endif

-- method TextBuffer::create_child_anchor
-- method type : OrdinaryMethod
-- Args: [ Arg
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
--       , Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "location in the buffer"
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
--               (TInterface Name { namespace = "Gtk" , name = "TextChildAnchor" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_text_buffer_create_child_anchor" gtk_text_buffer_create_child_anchor :: 
    Ptr TextBuffer ->                       -- buffer : TInterface (Name {namespace = "Gtk", name = "TextBuffer"})
    Ptr Gtk.TextIter.TextIter ->            -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    IO (Ptr Gtk.TextChildAnchor.TextChildAnchor)

-- | This is a convenience function which simply creates a child anchor
-- with 'GI.Gtk.Objects.TextChildAnchor.textChildAnchorNew' and inserts it into the buffer
-- with 'GI.Gtk.Objects.TextBuffer.textBufferInsertChildAnchor'. The new anchor is
-- owned by the buffer; no reference count is returned to
-- the caller of 'GI.Gtk.Objects.TextBuffer.textBufferCreateChildAnchor'.
textBufferCreateChildAnchor ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextBuffer a) =>
    a
    -- ^ /@buffer@/: a t'GI.Gtk.Objects.TextBuffer.TextBuffer'
    -> Gtk.TextIter.TextIter
    -- ^ /@iter@/: location in the buffer
    -> m Gtk.TextChildAnchor.TextChildAnchor
    -- ^ __Returns:__ the created child anchor
textBufferCreateChildAnchor buffer iter = liftIO $ do
    buffer' <- unsafeManagedPtrCastPtr buffer
    iter' <- unsafeManagedPtrGetPtr iter
    result <- gtk_text_buffer_create_child_anchor buffer' iter'
    checkUnexpectedReturnNULL "textBufferCreateChildAnchor" result
    result' <- (newObject Gtk.TextChildAnchor.TextChildAnchor) result
    touchManagedPtr buffer
    touchManagedPtr iter
    return result'

#if defined(ENABLE_OVERLOADING)
data TextBufferCreateChildAnchorMethodInfo
instance (signature ~ (Gtk.TextIter.TextIter -> m Gtk.TextChildAnchor.TextChildAnchor), MonadIO m, IsTextBuffer a) => O.OverloadedMethod TextBufferCreateChildAnchorMethodInfo a signature where
    overloadedMethod = textBufferCreateChildAnchor

instance O.OverloadedMethodInfo TextBufferCreateChildAnchorMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextBuffer.textBufferCreateChildAnchor",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextBuffer.html#v:textBufferCreateChildAnchor"
        })


#endif

-- method TextBuffer::create_mark
-- method type : OrdinaryMethod
-- Args: [ Arg
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
--       , Arg
--           { argCName = "mark_name"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "name for mark, or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "where"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "location to place mark"
--                 , sinceVersion = Nothing
--                 }
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
--                 { rawDocText = Just "whether the mark has left gravity"
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

foreign import ccall "gtk_text_buffer_create_mark" gtk_text_buffer_create_mark :: 
    Ptr TextBuffer ->                       -- buffer : TInterface (Name {namespace = "Gtk", name = "TextBuffer"})
    CString ->                              -- mark_name : TBasicType TUTF8
    Ptr Gtk.TextIter.TextIter ->            -- where : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    CInt ->                                 -- left_gravity : TBasicType TBoolean
    IO (Ptr Gtk.TextMark.TextMark)

-- | Creates a mark at position /@where@/. If /@markName@/ is 'P.Nothing', the mark
-- is anonymous; otherwise, the mark can be retrieved by name using
-- 'GI.Gtk.Objects.TextBuffer.textBufferGetMark'. If a mark has left gravity, and text is
-- inserted at the mark’s current location, the mark will be moved to
-- the left of the newly-inserted text. If the mark has right gravity
-- (/@leftGravity@/ = 'P.False'), the mark will end up on the right of
-- newly-inserted text. The standard left-to-right cursor is a mark
-- with right gravity (when you type, the cursor stays on the right
-- side of the text you’re typing).
-- 
-- The caller of this function does not own a
-- reference to the returned t'GI.Gtk.Objects.TextMark.TextMark', so you can ignore the
-- return value if you like. Marks are owned by the buffer and go
-- away when the buffer does.
-- 
-- Emits the [TextBuffer::markSet]("GI.Gtk.Objects.TextBuffer#g:signal:markSet") signal as notification of the mark\'s
-- initial placement.
textBufferCreateMark ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextBuffer a) =>
    a
    -- ^ /@buffer@/: a t'GI.Gtk.Objects.TextBuffer.TextBuffer'
    -> Maybe (T.Text)
    -- ^ /@markName@/: name for mark, or 'P.Nothing'
    -> Gtk.TextIter.TextIter
    -- ^ /@where@/: location to place mark
    -> Bool
    -- ^ /@leftGravity@/: whether the mark has left gravity
    -> m Gtk.TextMark.TextMark
    -- ^ __Returns:__ the new t'GI.Gtk.Objects.TextMark.TextMark' object
textBufferCreateMark buffer markName where_ leftGravity = liftIO $ do
    buffer' <- unsafeManagedPtrCastPtr buffer
    maybeMarkName <- case markName of
        Nothing -> return nullPtr
        Just jMarkName -> do
            jMarkName' <- textToCString jMarkName
            return jMarkName'
    where_' <- unsafeManagedPtrGetPtr where_
    let leftGravity' = (fromIntegral . fromEnum) leftGravity
    result <- gtk_text_buffer_create_mark buffer' maybeMarkName where_' leftGravity'
    checkUnexpectedReturnNULL "textBufferCreateMark" result
    result' <- (newObject Gtk.TextMark.TextMark) result
    touchManagedPtr buffer
    touchManagedPtr where_
    freeMem maybeMarkName
    return result'

#if defined(ENABLE_OVERLOADING)
data TextBufferCreateMarkMethodInfo
instance (signature ~ (Maybe (T.Text) -> Gtk.TextIter.TextIter -> Bool -> m Gtk.TextMark.TextMark), MonadIO m, IsTextBuffer a) => O.OverloadedMethod TextBufferCreateMarkMethodInfo a signature where
    overloadedMethod = textBufferCreateMark

instance O.OverloadedMethodInfo TextBufferCreateMarkMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextBuffer.textBufferCreateMark",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextBuffer.html#v:textBufferCreateMark"
        })


#endif

-- method TextBuffer::cut_clipboard
-- method type : OrdinaryMethod
-- Args: [ Arg
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
--       , Arg
--           { argCName = "clipboard"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Clipboard" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the #GtkClipboard object to cut to"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "default_editable"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "default editability of the buffer"
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

foreign import ccall "gtk_text_buffer_cut_clipboard" gtk_text_buffer_cut_clipboard :: 
    Ptr TextBuffer ->                       -- buffer : TInterface (Name {namespace = "Gtk", name = "TextBuffer"})
    Ptr Gtk.Clipboard.Clipboard ->          -- clipboard : TInterface (Name {namespace = "Gtk", name = "Clipboard"})
    CInt ->                                 -- default_editable : TBasicType TBoolean
    IO ()

-- | Copies the currently-selected text to a clipboard, then deletes
-- said text if it’s editable.
textBufferCutClipboard ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextBuffer a, Gtk.Clipboard.IsClipboard b) =>
    a
    -- ^ /@buffer@/: a t'GI.Gtk.Objects.TextBuffer.TextBuffer'
    -> b
    -- ^ /@clipboard@/: the t'GI.Gtk.Objects.Clipboard.Clipboard' object to cut to
    -> Bool
    -- ^ /@defaultEditable@/: default editability of the buffer
    -> m ()
textBufferCutClipboard buffer clipboard defaultEditable = liftIO $ do
    buffer' <- unsafeManagedPtrCastPtr buffer
    clipboard' <- unsafeManagedPtrCastPtr clipboard
    let defaultEditable' = (fromIntegral . fromEnum) defaultEditable
    gtk_text_buffer_cut_clipboard buffer' clipboard' defaultEditable'
    touchManagedPtr buffer
    touchManagedPtr clipboard
    return ()

#if defined(ENABLE_OVERLOADING)
data TextBufferCutClipboardMethodInfo
instance (signature ~ (b -> Bool -> m ()), MonadIO m, IsTextBuffer a, Gtk.Clipboard.IsClipboard b) => O.OverloadedMethod TextBufferCutClipboardMethodInfo a signature where
    overloadedMethod = textBufferCutClipboard

instance O.OverloadedMethodInfo TextBufferCutClipboardMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextBuffer.textBufferCutClipboard",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextBuffer.html#v:textBufferCutClipboard"
        })


#endif

-- method TextBuffer::delete
-- method type : OrdinaryMethod
-- Args: [ Arg
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
--       , Arg
--           { argCName = "start"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a position in @buffer"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "end"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "another position in @buffer"
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

foreign import ccall "gtk_text_buffer_delete" gtk_text_buffer_delete :: 
    Ptr TextBuffer ->                       -- buffer : TInterface (Name {namespace = "Gtk", name = "TextBuffer"})
    Ptr Gtk.TextIter.TextIter ->            -- start : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    Ptr Gtk.TextIter.TextIter ->            -- end : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    IO ()

-- | Deletes text between /@start@/ and /@end@/. The order of /@start@/ and /@end@/
-- is not actually relevant; 'GI.Gtk.Objects.TextBuffer.textBufferDelete' will reorder
-- them. This function actually emits the “delete-range” signal, and
-- the default handler of that signal deletes the text. Because the
-- buffer is modified, all outstanding iterators become invalid after
-- calling this function; however, the /@start@/ and /@end@/ will be
-- re-initialized to point to the location where text was deleted.
textBufferDelete ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextBuffer a) =>
    a
    -- ^ /@buffer@/: a t'GI.Gtk.Objects.TextBuffer.TextBuffer'
    -> Gtk.TextIter.TextIter
    -- ^ /@start@/: a position in /@buffer@/
    -> Gtk.TextIter.TextIter
    -- ^ /@end@/: another position in /@buffer@/
    -> m ()
textBufferDelete buffer start end = liftIO $ do
    buffer' <- unsafeManagedPtrCastPtr buffer
    start' <- unsafeManagedPtrGetPtr start
    end' <- unsafeManagedPtrGetPtr end
    gtk_text_buffer_delete buffer' start' end'
    touchManagedPtr buffer
    touchManagedPtr start
    touchManagedPtr end
    return ()

#if defined(ENABLE_OVERLOADING)
data TextBufferDeleteMethodInfo
instance (signature ~ (Gtk.TextIter.TextIter -> Gtk.TextIter.TextIter -> m ()), MonadIO m, IsTextBuffer a) => O.OverloadedMethod TextBufferDeleteMethodInfo a signature where
    overloadedMethod = textBufferDelete

instance O.OverloadedMethodInfo TextBufferDeleteMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextBuffer.textBufferDelete",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextBuffer.html#v:textBufferDelete"
        })


#endif

-- method TextBuffer::delete_interactive
-- method type : OrdinaryMethod
-- Args: [ Arg
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
--       , Arg
--           { argCName = "start_iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "start of range to delete"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "end_iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "end of range" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "default_editable"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "whether the buffer is editable by default"
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

foreign import ccall "gtk_text_buffer_delete_interactive" gtk_text_buffer_delete_interactive :: 
    Ptr TextBuffer ->                       -- buffer : TInterface (Name {namespace = "Gtk", name = "TextBuffer"})
    Ptr Gtk.TextIter.TextIter ->            -- start_iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    Ptr Gtk.TextIter.TextIter ->            -- end_iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    CInt ->                                 -- default_editable : TBasicType TBoolean
    IO CInt

-- | Deletes all editable text in the given range.
-- Calls 'GI.Gtk.Objects.TextBuffer.textBufferDelete' for each editable sub-range of
-- [/@start@/,/@end@/). /@start@/ and /@end@/ are revalidated to point to
-- the location of the last deleted range, or left untouched if
-- no text was deleted.
textBufferDeleteInteractive ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextBuffer a) =>
    a
    -- ^ /@buffer@/: a t'GI.Gtk.Objects.TextBuffer.TextBuffer'
    -> Gtk.TextIter.TextIter
    -- ^ /@startIter@/: start of range to delete
    -> Gtk.TextIter.TextIter
    -- ^ /@endIter@/: end of range
    -> Bool
    -- ^ /@defaultEditable@/: whether the buffer is editable by default
    -> m Bool
    -- ^ __Returns:__ whether some text was actually deleted
textBufferDeleteInteractive buffer startIter endIter defaultEditable = liftIO $ do
    buffer' <- unsafeManagedPtrCastPtr buffer
    startIter' <- unsafeManagedPtrGetPtr startIter
    endIter' <- unsafeManagedPtrGetPtr endIter
    let defaultEditable' = (fromIntegral . fromEnum) defaultEditable
    result <- gtk_text_buffer_delete_interactive buffer' startIter' endIter' defaultEditable'
    let result' = (/= 0) result
    touchManagedPtr buffer
    touchManagedPtr startIter
    touchManagedPtr endIter
    return result'

#if defined(ENABLE_OVERLOADING)
data TextBufferDeleteInteractiveMethodInfo
instance (signature ~ (Gtk.TextIter.TextIter -> Gtk.TextIter.TextIter -> Bool -> m Bool), MonadIO m, IsTextBuffer a) => O.OverloadedMethod TextBufferDeleteInteractiveMethodInfo a signature where
    overloadedMethod = textBufferDeleteInteractive

instance O.OverloadedMethodInfo TextBufferDeleteInteractiveMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextBuffer.textBufferDeleteInteractive",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextBuffer.html#v:textBufferDeleteInteractive"
        })


#endif

-- method TextBuffer::delete_mark
-- method type : OrdinaryMethod
-- Args: [ Arg
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
--       , Arg
--           { argCName = "mark"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextMark" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextMark in @buffer"
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

foreign import ccall "gtk_text_buffer_delete_mark" gtk_text_buffer_delete_mark :: 
    Ptr TextBuffer ->                       -- buffer : TInterface (Name {namespace = "Gtk", name = "TextBuffer"})
    Ptr Gtk.TextMark.TextMark ->            -- mark : TInterface (Name {namespace = "Gtk", name = "TextMark"})
    IO ()

-- | Deletes /@mark@/, so that it’s no longer located anywhere in the
-- buffer. Removes the reference the buffer holds to the mark, so if
-- you haven’t called 'GI.GObject.Objects.Object.objectRef' on the mark, it will be freed. Even
-- if the mark isn’t freed, most operations on /@mark@/ become
-- invalid, until it gets added to a buffer again with
-- 'GI.Gtk.Objects.TextBuffer.textBufferAddMark'. Use 'GI.Gtk.Objects.TextMark.textMarkGetDeleted' to
-- find out if a mark has been removed from its buffer.
-- The [TextBuffer::markDeleted]("GI.Gtk.Objects.TextBuffer#g:signal:markDeleted") signal will be emitted as notification after
-- the mark is deleted.
textBufferDeleteMark ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextBuffer a, Gtk.TextMark.IsTextMark b) =>
    a
    -- ^ /@buffer@/: a t'GI.Gtk.Objects.TextBuffer.TextBuffer'
    -> b
    -- ^ /@mark@/: a t'GI.Gtk.Objects.TextMark.TextMark' in /@buffer@/
    -> m ()
textBufferDeleteMark buffer mark = liftIO $ do
    buffer' <- unsafeManagedPtrCastPtr buffer
    mark' <- unsafeManagedPtrCastPtr mark
    gtk_text_buffer_delete_mark buffer' mark'
    touchManagedPtr buffer
    touchManagedPtr mark
    return ()

#if defined(ENABLE_OVERLOADING)
data TextBufferDeleteMarkMethodInfo
instance (signature ~ (b -> m ()), MonadIO m, IsTextBuffer a, Gtk.TextMark.IsTextMark b) => O.OverloadedMethod TextBufferDeleteMarkMethodInfo a signature where
    overloadedMethod = textBufferDeleteMark

instance O.OverloadedMethodInfo TextBufferDeleteMarkMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextBuffer.textBufferDeleteMark",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextBuffer.html#v:textBufferDeleteMark"
        })


#endif

-- method TextBuffer::delete_mark_by_name
-- method type : OrdinaryMethod
-- Args: [ Arg
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
--       , Arg
--           { argCName = "name"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "name of a mark in @buffer"
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

foreign import ccall "gtk_text_buffer_delete_mark_by_name" gtk_text_buffer_delete_mark_by_name :: 
    Ptr TextBuffer ->                       -- buffer : TInterface (Name {namespace = "Gtk", name = "TextBuffer"})
    CString ->                              -- name : TBasicType TUTF8
    IO ()

-- | Deletes the mark named /@name@/; the mark must exist. See
-- 'GI.Gtk.Objects.TextBuffer.textBufferDeleteMark' for details.
textBufferDeleteMarkByName ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextBuffer a) =>
    a
    -- ^ /@buffer@/: a t'GI.Gtk.Objects.TextBuffer.TextBuffer'
    -> T.Text
    -- ^ /@name@/: name of a mark in /@buffer@/
    -> m ()
textBufferDeleteMarkByName buffer name = liftIO $ do
    buffer' <- unsafeManagedPtrCastPtr buffer
    name' <- textToCString name
    gtk_text_buffer_delete_mark_by_name buffer' name'
    touchManagedPtr buffer
    freeMem name'
    return ()

#if defined(ENABLE_OVERLOADING)
data TextBufferDeleteMarkByNameMethodInfo
instance (signature ~ (T.Text -> m ()), MonadIO m, IsTextBuffer a) => O.OverloadedMethod TextBufferDeleteMarkByNameMethodInfo a signature where
    overloadedMethod = textBufferDeleteMarkByName

instance O.OverloadedMethodInfo TextBufferDeleteMarkByNameMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextBuffer.textBufferDeleteMarkByName",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextBuffer.html#v:textBufferDeleteMarkByName"
        })


#endif

-- method TextBuffer::delete_selection
-- method type : OrdinaryMethod
-- Args: [ Arg
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
--       , Arg
--           { argCName = "interactive"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "whether the deletion is caused by user interaction"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "default_editable"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "whether the buffer is editable by default"
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

foreign import ccall "gtk_text_buffer_delete_selection" gtk_text_buffer_delete_selection :: 
    Ptr TextBuffer ->                       -- buffer : TInterface (Name {namespace = "Gtk", name = "TextBuffer"})
    CInt ->                                 -- interactive : TBasicType TBoolean
    CInt ->                                 -- default_editable : TBasicType TBoolean
    IO CInt

-- | Deletes the range between the “insert” and “selection_bound” marks,
-- that is, the currently-selected text. If /@interactive@/ is 'P.True',
-- the editability of the selection will be considered (users can’t delete
-- uneditable text).
textBufferDeleteSelection ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextBuffer a) =>
    a
    -- ^ /@buffer@/: a t'GI.Gtk.Objects.TextBuffer.TextBuffer'
    -> Bool
    -- ^ /@interactive@/: whether the deletion is caused by user interaction
    -> Bool
    -- ^ /@defaultEditable@/: whether the buffer is editable by default
    -> m Bool
    -- ^ __Returns:__ whether there was a non-empty selection to delete
textBufferDeleteSelection buffer interactive defaultEditable = liftIO $ do
    buffer' <- unsafeManagedPtrCastPtr buffer
    let interactive' = (fromIntegral . fromEnum) interactive
    let defaultEditable' = (fromIntegral . fromEnum) defaultEditable
    result <- gtk_text_buffer_delete_selection buffer' interactive' defaultEditable'
    let result' = (/= 0) result
    touchManagedPtr buffer
    return result'

#if defined(ENABLE_OVERLOADING)
data TextBufferDeleteSelectionMethodInfo
instance (signature ~ (Bool -> Bool -> m Bool), MonadIO m, IsTextBuffer a) => O.OverloadedMethod TextBufferDeleteSelectionMethodInfo a signature where
    overloadedMethod = textBufferDeleteSelection

instance O.OverloadedMethodInfo TextBufferDeleteSelectionMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextBuffer.textBufferDeleteSelection",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextBuffer.html#v:textBufferDeleteSelection"
        })


#endif

-- method TextBuffer::deserialize
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "register_buffer"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextBuffer" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the #GtkTextBuffer @format is registered with"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "content_buffer"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextBuffer" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the #GtkTextBuffer to deserialize into"
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
--           , argType = TInterface Name { namespace = "Gdk" , name = "Atom" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the rich text format to use for deserializing"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "insertion point for the deserialized text"
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
--           , argType = TCArray False (-1) 5 (TBasicType TUInt8)
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "data to deserialize"
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
--           , argType = TBasicType TUInt64
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "length of @data" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: [ Arg
--              { argCName = "length"
--              , argType = TBasicType TUInt64
--              , direction = DirectionIn
--              , mayBeNull = False
--              , argDoc =
--                  Documentation
--                    { rawDocText = Just "length of @data" , sinceVersion = Nothing }
--              , argScope = ScopeTypeInvalid
--              , argClosure = -1
--              , argDestroy = -1
--              , argCallerAllocates = False
--              , transfer = TransferNothing
--              }
--          ]
-- returnType: Just (TBasicType TBoolean)
-- throws : True
-- Skip return : False

foreign import ccall "gtk_text_buffer_deserialize" gtk_text_buffer_deserialize :: 
    Ptr TextBuffer ->                       -- register_buffer : TInterface (Name {namespace = "Gtk", name = "TextBuffer"})
    Ptr TextBuffer ->                       -- content_buffer : TInterface (Name {namespace = "Gtk", name = "TextBuffer"})
    Ptr Gdk.Atom.Atom ->                    -- format : TInterface (Name {namespace = "Gdk", name = "Atom"})
    Ptr Gtk.TextIter.TextIter ->            -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    Ptr Word8 ->                            -- data : TCArray False (-1) 5 (TBasicType TUInt8)
    Word64 ->                               -- length : TBasicType TUInt64
    Ptr (Ptr GError) ->                     -- error
    IO CInt

-- | This function deserializes rich text in format /@format@/ and inserts
-- it at /@iter@/.
-- 
-- /@formats@/ to be used must be registered using
-- 'GI.Gtk.Objects.TextBuffer.textBufferRegisterDeserializeFormat' or
-- 'GI.Gtk.Objects.TextBuffer.textBufferRegisterDeserializeTagset' beforehand.
-- 
-- /Since: 2.10/
textBufferDeserialize ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextBuffer a, IsTextBuffer b) =>
    a
    -- ^ /@registerBuffer@/: the t'GI.Gtk.Objects.TextBuffer.TextBuffer' /@format@/ is registered with
    -> b
    -- ^ /@contentBuffer@/: the t'GI.Gtk.Objects.TextBuffer.TextBuffer' to deserialize into
    -> Gdk.Atom.Atom
    -- ^ /@format@/: the rich text format to use for deserializing
    -> Gtk.TextIter.TextIter
    -- ^ /@iter@/: insertion point for the deserialized text
    -> ByteString
    -- ^ /@data@/: data to deserialize
    -> m ()
    -- ^ /(Can throw 'Data.GI.Base.GError.GError')/
textBufferDeserialize registerBuffer contentBuffer format iter data_ = liftIO $ do
    let length_ = fromIntegral $ B.length data_
    registerBuffer' <- unsafeManagedPtrCastPtr registerBuffer
    contentBuffer' <- unsafeManagedPtrCastPtr contentBuffer
    format' <- unsafeManagedPtrGetPtr format
    iter' <- unsafeManagedPtrGetPtr iter
    data_' <- packByteString data_
    onException (do
        _ <- propagateGError $ gtk_text_buffer_deserialize registerBuffer' contentBuffer' format' iter' data_' length_
        touchManagedPtr registerBuffer
        touchManagedPtr contentBuffer
        touchManagedPtr format
        touchManagedPtr iter
        freeMem data_'
        return ()
     ) (do
        freeMem data_'
     )

#if defined(ENABLE_OVERLOADING)
data TextBufferDeserializeMethodInfo
instance (signature ~ (b -> Gdk.Atom.Atom -> Gtk.TextIter.TextIter -> ByteString -> m ()), MonadIO m, IsTextBuffer a, IsTextBuffer b) => O.OverloadedMethod TextBufferDeserializeMethodInfo a signature where
    overloadedMethod = textBufferDeserialize

instance O.OverloadedMethodInfo TextBufferDeserializeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextBuffer.textBufferDeserialize",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextBuffer.html#v:textBufferDeserialize"
        })


#endif

-- method TextBuffer::deserialize_get_can_create_tags
-- method type : OrdinaryMethod
-- Args: [ Arg
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
--       , Arg
--           { argCName = "format"
--           , argType = TInterface Name { namespace = "Gdk" , name = "Atom" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "a #GdkAtom representing a registered rich text format"
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

foreign import ccall "gtk_text_buffer_deserialize_get_can_create_tags" gtk_text_buffer_deserialize_get_can_create_tags :: 
    Ptr TextBuffer ->                       -- buffer : TInterface (Name {namespace = "Gtk", name = "TextBuffer"})
    Ptr Gdk.Atom.Atom ->                    -- format : TInterface (Name {namespace = "Gdk", name = "Atom"})
    IO CInt

-- | This functions returns the value set with
-- 'GI.Gtk.Objects.TextBuffer.textBufferDeserializeSetCanCreateTags'
-- 
-- /Since: 2.10/
textBufferDeserializeGetCanCreateTags ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextBuffer a) =>
    a
    -- ^ /@buffer@/: a t'GI.Gtk.Objects.TextBuffer.TextBuffer'
    -> Gdk.Atom.Atom
    -- ^ /@format@/: a t'GI.Gdk.Structs.Atom.Atom' representing a registered rich text format
    -> m Bool
    -- ^ __Returns:__ whether deserializing this format may create tags
textBufferDeserializeGetCanCreateTags buffer format = liftIO $ do
    buffer' <- unsafeManagedPtrCastPtr buffer
    format' <- unsafeManagedPtrGetPtr format
    result <- gtk_text_buffer_deserialize_get_can_create_tags buffer' format'
    let result' = (/= 0) result
    touchManagedPtr buffer
    touchManagedPtr format
    return result'

#if defined(ENABLE_OVERLOADING)
data TextBufferDeserializeGetCanCreateTagsMethodInfo
instance (signature ~ (Gdk.Atom.Atom -> m Bool), MonadIO m, IsTextBuffer a) => O.OverloadedMethod TextBufferDeserializeGetCanCreateTagsMethodInfo a signature where
    overloadedMethod = textBufferDeserializeGetCanCreateTags

instance O.OverloadedMethodInfo TextBufferDeserializeGetCanCreateTagsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextBuffer.textBufferDeserializeGetCanCreateTags",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextBuffer.html#v:textBufferDeserializeGetCanCreateTags"
        })


#endif

-- method TextBuffer::deserialize_set_can_create_tags
-- method type : OrdinaryMethod
-- Args: [ Arg
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
--       , Arg
--           { argCName = "format"
--           , argType = TInterface Name { namespace = "Gdk" , name = "Atom" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "a #GdkAtom representing a registered rich text format"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "can_create_tags"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "whether deserializing this format may create tags"
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

foreign import ccall "gtk_text_buffer_deserialize_set_can_create_tags" gtk_text_buffer_deserialize_set_can_create_tags :: 
    Ptr TextBuffer ->                       -- buffer : TInterface (Name {namespace = "Gtk", name = "TextBuffer"})
    Ptr Gdk.Atom.Atom ->                    -- format : TInterface (Name {namespace = "Gdk", name = "Atom"})
    CInt ->                                 -- can_create_tags : TBasicType TBoolean
    IO ()

-- | Use this function to allow a rich text deserialization function to
-- create new tags in the receiving buffer. Note that using this
-- function is almost always a bad idea, because the rich text
-- functions you register should know how to map the rich text format
-- they handler to your text buffers set of tags.
-- 
-- The ability of creating new (arbitrary!) tags in the receiving buffer
-- is meant for special rich text formats like the internal one that
-- is registered using 'GI.Gtk.Objects.TextBuffer.textBufferRegisterDeserializeTagset',
-- because that format is essentially a dump of the internal structure
-- of the source buffer, including its tag names.
-- 
-- You should allow creation of tags only if you know what you are
-- doing, e.g. if you defined a tagset name for your application
-- suite’s text buffers and you know that it’s fine to receive new
-- tags from these buffers, because you know that your application can
-- handle the newly created tags.
-- 
-- /Since: 2.10/
textBufferDeserializeSetCanCreateTags ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextBuffer a) =>
    a
    -- ^ /@buffer@/: a t'GI.Gtk.Objects.TextBuffer.TextBuffer'
    -> Gdk.Atom.Atom
    -- ^ /@format@/: a t'GI.Gdk.Structs.Atom.Atom' representing a registered rich text format
    -> Bool
    -- ^ /@canCreateTags@/: whether deserializing this format may create tags
    -> m ()
textBufferDeserializeSetCanCreateTags buffer format canCreateTags = liftIO $ do
    buffer' <- unsafeManagedPtrCastPtr buffer
    format' <- unsafeManagedPtrGetPtr format
    let canCreateTags' = (fromIntegral . fromEnum) canCreateTags
    gtk_text_buffer_deserialize_set_can_create_tags buffer' format' canCreateTags'
    touchManagedPtr buffer
    touchManagedPtr format
    return ()

#if defined(ENABLE_OVERLOADING)
data TextBufferDeserializeSetCanCreateTagsMethodInfo
instance (signature ~ (Gdk.Atom.Atom -> Bool -> m ()), MonadIO m, IsTextBuffer a) => O.OverloadedMethod TextBufferDeserializeSetCanCreateTagsMethodInfo a signature where
    overloadedMethod = textBufferDeserializeSetCanCreateTags

instance O.OverloadedMethodInfo TextBufferDeserializeSetCanCreateTagsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextBuffer.textBufferDeserializeSetCanCreateTags",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextBuffer.html#v:textBufferDeserializeSetCanCreateTags"
        })


#endif

-- method TextBuffer::end_user_action
-- method type : OrdinaryMethod
-- Args: [ Arg
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
-- returnType: Nothing
-- throws : False
-- Skip return : False

foreign import ccall "gtk_text_buffer_end_user_action" gtk_text_buffer_end_user_action :: 
    Ptr TextBuffer ->                       -- buffer : TInterface (Name {namespace = "Gtk", name = "TextBuffer"})
    IO ()

-- | Should be paired with a call to 'GI.Gtk.Objects.TextBuffer.textBufferBeginUserAction'.
-- See that function for a full explanation.
textBufferEndUserAction ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextBuffer a) =>
    a
    -- ^ /@buffer@/: a t'GI.Gtk.Objects.TextBuffer.TextBuffer'
    -> m ()
textBufferEndUserAction buffer = liftIO $ do
    buffer' <- unsafeManagedPtrCastPtr buffer
    gtk_text_buffer_end_user_action buffer'
    touchManagedPtr buffer
    return ()

#if defined(ENABLE_OVERLOADING)
data TextBufferEndUserActionMethodInfo
instance (signature ~ (m ()), MonadIO m, IsTextBuffer a) => O.OverloadedMethod TextBufferEndUserActionMethodInfo a signature where
    overloadedMethod = textBufferEndUserAction

instance O.OverloadedMethodInfo TextBufferEndUserActionMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextBuffer.textBufferEndUserAction",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextBuffer.html#v:textBufferEndUserAction"
        })


#endif

-- method TextBuffer::get_bounds
-- method type : OrdinaryMethod
-- Args: [ Arg
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
--       , Arg
--           { argCName = "start"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "iterator to initialize with first position in the buffer"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = True
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "end"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "iterator to initialize with the end iterator"
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

foreign import ccall "gtk_text_buffer_get_bounds" gtk_text_buffer_get_bounds :: 
    Ptr TextBuffer ->                       -- buffer : TInterface (Name {namespace = "Gtk", name = "TextBuffer"})
    Ptr Gtk.TextIter.TextIter ->            -- start : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    Ptr Gtk.TextIter.TextIter ->            -- end : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    IO ()

-- | Retrieves the first and last iterators in the buffer, i.e. the
-- entire buffer lies within the range [/@start@/,/@end@/).
textBufferGetBounds ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextBuffer a) =>
    a
    -- ^ /@buffer@/: a t'GI.Gtk.Objects.TextBuffer.TextBuffer'
    -> m ((Gtk.TextIter.TextIter, Gtk.TextIter.TextIter))
textBufferGetBounds buffer = liftIO $ do
    buffer' <- unsafeManagedPtrCastPtr buffer
    start <- SP.callocBoxedBytes 80 :: IO (Ptr Gtk.TextIter.TextIter)
    end <- SP.callocBoxedBytes 80 :: IO (Ptr Gtk.TextIter.TextIter)
    gtk_text_buffer_get_bounds buffer' start end
    start' <- (wrapBoxed Gtk.TextIter.TextIter) start
    end' <- (wrapBoxed Gtk.TextIter.TextIter) end
    touchManagedPtr buffer
    return (start', end')

#if defined(ENABLE_OVERLOADING)
data TextBufferGetBoundsMethodInfo
instance (signature ~ (m ((Gtk.TextIter.TextIter, Gtk.TextIter.TextIter))), MonadIO m, IsTextBuffer a) => O.OverloadedMethod TextBufferGetBoundsMethodInfo a signature where
    overloadedMethod = textBufferGetBounds

instance O.OverloadedMethodInfo TextBufferGetBoundsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextBuffer.textBufferGetBounds",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextBuffer.html#v:textBufferGetBounds"
        })


#endif

-- method TextBuffer::get_char_count
-- method type : OrdinaryMethod
-- Args: [ Arg
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
-- returnType: Just (TBasicType TInt)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_text_buffer_get_char_count" gtk_text_buffer_get_char_count :: 
    Ptr TextBuffer ->                       -- buffer : TInterface (Name {namespace = "Gtk", name = "TextBuffer"})
    IO Int32

-- | Gets the number of characters in the buffer; note that characters
-- and bytes are not the same, you can’t e.g. expect the contents of
-- the buffer in string form to be this many bytes long. The character
-- count is cached, so this function is very fast.
textBufferGetCharCount ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextBuffer a) =>
    a
    -- ^ /@buffer@/: a t'GI.Gtk.Objects.TextBuffer.TextBuffer'
    -> m Int32
    -- ^ __Returns:__ number of characters in the buffer
textBufferGetCharCount buffer = liftIO $ do
    buffer' <- unsafeManagedPtrCastPtr buffer
    result <- gtk_text_buffer_get_char_count buffer'
    touchManagedPtr buffer
    return result

#if defined(ENABLE_OVERLOADING)
data TextBufferGetCharCountMethodInfo
instance (signature ~ (m Int32), MonadIO m, IsTextBuffer a) => O.OverloadedMethod TextBufferGetCharCountMethodInfo a signature where
    overloadedMethod = textBufferGetCharCount

instance O.OverloadedMethodInfo TextBufferGetCharCountMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextBuffer.textBufferGetCharCount",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextBuffer.html#v:textBufferGetCharCount"
        })


#endif

-- method TextBuffer::get_copy_target_list
-- method type : OrdinaryMethod
-- Args: [ Arg
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "TargetList" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_text_buffer_get_copy_target_list" gtk_text_buffer_get_copy_target_list :: 
    Ptr TextBuffer ->                       -- buffer : TInterface (Name {namespace = "Gtk", name = "TextBuffer"})
    IO (Ptr Gtk.TargetList.TargetList)

-- | This function returns the list of targets this text buffer can
-- provide for copying and as DND source. The targets in the list are
-- added with /@info@/ values from the t'GI.Gtk.Enums.TextBufferTargetInfo' enum,
-- using 'GI.Gtk.Structs.TargetList.targetListAddRichTextTargets' and
-- 'GI.Gtk.Structs.TargetList.targetListAddTextTargets'.
-- 
-- /Since: 2.10/
textBufferGetCopyTargetList ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextBuffer a) =>
    a
    -- ^ /@buffer@/: a t'GI.Gtk.Objects.TextBuffer.TextBuffer'
    -> m Gtk.TargetList.TargetList
    -- ^ __Returns:__ the t'GI.Gtk.Structs.TargetList.TargetList'
textBufferGetCopyTargetList buffer = liftIO $ do
    buffer' <- unsafeManagedPtrCastPtr buffer
    result <- gtk_text_buffer_get_copy_target_list buffer'
    checkUnexpectedReturnNULL "textBufferGetCopyTargetList" result
    result' <- (newBoxed Gtk.TargetList.TargetList) result
    touchManagedPtr buffer
    return result'

#if defined(ENABLE_OVERLOADING)
data TextBufferGetCopyTargetListMethodInfo
instance (signature ~ (m Gtk.TargetList.TargetList), MonadIO m, IsTextBuffer a) => O.OverloadedMethod TextBufferGetCopyTargetListMethodInfo a signature where
    overloadedMethod = textBufferGetCopyTargetList

instance O.OverloadedMethodInfo TextBufferGetCopyTargetListMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextBuffer.textBufferGetCopyTargetList",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextBuffer.html#v:textBufferGetCopyTargetList"
        })


#endif

-- method TextBuffer::get_deserialize_formats
-- method type : OrdinaryMethod
-- Args: [ Arg
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
--       , Arg
--           { argCName = "n_formats"
--           , argType = TBasicType TInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "return location for the number of formats"
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
--              { argCName = "n_formats"
--              , argType = TBasicType TInt
--              , direction = DirectionOut
--              , mayBeNull = False
--              , argDoc =
--                  Documentation
--                    { rawDocText = Just "return location for the number of formats"
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
--                  (TInterface Name { namespace = "Gdk" , name = "Atom" }))
-- throws : False
-- Skip return : False

foreign import ccall "gtk_text_buffer_get_deserialize_formats" gtk_text_buffer_get_deserialize_formats :: 
    Ptr TextBuffer ->                       -- buffer : TInterface (Name {namespace = "Gtk", name = "TextBuffer"})
    Ptr Int32 ->                            -- n_formats : TBasicType TInt
    IO (Ptr (Ptr Gdk.Atom.Atom))

-- | This function returns the rich text deserialize formats registered
-- with /@buffer@/ using 'GI.Gtk.Objects.TextBuffer.textBufferRegisterDeserializeFormat' or
-- 'GI.Gtk.Objects.TextBuffer.textBufferRegisterDeserializeTagset'
-- 
-- /Since: 2.10/
textBufferGetDeserializeFormats ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextBuffer a) =>
    a
    -- ^ /@buffer@/: a t'GI.Gtk.Objects.TextBuffer.TextBuffer'
    -> m [Gdk.Atom.Atom]
    -- ^ __Returns:__ an array of
    --               @/GdkAtoms/@ representing the registered formats.
textBufferGetDeserializeFormats buffer = liftIO $ do
    buffer' <- unsafeManagedPtrCastPtr buffer
    nFormats <- allocMem :: IO (Ptr Int32)
    result <- gtk_text_buffer_get_deserialize_formats buffer' nFormats
    nFormats' <- peek nFormats
    checkUnexpectedReturnNULL "textBufferGetDeserializeFormats" result
    result' <- (unpackPtrArrayWithLength nFormats') result
    result'' <- mapM (newPtr Gdk.Atom.Atom) result'
    freeMem result
    touchManagedPtr buffer
    freeMem nFormats
    return result''

#if defined(ENABLE_OVERLOADING)
data TextBufferGetDeserializeFormatsMethodInfo
instance (signature ~ (m [Gdk.Atom.Atom]), MonadIO m, IsTextBuffer a) => O.OverloadedMethod TextBufferGetDeserializeFormatsMethodInfo a signature where
    overloadedMethod = textBufferGetDeserializeFormats

instance O.OverloadedMethodInfo TextBufferGetDeserializeFormatsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextBuffer.textBufferGetDeserializeFormats",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextBuffer.html#v:textBufferGetDeserializeFormats"
        })


#endif

-- method TextBuffer::get_end_iter
-- method type : OrdinaryMethod
-- Args: [ Arg
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
--       , Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "iterator to initialize"
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

foreign import ccall "gtk_text_buffer_get_end_iter" gtk_text_buffer_get_end_iter :: 
    Ptr TextBuffer ->                       -- buffer : TInterface (Name {namespace = "Gtk", name = "TextBuffer"})
    Ptr Gtk.TextIter.TextIter ->            -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    IO ()

-- | Initializes /@iter@/ with the “end iterator,” one past the last valid
-- character in the text buffer. If dereferenced with
-- 'GI.Gtk.Structs.TextIter.textIterGetChar', the end iterator has a character value of 0.
-- The entire buffer lies in the range from the first position in
-- the buffer (call 'GI.Gtk.Objects.TextBuffer.textBufferGetStartIter' to get
-- character position 0) to the end iterator.
textBufferGetEndIter ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextBuffer a) =>
    a
    -- ^ /@buffer@/: a t'GI.Gtk.Objects.TextBuffer.TextBuffer'
    -> m (Gtk.TextIter.TextIter)
textBufferGetEndIter buffer = liftIO $ do
    buffer' <- unsafeManagedPtrCastPtr buffer
    iter <- SP.callocBoxedBytes 80 :: IO (Ptr Gtk.TextIter.TextIter)
    gtk_text_buffer_get_end_iter buffer' iter
    iter' <- (wrapBoxed Gtk.TextIter.TextIter) iter
    touchManagedPtr buffer
    return iter'

#if defined(ENABLE_OVERLOADING)
data TextBufferGetEndIterMethodInfo
instance (signature ~ (m (Gtk.TextIter.TextIter)), MonadIO m, IsTextBuffer a) => O.OverloadedMethod TextBufferGetEndIterMethodInfo a signature where
    overloadedMethod = textBufferGetEndIter

instance O.OverloadedMethodInfo TextBufferGetEndIterMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextBuffer.textBufferGetEndIter",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextBuffer.html#v:textBufferGetEndIter"
        })


#endif

-- method TextBuffer::get_has_selection
-- method type : OrdinaryMethod
-- Args: [ Arg
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

foreign import ccall "gtk_text_buffer_get_has_selection" gtk_text_buffer_get_has_selection :: 
    Ptr TextBuffer ->                       -- buffer : TInterface (Name {namespace = "Gtk", name = "TextBuffer"})
    IO CInt

-- | Indicates whether the buffer has some text currently selected.
-- 
-- /Since: 2.10/
textBufferGetHasSelection ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextBuffer a) =>
    a
    -- ^ /@buffer@/: a t'GI.Gtk.Objects.TextBuffer.TextBuffer'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the there is text selected
textBufferGetHasSelection buffer = liftIO $ do
    buffer' <- unsafeManagedPtrCastPtr buffer
    result <- gtk_text_buffer_get_has_selection buffer'
    let result' = (/= 0) result
    touchManagedPtr buffer
    return result'

#if defined(ENABLE_OVERLOADING)
data TextBufferGetHasSelectionMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsTextBuffer a) => O.OverloadedMethod TextBufferGetHasSelectionMethodInfo a signature where
    overloadedMethod = textBufferGetHasSelection

instance O.OverloadedMethodInfo TextBufferGetHasSelectionMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextBuffer.textBufferGetHasSelection",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextBuffer.html#v:textBufferGetHasSelection"
        })


#endif

-- method TextBuffer::get_insert
-- method type : OrdinaryMethod
-- Args: [ Arg
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "TextMark" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_text_buffer_get_insert" gtk_text_buffer_get_insert :: 
    Ptr TextBuffer ->                       -- buffer : TInterface (Name {namespace = "Gtk", name = "TextBuffer"})
    IO (Ptr Gtk.TextMark.TextMark)

-- | Returns the mark that represents the cursor (insertion point).
-- Equivalent to calling 'GI.Gtk.Objects.TextBuffer.textBufferGetMark' to get the mark
-- named “insert”, but very slightly more efficient, and involves less
-- typing.
textBufferGetInsert ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextBuffer a) =>
    a
    -- ^ /@buffer@/: a t'GI.Gtk.Objects.TextBuffer.TextBuffer'
    -> m Gtk.TextMark.TextMark
    -- ^ __Returns:__ insertion point mark
textBufferGetInsert buffer = liftIO $ do
    buffer' <- unsafeManagedPtrCastPtr buffer
    result <- gtk_text_buffer_get_insert buffer'
    checkUnexpectedReturnNULL "textBufferGetInsert" result
    result' <- (newObject Gtk.TextMark.TextMark) result
    touchManagedPtr buffer
    return result'

#if defined(ENABLE_OVERLOADING)
data TextBufferGetInsertMethodInfo
instance (signature ~ (m Gtk.TextMark.TextMark), MonadIO m, IsTextBuffer a) => O.OverloadedMethod TextBufferGetInsertMethodInfo a signature where
    overloadedMethod = textBufferGetInsert

instance O.OverloadedMethodInfo TextBufferGetInsertMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextBuffer.textBufferGetInsert",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextBuffer.html#v:textBufferGetInsert"
        })


#endif

-- method TextBuffer::get_iter_at_child_anchor
-- method type : OrdinaryMethod
-- Args: [ Arg
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
--       , Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "an iterator to be initialized"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = True
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "anchor"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextChildAnchor" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a child anchor that appears in @buffer"
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

foreign import ccall "gtk_text_buffer_get_iter_at_child_anchor" gtk_text_buffer_get_iter_at_child_anchor :: 
    Ptr TextBuffer ->                       -- buffer : TInterface (Name {namespace = "Gtk", name = "TextBuffer"})
    Ptr Gtk.TextIter.TextIter ->            -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    Ptr Gtk.TextChildAnchor.TextChildAnchor -> -- anchor : TInterface (Name {namespace = "Gtk", name = "TextChildAnchor"})
    IO ()

-- | Obtains the location of /@anchor@/ within /@buffer@/.
textBufferGetIterAtChildAnchor ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextBuffer a, Gtk.TextChildAnchor.IsTextChildAnchor b) =>
    a
    -- ^ /@buffer@/: a t'GI.Gtk.Objects.TextBuffer.TextBuffer'
    -> b
    -- ^ /@anchor@/: a child anchor that appears in /@buffer@/
    -> m (Gtk.TextIter.TextIter)
textBufferGetIterAtChildAnchor buffer anchor = liftIO $ do
    buffer' <- unsafeManagedPtrCastPtr buffer
    iter <- SP.callocBoxedBytes 80 :: IO (Ptr Gtk.TextIter.TextIter)
    anchor' <- unsafeManagedPtrCastPtr anchor
    gtk_text_buffer_get_iter_at_child_anchor buffer' iter anchor'
    iter' <- (wrapBoxed Gtk.TextIter.TextIter) iter
    touchManagedPtr buffer
    touchManagedPtr anchor
    return iter'

#if defined(ENABLE_OVERLOADING)
data TextBufferGetIterAtChildAnchorMethodInfo
instance (signature ~ (b -> m (Gtk.TextIter.TextIter)), MonadIO m, IsTextBuffer a, Gtk.TextChildAnchor.IsTextChildAnchor b) => O.OverloadedMethod TextBufferGetIterAtChildAnchorMethodInfo a signature where
    overloadedMethod = textBufferGetIterAtChildAnchor

instance O.OverloadedMethodInfo TextBufferGetIterAtChildAnchorMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextBuffer.textBufferGetIterAtChildAnchor",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextBuffer.html#v:textBufferGetIterAtChildAnchor"
        })


#endif

-- method TextBuffer::get_iter_at_line
-- method type : OrdinaryMethod
-- Args: [ Arg
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
--       , Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "iterator to initialize"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = True
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "line_number"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "line number counting from 0"
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

foreign import ccall "gtk_text_buffer_get_iter_at_line" gtk_text_buffer_get_iter_at_line :: 
    Ptr TextBuffer ->                       -- buffer : TInterface (Name {namespace = "Gtk", name = "TextBuffer"})
    Ptr Gtk.TextIter.TextIter ->            -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    Int32 ->                                -- line_number : TBasicType TInt
    IO ()

-- | Initializes /@iter@/ to the start of the given line. If /@lineNumber@/ is greater
-- than the number of lines in the /@buffer@/, the end iterator is returned.
textBufferGetIterAtLine ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextBuffer a) =>
    a
    -- ^ /@buffer@/: a t'GI.Gtk.Objects.TextBuffer.TextBuffer'
    -> Int32
    -- ^ /@lineNumber@/: line number counting from 0
    -> m (Gtk.TextIter.TextIter)
textBufferGetIterAtLine buffer lineNumber = liftIO $ do
    buffer' <- unsafeManagedPtrCastPtr buffer
    iter <- SP.callocBoxedBytes 80 :: IO (Ptr Gtk.TextIter.TextIter)
    gtk_text_buffer_get_iter_at_line buffer' iter lineNumber
    iter' <- (wrapBoxed Gtk.TextIter.TextIter) iter
    touchManagedPtr buffer
    return iter'

#if defined(ENABLE_OVERLOADING)
data TextBufferGetIterAtLineMethodInfo
instance (signature ~ (Int32 -> m (Gtk.TextIter.TextIter)), MonadIO m, IsTextBuffer a) => O.OverloadedMethod TextBufferGetIterAtLineMethodInfo a signature where
    overloadedMethod = textBufferGetIterAtLine

instance O.OverloadedMethodInfo TextBufferGetIterAtLineMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextBuffer.textBufferGetIterAtLine",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextBuffer.html#v:textBufferGetIterAtLine"
        })


#endif

-- method TextBuffer::get_iter_at_line_index
-- method type : OrdinaryMethod
-- Args: [ Arg
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
--       , Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "iterator to initialize"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = True
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "line_number"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "line number counting from 0"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "byte_index"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "byte index from start of line"
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

foreign import ccall "gtk_text_buffer_get_iter_at_line_index" gtk_text_buffer_get_iter_at_line_index :: 
    Ptr TextBuffer ->                       -- buffer : TInterface (Name {namespace = "Gtk", name = "TextBuffer"})
    Ptr Gtk.TextIter.TextIter ->            -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    Int32 ->                                -- line_number : TBasicType TInt
    Int32 ->                                -- byte_index : TBasicType TInt
    IO ()

-- | Obtains an iterator pointing to /@byteIndex@/ within the given line.
-- /@byteIndex@/ must be the start of a UTF-8 character. Note bytes, not
-- characters; UTF-8 may encode one character as multiple bytes.
-- 
-- Before the 3.20 version, it was not allowed to pass an invalid location.
-- 
-- Since the 3.20 version, if /@lineNumber@/ is greater than the number of lines
-- in the /@buffer@/, the end iterator is returned. And if /@byteIndex@/ is off the
-- end of the line, the iterator at the end of the line is returned.
textBufferGetIterAtLineIndex ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextBuffer a) =>
    a
    -- ^ /@buffer@/: a t'GI.Gtk.Objects.TextBuffer.TextBuffer'
    -> Int32
    -- ^ /@lineNumber@/: line number counting from 0
    -> Int32
    -- ^ /@byteIndex@/: byte index from start of line
    -> m (Gtk.TextIter.TextIter)
textBufferGetIterAtLineIndex buffer lineNumber byteIndex = liftIO $ do
    buffer' <- unsafeManagedPtrCastPtr buffer
    iter <- SP.callocBoxedBytes 80 :: IO (Ptr Gtk.TextIter.TextIter)
    gtk_text_buffer_get_iter_at_line_index buffer' iter lineNumber byteIndex
    iter' <- (wrapBoxed Gtk.TextIter.TextIter) iter
    touchManagedPtr buffer
    return iter'

#if defined(ENABLE_OVERLOADING)
data TextBufferGetIterAtLineIndexMethodInfo
instance (signature ~ (Int32 -> Int32 -> m (Gtk.TextIter.TextIter)), MonadIO m, IsTextBuffer a) => O.OverloadedMethod TextBufferGetIterAtLineIndexMethodInfo a signature where
    overloadedMethod = textBufferGetIterAtLineIndex

instance O.OverloadedMethodInfo TextBufferGetIterAtLineIndexMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextBuffer.textBufferGetIterAtLineIndex",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextBuffer.html#v:textBufferGetIterAtLineIndex"
        })


#endif

-- method TextBuffer::get_iter_at_line_offset
-- method type : OrdinaryMethod
-- Args: [ Arg
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
--       , Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "iterator to initialize"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = True
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "line_number"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "line number counting from 0"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "char_offset"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "char offset from start of line"
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

foreign import ccall "gtk_text_buffer_get_iter_at_line_offset" gtk_text_buffer_get_iter_at_line_offset :: 
    Ptr TextBuffer ->                       -- buffer : TInterface (Name {namespace = "Gtk", name = "TextBuffer"})
    Ptr Gtk.TextIter.TextIter ->            -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    Int32 ->                                -- line_number : TBasicType TInt
    Int32 ->                                -- char_offset : TBasicType TInt
    IO ()

-- | Obtains an iterator pointing to /@charOffset@/ within the given line. Note
-- characters, not bytes; UTF-8 may encode one character as multiple bytes.
-- 
-- Before the 3.20 version, it was not allowed to pass an invalid location.
-- 
-- Since the 3.20 version, if /@lineNumber@/ is greater than the number of lines
-- in the /@buffer@/, the end iterator is returned. And if /@charOffset@/ is off the
-- end of the line, the iterator at the end of the line is returned.
textBufferGetIterAtLineOffset ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextBuffer a) =>
    a
    -- ^ /@buffer@/: a t'GI.Gtk.Objects.TextBuffer.TextBuffer'
    -> Int32
    -- ^ /@lineNumber@/: line number counting from 0
    -> Int32
    -- ^ /@charOffset@/: char offset from start of line
    -> m (Gtk.TextIter.TextIter)
textBufferGetIterAtLineOffset buffer lineNumber charOffset = liftIO $ do
    buffer' <- unsafeManagedPtrCastPtr buffer
    iter <- SP.callocBoxedBytes 80 :: IO (Ptr Gtk.TextIter.TextIter)
    gtk_text_buffer_get_iter_at_line_offset buffer' iter lineNumber charOffset
    iter' <- (wrapBoxed Gtk.TextIter.TextIter) iter
    touchManagedPtr buffer
    return iter'

#if defined(ENABLE_OVERLOADING)
data TextBufferGetIterAtLineOffsetMethodInfo
instance (signature ~ (Int32 -> Int32 -> m (Gtk.TextIter.TextIter)), MonadIO m, IsTextBuffer a) => O.OverloadedMethod TextBufferGetIterAtLineOffsetMethodInfo a signature where
    overloadedMethod = textBufferGetIterAtLineOffset

instance O.OverloadedMethodInfo TextBufferGetIterAtLineOffsetMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextBuffer.textBufferGetIterAtLineOffset",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextBuffer.html#v:textBufferGetIterAtLineOffset"
        })


#endif

-- method TextBuffer::get_iter_at_mark
-- method type : OrdinaryMethod
-- Args: [ Arg
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
--       , Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "iterator to initialize"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = True
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "mark"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextMark" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextMark in @buffer"
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

foreign import ccall "gtk_text_buffer_get_iter_at_mark" gtk_text_buffer_get_iter_at_mark :: 
    Ptr TextBuffer ->                       -- buffer : TInterface (Name {namespace = "Gtk", name = "TextBuffer"})
    Ptr Gtk.TextIter.TextIter ->            -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    Ptr Gtk.TextMark.TextMark ->            -- mark : TInterface (Name {namespace = "Gtk", name = "TextMark"})
    IO ()

-- | Initializes /@iter@/ with the current position of /@mark@/.
textBufferGetIterAtMark ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextBuffer a, Gtk.TextMark.IsTextMark b) =>
    a
    -- ^ /@buffer@/: a t'GI.Gtk.Objects.TextBuffer.TextBuffer'
    -> b
    -- ^ /@mark@/: a t'GI.Gtk.Objects.TextMark.TextMark' in /@buffer@/
    -> m (Gtk.TextIter.TextIter)
textBufferGetIterAtMark buffer mark = liftIO $ do
    buffer' <- unsafeManagedPtrCastPtr buffer
    iter <- SP.callocBoxedBytes 80 :: IO (Ptr Gtk.TextIter.TextIter)
    mark' <- unsafeManagedPtrCastPtr mark
    gtk_text_buffer_get_iter_at_mark buffer' iter mark'
    iter' <- (wrapBoxed Gtk.TextIter.TextIter) iter
    touchManagedPtr buffer
    touchManagedPtr mark
    return iter'

#if defined(ENABLE_OVERLOADING)
data TextBufferGetIterAtMarkMethodInfo
instance (signature ~ (b -> m (Gtk.TextIter.TextIter)), MonadIO m, IsTextBuffer a, Gtk.TextMark.IsTextMark b) => O.OverloadedMethod TextBufferGetIterAtMarkMethodInfo a signature where
    overloadedMethod = textBufferGetIterAtMark

instance O.OverloadedMethodInfo TextBufferGetIterAtMarkMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextBuffer.textBufferGetIterAtMark",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextBuffer.html#v:textBufferGetIterAtMark"
        })


#endif

-- method TextBuffer::get_iter_at_offset
-- method type : OrdinaryMethod
-- Args: [ Arg
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
--       , Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "iterator to initialize"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = True
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "char_offset"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "char offset from start of buffer, counting from 0, or -1"
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

foreign import ccall "gtk_text_buffer_get_iter_at_offset" gtk_text_buffer_get_iter_at_offset :: 
    Ptr TextBuffer ->                       -- buffer : TInterface (Name {namespace = "Gtk", name = "TextBuffer"})
    Ptr Gtk.TextIter.TextIter ->            -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    Int32 ->                                -- char_offset : TBasicType TInt
    IO ()

-- | Initializes /@iter@/ to a position /@charOffset@/ chars from the start
-- of the entire buffer. If /@charOffset@/ is -1 or greater than the number
-- of characters in the buffer, /@iter@/ is initialized to the end iterator,
-- the iterator one past the last valid character in the buffer.
textBufferGetIterAtOffset ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextBuffer a) =>
    a
    -- ^ /@buffer@/: a t'GI.Gtk.Objects.TextBuffer.TextBuffer'
    -> Int32
    -- ^ /@charOffset@/: char offset from start of buffer, counting from 0, or -1
    -> m (Gtk.TextIter.TextIter)
textBufferGetIterAtOffset buffer charOffset = liftIO $ do
    buffer' <- unsafeManagedPtrCastPtr buffer
    iter <- SP.callocBoxedBytes 80 :: IO (Ptr Gtk.TextIter.TextIter)
    gtk_text_buffer_get_iter_at_offset buffer' iter charOffset
    iter' <- (wrapBoxed Gtk.TextIter.TextIter) iter
    touchManagedPtr buffer
    return iter'

#if defined(ENABLE_OVERLOADING)
data TextBufferGetIterAtOffsetMethodInfo
instance (signature ~ (Int32 -> m (Gtk.TextIter.TextIter)), MonadIO m, IsTextBuffer a) => O.OverloadedMethod TextBufferGetIterAtOffsetMethodInfo a signature where
    overloadedMethod = textBufferGetIterAtOffset

instance O.OverloadedMethodInfo TextBufferGetIterAtOffsetMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextBuffer.textBufferGetIterAtOffset",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextBuffer.html#v:textBufferGetIterAtOffset"
        })


#endif

-- method TextBuffer::get_line_count
-- method type : OrdinaryMethod
-- Args: [ Arg
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
-- returnType: Just (TBasicType TInt)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_text_buffer_get_line_count" gtk_text_buffer_get_line_count :: 
    Ptr TextBuffer ->                       -- buffer : TInterface (Name {namespace = "Gtk", name = "TextBuffer"})
    IO Int32

-- | Obtains the number of lines in the buffer. This value is cached, so
-- the function is very fast.
textBufferGetLineCount ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextBuffer a) =>
    a
    -- ^ /@buffer@/: a t'GI.Gtk.Objects.TextBuffer.TextBuffer'
    -> m Int32
    -- ^ __Returns:__ number of lines in the buffer
textBufferGetLineCount buffer = liftIO $ do
    buffer' <- unsafeManagedPtrCastPtr buffer
    result <- gtk_text_buffer_get_line_count buffer'
    touchManagedPtr buffer
    return result

#if defined(ENABLE_OVERLOADING)
data TextBufferGetLineCountMethodInfo
instance (signature ~ (m Int32), MonadIO m, IsTextBuffer a) => O.OverloadedMethod TextBufferGetLineCountMethodInfo a signature where
    overloadedMethod = textBufferGetLineCount

instance O.OverloadedMethodInfo TextBufferGetLineCountMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextBuffer.textBufferGetLineCount",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextBuffer.html#v:textBufferGetLineCount"
        })


#endif

-- method TextBuffer::get_mark
-- method type : OrdinaryMethod
-- Args: [ Arg
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
--       , Arg
--           { argCName = "name"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a mark name" , sinceVersion = Nothing }
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

foreign import ccall "gtk_text_buffer_get_mark" gtk_text_buffer_get_mark :: 
    Ptr TextBuffer ->                       -- buffer : TInterface (Name {namespace = "Gtk", name = "TextBuffer"})
    CString ->                              -- name : TBasicType TUTF8
    IO (Ptr Gtk.TextMark.TextMark)

-- | Returns the mark named /@name@/ in buffer /@buffer@/, or 'P.Nothing' if no such
-- mark exists in the buffer.
textBufferGetMark ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextBuffer a) =>
    a
    -- ^ /@buffer@/: a t'GI.Gtk.Objects.TextBuffer.TextBuffer'
    -> T.Text
    -- ^ /@name@/: a mark name
    -> m (Maybe Gtk.TextMark.TextMark)
    -- ^ __Returns:__ a t'GI.Gtk.Objects.TextMark.TextMark', or 'P.Nothing'
textBufferGetMark buffer name = liftIO $ do
    buffer' <- unsafeManagedPtrCastPtr buffer
    name' <- textToCString name
    result <- gtk_text_buffer_get_mark buffer' name'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (newObject Gtk.TextMark.TextMark) result'
        return result''
    touchManagedPtr buffer
    freeMem name'
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data TextBufferGetMarkMethodInfo
instance (signature ~ (T.Text -> m (Maybe Gtk.TextMark.TextMark)), MonadIO m, IsTextBuffer a) => O.OverloadedMethod TextBufferGetMarkMethodInfo a signature where
    overloadedMethod = textBufferGetMark

instance O.OverloadedMethodInfo TextBufferGetMarkMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextBuffer.textBufferGetMark",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextBuffer.html#v:textBufferGetMark"
        })


#endif

-- method TextBuffer::get_modified
-- method type : OrdinaryMethod
-- Args: [ Arg
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

foreign import ccall "gtk_text_buffer_get_modified" gtk_text_buffer_get_modified :: 
    Ptr TextBuffer ->                       -- buffer : TInterface (Name {namespace = "Gtk", name = "TextBuffer"})
    IO CInt

-- | Indicates whether the buffer has been modified since the last call
-- to 'GI.Gtk.Objects.TextBuffer.textBufferSetModified' set the modification flag to
-- 'P.False'. Used for example to enable a “save” function in a text
-- editor.
textBufferGetModified ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextBuffer a) =>
    a
    -- ^ /@buffer@/: a t'GI.Gtk.Objects.TextBuffer.TextBuffer'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the buffer has been modified
textBufferGetModified buffer = liftIO $ do
    buffer' <- unsafeManagedPtrCastPtr buffer
    result <- gtk_text_buffer_get_modified buffer'
    let result' = (/= 0) result
    touchManagedPtr buffer
    return result'

#if defined(ENABLE_OVERLOADING)
data TextBufferGetModifiedMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsTextBuffer a) => O.OverloadedMethod TextBufferGetModifiedMethodInfo a signature where
    overloadedMethod = textBufferGetModified

instance O.OverloadedMethodInfo TextBufferGetModifiedMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextBuffer.textBufferGetModified",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextBuffer.html#v:textBufferGetModified"
        })


#endif

-- method TextBuffer::get_paste_target_list
-- method type : OrdinaryMethod
-- Args: [ Arg
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "TargetList" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_text_buffer_get_paste_target_list" gtk_text_buffer_get_paste_target_list :: 
    Ptr TextBuffer ->                       -- buffer : TInterface (Name {namespace = "Gtk", name = "TextBuffer"})
    IO (Ptr Gtk.TargetList.TargetList)

-- | This function returns the list of targets this text buffer supports
-- for pasting and as DND destination. The targets in the list are
-- added with /@info@/ values from the t'GI.Gtk.Enums.TextBufferTargetInfo' enum,
-- using 'GI.Gtk.Structs.TargetList.targetListAddRichTextTargets' and
-- 'GI.Gtk.Structs.TargetList.targetListAddTextTargets'.
-- 
-- /Since: 2.10/
textBufferGetPasteTargetList ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextBuffer a) =>
    a
    -- ^ /@buffer@/: a t'GI.Gtk.Objects.TextBuffer.TextBuffer'
    -> m Gtk.TargetList.TargetList
    -- ^ __Returns:__ the t'GI.Gtk.Structs.TargetList.TargetList'
textBufferGetPasteTargetList buffer = liftIO $ do
    buffer' <- unsafeManagedPtrCastPtr buffer
    result <- gtk_text_buffer_get_paste_target_list buffer'
    checkUnexpectedReturnNULL "textBufferGetPasteTargetList" result
    result' <- (newBoxed Gtk.TargetList.TargetList) result
    touchManagedPtr buffer
    return result'

#if defined(ENABLE_OVERLOADING)
data TextBufferGetPasteTargetListMethodInfo
instance (signature ~ (m Gtk.TargetList.TargetList), MonadIO m, IsTextBuffer a) => O.OverloadedMethod TextBufferGetPasteTargetListMethodInfo a signature where
    overloadedMethod = textBufferGetPasteTargetList

instance O.OverloadedMethodInfo TextBufferGetPasteTargetListMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextBuffer.textBufferGetPasteTargetList",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextBuffer.html#v:textBufferGetPasteTargetList"
        })


#endif

-- method TextBuffer::get_selection_bound
-- method type : OrdinaryMethod
-- Args: [ Arg
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "TextMark" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_text_buffer_get_selection_bound" gtk_text_buffer_get_selection_bound :: 
    Ptr TextBuffer ->                       -- buffer : TInterface (Name {namespace = "Gtk", name = "TextBuffer"})
    IO (Ptr Gtk.TextMark.TextMark)

-- | Returns the mark that represents the selection bound.  Equivalent
-- to calling 'GI.Gtk.Objects.TextBuffer.textBufferGetMark' to get the mark named
-- “selection_bound”, but very slightly more efficient, and involves
-- less typing.
-- 
-- The currently-selected text in /@buffer@/ is the region between the
-- “selection_bound” and “insert” marks. If “selection_bound” and
-- “insert” are in the same place, then there is no current selection.
-- 'GI.Gtk.Objects.TextBuffer.textBufferGetSelectionBounds' is another convenient function
-- for handling the selection, if you just want to know whether there’s a
-- selection and what its bounds are.
textBufferGetSelectionBound ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextBuffer a) =>
    a
    -- ^ /@buffer@/: a t'GI.Gtk.Objects.TextBuffer.TextBuffer'
    -> m Gtk.TextMark.TextMark
    -- ^ __Returns:__ selection bound mark
textBufferGetSelectionBound buffer = liftIO $ do
    buffer' <- unsafeManagedPtrCastPtr buffer
    result <- gtk_text_buffer_get_selection_bound buffer'
    checkUnexpectedReturnNULL "textBufferGetSelectionBound" result
    result' <- (newObject Gtk.TextMark.TextMark) result
    touchManagedPtr buffer
    return result'

#if defined(ENABLE_OVERLOADING)
data TextBufferGetSelectionBoundMethodInfo
instance (signature ~ (m Gtk.TextMark.TextMark), MonadIO m, IsTextBuffer a) => O.OverloadedMethod TextBufferGetSelectionBoundMethodInfo a signature where
    overloadedMethod = textBufferGetSelectionBound

instance O.OverloadedMethodInfo TextBufferGetSelectionBoundMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextBuffer.textBufferGetSelectionBound",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextBuffer.html#v:textBufferGetSelectionBound"
        })


#endif

-- method TextBuffer::get_selection_bounds
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "buffer"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextBuffer" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextBuffer a #GtkTextBuffer"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "start"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "iterator to initialize with selection start"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = True
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "end"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "iterator to initialize with selection end"
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

foreign import ccall "gtk_text_buffer_get_selection_bounds" gtk_text_buffer_get_selection_bounds :: 
    Ptr TextBuffer ->                       -- buffer : TInterface (Name {namespace = "Gtk", name = "TextBuffer"})
    Ptr Gtk.TextIter.TextIter ->            -- start : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    Ptr Gtk.TextIter.TextIter ->            -- end : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    IO CInt

-- | Returns 'P.True' if some text is selected; places the bounds
-- of the selection in /@start@/ and /@end@/ (if the selection has length 0,
-- then /@start@/ and /@end@/ are filled in with the same value).
-- /@start@/ and /@end@/ will be in ascending order. If /@start@/ and /@end@/ are
-- NULL, then they are not filled in, but the return value still indicates
-- whether text is selected.
textBufferGetSelectionBounds ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextBuffer a) =>
    a
    -- ^ /@buffer@/: a t'GI.Gtk.Objects.TextBuffer.TextBuffer' a t'GI.Gtk.Objects.TextBuffer.TextBuffer'
    -> m ((Bool, Gtk.TextIter.TextIter, Gtk.TextIter.TextIter))
    -- ^ __Returns:__ whether the selection has nonzero length
textBufferGetSelectionBounds buffer = liftIO $ do
    buffer' <- unsafeManagedPtrCastPtr buffer
    start <- SP.callocBoxedBytes 80 :: IO (Ptr Gtk.TextIter.TextIter)
    end <- SP.callocBoxedBytes 80 :: IO (Ptr Gtk.TextIter.TextIter)
    result <- gtk_text_buffer_get_selection_bounds buffer' start end
    let result' = (/= 0) result
    start' <- (wrapBoxed Gtk.TextIter.TextIter) start
    end' <- (wrapBoxed Gtk.TextIter.TextIter) end
    touchManagedPtr buffer
    return (result', start', end')

#if defined(ENABLE_OVERLOADING)
data TextBufferGetSelectionBoundsMethodInfo
instance (signature ~ (m ((Bool, Gtk.TextIter.TextIter, Gtk.TextIter.TextIter))), MonadIO m, IsTextBuffer a) => O.OverloadedMethod TextBufferGetSelectionBoundsMethodInfo a signature where
    overloadedMethod = textBufferGetSelectionBounds

instance O.OverloadedMethodInfo TextBufferGetSelectionBoundsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextBuffer.textBufferGetSelectionBounds",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextBuffer.html#v:textBufferGetSelectionBounds"
        })


#endif

-- method TextBuffer::get_serialize_formats
-- method type : OrdinaryMethod
-- Args: [ Arg
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
--       , Arg
--           { argCName = "n_formats"
--           , argType = TBasicType TInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "return location for the number of formats"
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
--              { argCName = "n_formats"
--              , argType = TBasicType TInt
--              , direction = DirectionOut
--              , mayBeNull = False
--              , argDoc =
--                  Documentation
--                    { rawDocText = Just "return location for the number of formats"
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
--                  (TInterface Name { namespace = "Gdk" , name = "Atom" }))
-- throws : False
-- Skip return : False

foreign import ccall "gtk_text_buffer_get_serialize_formats" gtk_text_buffer_get_serialize_formats :: 
    Ptr TextBuffer ->                       -- buffer : TInterface (Name {namespace = "Gtk", name = "TextBuffer"})
    Ptr Int32 ->                            -- n_formats : TBasicType TInt
    IO (Ptr (Ptr Gdk.Atom.Atom))

-- | This function returns the rich text serialize formats registered
-- with /@buffer@/ using 'GI.Gtk.Objects.TextBuffer.textBufferRegisterSerializeFormat' or
-- 'GI.Gtk.Objects.TextBuffer.textBufferRegisterSerializeTagset'
-- 
-- /Since: 2.10/
textBufferGetSerializeFormats ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextBuffer a) =>
    a
    -- ^ /@buffer@/: a t'GI.Gtk.Objects.TextBuffer.TextBuffer'
    -> m [Gdk.Atom.Atom]
    -- ^ __Returns:__ an array of
    --               @/GdkAtoms/@ representing the registered formats.
textBufferGetSerializeFormats buffer = liftIO $ do
    buffer' <- unsafeManagedPtrCastPtr buffer
    nFormats <- allocMem :: IO (Ptr Int32)
    result <- gtk_text_buffer_get_serialize_formats buffer' nFormats
    nFormats' <- peek nFormats
    checkUnexpectedReturnNULL "textBufferGetSerializeFormats" result
    result' <- (unpackPtrArrayWithLength nFormats') result
    result'' <- mapM (newPtr Gdk.Atom.Atom) result'
    freeMem result
    touchManagedPtr buffer
    freeMem nFormats
    return result''

#if defined(ENABLE_OVERLOADING)
data TextBufferGetSerializeFormatsMethodInfo
instance (signature ~ (m [Gdk.Atom.Atom]), MonadIO m, IsTextBuffer a) => O.OverloadedMethod TextBufferGetSerializeFormatsMethodInfo a signature where
    overloadedMethod = textBufferGetSerializeFormats

instance O.OverloadedMethodInfo TextBufferGetSerializeFormatsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextBuffer.textBufferGetSerializeFormats",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextBuffer.html#v:textBufferGetSerializeFormats"
        })


#endif

-- method TextBuffer::get_slice
-- method type : OrdinaryMethod
-- Args: [ Arg
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
--       , Arg
--           { argCName = "start"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "start of a range" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "end"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "end of a range" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "include_hidden_chars"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "whether to include invisible text"
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

foreign import ccall "gtk_text_buffer_get_slice" gtk_text_buffer_get_slice :: 
    Ptr TextBuffer ->                       -- buffer : TInterface (Name {namespace = "Gtk", name = "TextBuffer"})
    Ptr Gtk.TextIter.TextIter ->            -- start : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    Ptr Gtk.TextIter.TextIter ->            -- end : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    CInt ->                                 -- include_hidden_chars : TBasicType TBoolean
    IO CString

-- | Returns the text in the range [/@start@/,/@end@/). Excludes undisplayed
-- text (text marked with tags that set the invisibility attribute) if
-- /@includeHiddenChars@/ is 'P.False'. The returned string includes a
-- 0xFFFC character whenever the buffer contains
-- embedded images, so byte and character indexes into
-- the returned string do correspond to byte
-- and character indexes into the buffer. Contrast with
-- 'GI.Gtk.Objects.TextBuffer.textBufferGetText'. Note that 0xFFFC can occur in normal
-- text as well, so it is not a reliable indicator that a pixbuf or
-- widget is in the buffer.
textBufferGetSlice ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextBuffer a) =>
    a
    -- ^ /@buffer@/: a t'GI.Gtk.Objects.TextBuffer.TextBuffer'
    -> Gtk.TextIter.TextIter
    -- ^ /@start@/: start of a range
    -> Gtk.TextIter.TextIter
    -- ^ /@end@/: end of a range
    -> Bool
    -- ^ /@includeHiddenChars@/: whether to include invisible text
    -> m T.Text
    -- ^ __Returns:__ an allocated UTF-8 string
textBufferGetSlice buffer start end includeHiddenChars = liftIO $ do
    buffer' <- unsafeManagedPtrCastPtr buffer
    start' <- unsafeManagedPtrGetPtr start
    end' <- unsafeManagedPtrGetPtr end
    let includeHiddenChars' = (fromIntegral . fromEnum) includeHiddenChars
    result <- gtk_text_buffer_get_slice buffer' start' end' includeHiddenChars'
    checkUnexpectedReturnNULL "textBufferGetSlice" result
    result' <- cstringToText result
    freeMem result
    touchManagedPtr buffer
    touchManagedPtr start
    touchManagedPtr end
    return result'

#if defined(ENABLE_OVERLOADING)
data TextBufferGetSliceMethodInfo
instance (signature ~ (Gtk.TextIter.TextIter -> Gtk.TextIter.TextIter -> Bool -> m T.Text), MonadIO m, IsTextBuffer a) => O.OverloadedMethod TextBufferGetSliceMethodInfo a signature where
    overloadedMethod = textBufferGetSlice

instance O.OverloadedMethodInfo TextBufferGetSliceMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextBuffer.textBufferGetSlice",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextBuffer.html#v:textBufferGetSlice"
        })


#endif

-- method TextBuffer::get_start_iter
-- method type : OrdinaryMethod
-- Args: [ Arg
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
--       , Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "iterator to initialize"
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

foreign import ccall "gtk_text_buffer_get_start_iter" gtk_text_buffer_get_start_iter :: 
    Ptr TextBuffer ->                       -- buffer : TInterface (Name {namespace = "Gtk", name = "TextBuffer"})
    Ptr Gtk.TextIter.TextIter ->            -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    IO ()

-- | Initialized /@iter@/ with the first position in the text buffer. This
-- is the same as using 'GI.Gtk.Objects.TextBuffer.textBufferGetIterAtOffset' to get
-- the iter at character offset 0.
textBufferGetStartIter ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextBuffer a) =>
    a
    -- ^ /@buffer@/: a t'GI.Gtk.Objects.TextBuffer.TextBuffer'
    -> m (Gtk.TextIter.TextIter)
textBufferGetStartIter buffer = liftIO $ do
    buffer' <- unsafeManagedPtrCastPtr buffer
    iter <- SP.callocBoxedBytes 80 :: IO (Ptr Gtk.TextIter.TextIter)
    gtk_text_buffer_get_start_iter buffer' iter
    iter' <- (wrapBoxed Gtk.TextIter.TextIter) iter
    touchManagedPtr buffer
    return iter'

#if defined(ENABLE_OVERLOADING)
data TextBufferGetStartIterMethodInfo
instance (signature ~ (m (Gtk.TextIter.TextIter)), MonadIO m, IsTextBuffer a) => O.OverloadedMethod TextBufferGetStartIterMethodInfo a signature where
    overloadedMethod = textBufferGetStartIter

instance O.OverloadedMethodInfo TextBufferGetStartIterMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextBuffer.textBufferGetStartIter",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextBuffer.html#v:textBufferGetStartIter"
        })


#endif

-- method TextBuffer::get_tag_table
-- method type : OrdinaryMethod
-- Args: [ Arg
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
-- returnType: Just
--               (TInterface Name { namespace = "Gtk" , name = "TextTagTable" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_text_buffer_get_tag_table" gtk_text_buffer_get_tag_table :: 
    Ptr TextBuffer ->                       -- buffer : TInterface (Name {namespace = "Gtk", name = "TextBuffer"})
    IO (Ptr Gtk.TextTagTable.TextTagTable)

-- | Get the t'GI.Gtk.Objects.TextTagTable.TextTagTable' associated with this buffer.
textBufferGetTagTable ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextBuffer a) =>
    a
    -- ^ /@buffer@/: a t'GI.Gtk.Objects.TextBuffer.TextBuffer'
    -> m Gtk.TextTagTable.TextTagTable
    -- ^ __Returns:__ the buffer’s tag table
textBufferGetTagTable buffer = liftIO $ do
    buffer' <- unsafeManagedPtrCastPtr buffer
    result <- gtk_text_buffer_get_tag_table buffer'
    checkUnexpectedReturnNULL "textBufferGetTagTable" result
    result' <- (newObject Gtk.TextTagTable.TextTagTable) result
    touchManagedPtr buffer
    return result'

#if defined(ENABLE_OVERLOADING)
data TextBufferGetTagTableMethodInfo
instance (signature ~ (m Gtk.TextTagTable.TextTagTable), MonadIO m, IsTextBuffer a) => O.OverloadedMethod TextBufferGetTagTableMethodInfo a signature where
    overloadedMethod = textBufferGetTagTable

instance O.OverloadedMethodInfo TextBufferGetTagTableMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextBuffer.textBufferGetTagTable",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextBuffer.html#v:textBufferGetTagTable"
        })


#endif

-- method TextBuffer::get_text
-- method type : OrdinaryMethod
-- Args: [ Arg
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
--       , Arg
--           { argCName = "start"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "start of a range" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "end"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "end of a range" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "include_hidden_chars"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "whether to include invisible text"
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

foreign import ccall "gtk_text_buffer_get_text" gtk_text_buffer_get_text :: 
    Ptr TextBuffer ->                       -- buffer : TInterface (Name {namespace = "Gtk", name = "TextBuffer"})
    Ptr Gtk.TextIter.TextIter ->            -- start : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    Ptr Gtk.TextIter.TextIter ->            -- end : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    CInt ->                                 -- include_hidden_chars : TBasicType TBoolean
    IO CString

-- | Returns the text in the range [/@start@/,/@end@/). Excludes undisplayed
-- text (text marked with tags that set the invisibility attribute) if
-- /@includeHiddenChars@/ is 'P.False'. Does not include characters
-- representing embedded images, so byte and character indexes into
-- the returned string do not correspond to byte
-- and character indexes into the buffer. Contrast with
-- 'GI.Gtk.Objects.TextBuffer.textBufferGetSlice'.
textBufferGetText ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextBuffer a) =>
    a
    -- ^ /@buffer@/: a t'GI.Gtk.Objects.TextBuffer.TextBuffer'
    -> Gtk.TextIter.TextIter
    -- ^ /@start@/: start of a range
    -> Gtk.TextIter.TextIter
    -- ^ /@end@/: end of a range
    -> Bool
    -- ^ /@includeHiddenChars@/: whether to include invisible text
    -> m T.Text
    -- ^ __Returns:__ an allocated UTF-8 string
textBufferGetText buffer start end includeHiddenChars = liftIO $ do
    buffer' <- unsafeManagedPtrCastPtr buffer
    start' <- unsafeManagedPtrGetPtr start
    end' <- unsafeManagedPtrGetPtr end
    let includeHiddenChars' = (fromIntegral . fromEnum) includeHiddenChars
    result <- gtk_text_buffer_get_text buffer' start' end' includeHiddenChars'
    checkUnexpectedReturnNULL "textBufferGetText" result
    result' <- cstringToText result
    freeMem result
    touchManagedPtr buffer
    touchManagedPtr start
    touchManagedPtr end
    return result'

#if defined(ENABLE_OVERLOADING)
data TextBufferGetTextMethodInfo
instance (signature ~ (Gtk.TextIter.TextIter -> Gtk.TextIter.TextIter -> Bool -> m T.Text), MonadIO m, IsTextBuffer a) => O.OverloadedMethod TextBufferGetTextMethodInfo a signature where
    overloadedMethod = textBufferGetText

instance O.OverloadedMethodInfo TextBufferGetTextMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextBuffer.textBufferGetText",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextBuffer.html#v:textBufferGetText"
        })


#endif

-- method TextBuffer::insert
-- method type : OrdinaryMethod
-- Args: [ Arg
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
--       , Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a position in the buffer"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "text"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "text in UTF-8 format"
--                 , sinceVersion = Nothing
--                 }
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
--                 { rawDocText = Just "length of text in bytes, or -1"
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

foreign import ccall "gtk_text_buffer_insert" gtk_text_buffer_insert :: 
    Ptr TextBuffer ->                       -- buffer : TInterface (Name {namespace = "Gtk", name = "TextBuffer"})
    Ptr Gtk.TextIter.TextIter ->            -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    CString ->                              -- text : TBasicType TUTF8
    Int32 ->                                -- len : TBasicType TInt
    IO ()

-- | Inserts /@len@/ bytes of /@text@/ at position /@iter@/.  If /@len@/ is -1,
-- /@text@/ must be nul-terminated and will be inserted in its
-- entirety. Emits the “insert-text” signal; insertion actually occurs
-- in the default handler for the signal. /@iter@/ is invalidated when
-- insertion occurs (because the buffer contents change), but the
-- default signal handler revalidates it to point to the end of the
-- inserted text.
textBufferInsert ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextBuffer a) =>
    a
    -- ^ /@buffer@/: a t'GI.Gtk.Objects.TextBuffer.TextBuffer'
    -> Gtk.TextIter.TextIter
    -- ^ /@iter@/: a position in the buffer
    -> T.Text
    -- ^ /@text@/: text in UTF-8 format
    -> Int32
    -- ^ /@len@/: length of text in bytes, or -1
    -> m ()
textBufferInsert buffer iter text len = liftIO $ do
    buffer' <- unsafeManagedPtrCastPtr buffer
    iter' <- unsafeManagedPtrGetPtr iter
    text' <- textToCString text
    gtk_text_buffer_insert buffer' iter' text' len
    touchManagedPtr buffer
    touchManagedPtr iter
    freeMem text'
    return ()

#if defined(ENABLE_OVERLOADING)
data TextBufferInsertMethodInfo
instance (signature ~ (Gtk.TextIter.TextIter -> T.Text -> Int32 -> m ()), MonadIO m, IsTextBuffer a) => O.OverloadedMethod TextBufferInsertMethodInfo a signature where
    overloadedMethod = textBufferInsert

instance O.OverloadedMethodInfo TextBufferInsertMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextBuffer.textBufferInsert",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextBuffer.html#v:textBufferInsert"
        })


#endif

-- method TextBuffer::insert_at_cursor
-- method type : OrdinaryMethod
-- Args: [ Arg
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
--       , Arg
--           { argCName = "text"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "text in UTF-8 format"
--                 , sinceVersion = Nothing
--                 }
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
--                 { rawDocText = Just "length of text, in bytes"
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

foreign import ccall "gtk_text_buffer_insert_at_cursor" gtk_text_buffer_insert_at_cursor :: 
    Ptr TextBuffer ->                       -- buffer : TInterface (Name {namespace = "Gtk", name = "TextBuffer"})
    CString ->                              -- text : TBasicType TUTF8
    Int32 ->                                -- len : TBasicType TInt
    IO ()

-- | Simply calls 'GI.Gtk.Objects.TextBuffer.textBufferInsert', using the current
-- cursor position as the insertion point.
textBufferInsertAtCursor ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextBuffer a) =>
    a
    -- ^ /@buffer@/: a t'GI.Gtk.Objects.TextBuffer.TextBuffer'
    -> T.Text
    -- ^ /@text@/: text in UTF-8 format
    -> Int32
    -- ^ /@len@/: length of text, in bytes
    -> m ()
textBufferInsertAtCursor buffer text len = liftIO $ do
    buffer' <- unsafeManagedPtrCastPtr buffer
    text' <- textToCString text
    gtk_text_buffer_insert_at_cursor buffer' text' len
    touchManagedPtr buffer
    freeMem text'
    return ()

#if defined(ENABLE_OVERLOADING)
data TextBufferInsertAtCursorMethodInfo
instance (signature ~ (T.Text -> Int32 -> m ()), MonadIO m, IsTextBuffer a) => O.OverloadedMethod TextBufferInsertAtCursorMethodInfo a signature where
    overloadedMethod = textBufferInsertAtCursor

instance O.OverloadedMethodInfo TextBufferInsertAtCursorMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextBuffer.textBufferInsertAtCursor",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextBuffer.html#v:textBufferInsertAtCursor"
        })


#endif

-- method TextBuffer::insert_child_anchor
-- method type : OrdinaryMethod
-- Args: [ Arg
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
--       , Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "location to insert the anchor"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "anchor"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextChildAnchor" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextChildAnchor"
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

foreign import ccall "gtk_text_buffer_insert_child_anchor" gtk_text_buffer_insert_child_anchor :: 
    Ptr TextBuffer ->                       -- buffer : TInterface (Name {namespace = "Gtk", name = "TextBuffer"})
    Ptr Gtk.TextIter.TextIter ->            -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    Ptr Gtk.TextChildAnchor.TextChildAnchor -> -- anchor : TInterface (Name {namespace = "Gtk", name = "TextChildAnchor"})
    IO ()

-- | Inserts a child widget anchor into the text buffer at /@iter@/. The
-- anchor will be counted as one character in character counts, and
-- when obtaining the buffer contents as a string, will be represented
-- by the Unicode “object replacement character” 0xFFFC. Note that the
-- “slice” variants for obtaining portions of the buffer as a string
-- include this character for child anchors, but the “text” variants do
-- not. E.g. see 'GI.Gtk.Objects.TextBuffer.textBufferGetSlice' and
-- 'GI.Gtk.Objects.TextBuffer.textBufferGetText'. Consider
-- 'GI.Gtk.Objects.TextBuffer.textBufferCreateChildAnchor' as a more convenient
-- alternative to this function. The buffer will add a reference to
-- the anchor, so you can unref it after insertion.
textBufferInsertChildAnchor ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextBuffer a, Gtk.TextChildAnchor.IsTextChildAnchor b) =>
    a
    -- ^ /@buffer@/: a t'GI.Gtk.Objects.TextBuffer.TextBuffer'
    -> Gtk.TextIter.TextIter
    -- ^ /@iter@/: location to insert the anchor
    -> b
    -- ^ /@anchor@/: a t'GI.Gtk.Objects.TextChildAnchor.TextChildAnchor'
    -> m ()
textBufferInsertChildAnchor buffer iter anchor = liftIO $ do
    buffer' <- unsafeManagedPtrCastPtr buffer
    iter' <- unsafeManagedPtrGetPtr iter
    anchor' <- unsafeManagedPtrCastPtr anchor
    gtk_text_buffer_insert_child_anchor buffer' iter' anchor'
    touchManagedPtr buffer
    touchManagedPtr iter
    touchManagedPtr anchor
    return ()

#if defined(ENABLE_OVERLOADING)
data TextBufferInsertChildAnchorMethodInfo
instance (signature ~ (Gtk.TextIter.TextIter -> b -> m ()), MonadIO m, IsTextBuffer a, Gtk.TextChildAnchor.IsTextChildAnchor b) => O.OverloadedMethod TextBufferInsertChildAnchorMethodInfo a signature where
    overloadedMethod = textBufferInsertChildAnchor

instance O.OverloadedMethodInfo TextBufferInsertChildAnchorMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextBuffer.textBufferInsertChildAnchor",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextBuffer.html#v:textBufferInsertChildAnchor"
        })


#endif

-- method TextBuffer::insert_interactive
-- method type : OrdinaryMethod
-- Args: [ Arg
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
--       , Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a position in @buffer"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "text"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "some UTF-8 text" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "length of text in bytes, or -1"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "default_editable"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "default editability of buffer"
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

foreign import ccall "gtk_text_buffer_insert_interactive" gtk_text_buffer_insert_interactive :: 
    Ptr TextBuffer ->                       -- buffer : TInterface (Name {namespace = "Gtk", name = "TextBuffer"})
    Ptr Gtk.TextIter.TextIter ->            -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    CString ->                              -- text : TBasicType TUTF8
    Int32 ->                                -- len : TBasicType TInt
    CInt ->                                 -- default_editable : TBasicType TBoolean
    IO CInt

-- | Like 'GI.Gtk.Objects.TextBuffer.textBufferInsert', but the insertion will not occur if
-- /@iter@/ is at a non-editable location in the buffer. Usually you
-- want to prevent insertions at ineditable locations if the insertion
-- results from a user action (is interactive).
-- 
-- /@defaultEditable@/ indicates the editability of text that doesn\'t
-- have a tag affecting editability applied to it. Typically the
-- result of 'GI.Gtk.Objects.TextView.textViewGetEditable' is appropriate here.
textBufferInsertInteractive ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextBuffer a) =>
    a
    -- ^ /@buffer@/: a t'GI.Gtk.Objects.TextBuffer.TextBuffer'
    -> Gtk.TextIter.TextIter
    -- ^ /@iter@/: a position in /@buffer@/
    -> T.Text
    -- ^ /@text@/: some UTF-8 text
    -> Int32
    -- ^ /@len@/: length of text in bytes, or -1
    -> Bool
    -- ^ /@defaultEditable@/: default editability of buffer
    -> m Bool
    -- ^ __Returns:__ whether text was actually inserted
textBufferInsertInteractive buffer iter text len defaultEditable = liftIO $ do
    buffer' <- unsafeManagedPtrCastPtr buffer
    iter' <- unsafeManagedPtrGetPtr iter
    text' <- textToCString text
    let defaultEditable' = (fromIntegral . fromEnum) defaultEditable
    result <- gtk_text_buffer_insert_interactive buffer' iter' text' len defaultEditable'
    let result' = (/= 0) result
    touchManagedPtr buffer
    touchManagedPtr iter
    freeMem text'
    return result'

#if defined(ENABLE_OVERLOADING)
data TextBufferInsertInteractiveMethodInfo
instance (signature ~ (Gtk.TextIter.TextIter -> T.Text -> Int32 -> Bool -> m Bool), MonadIO m, IsTextBuffer a) => O.OverloadedMethod TextBufferInsertInteractiveMethodInfo a signature where
    overloadedMethod = textBufferInsertInteractive

instance O.OverloadedMethodInfo TextBufferInsertInteractiveMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextBuffer.textBufferInsertInteractive",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextBuffer.html#v:textBufferInsertInteractive"
        })


#endif

-- method TextBuffer::insert_interactive_at_cursor
-- method type : OrdinaryMethod
-- Args: [ Arg
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
--       , Arg
--           { argCName = "text"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "text in UTF-8 format"
--                 , sinceVersion = Nothing
--                 }
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
--                 { rawDocText = Just "length of text in bytes, or -1"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "default_editable"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "default editability of buffer"
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

foreign import ccall "gtk_text_buffer_insert_interactive_at_cursor" gtk_text_buffer_insert_interactive_at_cursor :: 
    Ptr TextBuffer ->                       -- buffer : TInterface (Name {namespace = "Gtk", name = "TextBuffer"})
    CString ->                              -- text : TBasicType TUTF8
    Int32 ->                                -- len : TBasicType TInt
    CInt ->                                 -- default_editable : TBasicType TBoolean
    IO CInt

-- | Calls 'GI.Gtk.Objects.TextBuffer.textBufferInsertInteractive' at the cursor
-- position.
-- 
-- /@defaultEditable@/ indicates the editability of text that doesn\'t
-- have a tag affecting editability applied to it. Typically the
-- result of 'GI.Gtk.Objects.TextView.textViewGetEditable' is appropriate here.
textBufferInsertInteractiveAtCursor ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextBuffer a) =>
    a
    -- ^ /@buffer@/: a t'GI.Gtk.Objects.TextBuffer.TextBuffer'
    -> T.Text
    -- ^ /@text@/: text in UTF-8 format
    -> Int32
    -- ^ /@len@/: length of text in bytes, or -1
    -> Bool
    -- ^ /@defaultEditable@/: default editability of buffer
    -> m Bool
    -- ^ __Returns:__ whether text was actually inserted
textBufferInsertInteractiveAtCursor buffer text len defaultEditable = liftIO $ do
    buffer' <- unsafeManagedPtrCastPtr buffer
    text' <- textToCString text
    let defaultEditable' = (fromIntegral . fromEnum) defaultEditable
    result <- gtk_text_buffer_insert_interactive_at_cursor buffer' text' len defaultEditable'
    let result' = (/= 0) result
    touchManagedPtr buffer
    freeMem text'
    return result'

#if defined(ENABLE_OVERLOADING)
data TextBufferInsertInteractiveAtCursorMethodInfo
instance (signature ~ (T.Text -> Int32 -> Bool -> m Bool), MonadIO m, IsTextBuffer a) => O.OverloadedMethod TextBufferInsertInteractiveAtCursorMethodInfo a signature where
    overloadedMethod = textBufferInsertInteractiveAtCursor

instance O.OverloadedMethodInfo TextBufferInsertInteractiveAtCursorMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextBuffer.textBufferInsertInteractiveAtCursor",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextBuffer.html#v:textBufferInsertInteractiveAtCursor"
        })


#endif

-- method TextBuffer::insert_markup
-- method type : OrdinaryMethod
-- Args: [ Arg
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
--       , Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "location to insert the markup"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "markup"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "a nul-terminated UTF-8 string containing [Pango markup][PangoMarkupFormat]"
--                 , sinceVersion = Nothing
--                 }
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
--                 { rawDocText = Just "length of @markup in bytes, or -1"
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

foreign import ccall "gtk_text_buffer_insert_markup" gtk_text_buffer_insert_markup :: 
    Ptr TextBuffer ->                       -- buffer : TInterface (Name {namespace = "Gtk", name = "TextBuffer"})
    Ptr Gtk.TextIter.TextIter ->            -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    CString ->                              -- markup : TBasicType TUTF8
    Int32 ->                                -- len : TBasicType TInt
    IO ()

-- | Inserts the text in /@markup@/ at position /@iter@/. /@markup@/ will be inserted
-- in its entirety and must be nul-terminated and valid UTF-8. Emits the
-- [TextBuffer::insertText]("GI.Gtk.Objects.TextBuffer#g:signal:insertText") signal, possibly multiple times; insertion
-- actually occurs in the default handler for the signal. /@iter@/ will point
-- to the end of the inserted text on return.
-- 
-- /Since: 3.16/
textBufferInsertMarkup ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextBuffer a) =>
    a
    -- ^ /@buffer@/: a t'GI.Gtk.Objects.TextBuffer.TextBuffer'
    -> Gtk.TextIter.TextIter
    -- ^ /@iter@/: location to insert the markup
    -> T.Text
    -- ^ /@markup@/: a nul-terminated UTF-8 string containing [Pango markup][PangoMarkupFormat]
    -> Int32
    -- ^ /@len@/: length of /@markup@/ in bytes, or -1
    -> m ()
textBufferInsertMarkup buffer iter markup len = liftIO $ do
    buffer' <- unsafeManagedPtrCastPtr buffer
    iter' <- unsafeManagedPtrGetPtr iter
    markup' <- textToCString markup
    gtk_text_buffer_insert_markup buffer' iter' markup' len
    touchManagedPtr buffer
    touchManagedPtr iter
    freeMem markup'
    return ()

#if defined(ENABLE_OVERLOADING)
data TextBufferInsertMarkupMethodInfo
instance (signature ~ (Gtk.TextIter.TextIter -> T.Text -> Int32 -> m ()), MonadIO m, IsTextBuffer a) => O.OverloadedMethod TextBufferInsertMarkupMethodInfo a signature where
    overloadedMethod = textBufferInsertMarkup

instance O.OverloadedMethodInfo TextBufferInsertMarkupMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextBuffer.textBufferInsertMarkup",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextBuffer.html#v:textBufferInsertMarkup"
        })


#endif

-- method TextBuffer::insert_pixbuf
-- method type : OrdinaryMethod
-- Args: [ Arg
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
--       , Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "location to insert the pixbuf"
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
-- returnType: Nothing
-- throws : False
-- Skip return : False

foreign import ccall "gtk_text_buffer_insert_pixbuf" gtk_text_buffer_insert_pixbuf :: 
    Ptr TextBuffer ->                       -- buffer : TInterface (Name {namespace = "Gtk", name = "TextBuffer"})
    Ptr Gtk.TextIter.TextIter ->            -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    Ptr GdkPixbuf.Pixbuf.Pixbuf ->          -- pixbuf : TInterface (Name {namespace = "GdkPixbuf", name = "Pixbuf"})
    IO ()

-- | Inserts an image into the text buffer at /@iter@/. The image will be
-- counted as one character in character counts, and when obtaining
-- the buffer contents as a string, will be represented by the Unicode
-- “object replacement character” 0xFFFC. Note that the “slice”
-- variants for obtaining portions of the buffer as a string include
-- this character for pixbufs, but the “text” variants do
-- not. e.g. see 'GI.Gtk.Objects.TextBuffer.textBufferGetSlice' and
-- 'GI.Gtk.Objects.TextBuffer.textBufferGetText'.
textBufferInsertPixbuf ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextBuffer a, GdkPixbuf.Pixbuf.IsPixbuf b) =>
    a
    -- ^ /@buffer@/: a t'GI.Gtk.Objects.TextBuffer.TextBuffer'
    -> Gtk.TextIter.TextIter
    -- ^ /@iter@/: location to insert the pixbuf
    -> b
    -- ^ /@pixbuf@/: a t'GI.GdkPixbuf.Objects.Pixbuf.Pixbuf'
    -> m ()
textBufferInsertPixbuf buffer iter pixbuf = liftIO $ do
    buffer' <- unsafeManagedPtrCastPtr buffer
    iter' <- unsafeManagedPtrGetPtr iter
    pixbuf' <- unsafeManagedPtrCastPtr pixbuf
    gtk_text_buffer_insert_pixbuf buffer' iter' pixbuf'
    touchManagedPtr buffer
    touchManagedPtr iter
    touchManagedPtr pixbuf
    return ()

#if defined(ENABLE_OVERLOADING)
data TextBufferInsertPixbufMethodInfo
instance (signature ~ (Gtk.TextIter.TextIter -> b -> m ()), MonadIO m, IsTextBuffer a, GdkPixbuf.Pixbuf.IsPixbuf b) => O.OverloadedMethod TextBufferInsertPixbufMethodInfo a signature where
    overloadedMethod = textBufferInsertPixbuf

instance O.OverloadedMethodInfo TextBufferInsertPixbufMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextBuffer.textBufferInsertPixbuf",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextBuffer.html#v:textBufferInsertPixbuf"
        })


#endif

-- method TextBuffer::insert_range
-- method type : OrdinaryMethod
-- Args: [ Arg
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
--       , Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a position in @buffer"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "start"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a position in a #GtkTextBuffer"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "end"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "another position in the same buffer as @start"
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

foreign import ccall "gtk_text_buffer_insert_range" gtk_text_buffer_insert_range :: 
    Ptr TextBuffer ->                       -- buffer : TInterface (Name {namespace = "Gtk", name = "TextBuffer"})
    Ptr Gtk.TextIter.TextIter ->            -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    Ptr Gtk.TextIter.TextIter ->            -- start : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    Ptr Gtk.TextIter.TextIter ->            -- end : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    IO ()

-- | Copies text, tags, and pixbufs between /@start@/ and /@end@/ (the order
-- of /@start@/ and /@end@/ doesn’t matter) and inserts the copy at /@iter@/.
-- Used instead of simply getting\/inserting text because it preserves
-- images and tags. If /@start@/ and /@end@/ are in a different buffer from
-- /@buffer@/, the two buffers must share the same tag table.
-- 
-- Implemented via emissions of the insert_text and apply_tag signals,
-- so expect those.
textBufferInsertRange ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextBuffer a) =>
    a
    -- ^ /@buffer@/: a t'GI.Gtk.Objects.TextBuffer.TextBuffer'
    -> Gtk.TextIter.TextIter
    -- ^ /@iter@/: a position in /@buffer@/
    -> Gtk.TextIter.TextIter
    -- ^ /@start@/: a position in a t'GI.Gtk.Objects.TextBuffer.TextBuffer'
    -> Gtk.TextIter.TextIter
    -- ^ /@end@/: another position in the same buffer as /@start@/
    -> m ()
textBufferInsertRange buffer iter start end = liftIO $ do
    buffer' <- unsafeManagedPtrCastPtr buffer
    iter' <- unsafeManagedPtrGetPtr iter
    start' <- unsafeManagedPtrGetPtr start
    end' <- unsafeManagedPtrGetPtr end
    gtk_text_buffer_insert_range buffer' iter' start' end'
    touchManagedPtr buffer
    touchManagedPtr iter
    touchManagedPtr start
    touchManagedPtr end
    return ()

#if defined(ENABLE_OVERLOADING)
data TextBufferInsertRangeMethodInfo
instance (signature ~ (Gtk.TextIter.TextIter -> Gtk.TextIter.TextIter -> Gtk.TextIter.TextIter -> m ()), MonadIO m, IsTextBuffer a) => O.OverloadedMethod TextBufferInsertRangeMethodInfo a signature where
    overloadedMethod = textBufferInsertRange

instance O.OverloadedMethodInfo TextBufferInsertRangeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextBuffer.textBufferInsertRange",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextBuffer.html#v:textBufferInsertRange"
        })


#endif

-- method TextBuffer::insert_range_interactive
-- method type : OrdinaryMethod
-- Args: [ Arg
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
--       , Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a position in @buffer"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "start"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a position in a #GtkTextBuffer"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "end"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "another position in the same buffer as @start"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "default_editable"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "default editability of the buffer"
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

foreign import ccall "gtk_text_buffer_insert_range_interactive" gtk_text_buffer_insert_range_interactive :: 
    Ptr TextBuffer ->                       -- buffer : TInterface (Name {namespace = "Gtk", name = "TextBuffer"})
    Ptr Gtk.TextIter.TextIter ->            -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    Ptr Gtk.TextIter.TextIter ->            -- start : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    Ptr Gtk.TextIter.TextIter ->            -- end : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    CInt ->                                 -- default_editable : TBasicType TBoolean
    IO CInt

-- | Same as 'GI.Gtk.Objects.TextBuffer.textBufferInsertRange', but does nothing if the
-- insertion point isn’t editable. The /@defaultEditable@/ parameter
-- indicates whether the text is editable at /@iter@/ if no tags
-- enclosing /@iter@/ affect editability. Typically the result of
-- 'GI.Gtk.Objects.TextView.textViewGetEditable' is appropriate here.
textBufferInsertRangeInteractive ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextBuffer a) =>
    a
    -- ^ /@buffer@/: a t'GI.Gtk.Objects.TextBuffer.TextBuffer'
    -> Gtk.TextIter.TextIter
    -- ^ /@iter@/: a position in /@buffer@/
    -> Gtk.TextIter.TextIter
    -- ^ /@start@/: a position in a t'GI.Gtk.Objects.TextBuffer.TextBuffer'
    -> Gtk.TextIter.TextIter
    -- ^ /@end@/: another position in the same buffer as /@start@/
    -> Bool
    -- ^ /@defaultEditable@/: default editability of the buffer
    -> m Bool
    -- ^ __Returns:__ whether an insertion was possible at /@iter@/
textBufferInsertRangeInteractive buffer iter start end defaultEditable = liftIO $ do
    buffer' <- unsafeManagedPtrCastPtr buffer
    iter' <- unsafeManagedPtrGetPtr iter
    start' <- unsafeManagedPtrGetPtr start
    end' <- unsafeManagedPtrGetPtr end
    let defaultEditable' = (fromIntegral . fromEnum) defaultEditable
    result <- gtk_text_buffer_insert_range_interactive buffer' iter' start' end' defaultEditable'
    let result' = (/= 0) result
    touchManagedPtr buffer
    touchManagedPtr iter
    touchManagedPtr start
    touchManagedPtr end
    return result'

#if defined(ENABLE_OVERLOADING)
data TextBufferInsertRangeInteractiveMethodInfo
instance (signature ~ (Gtk.TextIter.TextIter -> Gtk.TextIter.TextIter -> Gtk.TextIter.TextIter -> Bool -> m Bool), MonadIO m, IsTextBuffer a) => O.OverloadedMethod TextBufferInsertRangeInteractiveMethodInfo a signature where
    overloadedMethod = textBufferInsertRangeInteractive

instance O.OverloadedMethodInfo TextBufferInsertRangeInteractiveMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextBuffer.textBufferInsertRangeInteractive",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextBuffer.html#v:textBufferInsertRangeInteractive"
        })


#endif

-- method TextBuffer::move_mark
-- method type : OrdinaryMethod
-- Args: [ Arg
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
--       , Arg
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
--           { argCName = "where"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "new location for @mark in @buffer"
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

foreign import ccall "gtk_text_buffer_move_mark" gtk_text_buffer_move_mark :: 
    Ptr TextBuffer ->                       -- buffer : TInterface (Name {namespace = "Gtk", name = "TextBuffer"})
    Ptr Gtk.TextMark.TextMark ->            -- mark : TInterface (Name {namespace = "Gtk", name = "TextMark"})
    Ptr Gtk.TextIter.TextIter ->            -- where : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    IO ()

-- | Moves /@mark@/ to the new location /@where@/. Emits the [TextBuffer::markSet]("GI.Gtk.Objects.TextBuffer#g:signal:markSet")
-- signal as notification of the move.
textBufferMoveMark ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextBuffer a, Gtk.TextMark.IsTextMark b) =>
    a
    -- ^ /@buffer@/: a t'GI.Gtk.Objects.TextBuffer.TextBuffer'
    -> b
    -- ^ /@mark@/: a t'GI.Gtk.Objects.TextMark.TextMark'
    -> Gtk.TextIter.TextIter
    -- ^ /@where@/: new location for /@mark@/ in /@buffer@/
    -> m ()
textBufferMoveMark buffer mark where_ = liftIO $ do
    buffer' <- unsafeManagedPtrCastPtr buffer
    mark' <- unsafeManagedPtrCastPtr mark
    where_' <- unsafeManagedPtrGetPtr where_
    gtk_text_buffer_move_mark buffer' mark' where_'
    touchManagedPtr buffer
    touchManagedPtr mark
    touchManagedPtr where_
    return ()

#if defined(ENABLE_OVERLOADING)
data TextBufferMoveMarkMethodInfo
instance (signature ~ (b -> Gtk.TextIter.TextIter -> m ()), MonadIO m, IsTextBuffer a, Gtk.TextMark.IsTextMark b) => O.OverloadedMethod TextBufferMoveMarkMethodInfo a signature where
    overloadedMethod = textBufferMoveMark

instance O.OverloadedMethodInfo TextBufferMoveMarkMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextBuffer.textBufferMoveMark",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextBuffer.html#v:textBufferMoveMark"
        })


#endif

-- method TextBuffer::move_mark_by_name
-- method type : OrdinaryMethod
-- Args: [ Arg
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
--       , Arg
--           { argCName = "name"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "name of a mark" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "where"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "new location for mark"
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

foreign import ccall "gtk_text_buffer_move_mark_by_name" gtk_text_buffer_move_mark_by_name :: 
    Ptr TextBuffer ->                       -- buffer : TInterface (Name {namespace = "Gtk", name = "TextBuffer"})
    CString ->                              -- name : TBasicType TUTF8
    Ptr Gtk.TextIter.TextIter ->            -- where : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    IO ()

-- | Moves the mark named /@name@/ (which must exist) to location /@where@/.
-- See 'GI.Gtk.Objects.TextBuffer.textBufferMoveMark' for details.
textBufferMoveMarkByName ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextBuffer a) =>
    a
    -- ^ /@buffer@/: a t'GI.Gtk.Objects.TextBuffer.TextBuffer'
    -> T.Text
    -- ^ /@name@/: name of a mark
    -> Gtk.TextIter.TextIter
    -- ^ /@where@/: new location for mark
    -> m ()
textBufferMoveMarkByName buffer name where_ = liftIO $ do
    buffer' <- unsafeManagedPtrCastPtr buffer
    name' <- textToCString name
    where_' <- unsafeManagedPtrGetPtr where_
    gtk_text_buffer_move_mark_by_name buffer' name' where_'
    touchManagedPtr buffer
    touchManagedPtr where_
    freeMem name'
    return ()

#if defined(ENABLE_OVERLOADING)
data TextBufferMoveMarkByNameMethodInfo
instance (signature ~ (T.Text -> Gtk.TextIter.TextIter -> m ()), MonadIO m, IsTextBuffer a) => O.OverloadedMethod TextBufferMoveMarkByNameMethodInfo a signature where
    overloadedMethod = textBufferMoveMarkByName

instance O.OverloadedMethodInfo TextBufferMoveMarkByNameMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextBuffer.textBufferMoveMarkByName",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextBuffer.html#v:textBufferMoveMarkByName"
        })


#endif

-- method TextBuffer::paste_clipboard
-- method type : OrdinaryMethod
-- Args: [ Arg
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
--       , Arg
--           { argCName = "clipboard"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Clipboard" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the #GtkClipboard to paste from"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "override_location"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "location to insert pasted text, or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "default_editable"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "whether the buffer is editable by default"
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

foreign import ccall "gtk_text_buffer_paste_clipboard" gtk_text_buffer_paste_clipboard :: 
    Ptr TextBuffer ->                       -- buffer : TInterface (Name {namespace = "Gtk", name = "TextBuffer"})
    Ptr Gtk.Clipboard.Clipboard ->          -- clipboard : TInterface (Name {namespace = "Gtk", name = "Clipboard"})
    Ptr Gtk.TextIter.TextIter ->            -- override_location : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    CInt ->                                 -- default_editable : TBasicType TBoolean
    IO ()

-- | Pastes the contents of a clipboard. If /@overrideLocation@/ is 'P.Nothing', the
-- pasted text will be inserted at the cursor position, or the buffer selection
-- will be replaced if the selection is non-empty.
-- 
-- Note: pasting is asynchronous, that is, we’ll ask for the paste data and
-- return, and at some point later after the main loop runs, the paste data will
-- be inserted.
textBufferPasteClipboard ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextBuffer a, Gtk.Clipboard.IsClipboard b) =>
    a
    -- ^ /@buffer@/: a t'GI.Gtk.Objects.TextBuffer.TextBuffer'
    -> b
    -- ^ /@clipboard@/: the t'GI.Gtk.Objects.Clipboard.Clipboard' to paste from
    -> Maybe (Gtk.TextIter.TextIter)
    -- ^ /@overrideLocation@/: location to insert pasted text, or 'P.Nothing'
    -> Bool
    -- ^ /@defaultEditable@/: whether the buffer is editable by default
    -> m ()
textBufferPasteClipboard buffer clipboard overrideLocation defaultEditable = liftIO $ do
    buffer' <- unsafeManagedPtrCastPtr buffer
    clipboard' <- unsafeManagedPtrCastPtr clipboard
    maybeOverrideLocation <- case overrideLocation of
        Nothing -> return nullPtr
        Just jOverrideLocation -> do
            jOverrideLocation' <- unsafeManagedPtrGetPtr jOverrideLocation
            return jOverrideLocation'
    let defaultEditable' = (fromIntegral . fromEnum) defaultEditable
    gtk_text_buffer_paste_clipboard buffer' clipboard' maybeOverrideLocation defaultEditable'
    touchManagedPtr buffer
    touchManagedPtr clipboard
    whenJust overrideLocation touchManagedPtr
    return ()

#if defined(ENABLE_OVERLOADING)
data TextBufferPasteClipboardMethodInfo
instance (signature ~ (b -> Maybe (Gtk.TextIter.TextIter) -> Bool -> m ()), MonadIO m, IsTextBuffer a, Gtk.Clipboard.IsClipboard b) => O.OverloadedMethod TextBufferPasteClipboardMethodInfo a signature where
    overloadedMethod = textBufferPasteClipboard

instance O.OverloadedMethodInfo TextBufferPasteClipboardMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextBuffer.textBufferPasteClipboard",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextBuffer.html#v:textBufferPasteClipboard"
        })


#endif

-- method TextBuffer::place_cursor
-- method type : OrdinaryMethod
-- Args: [ Arg
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
--       , Arg
--           { argCName = "where"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "where to put the cursor"
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

foreign import ccall "gtk_text_buffer_place_cursor" gtk_text_buffer_place_cursor :: 
    Ptr TextBuffer ->                       -- buffer : TInterface (Name {namespace = "Gtk", name = "TextBuffer"})
    Ptr Gtk.TextIter.TextIter ->            -- where : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    IO ()

-- | This function moves the “insert” and “selection_bound” marks
-- simultaneously.  If you move them to the same place in two steps
-- with 'GI.Gtk.Objects.TextBuffer.textBufferMoveMark', you will temporarily select a
-- region in between their old and new locations, which can be pretty
-- inefficient since the temporarily-selected region will force stuff
-- to be recalculated. This function moves them as a unit, which can
-- be optimized.
textBufferPlaceCursor ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextBuffer a) =>
    a
    -- ^ /@buffer@/: a t'GI.Gtk.Objects.TextBuffer.TextBuffer'
    -> Gtk.TextIter.TextIter
    -- ^ /@where@/: where to put the cursor
    -> m ()
textBufferPlaceCursor buffer where_ = liftIO $ do
    buffer' <- unsafeManagedPtrCastPtr buffer
    where_' <- unsafeManagedPtrGetPtr where_
    gtk_text_buffer_place_cursor buffer' where_'
    touchManagedPtr buffer
    touchManagedPtr where_
    return ()

#if defined(ENABLE_OVERLOADING)
data TextBufferPlaceCursorMethodInfo
instance (signature ~ (Gtk.TextIter.TextIter -> m ()), MonadIO m, IsTextBuffer a) => O.OverloadedMethod TextBufferPlaceCursorMethodInfo a signature where
    overloadedMethod = textBufferPlaceCursor

instance O.OverloadedMethodInfo TextBufferPlaceCursorMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextBuffer.textBufferPlaceCursor",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextBuffer.html#v:textBufferPlaceCursor"
        })


#endif

-- method TextBuffer::register_deserialize_format
-- method type : OrdinaryMethod
-- Args: [ Arg
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
--       , Arg
--           { argCName = "mime_type"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the format\8217s mime-type"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "function"
--           , argType =
--               TInterface
--                 Name { namespace = "Gtk" , name = "TextBufferDeserializeFunc" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the deserialize function to register"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeNotified
--           , argClosure = 3
--           , argDestroy = 4
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
--                 { rawDocText = Just "@function\8217s user_data"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "user_data_destroy"
--           , argType =
--               TInterface Name { namespace = "GLib" , name = "DestroyNotify" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "a function to call when @user_data is no longer needed"
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
-- returnType: Just (TInterface Name { namespace = "Gdk" , name = "Atom" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_text_buffer_register_deserialize_format" gtk_text_buffer_register_deserialize_format :: 
    Ptr TextBuffer ->                       -- buffer : TInterface (Name {namespace = "Gtk", name = "TextBuffer"})
    CString ->                              -- mime_type : TBasicType TUTF8
    FunPtr Gtk.Callbacks.C_TextBufferDeserializeFunc -> -- function : TInterface (Name {namespace = "Gtk", name = "TextBufferDeserializeFunc"})
    Ptr () ->                               -- user_data : TBasicType TPtr
    FunPtr GLib.Callbacks.C_DestroyNotify -> -- user_data_destroy : TInterface (Name {namespace = "GLib", name = "DestroyNotify"})
    IO (Ptr Gdk.Atom.Atom)

-- | This function registers a rich text deserialization /@function@/ along with
-- its /@mimeType@/ with the passed /@buffer@/.
-- 
-- /Since: 2.10/
textBufferRegisterDeserializeFormat ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextBuffer a) =>
    a
    -- ^ /@buffer@/: a t'GI.Gtk.Objects.TextBuffer.TextBuffer'
    -> T.Text
    -- ^ /@mimeType@/: the format’s mime-type
    -> FunPtr Gtk.Callbacks.C_TextBufferDeserializeFunc
    -- ^ /@function@/: the deserialize function to register
    -> m Gdk.Atom.Atom
    -- ^ __Returns:__ the t'GI.Gdk.Structs.Atom.Atom' that corresponds to the
    --               newly registered format’s mime-type.
textBufferRegisterDeserializeFormat buffer mimeType function = liftIO $ do
    buffer' <- unsafeManagedPtrCastPtr buffer
    mimeType' <- textToCString mimeType
    let userData = castFunPtrToPtr function
    let userDataDestroy = SP.safeFreeFunPtrPtr
    result <- gtk_text_buffer_register_deserialize_format buffer' mimeType' function userData userDataDestroy
    checkUnexpectedReturnNULL "textBufferRegisterDeserializeFormat" result
    result' <- (newPtr Gdk.Atom.Atom) result
    touchManagedPtr buffer
    freeMem mimeType'
    return result'

#if defined(ENABLE_OVERLOADING)
data TextBufferRegisterDeserializeFormatMethodInfo
instance (signature ~ (T.Text -> FunPtr Gtk.Callbacks.C_TextBufferDeserializeFunc -> m Gdk.Atom.Atom), MonadIO m, IsTextBuffer a) => O.OverloadedMethod TextBufferRegisterDeserializeFormatMethodInfo a signature where
    overloadedMethod = textBufferRegisterDeserializeFormat

instance O.OverloadedMethodInfo TextBufferRegisterDeserializeFormatMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextBuffer.textBufferRegisterDeserializeFormat",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextBuffer.html#v:textBufferRegisterDeserializeFormat"
        })


#endif

-- method TextBuffer::register_deserialize_tagset
-- method type : OrdinaryMethod
-- Args: [ Arg
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
--       , Arg
--           { argCName = "tagset_name"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "an optional tagset name, on %NULL"
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

foreign import ccall "gtk_text_buffer_register_deserialize_tagset" gtk_text_buffer_register_deserialize_tagset :: 
    Ptr TextBuffer ->                       -- buffer : TInterface (Name {namespace = "Gtk", name = "TextBuffer"})
    CString ->                              -- tagset_name : TBasicType TUTF8
    IO (Ptr Gdk.Atom.Atom)

-- | This function registers GTK+’s internal rich text serialization
-- format with the passed /@buffer@/. See
-- 'GI.Gtk.Objects.TextBuffer.textBufferRegisterSerializeTagset' for details.
-- 
-- /Since: 2.10/
textBufferRegisterDeserializeTagset ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextBuffer a) =>
    a
    -- ^ /@buffer@/: a t'GI.Gtk.Objects.TextBuffer.TextBuffer'
    -> Maybe (T.Text)
    -- ^ /@tagsetName@/: an optional tagset name, on 'P.Nothing'
    -> m Gdk.Atom.Atom
    -- ^ __Returns:__ the t'GI.Gdk.Structs.Atom.Atom' that corresponds to the
    --               newly registered format’s mime-type.
textBufferRegisterDeserializeTagset buffer tagsetName = liftIO $ do
    buffer' <- unsafeManagedPtrCastPtr buffer
    maybeTagsetName <- case tagsetName of
        Nothing -> return nullPtr
        Just jTagsetName -> do
            jTagsetName' <- textToCString jTagsetName
            return jTagsetName'
    result <- gtk_text_buffer_register_deserialize_tagset buffer' maybeTagsetName
    checkUnexpectedReturnNULL "textBufferRegisterDeserializeTagset" result
    result' <- (newPtr Gdk.Atom.Atom) result
    touchManagedPtr buffer
    freeMem maybeTagsetName
    return result'

#if defined(ENABLE_OVERLOADING)
data TextBufferRegisterDeserializeTagsetMethodInfo
instance (signature ~ (Maybe (T.Text) -> m Gdk.Atom.Atom), MonadIO m, IsTextBuffer a) => O.OverloadedMethod TextBufferRegisterDeserializeTagsetMethodInfo a signature where
    overloadedMethod = textBufferRegisterDeserializeTagset

instance O.OverloadedMethodInfo TextBufferRegisterDeserializeTagsetMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextBuffer.textBufferRegisterDeserializeTagset",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextBuffer.html#v:textBufferRegisterDeserializeTagset"
        })


#endif

-- method TextBuffer::register_serialize_format
-- method type : OrdinaryMethod
-- Args: [ Arg
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
--       , Arg
--           { argCName = "mime_type"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the format\8217s mime-type"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "function"
--           , argType =
--               TInterface
--                 Name { namespace = "Gtk" , name = "TextBufferSerializeFunc" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the serialize function to register"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeNotified
--           , argClosure = 3
--           , argDestroy = 4
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
--                 { rawDocText = Just "@function\8217s user_data"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "user_data_destroy"
--           , argType =
--               TInterface Name { namespace = "GLib" , name = "DestroyNotify" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "a function to call when @user_data is no longer needed"
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
-- returnType: Just (TInterface Name { namespace = "Gdk" , name = "Atom" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_text_buffer_register_serialize_format" gtk_text_buffer_register_serialize_format :: 
    Ptr TextBuffer ->                       -- buffer : TInterface (Name {namespace = "Gtk", name = "TextBuffer"})
    CString ->                              -- mime_type : TBasicType TUTF8
    FunPtr Gtk.Callbacks.C_TextBufferSerializeFunc -> -- function : TInterface (Name {namespace = "Gtk", name = "TextBufferSerializeFunc"})
    Ptr () ->                               -- user_data : TBasicType TPtr
    FunPtr GLib.Callbacks.C_DestroyNotify -> -- user_data_destroy : TInterface (Name {namespace = "GLib", name = "DestroyNotify"})
    IO (Ptr Gdk.Atom.Atom)

-- | This function registers a rich text serialization /@function@/ along with
-- its /@mimeType@/ with the passed /@buffer@/.
-- 
-- /Since: 2.10/
textBufferRegisterSerializeFormat ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextBuffer a) =>
    a
    -- ^ /@buffer@/: a t'GI.Gtk.Objects.TextBuffer.TextBuffer'
    -> T.Text
    -- ^ /@mimeType@/: the format’s mime-type
    -> Gtk.Callbacks.TextBufferSerializeFunc
    -- ^ /@function@/: the serialize function to register
    -> m Gdk.Atom.Atom
    -- ^ __Returns:__ the t'GI.Gdk.Structs.Atom.Atom' that corresponds to the
    --               newly registered format’s mime-type.
textBufferRegisterSerializeFormat buffer mimeType function = liftIO $ do
    buffer' <- unsafeManagedPtrCastPtr buffer
    mimeType' <- textToCString mimeType
    function' <- Gtk.Callbacks.mk_TextBufferSerializeFunc (Gtk.Callbacks.wrap_TextBufferSerializeFunc Nothing (Gtk.Callbacks.drop_closures_TextBufferSerializeFunc function))
    let userData = castFunPtrToPtr function'
    let userDataDestroy = SP.safeFreeFunPtrPtr
    result <- gtk_text_buffer_register_serialize_format buffer' mimeType' function' userData userDataDestroy
    checkUnexpectedReturnNULL "textBufferRegisterSerializeFormat" result
    result' <- (newPtr Gdk.Atom.Atom) result
    touchManagedPtr buffer
    freeMem mimeType'
    return result'

#if defined(ENABLE_OVERLOADING)
data TextBufferRegisterSerializeFormatMethodInfo
instance (signature ~ (T.Text -> Gtk.Callbacks.TextBufferSerializeFunc -> m Gdk.Atom.Atom), MonadIO m, IsTextBuffer a) => O.OverloadedMethod TextBufferRegisterSerializeFormatMethodInfo a signature where
    overloadedMethod = textBufferRegisterSerializeFormat

instance O.OverloadedMethodInfo TextBufferRegisterSerializeFormatMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextBuffer.textBufferRegisterSerializeFormat",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextBuffer.html#v:textBufferRegisterSerializeFormat"
        })


#endif

-- method TextBuffer::register_serialize_tagset
-- method type : OrdinaryMethod
-- Args: [ Arg
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
--       , Arg
--           { argCName = "tagset_name"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "an optional tagset name, on %NULL"
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

foreign import ccall "gtk_text_buffer_register_serialize_tagset" gtk_text_buffer_register_serialize_tagset :: 
    Ptr TextBuffer ->                       -- buffer : TInterface (Name {namespace = "Gtk", name = "TextBuffer"})
    CString ->                              -- tagset_name : TBasicType TUTF8
    IO (Ptr Gdk.Atom.Atom)

-- | This function registers GTK+’s internal rich text serialization
-- format with the passed /@buffer@/. The internal format does not comply
-- to any standard rich text format and only works between t'GI.Gtk.Objects.TextBuffer.TextBuffer'
-- instances. It is capable of serializing all of a text buffer’s tags
-- and embedded pixbufs.
-- 
-- This function is just a wrapper around
-- 'GI.Gtk.Objects.TextBuffer.textBufferRegisterSerializeFormat'. The mime type used
-- for registering is “application\/x-gtk-text-buffer-rich-text”, or
-- “application\/x-gtk-text-buffer-rich-text;format=/@tagsetName@/” if a
-- /@tagsetName@/ was passed.
-- 
-- The /@tagsetName@/ can be used to restrict the transfer of rich text
-- to buffers with compatible sets of tags, in order to avoid unknown
-- tags from being pasted. It is probably the common case to pass an
-- identifier != 'P.Nothing' here, since the 'P.Nothing' tagset requires the
-- receiving buffer to deal with with pasting of arbitrary tags.
-- 
-- /Since: 2.10/
textBufferRegisterSerializeTagset ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextBuffer a) =>
    a
    -- ^ /@buffer@/: a t'GI.Gtk.Objects.TextBuffer.TextBuffer'
    -> Maybe (T.Text)
    -- ^ /@tagsetName@/: an optional tagset name, on 'P.Nothing'
    -> m Gdk.Atom.Atom
    -- ^ __Returns:__ the t'GI.Gdk.Structs.Atom.Atom' that corresponds to the
    --               newly registered format’s mime-type.
textBufferRegisterSerializeTagset buffer tagsetName = liftIO $ do
    buffer' <- unsafeManagedPtrCastPtr buffer
    maybeTagsetName <- case tagsetName of
        Nothing -> return nullPtr
        Just jTagsetName -> do
            jTagsetName' <- textToCString jTagsetName
            return jTagsetName'
    result <- gtk_text_buffer_register_serialize_tagset buffer' maybeTagsetName
    checkUnexpectedReturnNULL "textBufferRegisterSerializeTagset" result
    result' <- (newPtr Gdk.Atom.Atom) result
    touchManagedPtr buffer
    freeMem maybeTagsetName
    return result'

#if defined(ENABLE_OVERLOADING)
data TextBufferRegisterSerializeTagsetMethodInfo
instance (signature ~ (Maybe (T.Text) -> m Gdk.Atom.Atom), MonadIO m, IsTextBuffer a) => O.OverloadedMethod TextBufferRegisterSerializeTagsetMethodInfo a signature where
    overloadedMethod = textBufferRegisterSerializeTagset

instance O.OverloadedMethodInfo TextBufferRegisterSerializeTagsetMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextBuffer.textBufferRegisterSerializeTagset",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextBuffer.html#v:textBufferRegisterSerializeTagset"
        })


#endif

-- method TextBuffer::remove_all_tags
-- method type : OrdinaryMethod
-- Args: [ Arg
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
--       , Arg
--           { argCName = "start"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "one bound of range to be untagged"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "end"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "other bound of range to be untagged"
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

foreign import ccall "gtk_text_buffer_remove_all_tags" gtk_text_buffer_remove_all_tags :: 
    Ptr TextBuffer ->                       -- buffer : TInterface (Name {namespace = "Gtk", name = "TextBuffer"})
    Ptr Gtk.TextIter.TextIter ->            -- start : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    Ptr Gtk.TextIter.TextIter ->            -- end : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    IO ()

-- | Removes all tags in the range between /@start@/ and /@end@/.  Be careful
-- with this function; it could remove tags added in code unrelated to
-- the code you’re currently writing. That is, using this function is
-- probably a bad idea if you have two or more unrelated code sections
-- that add tags.
textBufferRemoveAllTags ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextBuffer a) =>
    a
    -- ^ /@buffer@/: a t'GI.Gtk.Objects.TextBuffer.TextBuffer'
    -> Gtk.TextIter.TextIter
    -- ^ /@start@/: one bound of range to be untagged
    -> Gtk.TextIter.TextIter
    -- ^ /@end@/: other bound of range to be untagged
    -> m ()
textBufferRemoveAllTags buffer start end = liftIO $ do
    buffer' <- unsafeManagedPtrCastPtr buffer
    start' <- unsafeManagedPtrGetPtr start
    end' <- unsafeManagedPtrGetPtr end
    gtk_text_buffer_remove_all_tags buffer' start' end'
    touchManagedPtr buffer
    touchManagedPtr start
    touchManagedPtr end
    return ()

#if defined(ENABLE_OVERLOADING)
data TextBufferRemoveAllTagsMethodInfo
instance (signature ~ (Gtk.TextIter.TextIter -> Gtk.TextIter.TextIter -> m ()), MonadIO m, IsTextBuffer a) => O.OverloadedMethod TextBufferRemoveAllTagsMethodInfo a signature where
    overloadedMethod = textBufferRemoveAllTags

instance O.OverloadedMethodInfo TextBufferRemoveAllTagsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextBuffer.textBufferRemoveAllTags",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextBuffer.html#v:textBufferRemoveAllTags"
        })


#endif

-- method TextBuffer::remove_selection_clipboard
-- method type : OrdinaryMethod
-- Args: [ Arg
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
--       , Arg
--           { argCName = "clipboard"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Clipboard" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "a #GtkClipboard added to @buffer by\n            gtk_text_buffer_add_selection_clipboard()"
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

foreign import ccall "gtk_text_buffer_remove_selection_clipboard" gtk_text_buffer_remove_selection_clipboard :: 
    Ptr TextBuffer ->                       -- buffer : TInterface (Name {namespace = "Gtk", name = "TextBuffer"})
    Ptr Gtk.Clipboard.Clipboard ->          -- clipboard : TInterface (Name {namespace = "Gtk", name = "Clipboard"})
    IO ()

-- | Removes a t'GI.Gtk.Objects.Clipboard.Clipboard' added with
-- 'GI.Gtk.Objects.TextBuffer.textBufferAddSelectionClipboard'.
textBufferRemoveSelectionClipboard ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextBuffer a, Gtk.Clipboard.IsClipboard b) =>
    a
    -- ^ /@buffer@/: a t'GI.Gtk.Objects.TextBuffer.TextBuffer'
    -> b
    -- ^ /@clipboard@/: a t'GI.Gtk.Objects.Clipboard.Clipboard' added to /@buffer@/ by
    --             'GI.Gtk.Objects.TextBuffer.textBufferAddSelectionClipboard'
    -> m ()
textBufferRemoveSelectionClipboard buffer clipboard = liftIO $ do
    buffer' <- unsafeManagedPtrCastPtr buffer
    clipboard' <- unsafeManagedPtrCastPtr clipboard
    gtk_text_buffer_remove_selection_clipboard buffer' clipboard'
    touchManagedPtr buffer
    touchManagedPtr clipboard
    return ()

#if defined(ENABLE_OVERLOADING)
data TextBufferRemoveSelectionClipboardMethodInfo
instance (signature ~ (b -> m ()), MonadIO m, IsTextBuffer a, Gtk.Clipboard.IsClipboard b) => O.OverloadedMethod TextBufferRemoveSelectionClipboardMethodInfo a signature where
    overloadedMethod = textBufferRemoveSelectionClipboard

instance O.OverloadedMethodInfo TextBufferRemoveSelectionClipboardMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextBuffer.textBufferRemoveSelectionClipboard",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextBuffer.html#v:textBufferRemoveSelectionClipboard"
        })


#endif

-- method TextBuffer::remove_tag
-- method type : OrdinaryMethod
-- Args: [ Arg
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
--       , Arg
--           { argCName = "start"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "one bound of range to be untagged"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "end"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "other bound of range to be untagged"
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

foreign import ccall "gtk_text_buffer_remove_tag" gtk_text_buffer_remove_tag :: 
    Ptr TextBuffer ->                       -- buffer : TInterface (Name {namespace = "Gtk", name = "TextBuffer"})
    Ptr Gtk.TextTag.TextTag ->              -- tag : TInterface (Name {namespace = "Gtk", name = "TextTag"})
    Ptr Gtk.TextIter.TextIter ->            -- start : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    Ptr Gtk.TextIter.TextIter ->            -- end : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    IO ()

-- | Emits the “remove-tag” signal. The default handler for the signal
-- removes all occurrences of /@tag@/ from the given range. /@start@/ and
-- /@end@/ don’t have to be in order.
textBufferRemoveTag ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextBuffer a, Gtk.TextTag.IsTextTag b) =>
    a
    -- ^ /@buffer@/: a t'GI.Gtk.Objects.TextBuffer.TextBuffer'
    -> b
    -- ^ /@tag@/: a t'GI.Gtk.Objects.TextTag.TextTag'
    -> Gtk.TextIter.TextIter
    -- ^ /@start@/: one bound of range to be untagged
    -> Gtk.TextIter.TextIter
    -- ^ /@end@/: other bound of range to be untagged
    -> m ()
textBufferRemoveTag buffer tag start end = liftIO $ do
    buffer' <- unsafeManagedPtrCastPtr buffer
    tag' <- unsafeManagedPtrCastPtr tag
    start' <- unsafeManagedPtrGetPtr start
    end' <- unsafeManagedPtrGetPtr end
    gtk_text_buffer_remove_tag buffer' tag' start' end'
    touchManagedPtr buffer
    touchManagedPtr tag
    touchManagedPtr start
    touchManagedPtr end
    return ()

#if defined(ENABLE_OVERLOADING)
data TextBufferRemoveTagMethodInfo
instance (signature ~ (b -> Gtk.TextIter.TextIter -> Gtk.TextIter.TextIter -> m ()), MonadIO m, IsTextBuffer a, Gtk.TextTag.IsTextTag b) => O.OverloadedMethod TextBufferRemoveTagMethodInfo a signature where
    overloadedMethod = textBufferRemoveTag

instance O.OverloadedMethodInfo TextBufferRemoveTagMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextBuffer.textBufferRemoveTag",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextBuffer.html#v:textBufferRemoveTag"
        })


#endif

-- method TextBuffer::remove_tag_by_name
-- method type : OrdinaryMethod
-- Args: [ Arg
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
--       , Arg
--           { argCName = "name"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "name of a #GtkTextTag"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "start"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "one bound of range to be untagged"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "end"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "other bound of range to be untagged"
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

foreign import ccall "gtk_text_buffer_remove_tag_by_name" gtk_text_buffer_remove_tag_by_name :: 
    Ptr TextBuffer ->                       -- buffer : TInterface (Name {namespace = "Gtk", name = "TextBuffer"})
    CString ->                              -- name : TBasicType TUTF8
    Ptr Gtk.TextIter.TextIter ->            -- start : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    Ptr Gtk.TextIter.TextIter ->            -- end : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    IO ()

-- | Calls 'GI.Gtk.Objects.TextTagTable.textTagTableLookup' on the buffer’s tag table to
-- get a t'GI.Gtk.Objects.TextTag.TextTag', then calls 'GI.Gtk.Objects.TextBuffer.textBufferRemoveTag'.
textBufferRemoveTagByName ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextBuffer a) =>
    a
    -- ^ /@buffer@/: a t'GI.Gtk.Objects.TextBuffer.TextBuffer'
    -> T.Text
    -- ^ /@name@/: name of a t'GI.Gtk.Objects.TextTag.TextTag'
    -> Gtk.TextIter.TextIter
    -- ^ /@start@/: one bound of range to be untagged
    -> Gtk.TextIter.TextIter
    -- ^ /@end@/: other bound of range to be untagged
    -> m ()
textBufferRemoveTagByName buffer name start end = liftIO $ do
    buffer' <- unsafeManagedPtrCastPtr buffer
    name' <- textToCString name
    start' <- unsafeManagedPtrGetPtr start
    end' <- unsafeManagedPtrGetPtr end
    gtk_text_buffer_remove_tag_by_name buffer' name' start' end'
    touchManagedPtr buffer
    touchManagedPtr start
    touchManagedPtr end
    freeMem name'
    return ()

#if defined(ENABLE_OVERLOADING)
data TextBufferRemoveTagByNameMethodInfo
instance (signature ~ (T.Text -> Gtk.TextIter.TextIter -> Gtk.TextIter.TextIter -> m ()), MonadIO m, IsTextBuffer a) => O.OverloadedMethod TextBufferRemoveTagByNameMethodInfo a signature where
    overloadedMethod = textBufferRemoveTagByName

instance O.OverloadedMethodInfo TextBufferRemoveTagByNameMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextBuffer.textBufferRemoveTagByName",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextBuffer.html#v:textBufferRemoveTagByName"
        })


#endif

-- method TextBuffer::select_range
-- method type : OrdinaryMethod
-- Args: [ Arg
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
--       , Arg
--           { argCName = "ins"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "where to put the \8220insert\8221 mark"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "bound"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "where to put the \8220selection_bound\8221 mark"
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

foreign import ccall "gtk_text_buffer_select_range" gtk_text_buffer_select_range :: 
    Ptr TextBuffer ->                       -- buffer : TInterface (Name {namespace = "Gtk", name = "TextBuffer"})
    Ptr Gtk.TextIter.TextIter ->            -- ins : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    Ptr Gtk.TextIter.TextIter ->            -- bound : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    IO ()

-- | This function moves the “insert” and “selection_bound” marks
-- simultaneously.  If you move them in two steps
-- with 'GI.Gtk.Objects.TextBuffer.textBufferMoveMark', you will temporarily select a
-- region in between their old and new locations, which can be pretty
-- inefficient since the temporarily-selected region will force stuff
-- to be recalculated. This function moves them as a unit, which can
-- be optimized.
-- 
-- /Since: 2.4/
textBufferSelectRange ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextBuffer a) =>
    a
    -- ^ /@buffer@/: a t'GI.Gtk.Objects.TextBuffer.TextBuffer'
    -> Gtk.TextIter.TextIter
    -- ^ /@ins@/: where to put the “insert” mark
    -> Gtk.TextIter.TextIter
    -- ^ /@bound@/: where to put the “selection_bound” mark
    -> m ()
textBufferSelectRange buffer ins bound = liftIO $ do
    buffer' <- unsafeManagedPtrCastPtr buffer
    ins' <- unsafeManagedPtrGetPtr ins
    bound' <- unsafeManagedPtrGetPtr bound
    gtk_text_buffer_select_range buffer' ins' bound'
    touchManagedPtr buffer
    touchManagedPtr ins
    touchManagedPtr bound
    return ()

#if defined(ENABLE_OVERLOADING)
data TextBufferSelectRangeMethodInfo
instance (signature ~ (Gtk.TextIter.TextIter -> Gtk.TextIter.TextIter -> m ()), MonadIO m, IsTextBuffer a) => O.OverloadedMethod TextBufferSelectRangeMethodInfo a signature where
    overloadedMethod = textBufferSelectRange

instance O.OverloadedMethodInfo TextBufferSelectRangeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextBuffer.textBufferSelectRange",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextBuffer.html#v:textBufferSelectRange"
        })


#endif

-- method TextBuffer::serialize
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "register_buffer"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextBuffer" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the #GtkTextBuffer @format is registered with"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "content_buffer"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextBuffer" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the #GtkTextBuffer to serialize"
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
--           , argType = TInterface Name { namespace = "Gdk" , name = "Atom" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the rich text format to use for serializing"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "start"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "start of block of text to serialize"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "end"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "end of block of test to serialize"
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
--           , argType = TBasicType TUInt64
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "return location for the length of the serialized data"
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
--              , argType = TBasicType TUInt64
--              , direction = DirectionOut
--              , mayBeNull = False
--              , argDoc =
--                  Documentation
--                    { rawDocText =
--                        Just "return location for the length of the serialized data"
--                    , sinceVersion = Nothing
--                    }
--              , argScope = ScopeTypeInvalid
--              , argClosure = -1
--              , argDestroy = -1
--              , argCallerAllocates = False
--              , transfer = TransferEverything
--              }
--          ]
-- returnType: Just (TCArray False (-1) 5 (TBasicType TUInt8))
-- throws : False
-- Skip return : False

foreign import ccall "gtk_text_buffer_serialize" gtk_text_buffer_serialize :: 
    Ptr TextBuffer ->                       -- register_buffer : TInterface (Name {namespace = "Gtk", name = "TextBuffer"})
    Ptr TextBuffer ->                       -- content_buffer : TInterface (Name {namespace = "Gtk", name = "TextBuffer"})
    Ptr Gdk.Atom.Atom ->                    -- format : TInterface (Name {namespace = "Gdk", name = "Atom"})
    Ptr Gtk.TextIter.TextIter ->            -- start : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    Ptr Gtk.TextIter.TextIter ->            -- end : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    Ptr Word64 ->                           -- length : TBasicType TUInt64
    IO (Ptr Word8)

-- | This function serializes the portion of text between /@start@/
-- and /@end@/ in the rich text format represented by /@format@/.
-- 
-- /@formats@/ to be used must be registered using
-- 'GI.Gtk.Objects.TextBuffer.textBufferRegisterSerializeFormat' or
-- 'GI.Gtk.Objects.TextBuffer.textBufferRegisterSerializeTagset' beforehand.
-- 
-- /Since: 2.10/
textBufferSerialize ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextBuffer a, IsTextBuffer b) =>
    a
    -- ^ /@registerBuffer@/: the t'GI.Gtk.Objects.TextBuffer.TextBuffer' /@format@/ is registered with
    -> b
    -- ^ /@contentBuffer@/: the t'GI.Gtk.Objects.TextBuffer.TextBuffer' to serialize
    -> Gdk.Atom.Atom
    -- ^ /@format@/: the rich text format to use for serializing
    -> Gtk.TextIter.TextIter
    -- ^ /@start@/: start of block of text to serialize
    -> Gtk.TextIter.TextIter
    -- ^ /@end@/: end of block of test to serialize
    -> m ByteString
    -- ^ __Returns:__ the serialized
    --               data, encoded as /@format@/
textBufferSerialize registerBuffer contentBuffer format start end = liftIO $ do
    registerBuffer' <- unsafeManagedPtrCastPtr registerBuffer
    contentBuffer' <- unsafeManagedPtrCastPtr contentBuffer
    format' <- unsafeManagedPtrGetPtr format
    start' <- unsafeManagedPtrGetPtr start
    end' <- unsafeManagedPtrGetPtr end
    length_ <- allocMem :: IO (Ptr Word64)
    result <- gtk_text_buffer_serialize registerBuffer' contentBuffer' format' start' end' length_
    length_' <- peek length_
    checkUnexpectedReturnNULL "textBufferSerialize" result
    result' <- (unpackByteStringWithLength length_') result
    freeMem result
    touchManagedPtr registerBuffer
    touchManagedPtr contentBuffer
    touchManagedPtr format
    touchManagedPtr start
    touchManagedPtr end
    freeMem length_
    return result'

#if defined(ENABLE_OVERLOADING)
data TextBufferSerializeMethodInfo
instance (signature ~ (b -> Gdk.Atom.Atom -> Gtk.TextIter.TextIter -> Gtk.TextIter.TextIter -> m ByteString), MonadIO m, IsTextBuffer a, IsTextBuffer b) => O.OverloadedMethod TextBufferSerializeMethodInfo a signature where
    overloadedMethod = textBufferSerialize

instance O.OverloadedMethodInfo TextBufferSerializeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextBuffer.textBufferSerialize",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextBuffer.html#v:textBufferSerialize"
        })


#endif

-- method TextBuffer::set_modified
-- method type : OrdinaryMethod
-- Args: [ Arg
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
--       , Arg
--           { argCName = "setting"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "modification flag setting"
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

foreign import ccall "gtk_text_buffer_set_modified" gtk_text_buffer_set_modified :: 
    Ptr TextBuffer ->                       -- buffer : TInterface (Name {namespace = "Gtk", name = "TextBuffer"})
    CInt ->                                 -- setting : TBasicType TBoolean
    IO ()

-- | Used to keep track of whether the buffer has been modified since the
-- last time it was saved. Whenever the buffer is saved to disk, call
-- gtk_text_buffer_set_modified (/@buffer@/, FALSE). When the buffer is modified,
-- it will automatically toggled on the modified bit again. When the modified
-- bit flips, the buffer emits the [TextBuffer::modifiedChanged]("GI.Gtk.Objects.TextBuffer#g:signal:modifiedChanged") signal.
textBufferSetModified ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextBuffer a) =>
    a
    -- ^ /@buffer@/: a t'GI.Gtk.Objects.TextBuffer.TextBuffer'
    -> Bool
    -- ^ /@setting@/: modification flag setting
    -> m ()
textBufferSetModified buffer setting = liftIO $ do
    buffer' <- unsafeManagedPtrCastPtr buffer
    let setting' = (fromIntegral . fromEnum) setting
    gtk_text_buffer_set_modified buffer' setting'
    touchManagedPtr buffer
    return ()

#if defined(ENABLE_OVERLOADING)
data TextBufferSetModifiedMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsTextBuffer a) => O.OverloadedMethod TextBufferSetModifiedMethodInfo a signature where
    overloadedMethod = textBufferSetModified

instance O.OverloadedMethodInfo TextBufferSetModifiedMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextBuffer.textBufferSetModified",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextBuffer.html#v:textBufferSetModified"
        })


#endif

-- method TextBuffer::set_text
-- method type : OrdinaryMethod
-- Args: [ Arg
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
--       , Arg
--           { argCName = "text"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "UTF-8 text to insert"
--                 , sinceVersion = Nothing
--                 }
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
--                 { rawDocText = Just "length of @text in bytes"
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

foreign import ccall "gtk_text_buffer_set_text" gtk_text_buffer_set_text :: 
    Ptr TextBuffer ->                       -- buffer : TInterface (Name {namespace = "Gtk", name = "TextBuffer"})
    CString ->                              -- text : TBasicType TUTF8
    Int32 ->                                -- len : TBasicType TInt
    IO ()

-- | Deletes current contents of /@buffer@/, and inserts /@text@/ instead. If
-- /@len@/ is -1, /@text@/ must be nul-terminated. /@text@/ must be valid UTF-8.
textBufferSetText ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextBuffer a) =>
    a
    -- ^ /@buffer@/: a t'GI.Gtk.Objects.TextBuffer.TextBuffer'
    -> T.Text
    -- ^ /@text@/: UTF-8 text to insert
    -> Int32
    -- ^ /@len@/: length of /@text@/ in bytes
    -> m ()
textBufferSetText buffer text len = liftIO $ do
    buffer' <- unsafeManagedPtrCastPtr buffer
    text' <- textToCString text
    gtk_text_buffer_set_text buffer' text' len
    touchManagedPtr buffer
    freeMem text'
    return ()

#if defined(ENABLE_OVERLOADING)
data TextBufferSetTextMethodInfo
instance (signature ~ (T.Text -> Int32 -> m ()), MonadIO m, IsTextBuffer a) => O.OverloadedMethod TextBufferSetTextMethodInfo a signature where
    overloadedMethod = textBufferSetText

instance O.OverloadedMethodInfo TextBufferSetTextMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextBuffer.textBufferSetText",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextBuffer.html#v:textBufferSetText"
        })


#endif

-- method TextBuffer::unregister_deserialize_format
-- method type : OrdinaryMethod
-- Args: [ Arg
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
--       , Arg
--           { argCName = "format"
--           , argType = TInterface Name { namespace = "Gdk" , name = "Atom" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "a #GdkAtom representing a registered rich text format."
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

foreign import ccall "gtk_text_buffer_unregister_deserialize_format" gtk_text_buffer_unregister_deserialize_format :: 
    Ptr TextBuffer ->                       -- buffer : TInterface (Name {namespace = "Gtk", name = "TextBuffer"})
    Ptr Gdk.Atom.Atom ->                    -- format : TInterface (Name {namespace = "Gdk", name = "Atom"})
    IO ()

-- | This function unregisters a rich text format that was previously
-- registered using 'GI.Gtk.Objects.TextBuffer.textBufferRegisterDeserializeFormat' or
-- 'GI.Gtk.Objects.TextBuffer.textBufferRegisterDeserializeTagset'.
-- 
-- /Since: 2.10/
textBufferUnregisterDeserializeFormat ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextBuffer a) =>
    a
    -- ^ /@buffer@/: a t'GI.Gtk.Objects.TextBuffer.TextBuffer'
    -> Gdk.Atom.Atom
    -- ^ /@format@/: a t'GI.Gdk.Structs.Atom.Atom' representing a registered rich text format.
    -> m ()
textBufferUnregisterDeserializeFormat buffer format = liftIO $ do
    buffer' <- unsafeManagedPtrCastPtr buffer
    format' <- unsafeManagedPtrGetPtr format
    gtk_text_buffer_unregister_deserialize_format buffer' format'
    touchManagedPtr buffer
    touchManagedPtr format
    return ()

#if defined(ENABLE_OVERLOADING)
data TextBufferUnregisterDeserializeFormatMethodInfo
instance (signature ~ (Gdk.Atom.Atom -> m ()), MonadIO m, IsTextBuffer a) => O.OverloadedMethod TextBufferUnregisterDeserializeFormatMethodInfo a signature where
    overloadedMethod = textBufferUnregisterDeserializeFormat

instance O.OverloadedMethodInfo TextBufferUnregisterDeserializeFormatMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextBuffer.textBufferUnregisterDeserializeFormat",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextBuffer.html#v:textBufferUnregisterDeserializeFormat"
        })


#endif

-- method TextBuffer::unregister_serialize_format
-- method type : OrdinaryMethod
-- Args: [ Arg
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
--       , Arg
--           { argCName = "format"
--           , argType = TInterface Name { namespace = "Gdk" , name = "Atom" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "a #GdkAtom representing a registered rich text format."
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

foreign import ccall "gtk_text_buffer_unregister_serialize_format" gtk_text_buffer_unregister_serialize_format :: 
    Ptr TextBuffer ->                       -- buffer : TInterface (Name {namespace = "Gtk", name = "TextBuffer"})
    Ptr Gdk.Atom.Atom ->                    -- format : TInterface (Name {namespace = "Gdk", name = "Atom"})
    IO ()

-- | This function unregisters a rich text format that was previously
-- registered using 'GI.Gtk.Objects.TextBuffer.textBufferRegisterSerializeFormat' or
-- 'GI.Gtk.Objects.TextBuffer.textBufferRegisterSerializeTagset'
-- 
-- /Since: 2.10/
textBufferUnregisterSerializeFormat ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextBuffer a) =>
    a
    -- ^ /@buffer@/: a t'GI.Gtk.Objects.TextBuffer.TextBuffer'
    -> Gdk.Atom.Atom
    -- ^ /@format@/: a t'GI.Gdk.Structs.Atom.Atom' representing a registered rich text format.
    -> m ()
textBufferUnregisterSerializeFormat buffer format = liftIO $ do
    buffer' <- unsafeManagedPtrCastPtr buffer
    format' <- unsafeManagedPtrGetPtr format
    gtk_text_buffer_unregister_serialize_format buffer' format'
    touchManagedPtr buffer
    touchManagedPtr format
    return ()

#if defined(ENABLE_OVERLOADING)
data TextBufferUnregisterSerializeFormatMethodInfo
instance (signature ~ (Gdk.Atom.Atom -> m ()), MonadIO m, IsTextBuffer a) => O.OverloadedMethod TextBufferUnregisterSerializeFormatMethodInfo a signature where
    overloadedMethod = textBufferUnregisterSerializeFormat

instance O.OverloadedMethodInfo TextBufferUnregisterSerializeFormatMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextBuffer.textBufferUnregisterSerializeFormat",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextBuffer.html#v:textBufferUnregisterSerializeFormat"
        })


#endif


