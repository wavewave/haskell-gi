{-# LANGUAGE TypeApplications #-}


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

module GI.Gtk.Structs.TextIter
    ( 

-- * Exported types
    TextIter(..)                            ,
    newZeroTextIter                         ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [assign]("GI.Gtk.Structs.TextIter#g:method:assign"), [backwardChar]("GI.Gtk.Structs.TextIter#g:method:backwardChar"), [backwardChars]("GI.Gtk.Structs.TextIter#g:method:backwardChars"), [backwardCursorPosition]("GI.Gtk.Structs.TextIter#g:method:backwardCursorPosition"), [backwardCursorPositions]("GI.Gtk.Structs.TextIter#g:method:backwardCursorPositions"), [backwardFindChar]("GI.Gtk.Structs.TextIter#g:method:backwardFindChar"), [backwardLine]("GI.Gtk.Structs.TextIter#g:method:backwardLine"), [backwardLines]("GI.Gtk.Structs.TextIter#g:method:backwardLines"), [backwardSearch]("GI.Gtk.Structs.TextIter#g:method:backwardSearch"), [backwardSentenceStart]("GI.Gtk.Structs.TextIter#g:method:backwardSentenceStart"), [backwardSentenceStarts]("GI.Gtk.Structs.TextIter#g:method:backwardSentenceStarts"), [backwardToTagToggle]("GI.Gtk.Structs.TextIter#g:method:backwardToTagToggle"), [backwardVisibleCursorPosition]("GI.Gtk.Structs.TextIter#g:method:backwardVisibleCursorPosition"), [backwardVisibleCursorPositions]("GI.Gtk.Structs.TextIter#g:method:backwardVisibleCursorPositions"), [backwardVisibleLine]("GI.Gtk.Structs.TextIter#g:method:backwardVisibleLine"), [backwardVisibleLines]("GI.Gtk.Structs.TextIter#g:method:backwardVisibleLines"), [backwardVisibleWordStart]("GI.Gtk.Structs.TextIter#g:method:backwardVisibleWordStart"), [backwardVisibleWordStarts]("GI.Gtk.Structs.TextIter#g:method:backwardVisibleWordStarts"), [backwardWordStart]("GI.Gtk.Structs.TextIter#g:method:backwardWordStart"), [backwardWordStarts]("GI.Gtk.Structs.TextIter#g:method:backwardWordStarts"), [beginsTag]("GI.Gtk.Structs.TextIter#g:method:beginsTag"), [canInsert]("GI.Gtk.Structs.TextIter#g:method:canInsert"), [compare]("GI.Gtk.Structs.TextIter#g:method:compare"), [copy]("GI.Gtk.Structs.TextIter#g:method:copy"), [editable]("GI.Gtk.Structs.TextIter#g:method:editable"), [endsLine]("GI.Gtk.Structs.TextIter#g:method:endsLine"), [endsSentence]("GI.Gtk.Structs.TextIter#g:method:endsSentence"), [endsTag]("GI.Gtk.Structs.TextIter#g:method:endsTag"), [endsWord]("GI.Gtk.Structs.TextIter#g:method:endsWord"), [equal]("GI.Gtk.Structs.TextIter#g:method:equal"), [forwardChar]("GI.Gtk.Structs.TextIter#g:method:forwardChar"), [forwardChars]("GI.Gtk.Structs.TextIter#g:method:forwardChars"), [forwardCursorPosition]("GI.Gtk.Structs.TextIter#g:method:forwardCursorPosition"), [forwardCursorPositions]("GI.Gtk.Structs.TextIter#g:method:forwardCursorPositions"), [forwardFindChar]("GI.Gtk.Structs.TextIter#g:method:forwardFindChar"), [forwardLine]("GI.Gtk.Structs.TextIter#g:method:forwardLine"), [forwardLines]("GI.Gtk.Structs.TextIter#g:method:forwardLines"), [forwardSearch]("GI.Gtk.Structs.TextIter#g:method:forwardSearch"), [forwardSentenceEnd]("GI.Gtk.Structs.TextIter#g:method:forwardSentenceEnd"), [forwardSentenceEnds]("GI.Gtk.Structs.TextIter#g:method:forwardSentenceEnds"), [forwardToEnd]("GI.Gtk.Structs.TextIter#g:method:forwardToEnd"), [forwardToLineEnd]("GI.Gtk.Structs.TextIter#g:method:forwardToLineEnd"), [forwardToTagToggle]("GI.Gtk.Structs.TextIter#g:method:forwardToTagToggle"), [forwardVisibleCursorPosition]("GI.Gtk.Structs.TextIter#g:method:forwardVisibleCursorPosition"), [forwardVisibleCursorPositions]("GI.Gtk.Structs.TextIter#g:method:forwardVisibleCursorPositions"), [forwardVisibleLine]("GI.Gtk.Structs.TextIter#g:method:forwardVisibleLine"), [forwardVisibleLines]("GI.Gtk.Structs.TextIter#g:method:forwardVisibleLines"), [forwardVisibleWordEnd]("GI.Gtk.Structs.TextIter#g:method:forwardVisibleWordEnd"), [forwardVisibleWordEnds]("GI.Gtk.Structs.TextIter#g:method:forwardVisibleWordEnds"), [forwardWordEnd]("GI.Gtk.Structs.TextIter#g:method:forwardWordEnd"), [forwardWordEnds]("GI.Gtk.Structs.TextIter#g:method:forwardWordEnds"), [free]("GI.Gtk.Structs.TextIter#g:method:free"), [hasTag]("GI.Gtk.Structs.TextIter#g:method:hasTag"), [inRange]("GI.Gtk.Structs.TextIter#g:method:inRange"), [insideSentence]("GI.Gtk.Structs.TextIter#g:method:insideSentence"), [insideWord]("GI.Gtk.Structs.TextIter#g:method:insideWord"), [isCursorPosition]("GI.Gtk.Structs.TextIter#g:method:isCursorPosition"), [isEnd]("GI.Gtk.Structs.TextIter#g:method:isEnd"), [isStart]("GI.Gtk.Structs.TextIter#g:method:isStart"), [order]("GI.Gtk.Structs.TextIter#g:method:order"), [startsLine]("GI.Gtk.Structs.TextIter#g:method:startsLine"), [startsSentence]("GI.Gtk.Structs.TextIter#g:method:startsSentence"), [startsTag]("GI.Gtk.Structs.TextIter#g:method:startsTag"), [startsWord]("GI.Gtk.Structs.TextIter#g:method:startsWord"), [togglesTag]("GI.Gtk.Structs.TextIter#g:method:togglesTag").
-- 
-- ==== Getters
-- [getAttributes]("GI.Gtk.Structs.TextIter#g:method:getAttributes"), [getBuffer]("GI.Gtk.Structs.TextIter#g:method:getBuffer"), [getBytesInLine]("GI.Gtk.Structs.TextIter#g:method:getBytesInLine"), [getChar]("GI.Gtk.Structs.TextIter#g:method:getChar"), [getCharsInLine]("GI.Gtk.Structs.TextIter#g:method:getCharsInLine"), [getChildAnchor]("GI.Gtk.Structs.TextIter#g:method:getChildAnchor"), [getLanguage]("GI.Gtk.Structs.TextIter#g:method:getLanguage"), [getLine]("GI.Gtk.Structs.TextIter#g:method:getLine"), [getLineIndex]("GI.Gtk.Structs.TextIter#g:method:getLineIndex"), [getLineOffset]("GI.Gtk.Structs.TextIter#g:method:getLineOffset"), [getMarks]("GI.Gtk.Structs.TextIter#g:method:getMarks"), [getOffset]("GI.Gtk.Structs.TextIter#g:method:getOffset"), [getPixbuf]("GI.Gtk.Structs.TextIter#g:method:getPixbuf"), [getSlice]("GI.Gtk.Structs.TextIter#g:method:getSlice"), [getTags]("GI.Gtk.Structs.TextIter#g:method:getTags"), [getText]("GI.Gtk.Structs.TextIter#g:method:getText"), [getToggledTags]("GI.Gtk.Structs.TextIter#g:method:getToggledTags"), [getVisibleLineIndex]("GI.Gtk.Structs.TextIter#g:method:getVisibleLineIndex"), [getVisibleLineOffset]("GI.Gtk.Structs.TextIter#g:method:getVisibleLineOffset"), [getVisibleSlice]("GI.Gtk.Structs.TextIter#g:method:getVisibleSlice"), [getVisibleText]("GI.Gtk.Structs.TextIter#g:method:getVisibleText").
-- 
-- ==== Setters
-- [setLine]("GI.Gtk.Structs.TextIter#g:method:setLine"), [setLineIndex]("GI.Gtk.Structs.TextIter#g:method:setLineIndex"), [setLineOffset]("GI.Gtk.Structs.TextIter#g:method:setLineOffset"), [setOffset]("GI.Gtk.Structs.TextIter#g:method:setOffset"), [setVisibleLineIndex]("GI.Gtk.Structs.TextIter#g:method:setVisibleLineIndex"), [setVisibleLineOffset]("GI.Gtk.Structs.TextIter#g:method:setVisibleLineOffset").

#if defined(ENABLE_OVERLOADING)
    ResolveTextIterMethod                   ,
#endif

-- ** assign #method:assign#

#if defined(ENABLE_OVERLOADING)
    TextIterAssignMethodInfo                ,
#endif
    textIterAssign                          ,


-- ** backwardChar #method:backwardChar#

#if defined(ENABLE_OVERLOADING)
    TextIterBackwardCharMethodInfo          ,
#endif
    textIterBackwardChar                    ,


-- ** backwardChars #method:backwardChars#

#if defined(ENABLE_OVERLOADING)
    TextIterBackwardCharsMethodInfo         ,
#endif
    textIterBackwardChars                   ,


-- ** backwardCursorPosition #method:backwardCursorPosition#

#if defined(ENABLE_OVERLOADING)
    TextIterBackwardCursorPositionMethodInfo,
#endif
    textIterBackwardCursorPosition          ,


-- ** backwardCursorPositions #method:backwardCursorPositions#

#if defined(ENABLE_OVERLOADING)
    TextIterBackwardCursorPositionsMethodInfo,
#endif
    textIterBackwardCursorPositions         ,


-- ** backwardFindChar #method:backwardFindChar#

#if defined(ENABLE_OVERLOADING)
    TextIterBackwardFindCharMethodInfo      ,
#endif
    textIterBackwardFindChar                ,


-- ** backwardLine #method:backwardLine#

#if defined(ENABLE_OVERLOADING)
    TextIterBackwardLineMethodInfo          ,
#endif
    textIterBackwardLine                    ,


-- ** backwardLines #method:backwardLines#

#if defined(ENABLE_OVERLOADING)
    TextIterBackwardLinesMethodInfo         ,
#endif
    textIterBackwardLines                   ,


-- ** backwardSearch #method:backwardSearch#

#if defined(ENABLE_OVERLOADING)
    TextIterBackwardSearchMethodInfo        ,
#endif
    textIterBackwardSearch                  ,


-- ** backwardSentenceStart #method:backwardSentenceStart#

#if defined(ENABLE_OVERLOADING)
    TextIterBackwardSentenceStartMethodInfo ,
#endif
    textIterBackwardSentenceStart           ,


-- ** backwardSentenceStarts #method:backwardSentenceStarts#

#if defined(ENABLE_OVERLOADING)
    TextIterBackwardSentenceStartsMethodInfo,
#endif
    textIterBackwardSentenceStarts          ,


-- ** backwardToTagToggle #method:backwardToTagToggle#

#if defined(ENABLE_OVERLOADING)
    TextIterBackwardToTagToggleMethodInfo   ,
#endif
    textIterBackwardToTagToggle             ,


-- ** backwardVisibleCursorPosition #method:backwardVisibleCursorPosition#

#if defined(ENABLE_OVERLOADING)
    TextIterBackwardVisibleCursorPositionMethodInfo,
#endif
    textIterBackwardVisibleCursorPosition   ,


-- ** backwardVisibleCursorPositions #method:backwardVisibleCursorPositions#

#if defined(ENABLE_OVERLOADING)
    TextIterBackwardVisibleCursorPositionsMethodInfo,
#endif
    textIterBackwardVisibleCursorPositions  ,


-- ** backwardVisibleLine #method:backwardVisibleLine#

#if defined(ENABLE_OVERLOADING)
    TextIterBackwardVisibleLineMethodInfo   ,
#endif
    textIterBackwardVisibleLine             ,


-- ** backwardVisibleLines #method:backwardVisibleLines#

#if defined(ENABLE_OVERLOADING)
    TextIterBackwardVisibleLinesMethodInfo  ,
#endif
    textIterBackwardVisibleLines            ,


-- ** backwardVisibleWordStart #method:backwardVisibleWordStart#

#if defined(ENABLE_OVERLOADING)
    TextIterBackwardVisibleWordStartMethodInfo,
#endif
    textIterBackwardVisibleWordStart        ,


-- ** backwardVisibleWordStarts #method:backwardVisibleWordStarts#

#if defined(ENABLE_OVERLOADING)
    TextIterBackwardVisibleWordStartsMethodInfo,
#endif
    textIterBackwardVisibleWordStarts       ,


-- ** backwardWordStart #method:backwardWordStart#

#if defined(ENABLE_OVERLOADING)
    TextIterBackwardWordStartMethodInfo     ,
#endif
    textIterBackwardWordStart               ,


-- ** backwardWordStarts #method:backwardWordStarts#

#if defined(ENABLE_OVERLOADING)
    TextIterBackwardWordStartsMethodInfo    ,
#endif
    textIterBackwardWordStarts              ,


-- ** beginsTag #method:beginsTag#

#if defined(ENABLE_OVERLOADING)
    TextIterBeginsTagMethodInfo             ,
#endif
    textIterBeginsTag                       ,


-- ** canInsert #method:canInsert#

#if defined(ENABLE_OVERLOADING)
    TextIterCanInsertMethodInfo             ,
#endif
    textIterCanInsert                       ,


-- ** compare #method:compare#

#if defined(ENABLE_OVERLOADING)
    TextIterCompareMethodInfo               ,
#endif
    textIterCompare                         ,


-- ** copy #method:copy#

#if defined(ENABLE_OVERLOADING)
    TextIterCopyMethodInfo                  ,
#endif
    textIterCopy                            ,


-- ** editable #method:editable#

#if defined(ENABLE_OVERLOADING)
    TextIterEditableMethodInfo              ,
#endif
    textIterEditable                        ,


-- ** endsLine #method:endsLine#

#if defined(ENABLE_OVERLOADING)
    TextIterEndsLineMethodInfo              ,
#endif
    textIterEndsLine                        ,


-- ** endsSentence #method:endsSentence#

#if defined(ENABLE_OVERLOADING)
    TextIterEndsSentenceMethodInfo          ,
#endif
    textIterEndsSentence                    ,


-- ** endsTag #method:endsTag#

#if defined(ENABLE_OVERLOADING)
    TextIterEndsTagMethodInfo               ,
#endif
    textIterEndsTag                         ,


-- ** endsWord #method:endsWord#

#if defined(ENABLE_OVERLOADING)
    TextIterEndsWordMethodInfo              ,
#endif
    textIterEndsWord                        ,


-- ** equal #method:equal#

#if defined(ENABLE_OVERLOADING)
    TextIterEqualMethodInfo                 ,
#endif
    textIterEqual                           ,


-- ** forwardChar #method:forwardChar#

#if defined(ENABLE_OVERLOADING)
    TextIterForwardCharMethodInfo           ,
#endif
    textIterForwardChar                     ,


-- ** forwardChars #method:forwardChars#

#if defined(ENABLE_OVERLOADING)
    TextIterForwardCharsMethodInfo          ,
#endif
    textIterForwardChars                    ,


-- ** forwardCursorPosition #method:forwardCursorPosition#

#if defined(ENABLE_OVERLOADING)
    TextIterForwardCursorPositionMethodInfo ,
#endif
    textIterForwardCursorPosition           ,


-- ** forwardCursorPositions #method:forwardCursorPositions#

#if defined(ENABLE_OVERLOADING)
    TextIterForwardCursorPositionsMethodInfo,
#endif
    textIterForwardCursorPositions          ,


-- ** forwardFindChar #method:forwardFindChar#

#if defined(ENABLE_OVERLOADING)
    TextIterForwardFindCharMethodInfo       ,
#endif
    textIterForwardFindChar                 ,


-- ** forwardLine #method:forwardLine#

#if defined(ENABLE_OVERLOADING)
    TextIterForwardLineMethodInfo           ,
#endif
    textIterForwardLine                     ,


-- ** forwardLines #method:forwardLines#

#if defined(ENABLE_OVERLOADING)
    TextIterForwardLinesMethodInfo          ,
#endif
    textIterForwardLines                    ,


-- ** forwardSearch #method:forwardSearch#

#if defined(ENABLE_OVERLOADING)
    TextIterForwardSearchMethodInfo         ,
#endif
    textIterForwardSearch                   ,


-- ** forwardSentenceEnd #method:forwardSentenceEnd#

#if defined(ENABLE_OVERLOADING)
    TextIterForwardSentenceEndMethodInfo    ,
#endif
    textIterForwardSentenceEnd              ,


-- ** forwardSentenceEnds #method:forwardSentenceEnds#

#if defined(ENABLE_OVERLOADING)
    TextIterForwardSentenceEndsMethodInfo   ,
#endif
    textIterForwardSentenceEnds             ,


-- ** forwardToEnd #method:forwardToEnd#

#if defined(ENABLE_OVERLOADING)
    TextIterForwardToEndMethodInfo          ,
#endif
    textIterForwardToEnd                    ,


-- ** forwardToLineEnd #method:forwardToLineEnd#

#if defined(ENABLE_OVERLOADING)
    TextIterForwardToLineEndMethodInfo      ,
#endif
    textIterForwardToLineEnd                ,


-- ** forwardToTagToggle #method:forwardToTagToggle#

#if defined(ENABLE_OVERLOADING)
    TextIterForwardToTagToggleMethodInfo    ,
#endif
    textIterForwardToTagToggle              ,


-- ** forwardVisibleCursorPosition #method:forwardVisibleCursorPosition#

#if defined(ENABLE_OVERLOADING)
    TextIterForwardVisibleCursorPositionMethodInfo,
#endif
    textIterForwardVisibleCursorPosition    ,


-- ** forwardVisibleCursorPositions #method:forwardVisibleCursorPositions#

#if defined(ENABLE_OVERLOADING)
    TextIterForwardVisibleCursorPositionsMethodInfo,
#endif
    textIterForwardVisibleCursorPositions   ,


-- ** forwardVisibleLine #method:forwardVisibleLine#

#if defined(ENABLE_OVERLOADING)
    TextIterForwardVisibleLineMethodInfo    ,
#endif
    textIterForwardVisibleLine              ,


-- ** forwardVisibleLines #method:forwardVisibleLines#

#if defined(ENABLE_OVERLOADING)
    TextIterForwardVisibleLinesMethodInfo   ,
#endif
    textIterForwardVisibleLines             ,


-- ** forwardVisibleWordEnd #method:forwardVisibleWordEnd#

#if defined(ENABLE_OVERLOADING)
    TextIterForwardVisibleWordEndMethodInfo ,
#endif
    textIterForwardVisibleWordEnd           ,


-- ** forwardVisibleWordEnds #method:forwardVisibleWordEnds#

#if defined(ENABLE_OVERLOADING)
    TextIterForwardVisibleWordEndsMethodInfo,
#endif
    textIterForwardVisibleWordEnds          ,


-- ** forwardWordEnd #method:forwardWordEnd#

#if defined(ENABLE_OVERLOADING)
    TextIterForwardWordEndMethodInfo        ,
#endif
    textIterForwardWordEnd                  ,


-- ** forwardWordEnds #method:forwardWordEnds#

#if defined(ENABLE_OVERLOADING)
    TextIterForwardWordEndsMethodInfo       ,
#endif
    textIterForwardWordEnds                 ,


-- ** free #method:free#

#if defined(ENABLE_OVERLOADING)
    TextIterFreeMethodInfo                  ,
#endif
    textIterFree                            ,


-- ** getAttributes #method:getAttributes#

#if defined(ENABLE_OVERLOADING)
    TextIterGetAttributesMethodInfo         ,
#endif
    textIterGetAttributes                   ,


-- ** getBuffer #method:getBuffer#

#if defined(ENABLE_OVERLOADING)
    TextIterGetBufferMethodInfo             ,
#endif
    textIterGetBuffer                       ,


-- ** getBytesInLine #method:getBytesInLine#

#if defined(ENABLE_OVERLOADING)
    TextIterGetBytesInLineMethodInfo        ,
#endif
    textIterGetBytesInLine                  ,


-- ** getChar #method:getChar#

#if defined(ENABLE_OVERLOADING)
    TextIterGetCharMethodInfo               ,
#endif
    textIterGetChar                         ,


-- ** getCharsInLine #method:getCharsInLine#

#if defined(ENABLE_OVERLOADING)
    TextIterGetCharsInLineMethodInfo        ,
#endif
    textIterGetCharsInLine                  ,


-- ** getChildAnchor #method:getChildAnchor#

#if defined(ENABLE_OVERLOADING)
    TextIterGetChildAnchorMethodInfo        ,
#endif
    textIterGetChildAnchor                  ,


-- ** getLanguage #method:getLanguage#

#if defined(ENABLE_OVERLOADING)
    TextIterGetLanguageMethodInfo           ,
#endif
    textIterGetLanguage                     ,


-- ** getLine #method:getLine#

#if defined(ENABLE_OVERLOADING)
    TextIterGetLineMethodInfo               ,
#endif
    textIterGetLine                         ,


-- ** getLineIndex #method:getLineIndex#

#if defined(ENABLE_OVERLOADING)
    TextIterGetLineIndexMethodInfo          ,
#endif
    textIterGetLineIndex                    ,


-- ** getLineOffset #method:getLineOffset#

#if defined(ENABLE_OVERLOADING)
    TextIterGetLineOffsetMethodInfo         ,
#endif
    textIterGetLineOffset                   ,


-- ** getMarks #method:getMarks#

#if defined(ENABLE_OVERLOADING)
    TextIterGetMarksMethodInfo              ,
#endif
    textIterGetMarks                        ,


-- ** getOffset #method:getOffset#

#if defined(ENABLE_OVERLOADING)
    TextIterGetOffsetMethodInfo             ,
#endif
    textIterGetOffset                       ,


-- ** getPixbuf #method:getPixbuf#

#if defined(ENABLE_OVERLOADING)
    TextIterGetPixbufMethodInfo             ,
#endif
    textIterGetPixbuf                       ,


-- ** getSlice #method:getSlice#

#if defined(ENABLE_OVERLOADING)
    TextIterGetSliceMethodInfo              ,
#endif
    textIterGetSlice                        ,


-- ** getTags #method:getTags#

#if defined(ENABLE_OVERLOADING)
    TextIterGetTagsMethodInfo               ,
#endif
    textIterGetTags                         ,


-- ** getText #method:getText#

#if defined(ENABLE_OVERLOADING)
    TextIterGetTextMethodInfo               ,
#endif
    textIterGetText                         ,


-- ** getToggledTags #method:getToggledTags#

#if defined(ENABLE_OVERLOADING)
    TextIterGetToggledTagsMethodInfo        ,
#endif
    textIterGetToggledTags                  ,


-- ** getVisibleLineIndex #method:getVisibleLineIndex#

#if defined(ENABLE_OVERLOADING)
    TextIterGetVisibleLineIndexMethodInfo   ,
#endif
    textIterGetVisibleLineIndex             ,


-- ** getVisibleLineOffset #method:getVisibleLineOffset#

#if defined(ENABLE_OVERLOADING)
    TextIterGetVisibleLineOffsetMethodInfo  ,
#endif
    textIterGetVisibleLineOffset            ,


-- ** getVisibleSlice #method:getVisibleSlice#

#if defined(ENABLE_OVERLOADING)
    TextIterGetVisibleSliceMethodInfo       ,
#endif
    textIterGetVisibleSlice                 ,


-- ** getVisibleText #method:getVisibleText#

#if defined(ENABLE_OVERLOADING)
    TextIterGetVisibleTextMethodInfo        ,
#endif
    textIterGetVisibleText                  ,


-- ** hasTag #method:hasTag#

#if defined(ENABLE_OVERLOADING)
    TextIterHasTagMethodInfo                ,
#endif
    textIterHasTag                          ,


-- ** inRange #method:inRange#

#if defined(ENABLE_OVERLOADING)
    TextIterInRangeMethodInfo               ,
#endif
    textIterInRange                         ,


-- ** insideSentence #method:insideSentence#

#if defined(ENABLE_OVERLOADING)
    TextIterInsideSentenceMethodInfo        ,
#endif
    textIterInsideSentence                  ,


-- ** insideWord #method:insideWord#

#if defined(ENABLE_OVERLOADING)
    TextIterInsideWordMethodInfo            ,
#endif
    textIterInsideWord                      ,


-- ** isCursorPosition #method:isCursorPosition#

#if defined(ENABLE_OVERLOADING)
    TextIterIsCursorPositionMethodInfo      ,
#endif
    textIterIsCursorPosition                ,


-- ** isEnd #method:isEnd#

#if defined(ENABLE_OVERLOADING)
    TextIterIsEndMethodInfo                 ,
#endif
    textIterIsEnd                           ,


-- ** isStart #method:isStart#

#if defined(ENABLE_OVERLOADING)
    TextIterIsStartMethodInfo               ,
#endif
    textIterIsStart                         ,


-- ** order #method:order#

#if defined(ENABLE_OVERLOADING)
    TextIterOrderMethodInfo                 ,
#endif
    textIterOrder                           ,


-- ** setLine #method:setLine#

#if defined(ENABLE_OVERLOADING)
    TextIterSetLineMethodInfo               ,
#endif
    textIterSetLine                         ,


-- ** setLineIndex #method:setLineIndex#

#if defined(ENABLE_OVERLOADING)
    TextIterSetLineIndexMethodInfo          ,
#endif
    textIterSetLineIndex                    ,


-- ** setLineOffset #method:setLineOffset#

#if defined(ENABLE_OVERLOADING)
    TextIterSetLineOffsetMethodInfo         ,
#endif
    textIterSetLineOffset                   ,


-- ** setOffset #method:setOffset#

#if defined(ENABLE_OVERLOADING)
    TextIterSetOffsetMethodInfo             ,
#endif
    textIterSetOffset                       ,


-- ** setVisibleLineIndex #method:setVisibleLineIndex#

#if defined(ENABLE_OVERLOADING)
    TextIterSetVisibleLineIndexMethodInfo   ,
#endif
    textIterSetVisibleLineIndex             ,


-- ** setVisibleLineOffset #method:setVisibleLineOffset#

#if defined(ENABLE_OVERLOADING)
    TextIterSetVisibleLineOffsetMethodInfo  ,
#endif
    textIterSetVisibleLineOffset            ,


-- ** startsLine #method:startsLine#

#if defined(ENABLE_OVERLOADING)
    TextIterStartsLineMethodInfo            ,
#endif
    textIterStartsLine                      ,


-- ** startsSentence #method:startsSentence#

#if defined(ENABLE_OVERLOADING)
    TextIterStartsSentenceMethodInfo        ,
#endif
    textIterStartsSentence                  ,


-- ** startsTag #method:startsTag#

#if defined(ENABLE_OVERLOADING)
    TextIterStartsTagMethodInfo             ,
#endif
    textIterStartsTag                       ,


-- ** startsWord #method:startsWord#

#if defined(ENABLE_OVERLOADING)
    TextIterStartsWordMethodInfo            ,
#endif
    textIterStartsWord                      ,


-- ** togglesTag #method:togglesTag#

#if defined(ENABLE_OVERLOADING)
    TextIterTogglesTagMethodInfo            ,
#endif
    textIterTogglesTag                      ,




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

import qualified GI.GdkPixbuf.Objects.Pixbuf as GdkPixbuf.Pixbuf
import qualified GI.Gtk.Callbacks as Gtk.Callbacks
import {-# SOURCE #-} qualified GI.Gtk.Flags as Gtk.Flags
import {-# SOURCE #-} qualified GI.Gtk.Objects.TextBuffer as Gtk.TextBuffer
import {-# SOURCE #-} qualified GI.Gtk.Objects.TextChildAnchor as Gtk.TextChildAnchor
import {-# SOURCE #-} qualified GI.Gtk.Objects.TextMark as Gtk.TextMark
import {-# SOURCE #-} qualified GI.Gtk.Objects.TextTag as Gtk.TextTag
import {-# SOURCE #-} qualified GI.Gtk.Structs.TextAttributes as Gtk.TextAttributes
import qualified GI.Pango.Structs.Language as Pango.Language

-- | Memory-managed wrapper type.
newtype TextIter = TextIter (SP.ManagedPtr TextIter)
    deriving (Eq)

instance SP.ManagedPtrNewtype TextIter where
    toManagedPtr (TextIter p) = p

foreign import ccall "gtk_text_iter_get_type" c_gtk_text_iter_get_type :: 
    IO GType

type instance O.ParentTypes TextIter = '[]
instance O.HasParentTypes TextIter

instance B.Types.TypedObject TextIter where
    glibType = c_gtk_text_iter_get_type

instance B.Types.GBoxed TextIter

-- | Convert 'TextIter' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe TextIter) where
    gvalueGType_ = c_gtk_text_iter_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_boxed gv (FP.nullPtr :: FP.Ptr TextIter)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_boxed gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_boxed gv :: IO (Ptr TextIter)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newBoxed TextIter ptr
        else return P.Nothing
        
    

-- | Construct a `TextIter` struct initialized to zero.
newZeroTextIter :: MonadIO m => m TextIter
newZeroTextIter = liftIO $ callocBoxedBytes 80 >>= wrapBoxed TextIter

instance tag ~ 'AttrSet => Constructible TextIter tag where
    new _ attrs = do
        o <- newZeroTextIter
        GI.Attributes.set o attrs
        return o



#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList TextIter
type instance O.AttributeList TextIter = TextIterAttributeList
type TextIterAttributeList = ('[ ] :: [(Symbol, *)])
#endif

-- method TextIter::assign
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextIter" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "other"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "another #GtkTextIter"
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

foreign import ccall "gtk_text_iter_assign" gtk_text_iter_assign :: 
    Ptr TextIter ->                         -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    Ptr TextIter ->                         -- other : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    IO ()

-- | Assigns the value of /@other@/ to /@iter@/.  This function
-- is not useful in applications, because iterators can be assigned
-- with @GtkTextIter i = j;@. The
-- function is used by language bindings.
-- 
-- /Since: 3.2/
textIterAssign ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TextIter
    -- ^ /@iter@/: a t'GI.Gtk.Structs.TextIter.TextIter'
    -> TextIter
    -- ^ /@other@/: another t'GI.Gtk.Structs.TextIter.TextIter'
    -> m ()
textIterAssign iter other = liftIO $ do
    iter' <- unsafeManagedPtrGetPtr iter
    other' <- unsafeManagedPtrGetPtr other
    gtk_text_iter_assign iter' other'
    touchManagedPtr iter
    touchManagedPtr other
    return ()

#if defined(ENABLE_OVERLOADING)
data TextIterAssignMethodInfo
instance (signature ~ (TextIter -> m ()), MonadIO m) => O.OverloadedMethod TextIterAssignMethodInfo TextIter signature where
    overloadedMethod = textIterAssign

instance O.OverloadedMethodInfo TextIterAssignMethodInfo TextIter where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextIter.textIterAssign",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextIter.html#v:textIterAssign"
        })


#endif

-- method TextIter::backward_char
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "an iterator" , sinceVersion = Nothing }
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

foreign import ccall "gtk_text_iter_backward_char" gtk_text_iter_backward_char :: 
    Ptr TextIter ->                         -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    IO CInt

-- | Moves backward by one character offset. Returns 'P.True' if movement
-- was possible; if /@iter@/ was the first in the buffer (character
-- offset 0), 'GI.Gtk.Structs.TextIter.textIterBackwardChar' returns 'P.False' for convenience when
-- writing loops.
textIterBackwardChar ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TextIter
    -- ^ /@iter@/: an iterator
    -> m Bool
    -- ^ __Returns:__ whether movement was possible
textIterBackwardChar iter = liftIO $ do
    iter' <- unsafeManagedPtrGetPtr iter
    result <- gtk_text_iter_backward_char iter'
    let result' = (/= 0) result
    touchManagedPtr iter
    return result'

#if defined(ENABLE_OVERLOADING)
data TextIterBackwardCharMethodInfo
instance (signature ~ (m Bool), MonadIO m) => O.OverloadedMethod TextIterBackwardCharMethodInfo TextIter signature where
    overloadedMethod = textIterBackwardChar

instance O.OverloadedMethodInfo TextIterBackwardCharMethodInfo TextIter where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextIter.textIterBackwardChar",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextIter.html#v:textIterBackwardChar"
        })


#endif

-- method TextIter::backward_chars
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "an iterator" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "count"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "number of characters to move"
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

foreign import ccall "gtk_text_iter_backward_chars" gtk_text_iter_backward_chars :: 
    Ptr TextIter ->                         -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    Int32 ->                                -- count : TBasicType TInt
    IO CInt

-- | Moves /@count@/ characters backward, if possible (if /@count@/ would move
-- past the start or end of the buffer, moves to the start or end of
-- the buffer).  The return value indicates whether the iterator moved
-- onto a dereferenceable position; if the iterator didn’t move, or
-- moved onto the end iterator, then 'P.False' is returned. If /@count@/ is 0,
-- the function does nothing and returns 'P.False'.
textIterBackwardChars ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TextIter
    -- ^ /@iter@/: an iterator
    -> Int32
    -- ^ /@count@/: number of characters to move
    -> m Bool
    -- ^ __Returns:__ whether /@iter@/ moved and is dereferenceable
textIterBackwardChars iter count = liftIO $ do
    iter' <- unsafeManagedPtrGetPtr iter
    result <- gtk_text_iter_backward_chars iter' count
    let result' = (/= 0) result
    touchManagedPtr iter
    return result'

#if defined(ENABLE_OVERLOADING)
data TextIterBackwardCharsMethodInfo
instance (signature ~ (Int32 -> m Bool), MonadIO m) => O.OverloadedMethod TextIterBackwardCharsMethodInfo TextIter signature where
    overloadedMethod = textIterBackwardChars

instance O.OverloadedMethodInfo TextIterBackwardCharsMethodInfo TextIter where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextIter.textIterBackwardChars",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextIter.html#v:textIterBackwardChars"
        })


#endif

-- method TextIter::backward_cursor_position
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextIter" , sinceVersion = Nothing }
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

foreign import ccall "gtk_text_iter_backward_cursor_position" gtk_text_iter_backward_cursor_position :: 
    Ptr TextIter ->                         -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    IO CInt

-- | Like 'GI.Gtk.Structs.TextIter.textIterForwardCursorPosition', but moves backward.
textIterBackwardCursorPosition ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TextIter
    -- ^ /@iter@/: a t'GI.Gtk.Structs.TextIter.TextIter'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if we moved
textIterBackwardCursorPosition iter = liftIO $ do
    iter' <- unsafeManagedPtrGetPtr iter
    result <- gtk_text_iter_backward_cursor_position iter'
    let result' = (/= 0) result
    touchManagedPtr iter
    return result'

#if defined(ENABLE_OVERLOADING)
data TextIterBackwardCursorPositionMethodInfo
instance (signature ~ (m Bool), MonadIO m) => O.OverloadedMethod TextIterBackwardCursorPositionMethodInfo TextIter signature where
    overloadedMethod = textIterBackwardCursorPosition

instance O.OverloadedMethodInfo TextIterBackwardCursorPositionMethodInfo TextIter where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextIter.textIterBackwardCursorPosition",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextIter.html#v:textIterBackwardCursorPosition"
        })


#endif

-- method TextIter::backward_cursor_positions
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextIter" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "count"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "number of positions to move"
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

foreign import ccall "gtk_text_iter_backward_cursor_positions" gtk_text_iter_backward_cursor_positions :: 
    Ptr TextIter ->                         -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    Int32 ->                                -- count : TBasicType TInt
    IO CInt

-- | Moves up to /@count@/ cursor positions. See
-- 'GI.Gtk.Structs.TextIter.textIterForwardCursorPosition' for details.
textIterBackwardCursorPositions ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TextIter
    -- ^ /@iter@/: a t'GI.Gtk.Structs.TextIter.TextIter'
    -> Int32
    -- ^ /@count@/: number of positions to move
    -> m Bool
    -- ^ __Returns:__ 'P.True' if we moved and the new position is dereferenceable
textIterBackwardCursorPositions iter count = liftIO $ do
    iter' <- unsafeManagedPtrGetPtr iter
    result <- gtk_text_iter_backward_cursor_positions iter' count
    let result' = (/= 0) result
    touchManagedPtr iter
    return result'

#if defined(ENABLE_OVERLOADING)
data TextIterBackwardCursorPositionsMethodInfo
instance (signature ~ (Int32 -> m Bool), MonadIO m) => O.OverloadedMethod TextIterBackwardCursorPositionsMethodInfo TextIter signature where
    overloadedMethod = textIterBackwardCursorPositions

instance O.OverloadedMethodInfo TextIterBackwardCursorPositionsMethodInfo TextIter where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextIter.textIterBackwardCursorPositions",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextIter.html#v:textIterBackwardCursorPositions"
        })


#endif

-- method TextIter::backward_find_char
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextIter" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "pred"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextCharPredicate" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "function to be called on each character"
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
--                 { rawDocText = Just "user data for @pred"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "limit"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "search limit, or %NULL for none"
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

foreign import ccall "gtk_text_iter_backward_find_char" gtk_text_iter_backward_find_char :: 
    Ptr TextIter ->                         -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    FunPtr Gtk.Callbacks.C_TextCharPredicate -> -- pred : TInterface (Name {namespace = "Gtk", name = "TextCharPredicate"})
    Ptr () ->                               -- user_data : TBasicType TPtr
    Ptr TextIter ->                         -- limit : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    IO CInt

-- | Same as 'GI.Gtk.Structs.TextIter.textIterForwardFindChar', but goes backward from /@iter@/.
textIterBackwardFindChar ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TextIter
    -- ^ /@iter@/: a t'GI.Gtk.Structs.TextIter.TextIter'
    -> Gtk.Callbacks.TextCharPredicate
    -- ^ /@pred@/: function to be called on each character
    -> Maybe (TextIter)
    -- ^ /@limit@/: search limit, or 'P.Nothing' for none
    -> m Bool
    -- ^ __Returns:__ whether a match was found
textIterBackwardFindChar iter pred limit = liftIO $ do
    iter' <- unsafeManagedPtrGetPtr iter
    pred' <- Gtk.Callbacks.mk_TextCharPredicate (Gtk.Callbacks.wrap_TextCharPredicate Nothing (Gtk.Callbacks.drop_closures_TextCharPredicate pred))
    maybeLimit <- case limit of
        Nothing -> return nullPtr
        Just jLimit -> do
            jLimit' <- unsafeManagedPtrGetPtr jLimit
            return jLimit'
    let userData = nullPtr
    result <- gtk_text_iter_backward_find_char iter' pred' userData maybeLimit
    let result' = (/= 0) result
    safeFreeFunPtr $ castFunPtrToPtr pred'
    touchManagedPtr iter
    whenJust limit touchManagedPtr
    return result'

#if defined(ENABLE_OVERLOADING)
data TextIterBackwardFindCharMethodInfo
instance (signature ~ (Gtk.Callbacks.TextCharPredicate -> Maybe (TextIter) -> m Bool), MonadIO m) => O.OverloadedMethod TextIterBackwardFindCharMethodInfo TextIter signature where
    overloadedMethod = textIterBackwardFindChar

instance O.OverloadedMethodInfo TextIterBackwardFindCharMethodInfo TextIter where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextIter.textIterBackwardFindChar",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextIter.html#v:textIterBackwardFindChar"
        })


#endif

-- method TextIter::backward_line
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "an iterator" , sinceVersion = Nothing }
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

foreign import ccall "gtk_text_iter_backward_line" gtk_text_iter_backward_line :: 
    Ptr TextIter ->                         -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    IO CInt

-- | Moves /@iter@/ to the start of the previous line. Returns 'P.True' if
-- /@iter@/ could be moved; i.e. if /@iter@/ was at character offset 0, this
-- function returns 'P.False'. Therefore if /@iter@/ was already on line 0,
-- but not at the start of the line, /@iter@/ is snapped to the start of
-- the line and the function returns 'P.True'. (Note that this implies that
-- in a loop calling this function, the line number may not change on
-- every iteration, if your first iteration is on line 0.)
textIterBackwardLine ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TextIter
    -- ^ /@iter@/: an iterator
    -> m Bool
    -- ^ __Returns:__ whether /@iter@/ moved
textIterBackwardLine iter = liftIO $ do
    iter' <- unsafeManagedPtrGetPtr iter
    result <- gtk_text_iter_backward_line iter'
    let result' = (/= 0) result
    touchManagedPtr iter
    return result'

#if defined(ENABLE_OVERLOADING)
data TextIterBackwardLineMethodInfo
instance (signature ~ (m Bool), MonadIO m) => O.OverloadedMethod TextIterBackwardLineMethodInfo TextIter signature where
    overloadedMethod = textIterBackwardLine

instance O.OverloadedMethodInfo TextIterBackwardLineMethodInfo TextIter where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextIter.textIterBackwardLine",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextIter.html#v:textIterBackwardLine"
        })


#endif

-- method TextIter::backward_lines
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextIter" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "count"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "number of lines to move backward"
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

foreign import ccall "gtk_text_iter_backward_lines" gtk_text_iter_backward_lines :: 
    Ptr TextIter ->                         -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    Int32 ->                                -- count : TBasicType TInt
    IO CInt

-- | Moves /@count@/ lines backward, if possible (if /@count@/ would move
-- past the start or end of the buffer, moves to the start or end of
-- the buffer).  The return value indicates whether the iterator moved
-- onto a dereferenceable position; if the iterator didn’t move, or
-- moved onto the end iterator, then 'P.False' is returned. If /@count@/ is 0,
-- the function does nothing and returns 'P.False'. If /@count@/ is negative,
-- moves forward by 0 - /@count@/ lines.
textIterBackwardLines ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TextIter
    -- ^ /@iter@/: a t'GI.Gtk.Structs.TextIter.TextIter'
    -> Int32
    -- ^ /@count@/: number of lines to move backward
    -> m Bool
    -- ^ __Returns:__ whether /@iter@/ moved and is dereferenceable
textIterBackwardLines iter count = liftIO $ do
    iter' <- unsafeManagedPtrGetPtr iter
    result <- gtk_text_iter_backward_lines iter' count
    let result' = (/= 0) result
    touchManagedPtr iter
    return result'

#if defined(ENABLE_OVERLOADING)
data TextIterBackwardLinesMethodInfo
instance (signature ~ (Int32 -> m Bool), MonadIO m) => O.OverloadedMethod TextIterBackwardLinesMethodInfo TextIter signature where
    overloadedMethod = textIterBackwardLines

instance O.OverloadedMethodInfo TextIterBackwardLinesMethodInfo TextIter where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextIter.textIterBackwardLines",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextIter.html#v:textIterBackwardLines"
        })


#endif

-- method TextIter::backward_search
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextIter where the search begins"
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
--                 { rawDocText = Just "search string" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "flags"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextSearchFlags" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "bitmask of flags affecting the search"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "match_start"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "return location for start of match, or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = True
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "match_end"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "return location for end of match, or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = True
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "limit"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "location of last possible @match_start, or %NULL for start of buffer"
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

foreign import ccall "gtk_text_iter_backward_search" gtk_text_iter_backward_search :: 
    Ptr TextIter ->                         -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    CString ->                              -- str : TBasicType TUTF8
    CUInt ->                                -- flags : TInterface (Name {namespace = "Gtk", name = "TextSearchFlags"})
    Ptr TextIter ->                         -- match_start : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    Ptr TextIter ->                         -- match_end : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    Ptr TextIter ->                         -- limit : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    IO CInt

-- | Same as 'GI.Gtk.Structs.TextIter.textIterForwardSearch', but moves backward.
-- 
-- /@matchEnd@/ will never be set to a t'GI.Gtk.Structs.TextIter.TextIter' located after /@iter@/, even if
-- there is a possible /@matchStart@/ before or at /@iter@/.
textIterBackwardSearch ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TextIter
    -- ^ /@iter@/: a t'GI.Gtk.Structs.TextIter.TextIter' where the search begins
    -> T.Text
    -- ^ /@str@/: search string
    -> [Gtk.Flags.TextSearchFlags]
    -- ^ /@flags@/: bitmask of flags affecting the search
    -> Maybe (TextIter)
    -- ^ /@limit@/: location of last possible /@matchStart@/, or 'P.Nothing' for start of buffer
    -> m ((Bool, TextIter, TextIter))
    -- ^ __Returns:__ whether a match was found
textIterBackwardSearch iter str flags limit = liftIO $ do
    iter' <- unsafeManagedPtrGetPtr iter
    str' <- textToCString str
    let flags' = gflagsToWord flags
    matchStart <- SP.callocBoxedBytes 80 :: IO (Ptr TextIter)
    matchEnd <- SP.callocBoxedBytes 80 :: IO (Ptr TextIter)
    maybeLimit <- case limit of
        Nothing -> return nullPtr
        Just jLimit -> do
            jLimit' <- unsafeManagedPtrGetPtr jLimit
            return jLimit'
    result <- gtk_text_iter_backward_search iter' str' flags' matchStart matchEnd maybeLimit
    let result' = (/= 0) result
    matchStart' <- (wrapBoxed TextIter) matchStart
    matchEnd' <- (wrapBoxed TextIter) matchEnd
    touchManagedPtr iter
    whenJust limit touchManagedPtr
    freeMem str'
    return (result', matchStart', matchEnd')

#if defined(ENABLE_OVERLOADING)
data TextIterBackwardSearchMethodInfo
instance (signature ~ (T.Text -> [Gtk.Flags.TextSearchFlags] -> Maybe (TextIter) -> m ((Bool, TextIter, TextIter))), MonadIO m) => O.OverloadedMethod TextIterBackwardSearchMethodInfo TextIter signature where
    overloadedMethod = textIterBackwardSearch

instance O.OverloadedMethodInfo TextIterBackwardSearchMethodInfo TextIter where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextIter.textIterBackwardSearch",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextIter.html#v:textIterBackwardSearch"
        })


#endif

-- method TextIter::backward_sentence_start
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextIter" , sinceVersion = Nothing }
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

foreign import ccall "gtk_text_iter_backward_sentence_start" gtk_text_iter_backward_sentence_start :: 
    Ptr TextIter ->                         -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    IO CInt

-- | Moves backward to the previous sentence start; if /@iter@/ is already at
-- the start of a sentence, moves backward to the next one.  Sentence
-- boundaries are determined by Pango and should be correct for nearly
-- any language (if not, the correct fix would be to the Pango text
-- boundary algorithms).
textIterBackwardSentenceStart ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TextIter
    -- ^ /@iter@/: a t'GI.Gtk.Structs.TextIter.TextIter'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if /@iter@/ moved and is not the end iterator
textIterBackwardSentenceStart iter = liftIO $ do
    iter' <- unsafeManagedPtrGetPtr iter
    result <- gtk_text_iter_backward_sentence_start iter'
    let result' = (/= 0) result
    touchManagedPtr iter
    return result'

#if defined(ENABLE_OVERLOADING)
data TextIterBackwardSentenceStartMethodInfo
instance (signature ~ (m Bool), MonadIO m) => O.OverloadedMethod TextIterBackwardSentenceStartMethodInfo TextIter signature where
    overloadedMethod = textIterBackwardSentenceStart

instance O.OverloadedMethodInfo TextIterBackwardSentenceStartMethodInfo TextIter where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextIter.textIterBackwardSentenceStart",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextIter.html#v:textIterBackwardSentenceStart"
        })


#endif

-- method TextIter::backward_sentence_starts
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextIter" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "count"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "number of sentences to move"
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

foreign import ccall "gtk_text_iter_backward_sentence_starts" gtk_text_iter_backward_sentence_starts :: 
    Ptr TextIter ->                         -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    Int32 ->                                -- count : TBasicType TInt
    IO CInt

-- | Calls 'GI.Gtk.Structs.TextIter.textIterBackwardSentenceStart' up to /@count@/ times,
-- or until it returns 'P.False'. If /@count@/ is negative, moves forward
-- instead of backward.
textIterBackwardSentenceStarts ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TextIter
    -- ^ /@iter@/: a t'GI.Gtk.Structs.TextIter.TextIter'
    -> Int32
    -- ^ /@count@/: number of sentences to move
    -> m Bool
    -- ^ __Returns:__ 'P.True' if /@iter@/ moved and is not the end iterator
textIterBackwardSentenceStarts iter count = liftIO $ do
    iter' <- unsafeManagedPtrGetPtr iter
    result <- gtk_text_iter_backward_sentence_starts iter' count
    let result' = (/= 0) result
    touchManagedPtr iter
    return result'

#if defined(ENABLE_OVERLOADING)
data TextIterBackwardSentenceStartsMethodInfo
instance (signature ~ (Int32 -> m Bool), MonadIO m) => O.OverloadedMethod TextIterBackwardSentenceStartsMethodInfo TextIter signature where
    overloadedMethod = textIterBackwardSentenceStarts

instance O.OverloadedMethodInfo TextIterBackwardSentenceStartsMethodInfo TextIter where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextIter.textIterBackwardSentenceStarts",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextIter.html#v:textIterBackwardSentenceStarts"
        })


#endif

-- method TextIter::backward_to_tag_toggle
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextIter" , sinceVersion = Nothing }
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
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextTag, or %NULL"
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

foreign import ccall "gtk_text_iter_backward_to_tag_toggle" gtk_text_iter_backward_to_tag_toggle :: 
    Ptr TextIter ->                         -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    Ptr Gtk.TextTag.TextTag ->              -- tag : TInterface (Name {namespace = "Gtk", name = "TextTag"})
    IO CInt

-- | Moves backward to the next toggle (on or off) of the
-- t'GI.Gtk.Objects.TextTag.TextTag' /@tag@/, or to the next toggle of any tag if
-- /@tag@/ is 'P.Nothing'. If no matching tag toggles are found,
-- returns 'P.False', otherwise 'P.True'. Does not return toggles
-- located at /@iter@/, only toggles before /@iter@/. Sets /@iter@/
-- to the location of the toggle, or the start of the buffer
-- if no toggle is found.
textIterBackwardToTagToggle ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.TextTag.IsTextTag a) =>
    TextIter
    -- ^ /@iter@/: a t'GI.Gtk.Structs.TextIter.TextIter'
    -> Maybe (a)
    -- ^ /@tag@/: a t'GI.Gtk.Objects.TextTag.TextTag', or 'P.Nothing'
    -> m Bool
    -- ^ __Returns:__ whether we found a tag toggle before /@iter@/
textIterBackwardToTagToggle iter tag = liftIO $ do
    iter' <- unsafeManagedPtrGetPtr iter
    maybeTag <- case tag of
        Nothing -> return nullPtr
        Just jTag -> do
            jTag' <- unsafeManagedPtrCastPtr jTag
            return jTag'
    result <- gtk_text_iter_backward_to_tag_toggle iter' maybeTag
    let result' = (/= 0) result
    touchManagedPtr iter
    whenJust tag touchManagedPtr
    return result'

#if defined(ENABLE_OVERLOADING)
data TextIterBackwardToTagToggleMethodInfo
instance (signature ~ (Maybe (a) -> m Bool), MonadIO m, Gtk.TextTag.IsTextTag a) => O.OverloadedMethod TextIterBackwardToTagToggleMethodInfo TextIter signature where
    overloadedMethod = textIterBackwardToTagToggle

instance O.OverloadedMethodInfo TextIterBackwardToTagToggleMethodInfo TextIter where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextIter.textIterBackwardToTagToggle",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextIter.html#v:textIterBackwardToTagToggle"
        })


#endif

-- method TextIter::backward_visible_cursor_position
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextIter" , sinceVersion = Nothing }
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

foreign import ccall "gtk_text_iter_backward_visible_cursor_position" gtk_text_iter_backward_visible_cursor_position :: 
    Ptr TextIter ->                         -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    IO CInt

-- | Moves /@iter@/ forward to the previous visible cursor position. See
-- 'GI.Gtk.Structs.TextIter.textIterBackwardCursorPosition' for details.
-- 
-- /Since: 2.4/
textIterBackwardVisibleCursorPosition ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TextIter
    -- ^ /@iter@/: a t'GI.Gtk.Structs.TextIter.TextIter'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if we moved and the new position is dereferenceable
textIterBackwardVisibleCursorPosition iter = liftIO $ do
    iter' <- unsafeManagedPtrGetPtr iter
    result <- gtk_text_iter_backward_visible_cursor_position iter'
    let result' = (/= 0) result
    touchManagedPtr iter
    return result'

#if defined(ENABLE_OVERLOADING)
data TextIterBackwardVisibleCursorPositionMethodInfo
instance (signature ~ (m Bool), MonadIO m) => O.OverloadedMethod TextIterBackwardVisibleCursorPositionMethodInfo TextIter signature where
    overloadedMethod = textIterBackwardVisibleCursorPosition

instance O.OverloadedMethodInfo TextIterBackwardVisibleCursorPositionMethodInfo TextIter where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextIter.textIterBackwardVisibleCursorPosition",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextIter.html#v:textIterBackwardVisibleCursorPosition"
        })


#endif

-- method TextIter::backward_visible_cursor_positions
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextIter" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "count"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "number of positions to move"
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

foreign import ccall "gtk_text_iter_backward_visible_cursor_positions" gtk_text_iter_backward_visible_cursor_positions :: 
    Ptr TextIter ->                         -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    Int32 ->                                -- count : TBasicType TInt
    IO CInt

-- | Moves up to /@count@/ visible cursor positions. See
-- 'GI.Gtk.Structs.TextIter.textIterBackwardCursorPosition' for details.
-- 
-- /Since: 2.4/
textIterBackwardVisibleCursorPositions ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TextIter
    -- ^ /@iter@/: a t'GI.Gtk.Structs.TextIter.TextIter'
    -> Int32
    -- ^ /@count@/: number of positions to move
    -> m Bool
    -- ^ __Returns:__ 'P.True' if we moved and the new position is dereferenceable
textIterBackwardVisibleCursorPositions iter count = liftIO $ do
    iter' <- unsafeManagedPtrGetPtr iter
    result <- gtk_text_iter_backward_visible_cursor_positions iter' count
    let result' = (/= 0) result
    touchManagedPtr iter
    return result'

#if defined(ENABLE_OVERLOADING)
data TextIterBackwardVisibleCursorPositionsMethodInfo
instance (signature ~ (Int32 -> m Bool), MonadIO m) => O.OverloadedMethod TextIterBackwardVisibleCursorPositionsMethodInfo TextIter signature where
    overloadedMethod = textIterBackwardVisibleCursorPositions

instance O.OverloadedMethodInfo TextIterBackwardVisibleCursorPositionsMethodInfo TextIter where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextIter.textIterBackwardVisibleCursorPositions",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextIter.html#v:textIterBackwardVisibleCursorPositions"
        })


#endif

-- method TextIter::backward_visible_line
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "an iterator" , sinceVersion = Nothing }
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

foreign import ccall "gtk_text_iter_backward_visible_line" gtk_text_iter_backward_visible_line :: 
    Ptr TextIter ->                         -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    IO CInt

-- | Moves /@iter@/ to the start of the previous visible line. Returns 'P.True' if
-- /@iter@/ could be moved; i.e. if /@iter@/ was at character offset 0, this
-- function returns 'P.False'. Therefore if /@iter@/ was already on line 0,
-- but not at the start of the line, /@iter@/ is snapped to the start of
-- the line and the function returns 'P.True'. (Note that this implies that
-- in a loop calling this function, the line number may not change on
-- every iteration, if your first iteration is on line 0.)
-- 
-- /Since: 2.8/
textIterBackwardVisibleLine ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TextIter
    -- ^ /@iter@/: an iterator
    -> m Bool
    -- ^ __Returns:__ whether /@iter@/ moved
textIterBackwardVisibleLine iter = liftIO $ do
    iter' <- unsafeManagedPtrGetPtr iter
    result <- gtk_text_iter_backward_visible_line iter'
    let result' = (/= 0) result
    touchManagedPtr iter
    return result'

#if defined(ENABLE_OVERLOADING)
data TextIterBackwardVisibleLineMethodInfo
instance (signature ~ (m Bool), MonadIO m) => O.OverloadedMethod TextIterBackwardVisibleLineMethodInfo TextIter signature where
    overloadedMethod = textIterBackwardVisibleLine

instance O.OverloadedMethodInfo TextIterBackwardVisibleLineMethodInfo TextIter where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextIter.textIterBackwardVisibleLine",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextIter.html#v:textIterBackwardVisibleLine"
        })


#endif

-- method TextIter::backward_visible_lines
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextIter" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "count"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "number of lines to move backward"
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

foreign import ccall "gtk_text_iter_backward_visible_lines" gtk_text_iter_backward_visible_lines :: 
    Ptr TextIter ->                         -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    Int32 ->                                -- count : TBasicType TInt
    IO CInt

-- | Moves /@count@/ visible lines backward, if possible (if /@count@/ would move
-- past the start or end of the buffer, moves to the start or end of
-- the buffer).  The return value indicates whether the iterator moved
-- onto a dereferenceable position; if the iterator didn’t move, or
-- moved onto the end iterator, then 'P.False' is returned. If /@count@/ is 0,
-- the function does nothing and returns 'P.False'. If /@count@/ is negative,
-- moves forward by 0 - /@count@/ lines.
-- 
-- /Since: 2.8/
textIterBackwardVisibleLines ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TextIter
    -- ^ /@iter@/: a t'GI.Gtk.Structs.TextIter.TextIter'
    -> Int32
    -- ^ /@count@/: number of lines to move backward
    -> m Bool
    -- ^ __Returns:__ whether /@iter@/ moved and is dereferenceable
textIterBackwardVisibleLines iter count = liftIO $ do
    iter' <- unsafeManagedPtrGetPtr iter
    result <- gtk_text_iter_backward_visible_lines iter' count
    let result' = (/= 0) result
    touchManagedPtr iter
    return result'

#if defined(ENABLE_OVERLOADING)
data TextIterBackwardVisibleLinesMethodInfo
instance (signature ~ (Int32 -> m Bool), MonadIO m) => O.OverloadedMethod TextIterBackwardVisibleLinesMethodInfo TextIter signature where
    overloadedMethod = textIterBackwardVisibleLines

instance O.OverloadedMethodInfo TextIterBackwardVisibleLinesMethodInfo TextIter where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextIter.textIterBackwardVisibleLines",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextIter.html#v:textIterBackwardVisibleLines"
        })


#endif

-- method TextIter::backward_visible_word_start
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextIter" , sinceVersion = Nothing }
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

foreign import ccall "gtk_text_iter_backward_visible_word_start" gtk_text_iter_backward_visible_word_start :: 
    Ptr TextIter ->                         -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    IO CInt

-- | Moves backward to the previous visible word start. (If /@iter@/ is currently
-- on a word start, moves backward to the next one after that.) Word breaks
-- are determined by Pango and should be correct for nearly any
-- language (if not, the correct fix would be to the Pango word break
-- algorithms).
-- 
-- /Since: 2.4/
textIterBackwardVisibleWordStart ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TextIter
    -- ^ /@iter@/: a t'GI.Gtk.Structs.TextIter.TextIter'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if /@iter@/ moved and is not the end iterator
textIterBackwardVisibleWordStart iter = liftIO $ do
    iter' <- unsafeManagedPtrGetPtr iter
    result <- gtk_text_iter_backward_visible_word_start iter'
    let result' = (/= 0) result
    touchManagedPtr iter
    return result'

#if defined(ENABLE_OVERLOADING)
data TextIterBackwardVisibleWordStartMethodInfo
instance (signature ~ (m Bool), MonadIO m) => O.OverloadedMethod TextIterBackwardVisibleWordStartMethodInfo TextIter signature where
    overloadedMethod = textIterBackwardVisibleWordStart

instance O.OverloadedMethodInfo TextIterBackwardVisibleWordStartMethodInfo TextIter where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextIter.textIterBackwardVisibleWordStart",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextIter.html#v:textIterBackwardVisibleWordStart"
        })


#endif

-- method TextIter::backward_visible_word_starts
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextIter" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "count"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "number of times to move"
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

foreign import ccall "gtk_text_iter_backward_visible_word_starts" gtk_text_iter_backward_visible_word_starts :: 
    Ptr TextIter ->                         -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    Int32 ->                                -- count : TBasicType TInt
    IO CInt

-- | Calls 'GI.Gtk.Structs.TextIter.textIterBackwardVisibleWordStart' up to /@count@/ times.
-- 
-- /Since: 2.4/
textIterBackwardVisibleWordStarts ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TextIter
    -- ^ /@iter@/: a t'GI.Gtk.Structs.TextIter.TextIter'
    -> Int32
    -- ^ /@count@/: number of times to move
    -> m Bool
    -- ^ __Returns:__ 'P.True' if /@iter@/ moved and is not the end iterator
textIterBackwardVisibleWordStarts iter count = liftIO $ do
    iter' <- unsafeManagedPtrGetPtr iter
    result <- gtk_text_iter_backward_visible_word_starts iter' count
    let result' = (/= 0) result
    touchManagedPtr iter
    return result'

#if defined(ENABLE_OVERLOADING)
data TextIterBackwardVisibleWordStartsMethodInfo
instance (signature ~ (Int32 -> m Bool), MonadIO m) => O.OverloadedMethod TextIterBackwardVisibleWordStartsMethodInfo TextIter signature where
    overloadedMethod = textIterBackwardVisibleWordStarts

instance O.OverloadedMethodInfo TextIterBackwardVisibleWordStartsMethodInfo TextIter where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextIter.textIterBackwardVisibleWordStarts",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextIter.html#v:textIterBackwardVisibleWordStarts"
        })


#endif

-- method TextIter::backward_word_start
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextIter" , sinceVersion = Nothing }
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

foreign import ccall "gtk_text_iter_backward_word_start" gtk_text_iter_backward_word_start :: 
    Ptr TextIter ->                         -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    IO CInt

-- | Moves backward to the previous word start. (If /@iter@/ is currently on a
-- word start, moves backward to the next one after that.) Word breaks
-- are determined by Pango and should be correct for nearly any
-- language (if not, the correct fix would be to the Pango word break
-- algorithms).
textIterBackwardWordStart ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TextIter
    -- ^ /@iter@/: a t'GI.Gtk.Structs.TextIter.TextIter'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if /@iter@/ moved and is not the end iterator
textIterBackwardWordStart iter = liftIO $ do
    iter' <- unsafeManagedPtrGetPtr iter
    result <- gtk_text_iter_backward_word_start iter'
    let result' = (/= 0) result
    touchManagedPtr iter
    return result'

#if defined(ENABLE_OVERLOADING)
data TextIterBackwardWordStartMethodInfo
instance (signature ~ (m Bool), MonadIO m) => O.OverloadedMethod TextIterBackwardWordStartMethodInfo TextIter signature where
    overloadedMethod = textIterBackwardWordStart

instance O.OverloadedMethodInfo TextIterBackwardWordStartMethodInfo TextIter where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextIter.textIterBackwardWordStart",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextIter.html#v:textIterBackwardWordStart"
        })


#endif

-- method TextIter::backward_word_starts
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextIter" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "count"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "number of times to move"
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

foreign import ccall "gtk_text_iter_backward_word_starts" gtk_text_iter_backward_word_starts :: 
    Ptr TextIter ->                         -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    Int32 ->                                -- count : TBasicType TInt
    IO CInt

-- | Calls 'GI.Gtk.Structs.TextIter.textIterBackwardWordStart' up to /@count@/ times.
textIterBackwardWordStarts ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TextIter
    -- ^ /@iter@/: a t'GI.Gtk.Structs.TextIter.TextIter'
    -> Int32
    -- ^ /@count@/: number of times to move
    -> m Bool
    -- ^ __Returns:__ 'P.True' if /@iter@/ moved and is not the end iterator
textIterBackwardWordStarts iter count = liftIO $ do
    iter' <- unsafeManagedPtrGetPtr iter
    result <- gtk_text_iter_backward_word_starts iter' count
    let result' = (/= 0) result
    touchManagedPtr iter
    return result'

#if defined(ENABLE_OVERLOADING)
data TextIterBackwardWordStartsMethodInfo
instance (signature ~ (Int32 -> m Bool), MonadIO m) => O.OverloadedMethod TextIterBackwardWordStartsMethodInfo TextIter signature where
    overloadedMethod = textIterBackwardWordStarts

instance O.OverloadedMethodInfo TextIterBackwardWordStartsMethodInfo TextIter where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextIter.textIterBackwardWordStarts",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextIter.html#v:textIterBackwardWordStarts"
        })


#endif

-- method TextIter::begins_tag
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "an iterator" , sinceVersion = Nothing }
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
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextTag, or %NULL"
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

foreign import ccall "gtk_text_iter_begins_tag" gtk_text_iter_begins_tag :: 
    Ptr TextIter ->                         -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    Ptr Gtk.TextTag.TextTag ->              -- tag : TInterface (Name {namespace = "Gtk", name = "TextTag"})
    IO CInt

{-# DEPRECATED textIterBeginsTag ["(Since version 3.20)","Use 'GI.Gtk.Structs.TextIter.textIterStartsTag' instead."] #-}
-- | Returns 'P.True' if /@tag@/ is toggled on at exactly this point. If /@tag@/
-- is 'P.Nothing', returns 'P.True' if any tag is toggled on at this point.
-- 
-- Note that if 'GI.Gtk.Structs.TextIter.textIterBeginsTag' returns 'P.True', it means that /@iter@/ is
-- at the beginning of the tagged range, and that the
-- character at /@iter@/ is inside the tagged range. In other
-- words, unlike 'GI.Gtk.Structs.TextIter.textIterEndsTag', if 'GI.Gtk.Structs.TextIter.textIterBeginsTag' returns
-- 'P.True', 'GI.Gtk.Structs.TextIter.textIterHasTag' will also return 'P.True' for the same
-- parameters.
textIterBeginsTag ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.TextTag.IsTextTag a) =>
    TextIter
    -- ^ /@iter@/: an iterator
    -> Maybe (a)
    -- ^ /@tag@/: a t'GI.Gtk.Objects.TextTag.TextTag', or 'P.Nothing'
    -> m Bool
    -- ^ __Returns:__ whether /@iter@/ is the start of a range tagged with /@tag@/
textIterBeginsTag iter tag = liftIO $ do
    iter' <- unsafeManagedPtrGetPtr iter
    maybeTag <- case tag of
        Nothing -> return nullPtr
        Just jTag -> do
            jTag' <- unsafeManagedPtrCastPtr jTag
            return jTag'
    result <- gtk_text_iter_begins_tag iter' maybeTag
    let result' = (/= 0) result
    touchManagedPtr iter
    whenJust tag touchManagedPtr
    return result'

#if defined(ENABLE_OVERLOADING)
data TextIterBeginsTagMethodInfo
instance (signature ~ (Maybe (a) -> m Bool), MonadIO m, Gtk.TextTag.IsTextTag a) => O.OverloadedMethod TextIterBeginsTagMethodInfo TextIter signature where
    overloadedMethod = textIterBeginsTag

instance O.OverloadedMethodInfo TextIterBeginsTagMethodInfo TextIter where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextIter.textIterBeginsTag",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextIter.html#v:textIterBeginsTag"
        })


#endif

-- method TextIter::can_insert
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "an iterator" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "default_editability"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "%TRUE if text is editable by default"
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

foreign import ccall "gtk_text_iter_can_insert" gtk_text_iter_can_insert :: 
    Ptr TextIter ->                         -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    CInt ->                                 -- default_editability : TBasicType TBoolean
    IO CInt

-- | Considering the default editability of the buffer, and tags that
-- affect editability, determines whether text inserted at /@iter@/ would
-- be editable. If text inserted at /@iter@/ would be editable then the
-- user should be allowed to insert text at /@iter@/.
-- 'GI.Gtk.Objects.TextBuffer.textBufferInsertInteractive' uses this function to decide
-- whether insertions are allowed at a given position.
textIterCanInsert ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TextIter
    -- ^ /@iter@/: an iterator
    -> Bool
    -- ^ /@defaultEditability@/: 'P.True' if text is editable by default
    -> m Bool
    -- ^ __Returns:__ whether text inserted at /@iter@/ would be editable
textIterCanInsert iter defaultEditability = liftIO $ do
    iter' <- unsafeManagedPtrGetPtr iter
    let defaultEditability' = (fromIntegral . fromEnum) defaultEditability
    result <- gtk_text_iter_can_insert iter' defaultEditability'
    let result' = (/= 0) result
    touchManagedPtr iter
    return result'

#if defined(ENABLE_OVERLOADING)
data TextIterCanInsertMethodInfo
instance (signature ~ (Bool -> m Bool), MonadIO m) => O.OverloadedMethod TextIterCanInsertMethodInfo TextIter signature where
    overloadedMethod = textIterCanInsert

instance O.OverloadedMethodInfo TextIterCanInsertMethodInfo TextIter where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextIter.textIterCanInsert",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextIter.html#v:textIterCanInsert"
        })


#endif

-- method TextIter::compare
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "lhs"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextIter" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "rhs"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "another #GtkTextIter"
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

foreign import ccall "gtk_text_iter_compare" gtk_text_iter_compare :: 
    Ptr TextIter ->                         -- lhs : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    Ptr TextIter ->                         -- rhs : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    IO Int32

-- | A @/qsort()/@-style function that returns negative if /@lhs@/ is less than
-- /@rhs@/, positive if /@lhs@/ is greater than /@rhs@/, and 0 if they’re equal.
-- Ordering is in character offset order, i.e. the first character in the buffer
-- is less than the second character in the buffer.
textIterCompare ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TextIter
    -- ^ /@lhs@/: a t'GI.Gtk.Structs.TextIter.TextIter'
    -> TextIter
    -- ^ /@rhs@/: another t'GI.Gtk.Structs.TextIter.TextIter'
    -> m Int32
    -- ^ __Returns:__ -1 if /@lhs@/ is less than /@rhs@/, 1 if /@lhs@/ is greater, 0 if they are equal
textIterCompare lhs rhs = liftIO $ do
    lhs' <- unsafeManagedPtrGetPtr lhs
    rhs' <- unsafeManagedPtrGetPtr rhs
    result <- gtk_text_iter_compare lhs' rhs'
    touchManagedPtr lhs
    touchManagedPtr rhs
    return result

#if defined(ENABLE_OVERLOADING)
data TextIterCompareMethodInfo
instance (signature ~ (TextIter -> m Int32), MonadIO m) => O.OverloadedMethod TextIterCompareMethodInfo TextIter signature where
    overloadedMethod = textIterCompare

instance O.OverloadedMethodInfo TextIterCompareMethodInfo TextIter where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextIter.textIterCompare",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextIter.html#v:textIterCompare"
        })


#endif

-- method TextIter::copy
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "an iterator" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "TextIter" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_text_iter_copy" gtk_text_iter_copy :: 
    Ptr TextIter ->                         -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    IO (Ptr TextIter)

-- | Creates a dynamically-allocated copy of an iterator. This function
-- is not useful in applications, because iterators can be copied with a
-- simple assignment (@GtkTextIter i = j;@). The
-- function is used by language bindings.
textIterCopy ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TextIter
    -- ^ /@iter@/: an iterator
    -> m TextIter
    -- ^ __Returns:__ a copy of the /@iter@/, free with 'GI.Gtk.Structs.TextIter.textIterFree'
textIterCopy iter = liftIO $ do
    iter' <- unsafeManagedPtrGetPtr iter
    result <- gtk_text_iter_copy iter'
    checkUnexpectedReturnNULL "textIterCopy" result
    result' <- (wrapBoxed TextIter) result
    touchManagedPtr iter
    return result'

#if defined(ENABLE_OVERLOADING)
data TextIterCopyMethodInfo
instance (signature ~ (m TextIter), MonadIO m) => O.OverloadedMethod TextIterCopyMethodInfo TextIter signature where
    overloadedMethod = textIterCopy

instance O.OverloadedMethodInfo TextIterCopyMethodInfo TextIter where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextIter.textIterCopy",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextIter.html#v:textIterCopy"
        })


#endif

-- method TextIter::editable
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "an iterator" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "default_setting"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "%TRUE if text is editable by default"
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

foreign import ccall "gtk_text_iter_editable" gtk_text_iter_editable :: 
    Ptr TextIter ->                         -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    CInt ->                                 -- default_setting : TBasicType TBoolean
    IO CInt

-- | Returns whether the character at /@iter@/ is within an editable region
-- of text.  Non-editable text is “locked” and can’t be changed by the
-- user via t'GI.Gtk.Objects.TextView.TextView'. This function is simply a convenience
-- wrapper around 'GI.Gtk.Structs.TextIter.textIterGetAttributes'. If no tags applied
-- to this text affect editability, /@defaultSetting@/ will be returned.
-- 
-- You don’t want to use this function to decide whether text can be
-- inserted at /@iter@/, because for insertion you don’t want to know
-- whether the char at /@iter@/ is inside an editable range, you want to
-- know whether a new character inserted at /@iter@/ would be inside an
-- editable range. Use 'GI.Gtk.Structs.TextIter.textIterCanInsert' to handle this
-- case.
textIterEditable ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TextIter
    -- ^ /@iter@/: an iterator
    -> Bool
    -- ^ /@defaultSetting@/: 'P.True' if text is editable by default
    -> m Bool
    -- ^ __Returns:__ whether /@iter@/ is inside an editable range
textIterEditable iter defaultSetting = liftIO $ do
    iter' <- unsafeManagedPtrGetPtr iter
    let defaultSetting' = (fromIntegral . fromEnum) defaultSetting
    result <- gtk_text_iter_editable iter' defaultSetting'
    let result' = (/= 0) result
    touchManagedPtr iter
    return result'

#if defined(ENABLE_OVERLOADING)
data TextIterEditableMethodInfo
instance (signature ~ (Bool -> m Bool), MonadIO m) => O.OverloadedMethod TextIterEditableMethodInfo TextIter signature where
    overloadedMethod = textIterEditable

instance O.OverloadedMethodInfo TextIterEditableMethodInfo TextIter where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextIter.textIterEditable",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextIter.html#v:textIterEditable"
        })


#endif

-- method TextIter::ends_line
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "an iterator" , sinceVersion = Nothing }
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

foreign import ccall "gtk_text_iter_ends_line" gtk_text_iter_ends_line :: 
    Ptr TextIter ->                         -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    IO CInt

-- | Returns 'P.True' if /@iter@/ points to the start of the paragraph
-- delimiter characters for a line (delimiters will be either a
-- newline, a carriage return, a carriage return followed by a
-- newline, or a Unicode paragraph separator character). Note that an
-- iterator pointing to the \\n of a \\r\\n pair will not be counted as
-- the end of a line, the line ends before the \\r. The end iterator is
-- considered to be at the end of a line, even though there are no
-- paragraph delimiter chars there.
textIterEndsLine ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TextIter
    -- ^ /@iter@/: an iterator
    -> m Bool
    -- ^ __Returns:__ whether /@iter@/ is at the end of a line
textIterEndsLine iter = liftIO $ do
    iter' <- unsafeManagedPtrGetPtr iter
    result <- gtk_text_iter_ends_line iter'
    let result' = (/= 0) result
    touchManagedPtr iter
    return result'

#if defined(ENABLE_OVERLOADING)
data TextIterEndsLineMethodInfo
instance (signature ~ (m Bool), MonadIO m) => O.OverloadedMethod TextIterEndsLineMethodInfo TextIter signature where
    overloadedMethod = textIterEndsLine

instance O.OverloadedMethodInfo TextIterEndsLineMethodInfo TextIter where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextIter.textIterEndsLine",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextIter.html#v:textIterEndsLine"
        })


#endif

-- method TextIter::ends_sentence
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextIter" , sinceVersion = Nothing }
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

foreign import ccall "gtk_text_iter_ends_sentence" gtk_text_iter_ends_sentence :: 
    Ptr TextIter ->                         -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    IO CInt

-- | Determines whether /@iter@/ ends a sentence.  Sentence boundaries are
-- determined by Pango and should be correct for nearly any language
-- (if not, the correct fix would be to the Pango text boundary
-- algorithms).
textIterEndsSentence ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TextIter
    -- ^ /@iter@/: a t'GI.Gtk.Structs.TextIter.TextIter'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if /@iter@/ is at the end of a sentence.
textIterEndsSentence iter = liftIO $ do
    iter' <- unsafeManagedPtrGetPtr iter
    result <- gtk_text_iter_ends_sentence iter'
    let result' = (/= 0) result
    touchManagedPtr iter
    return result'

#if defined(ENABLE_OVERLOADING)
data TextIterEndsSentenceMethodInfo
instance (signature ~ (m Bool), MonadIO m) => O.OverloadedMethod TextIterEndsSentenceMethodInfo TextIter signature where
    overloadedMethod = textIterEndsSentence

instance O.OverloadedMethodInfo TextIterEndsSentenceMethodInfo TextIter where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextIter.textIterEndsSentence",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextIter.html#v:textIterEndsSentence"
        })


#endif

-- method TextIter::ends_tag
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "an iterator" , sinceVersion = Nothing }
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
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextTag, or %NULL"
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

foreign import ccall "gtk_text_iter_ends_tag" gtk_text_iter_ends_tag :: 
    Ptr TextIter ->                         -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    Ptr Gtk.TextTag.TextTag ->              -- tag : TInterface (Name {namespace = "Gtk", name = "TextTag"})
    IO CInt

-- | Returns 'P.True' if /@tag@/ is toggled off at exactly this point. If /@tag@/
-- is 'P.Nothing', returns 'P.True' if any tag is toggled off at this point.
-- 
-- Note that if 'GI.Gtk.Structs.TextIter.textIterEndsTag' returns 'P.True', it means that /@iter@/ is
-- at the end of the tagged range, but that the character
-- at /@iter@/ is outside the tagged range. In other words,
-- unlike 'GI.Gtk.Structs.TextIter.textIterStartsTag', if 'GI.Gtk.Structs.TextIter.textIterEndsTag' returns 'P.True',
-- 'GI.Gtk.Structs.TextIter.textIterHasTag' will return 'P.False' for the same parameters.
textIterEndsTag ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.TextTag.IsTextTag a) =>
    TextIter
    -- ^ /@iter@/: an iterator
    -> Maybe (a)
    -- ^ /@tag@/: a t'GI.Gtk.Objects.TextTag.TextTag', or 'P.Nothing'
    -> m Bool
    -- ^ __Returns:__ whether /@iter@/ is the end of a range tagged with /@tag@/
textIterEndsTag iter tag = liftIO $ do
    iter' <- unsafeManagedPtrGetPtr iter
    maybeTag <- case tag of
        Nothing -> return nullPtr
        Just jTag -> do
            jTag' <- unsafeManagedPtrCastPtr jTag
            return jTag'
    result <- gtk_text_iter_ends_tag iter' maybeTag
    let result' = (/= 0) result
    touchManagedPtr iter
    whenJust tag touchManagedPtr
    return result'

#if defined(ENABLE_OVERLOADING)
data TextIterEndsTagMethodInfo
instance (signature ~ (Maybe (a) -> m Bool), MonadIO m, Gtk.TextTag.IsTextTag a) => O.OverloadedMethod TextIterEndsTagMethodInfo TextIter signature where
    overloadedMethod = textIterEndsTag

instance O.OverloadedMethodInfo TextIterEndsTagMethodInfo TextIter where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextIter.textIterEndsTag",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextIter.html#v:textIterEndsTag"
        })


#endif

-- method TextIter::ends_word
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextIter" , sinceVersion = Nothing }
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

foreign import ccall "gtk_text_iter_ends_word" gtk_text_iter_ends_word :: 
    Ptr TextIter ->                         -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    IO CInt

-- | Determines whether /@iter@/ ends a natural-language word.  Word breaks
-- are determined by Pango and should be correct for nearly any
-- language (if not, the correct fix would be to the Pango word break
-- algorithms).
textIterEndsWord ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TextIter
    -- ^ /@iter@/: a t'GI.Gtk.Structs.TextIter.TextIter'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if /@iter@/ is at the end of a word
textIterEndsWord iter = liftIO $ do
    iter' <- unsafeManagedPtrGetPtr iter
    result <- gtk_text_iter_ends_word iter'
    let result' = (/= 0) result
    touchManagedPtr iter
    return result'

#if defined(ENABLE_OVERLOADING)
data TextIterEndsWordMethodInfo
instance (signature ~ (m Bool), MonadIO m) => O.OverloadedMethod TextIterEndsWordMethodInfo TextIter signature where
    overloadedMethod = textIterEndsWord

instance O.OverloadedMethodInfo TextIterEndsWordMethodInfo TextIter where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextIter.textIterEndsWord",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextIter.html#v:textIterEndsWord"
        })


#endif

-- method TextIter::equal
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "lhs"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextIter" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "rhs"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "another #GtkTextIter"
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

foreign import ccall "gtk_text_iter_equal" gtk_text_iter_equal :: 
    Ptr TextIter ->                         -- lhs : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    Ptr TextIter ->                         -- rhs : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    IO CInt

-- | Tests whether two iterators are equal, using the fastest possible
-- mechanism. This function is very fast; you can expect it to perform
-- better than e.g. getting the character offset for each iterator and
-- comparing the offsets yourself. Also, it’s a bit faster than
-- 'GI.Gtk.Structs.TextIter.textIterCompare'.
textIterEqual ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TextIter
    -- ^ /@lhs@/: a t'GI.Gtk.Structs.TextIter.TextIter'
    -> TextIter
    -- ^ /@rhs@/: another t'GI.Gtk.Structs.TextIter.TextIter'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the iterators point to the same place in the buffer
textIterEqual lhs rhs = liftIO $ do
    lhs' <- unsafeManagedPtrGetPtr lhs
    rhs' <- unsafeManagedPtrGetPtr rhs
    result <- gtk_text_iter_equal lhs' rhs'
    let result' = (/= 0) result
    touchManagedPtr lhs
    touchManagedPtr rhs
    return result'

#if defined(ENABLE_OVERLOADING)
data TextIterEqualMethodInfo
instance (signature ~ (TextIter -> m Bool), MonadIO m) => O.OverloadedMethod TextIterEqualMethodInfo TextIter signature where
    overloadedMethod = textIterEqual

instance O.OverloadedMethodInfo TextIterEqualMethodInfo TextIter where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextIter.textIterEqual",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextIter.html#v:textIterEqual"
        })


#endif

-- method TextIter::forward_char
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "an iterator" , sinceVersion = Nothing }
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

foreign import ccall "gtk_text_iter_forward_char" gtk_text_iter_forward_char :: 
    Ptr TextIter ->                         -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    IO CInt

-- | Moves /@iter@/ forward by one character offset. Note that images
-- embedded in the buffer occupy 1 character slot, so
-- 'GI.Gtk.Structs.TextIter.textIterForwardChar' may actually move onto an image instead
-- of a character, if you have images in your buffer.  If /@iter@/ is the
-- end iterator or one character before it, /@iter@/ will now point at
-- the end iterator, and 'GI.Gtk.Structs.TextIter.textIterForwardChar' returns 'P.False' for
-- convenience when writing loops.
textIterForwardChar ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TextIter
    -- ^ /@iter@/: an iterator
    -> m Bool
    -- ^ __Returns:__ whether /@iter@/ moved and is dereferenceable
textIterForwardChar iter = liftIO $ do
    iter' <- unsafeManagedPtrGetPtr iter
    result <- gtk_text_iter_forward_char iter'
    let result' = (/= 0) result
    touchManagedPtr iter
    return result'

#if defined(ENABLE_OVERLOADING)
data TextIterForwardCharMethodInfo
instance (signature ~ (m Bool), MonadIO m) => O.OverloadedMethod TextIterForwardCharMethodInfo TextIter signature where
    overloadedMethod = textIterForwardChar

instance O.OverloadedMethodInfo TextIterForwardCharMethodInfo TextIter where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextIter.textIterForwardChar",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextIter.html#v:textIterForwardChar"
        })


#endif

-- method TextIter::forward_chars
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "an iterator" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "count"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "number of characters to move, may be negative"
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

foreign import ccall "gtk_text_iter_forward_chars" gtk_text_iter_forward_chars :: 
    Ptr TextIter ->                         -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    Int32 ->                                -- count : TBasicType TInt
    IO CInt

-- | Moves /@count@/ characters if possible (if /@count@/ would move past the
-- start or end of the buffer, moves to the start or end of the
-- buffer). The return value indicates whether the new position of
-- /@iter@/ is different from its original position, and dereferenceable
-- (the last iterator in the buffer is not dereferenceable). If /@count@/
-- is 0, the function does nothing and returns 'P.False'.
textIterForwardChars ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TextIter
    -- ^ /@iter@/: an iterator
    -> Int32
    -- ^ /@count@/: number of characters to move, may be negative
    -> m Bool
    -- ^ __Returns:__ whether /@iter@/ moved and is dereferenceable
textIterForwardChars iter count = liftIO $ do
    iter' <- unsafeManagedPtrGetPtr iter
    result <- gtk_text_iter_forward_chars iter' count
    let result' = (/= 0) result
    touchManagedPtr iter
    return result'

#if defined(ENABLE_OVERLOADING)
data TextIterForwardCharsMethodInfo
instance (signature ~ (Int32 -> m Bool), MonadIO m) => O.OverloadedMethod TextIterForwardCharsMethodInfo TextIter signature where
    overloadedMethod = textIterForwardChars

instance O.OverloadedMethodInfo TextIterForwardCharsMethodInfo TextIter where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextIter.textIterForwardChars",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextIter.html#v:textIterForwardChars"
        })


#endif

-- method TextIter::forward_cursor_position
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextIter" , sinceVersion = Nothing }
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

foreign import ccall "gtk_text_iter_forward_cursor_position" gtk_text_iter_forward_cursor_position :: 
    Ptr TextIter ->                         -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    IO CInt

-- | Moves /@iter@/ forward by a single cursor position. Cursor positions
-- are (unsurprisingly) positions where the cursor can appear. Perhaps
-- surprisingly, there may not be a cursor position between all
-- characters. The most common example for European languages would be
-- a carriage return\/newline sequence. For some Unicode characters,
-- the equivalent of say the letter “a” with an accent mark will be
-- represented as two characters, first the letter then a \"combining
-- mark\" that causes the accent to be rendered; so the cursor can’t go
-- between those two characters. See also the t'GI.Pango.Structs.LogAttr.LogAttr'-struct and
-- 'GI.Pango.Functions.break' function.
textIterForwardCursorPosition ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TextIter
    -- ^ /@iter@/: a t'GI.Gtk.Structs.TextIter.TextIter'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if we moved and the new position is dereferenceable
textIterForwardCursorPosition iter = liftIO $ do
    iter' <- unsafeManagedPtrGetPtr iter
    result <- gtk_text_iter_forward_cursor_position iter'
    let result' = (/= 0) result
    touchManagedPtr iter
    return result'

#if defined(ENABLE_OVERLOADING)
data TextIterForwardCursorPositionMethodInfo
instance (signature ~ (m Bool), MonadIO m) => O.OverloadedMethod TextIterForwardCursorPositionMethodInfo TextIter signature where
    overloadedMethod = textIterForwardCursorPosition

instance O.OverloadedMethodInfo TextIterForwardCursorPositionMethodInfo TextIter where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextIter.textIterForwardCursorPosition",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextIter.html#v:textIterForwardCursorPosition"
        })


#endif

-- method TextIter::forward_cursor_positions
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextIter" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "count"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "number of positions to move"
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

foreign import ccall "gtk_text_iter_forward_cursor_positions" gtk_text_iter_forward_cursor_positions :: 
    Ptr TextIter ->                         -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    Int32 ->                                -- count : TBasicType TInt
    IO CInt

-- | Moves up to /@count@/ cursor positions. See
-- 'GI.Gtk.Structs.TextIter.textIterForwardCursorPosition' for details.
textIterForwardCursorPositions ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TextIter
    -- ^ /@iter@/: a t'GI.Gtk.Structs.TextIter.TextIter'
    -> Int32
    -- ^ /@count@/: number of positions to move
    -> m Bool
    -- ^ __Returns:__ 'P.True' if we moved and the new position is dereferenceable
textIterForwardCursorPositions iter count = liftIO $ do
    iter' <- unsafeManagedPtrGetPtr iter
    result <- gtk_text_iter_forward_cursor_positions iter' count
    let result' = (/= 0) result
    touchManagedPtr iter
    return result'

#if defined(ENABLE_OVERLOADING)
data TextIterForwardCursorPositionsMethodInfo
instance (signature ~ (Int32 -> m Bool), MonadIO m) => O.OverloadedMethod TextIterForwardCursorPositionsMethodInfo TextIter signature where
    overloadedMethod = textIterForwardCursorPositions

instance O.OverloadedMethodInfo TextIterForwardCursorPositionsMethodInfo TextIter where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextIter.textIterForwardCursorPositions",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextIter.html#v:textIterForwardCursorPositions"
        })


#endif

-- method TextIter::forward_find_char
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextIter" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "pred"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextCharPredicate" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a function to be called on each character"
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
--                 { rawDocText = Just "user data for @pred"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "limit"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "search limit, or %NULL for none"
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

foreign import ccall "gtk_text_iter_forward_find_char" gtk_text_iter_forward_find_char :: 
    Ptr TextIter ->                         -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    FunPtr Gtk.Callbacks.C_TextCharPredicate -> -- pred : TInterface (Name {namespace = "Gtk", name = "TextCharPredicate"})
    Ptr () ->                               -- user_data : TBasicType TPtr
    Ptr TextIter ->                         -- limit : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    IO CInt

-- | Advances /@iter@/, calling /@pred@/ on each character. If
-- /@pred@/ returns 'P.True', returns 'P.True' and stops scanning.
-- If /@pred@/ never returns 'P.True', /@iter@/ is set to /@limit@/ if
-- /@limit@/ is non-'P.Nothing', otherwise to the end iterator.
textIterForwardFindChar ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TextIter
    -- ^ /@iter@/: a t'GI.Gtk.Structs.TextIter.TextIter'
    -> Gtk.Callbacks.TextCharPredicate
    -- ^ /@pred@/: a function to be called on each character
    -> Maybe (TextIter)
    -- ^ /@limit@/: search limit, or 'P.Nothing' for none
    -> m Bool
    -- ^ __Returns:__ whether a match was found
textIterForwardFindChar iter pred limit = liftIO $ do
    iter' <- unsafeManagedPtrGetPtr iter
    pred' <- Gtk.Callbacks.mk_TextCharPredicate (Gtk.Callbacks.wrap_TextCharPredicate Nothing (Gtk.Callbacks.drop_closures_TextCharPredicate pred))
    maybeLimit <- case limit of
        Nothing -> return nullPtr
        Just jLimit -> do
            jLimit' <- unsafeManagedPtrGetPtr jLimit
            return jLimit'
    let userData = nullPtr
    result <- gtk_text_iter_forward_find_char iter' pred' userData maybeLimit
    let result' = (/= 0) result
    safeFreeFunPtr $ castFunPtrToPtr pred'
    touchManagedPtr iter
    whenJust limit touchManagedPtr
    return result'

#if defined(ENABLE_OVERLOADING)
data TextIterForwardFindCharMethodInfo
instance (signature ~ (Gtk.Callbacks.TextCharPredicate -> Maybe (TextIter) -> m Bool), MonadIO m) => O.OverloadedMethod TextIterForwardFindCharMethodInfo TextIter signature where
    overloadedMethod = textIterForwardFindChar

instance O.OverloadedMethodInfo TextIterForwardFindCharMethodInfo TextIter where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextIter.textIterForwardFindChar",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextIter.html#v:textIterForwardFindChar"
        })


#endif

-- method TextIter::forward_line
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "an iterator" , sinceVersion = Nothing }
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

foreign import ccall "gtk_text_iter_forward_line" gtk_text_iter_forward_line :: 
    Ptr TextIter ->                         -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    IO CInt

-- | Moves /@iter@/ to the start of the next line. If the iter is already on the
-- last line of the buffer, moves the iter to the end of the current line.
-- If after the operation, the iter is at the end of the buffer and not
-- dereferencable, returns 'P.False'. Otherwise, returns 'P.True'.
textIterForwardLine ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TextIter
    -- ^ /@iter@/: an iterator
    -> m Bool
    -- ^ __Returns:__ whether /@iter@/ can be dereferenced
textIterForwardLine iter = liftIO $ do
    iter' <- unsafeManagedPtrGetPtr iter
    result <- gtk_text_iter_forward_line iter'
    let result' = (/= 0) result
    touchManagedPtr iter
    return result'

#if defined(ENABLE_OVERLOADING)
data TextIterForwardLineMethodInfo
instance (signature ~ (m Bool), MonadIO m) => O.OverloadedMethod TextIterForwardLineMethodInfo TextIter signature where
    overloadedMethod = textIterForwardLine

instance O.OverloadedMethodInfo TextIterForwardLineMethodInfo TextIter where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextIter.textIterForwardLine",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextIter.html#v:textIterForwardLine"
        })


#endif

-- method TextIter::forward_lines
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextIter" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "count"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "number of lines to move forward"
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

foreign import ccall "gtk_text_iter_forward_lines" gtk_text_iter_forward_lines :: 
    Ptr TextIter ->                         -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    Int32 ->                                -- count : TBasicType TInt
    IO CInt

-- | Moves /@count@/ lines forward, if possible (if /@count@/ would move
-- past the start or end of the buffer, moves to the start or end of
-- the buffer).  The return value indicates whether the iterator moved
-- onto a dereferenceable position; if the iterator didn’t move, or
-- moved onto the end iterator, then 'P.False' is returned. If /@count@/ is 0,
-- the function does nothing and returns 'P.False'. If /@count@/ is negative,
-- moves backward by 0 - /@count@/ lines.
textIterForwardLines ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TextIter
    -- ^ /@iter@/: a t'GI.Gtk.Structs.TextIter.TextIter'
    -> Int32
    -- ^ /@count@/: number of lines to move forward
    -> m Bool
    -- ^ __Returns:__ whether /@iter@/ moved and is dereferenceable
textIterForwardLines iter count = liftIO $ do
    iter' <- unsafeManagedPtrGetPtr iter
    result <- gtk_text_iter_forward_lines iter' count
    let result' = (/= 0) result
    touchManagedPtr iter
    return result'

#if defined(ENABLE_OVERLOADING)
data TextIterForwardLinesMethodInfo
instance (signature ~ (Int32 -> m Bool), MonadIO m) => O.OverloadedMethod TextIterForwardLinesMethodInfo TextIter signature where
    overloadedMethod = textIterForwardLines

instance O.OverloadedMethodInfo TextIterForwardLinesMethodInfo TextIter where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextIter.textIterForwardLines",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextIter.html#v:textIterForwardLines"
        })


#endif

-- method TextIter::forward_search
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "start of search" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "a search string" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "flags"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextSearchFlags" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "flags affecting how the search is done"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "match_start"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "return location for start of match, or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = True
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "match_end"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "return location for end of match, or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = True
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "limit"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "location of last possible @match_end, or %NULL for the end of the buffer"
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

foreign import ccall "gtk_text_iter_forward_search" gtk_text_iter_forward_search :: 
    Ptr TextIter ->                         -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    CString ->                              -- str : TBasicType TUTF8
    CUInt ->                                -- flags : TInterface (Name {namespace = "Gtk", name = "TextSearchFlags"})
    Ptr TextIter ->                         -- match_start : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    Ptr TextIter ->                         -- match_end : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    Ptr TextIter ->                         -- limit : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    IO CInt

-- | Searches forward for /@str@/. Any match is returned by setting
-- /@matchStart@/ to the first character of the match and /@matchEnd@/ to the
-- first character after the match. The search will not continue past
-- /@limit@/. Note that a search is a linear or O(n) operation, so you
-- may wish to use /@limit@/ to avoid locking up your UI on large
-- buffers.
-- 
-- /@matchStart@/ will never be set to a t'GI.Gtk.Structs.TextIter.TextIter' located before /@iter@/, even if
-- there is a possible /@matchEnd@/ after or at /@iter@/.
textIterForwardSearch ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TextIter
    -- ^ /@iter@/: start of search
    -> T.Text
    -- ^ /@str@/: a search string
    -> [Gtk.Flags.TextSearchFlags]
    -- ^ /@flags@/: flags affecting how the search is done
    -> Maybe (TextIter)
    -- ^ /@limit@/: location of last possible /@matchEnd@/, or 'P.Nothing' for the end of the buffer
    -> m ((Bool, TextIter, TextIter))
    -- ^ __Returns:__ whether a match was found
textIterForwardSearch iter str flags limit = liftIO $ do
    iter' <- unsafeManagedPtrGetPtr iter
    str' <- textToCString str
    let flags' = gflagsToWord flags
    matchStart <- SP.callocBoxedBytes 80 :: IO (Ptr TextIter)
    matchEnd <- SP.callocBoxedBytes 80 :: IO (Ptr TextIter)
    maybeLimit <- case limit of
        Nothing -> return nullPtr
        Just jLimit -> do
            jLimit' <- unsafeManagedPtrGetPtr jLimit
            return jLimit'
    result <- gtk_text_iter_forward_search iter' str' flags' matchStart matchEnd maybeLimit
    let result' = (/= 0) result
    matchStart' <- (wrapBoxed TextIter) matchStart
    matchEnd' <- (wrapBoxed TextIter) matchEnd
    touchManagedPtr iter
    whenJust limit touchManagedPtr
    freeMem str'
    return (result', matchStart', matchEnd')

#if defined(ENABLE_OVERLOADING)
data TextIterForwardSearchMethodInfo
instance (signature ~ (T.Text -> [Gtk.Flags.TextSearchFlags] -> Maybe (TextIter) -> m ((Bool, TextIter, TextIter))), MonadIO m) => O.OverloadedMethod TextIterForwardSearchMethodInfo TextIter signature where
    overloadedMethod = textIterForwardSearch

instance O.OverloadedMethodInfo TextIterForwardSearchMethodInfo TextIter where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextIter.textIterForwardSearch",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextIter.html#v:textIterForwardSearch"
        })


#endif

-- method TextIter::forward_sentence_end
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextIter" , sinceVersion = Nothing }
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

foreign import ccall "gtk_text_iter_forward_sentence_end" gtk_text_iter_forward_sentence_end :: 
    Ptr TextIter ->                         -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    IO CInt

-- | Moves forward to the next sentence end. (If /@iter@/ is at the end of
-- a sentence, moves to the next end of sentence.)  Sentence
-- boundaries are determined by Pango and should be correct for nearly
-- any language (if not, the correct fix would be to the Pango text
-- boundary algorithms).
textIterForwardSentenceEnd ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TextIter
    -- ^ /@iter@/: a t'GI.Gtk.Structs.TextIter.TextIter'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if /@iter@/ moved and is not the end iterator
textIterForwardSentenceEnd iter = liftIO $ do
    iter' <- unsafeManagedPtrGetPtr iter
    result <- gtk_text_iter_forward_sentence_end iter'
    let result' = (/= 0) result
    touchManagedPtr iter
    return result'

#if defined(ENABLE_OVERLOADING)
data TextIterForwardSentenceEndMethodInfo
instance (signature ~ (m Bool), MonadIO m) => O.OverloadedMethod TextIterForwardSentenceEndMethodInfo TextIter signature where
    overloadedMethod = textIterForwardSentenceEnd

instance O.OverloadedMethodInfo TextIterForwardSentenceEndMethodInfo TextIter where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextIter.textIterForwardSentenceEnd",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextIter.html#v:textIterForwardSentenceEnd"
        })


#endif

-- method TextIter::forward_sentence_ends
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextIter" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "count"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "number of sentences to move"
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

foreign import ccall "gtk_text_iter_forward_sentence_ends" gtk_text_iter_forward_sentence_ends :: 
    Ptr TextIter ->                         -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    Int32 ->                                -- count : TBasicType TInt
    IO CInt

-- | Calls 'GI.Gtk.Structs.TextIter.textIterForwardSentenceEnd' /@count@/ times (or until
-- 'GI.Gtk.Structs.TextIter.textIterForwardSentenceEnd' returns 'P.False'). If /@count@/ is
-- negative, moves backward instead of forward.
textIterForwardSentenceEnds ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TextIter
    -- ^ /@iter@/: a t'GI.Gtk.Structs.TextIter.TextIter'
    -> Int32
    -- ^ /@count@/: number of sentences to move
    -> m Bool
    -- ^ __Returns:__ 'P.True' if /@iter@/ moved and is not the end iterator
textIterForwardSentenceEnds iter count = liftIO $ do
    iter' <- unsafeManagedPtrGetPtr iter
    result <- gtk_text_iter_forward_sentence_ends iter' count
    let result' = (/= 0) result
    touchManagedPtr iter
    return result'

#if defined(ENABLE_OVERLOADING)
data TextIterForwardSentenceEndsMethodInfo
instance (signature ~ (Int32 -> m Bool), MonadIO m) => O.OverloadedMethod TextIterForwardSentenceEndsMethodInfo TextIter signature where
    overloadedMethod = textIterForwardSentenceEnds

instance O.OverloadedMethodInfo TextIterForwardSentenceEndsMethodInfo TextIter where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextIter.textIterForwardSentenceEnds",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextIter.html#v:textIterForwardSentenceEnds"
        })


#endif

-- method TextIter::forward_to_end
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextIter" , sinceVersion = Nothing }
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

foreign import ccall "gtk_text_iter_forward_to_end" gtk_text_iter_forward_to_end :: 
    Ptr TextIter ->                         -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    IO ()

-- | Moves /@iter@/ forward to the “end iterator,” which points one past the last
-- valid character in the buffer. 'GI.Gtk.Structs.TextIter.textIterGetChar' called on the
-- end iterator returns 0, which is convenient for writing loops.
textIterForwardToEnd ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TextIter
    -- ^ /@iter@/: a t'GI.Gtk.Structs.TextIter.TextIter'
    -> m ()
textIterForwardToEnd iter = liftIO $ do
    iter' <- unsafeManagedPtrGetPtr iter
    gtk_text_iter_forward_to_end iter'
    touchManagedPtr iter
    return ()

#if defined(ENABLE_OVERLOADING)
data TextIterForwardToEndMethodInfo
instance (signature ~ (m ()), MonadIO m) => O.OverloadedMethod TextIterForwardToEndMethodInfo TextIter signature where
    overloadedMethod = textIterForwardToEnd

instance O.OverloadedMethodInfo TextIterForwardToEndMethodInfo TextIter where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextIter.textIterForwardToEnd",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextIter.html#v:textIterForwardToEnd"
        })


#endif

-- method TextIter::forward_to_line_end
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextIter" , sinceVersion = Nothing }
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

foreign import ccall "gtk_text_iter_forward_to_line_end" gtk_text_iter_forward_to_line_end :: 
    Ptr TextIter ->                         -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    IO CInt

-- | Moves the iterator to point to the paragraph delimiter characters,
-- which will be either a newline, a carriage return, a carriage
-- return\/newline in sequence, or the Unicode paragraph separator
-- character. If the iterator is already at the paragraph delimiter
-- characters, moves to the paragraph delimiter characters for the
-- next line. If /@iter@/ is on the last line in the buffer, which does
-- not end in paragraph delimiters, moves to the end iterator (end of
-- the last line), and returns 'P.False'.
textIterForwardToLineEnd ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TextIter
    -- ^ /@iter@/: a t'GI.Gtk.Structs.TextIter.TextIter'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if we moved and the new location is not the end iterator
textIterForwardToLineEnd iter = liftIO $ do
    iter' <- unsafeManagedPtrGetPtr iter
    result <- gtk_text_iter_forward_to_line_end iter'
    let result' = (/= 0) result
    touchManagedPtr iter
    return result'

#if defined(ENABLE_OVERLOADING)
data TextIterForwardToLineEndMethodInfo
instance (signature ~ (m Bool), MonadIO m) => O.OverloadedMethod TextIterForwardToLineEndMethodInfo TextIter signature where
    overloadedMethod = textIterForwardToLineEnd

instance O.OverloadedMethodInfo TextIterForwardToLineEndMethodInfo TextIter where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextIter.textIterForwardToLineEnd",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextIter.html#v:textIterForwardToLineEnd"
        })


#endif

-- method TextIter::forward_to_tag_toggle
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextIter" , sinceVersion = Nothing }
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
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextTag, or %NULL"
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

foreign import ccall "gtk_text_iter_forward_to_tag_toggle" gtk_text_iter_forward_to_tag_toggle :: 
    Ptr TextIter ->                         -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    Ptr Gtk.TextTag.TextTag ->              -- tag : TInterface (Name {namespace = "Gtk", name = "TextTag"})
    IO CInt

-- | Moves forward to the next toggle (on or off) of the
-- t'GI.Gtk.Objects.TextTag.TextTag' /@tag@/, or to the next toggle of any tag if
-- /@tag@/ is 'P.Nothing'. If no matching tag toggles are found,
-- returns 'P.False', otherwise 'P.True'. Does not return toggles
-- located at /@iter@/, only toggles after /@iter@/. Sets /@iter@/ to
-- the location of the toggle, or to the end of the buffer
-- if no toggle is found.
textIterForwardToTagToggle ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.TextTag.IsTextTag a) =>
    TextIter
    -- ^ /@iter@/: a t'GI.Gtk.Structs.TextIter.TextIter'
    -> Maybe (a)
    -- ^ /@tag@/: a t'GI.Gtk.Objects.TextTag.TextTag', or 'P.Nothing'
    -> m Bool
    -- ^ __Returns:__ whether we found a tag toggle after /@iter@/
textIterForwardToTagToggle iter tag = liftIO $ do
    iter' <- unsafeManagedPtrGetPtr iter
    maybeTag <- case tag of
        Nothing -> return nullPtr
        Just jTag -> do
            jTag' <- unsafeManagedPtrCastPtr jTag
            return jTag'
    result <- gtk_text_iter_forward_to_tag_toggle iter' maybeTag
    let result' = (/= 0) result
    touchManagedPtr iter
    whenJust tag touchManagedPtr
    return result'

#if defined(ENABLE_OVERLOADING)
data TextIterForwardToTagToggleMethodInfo
instance (signature ~ (Maybe (a) -> m Bool), MonadIO m, Gtk.TextTag.IsTextTag a) => O.OverloadedMethod TextIterForwardToTagToggleMethodInfo TextIter signature where
    overloadedMethod = textIterForwardToTagToggle

instance O.OverloadedMethodInfo TextIterForwardToTagToggleMethodInfo TextIter where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextIter.textIterForwardToTagToggle",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextIter.html#v:textIterForwardToTagToggle"
        })


#endif

-- method TextIter::forward_visible_cursor_position
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextIter" , sinceVersion = Nothing }
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

foreign import ccall "gtk_text_iter_forward_visible_cursor_position" gtk_text_iter_forward_visible_cursor_position :: 
    Ptr TextIter ->                         -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    IO CInt

-- | Moves /@iter@/ forward to the next visible cursor position. See
-- 'GI.Gtk.Structs.TextIter.textIterForwardCursorPosition' for details.
-- 
-- /Since: 2.4/
textIterForwardVisibleCursorPosition ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TextIter
    -- ^ /@iter@/: a t'GI.Gtk.Structs.TextIter.TextIter'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if we moved and the new position is dereferenceable
textIterForwardVisibleCursorPosition iter = liftIO $ do
    iter' <- unsafeManagedPtrGetPtr iter
    result <- gtk_text_iter_forward_visible_cursor_position iter'
    let result' = (/= 0) result
    touchManagedPtr iter
    return result'

#if defined(ENABLE_OVERLOADING)
data TextIterForwardVisibleCursorPositionMethodInfo
instance (signature ~ (m Bool), MonadIO m) => O.OverloadedMethod TextIterForwardVisibleCursorPositionMethodInfo TextIter signature where
    overloadedMethod = textIterForwardVisibleCursorPosition

instance O.OverloadedMethodInfo TextIterForwardVisibleCursorPositionMethodInfo TextIter where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextIter.textIterForwardVisibleCursorPosition",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextIter.html#v:textIterForwardVisibleCursorPosition"
        })


#endif

-- method TextIter::forward_visible_cursor_positions
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextIter" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "count"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "number of positions to move"
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

foreign import ccall "gtk_text_iter_forward_visible_cursor_positions" gtk_text_iter_forward_visible_cursor_positions :: 
    Ptr TextIter ->                         -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    Int32 ->                                -- count : TBasicType TInt
    IO CInt

-- | Moves up to /@count@/ visible cursor positions. See
-- 'GI.Gtk.Structs.TextIter.textIterForwardCursorPosition' for details.
-- 
-- /Since: 2.4/
textIterForwardVisibleCursorPositions ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TextIter
    -- ^ /@iter@/: a t'GI.Gtk.Structs.TextIter.TextIter'
    -> Int32
    -- ^ /@count@/: number of positions to move
    -> m Bool
    -- ^ __Returns:__ 'P.True' if we moved and the new position is dereferenceable
textIterForwardVisibleCursorPositions iter count = liftIO $ do
    iter' <- unsafeManagedPtrGetPtr iter
    result <- gtk_text_iter_forward_visible_cursor_positions iter' count
    let result' = (/= 0) result
    touchManagedPtr iter
    return result'

#if defined(ENABLE_OVERLOADING)
data TextIterForwardVisibleCursorPositionsMethodInfo
instance (signature ~ (Int32 -> m Bool), MonadIO m) => O.OverloadedMethod TextIterForwardVisibleCursorPositionsMethodInfo TextIter signature where
    overloadedMethod = textIterForwardVisibleCursorPositions

instance O.OverloadedMethodInfo TextIterForwardVisibleCursorPositionsMethodInfo TextIter where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextIter.textIterForwardVisibleCursorPositions",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextIter.html#v:textIterForwardVisibleCursorPositions"
        })


#endif

-- method TextIter::forward_visible_line
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "an iterator" , sinceVersion = Nothing }
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

foreign import ccall "gtk_text_iter_forward_visible_line" gtk_text_iter_forward_visible_line :: 
    Ptr TextIter ->                         -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    IO CInt

-- | Moves /@iter@/ to the start of the next visible line. Returns 'P.True' if there
-- was a next line to move to, and 'P.False' if /@iter@/ was simply moved to
-- the end of the buffer and is now not dereferenceable, or if /@iter@/ was
-- already at the end of the buffer.
-- 
-- /Since: 2.8/
textIterForwardVisibleLine ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TextIter
    -- ^ /@iter@/: an iterator
    -> m Bool
    -- ^ __Returns:__ whether /@iter@/ can be dereferenced
textIterForwardVisibleLine iter = liftIO $ do
    iter' <- unsafeManagedPtrGetPtr iter
    result <- gtk_text_iter_forward_visible_line iter'
    let result' = (/= 0) result
    touchManagedPtr iter
    return result'

#if defined(ENABLE_OVERLOADING)
data TextIterForwardVisibleLineMethodInfo
instance (signature ~ (m Bool), MonadIO m) => O.OverloadedMethod TextIterForwardVisibleLineMethodInfo TextIter signature where
    overloadedMethod = textIterForwardVisibleLine

instance O.OverloadedMethodInfo TextIterForwardVisibleLineMethodInfo TextIter where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextIter.textIterForwardVisibleLine",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextIter.html#v:textIterForwardVisibleLine"
        })


#endif

-- method TextIter::forward_visible_lines
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextIter" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "count"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "number of lines to move forward"
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

foreign import ccall "gtk_text_iter_forward_visible_lines" gtk_text_iter_forward_visible_lines :: 
    Ptr TextIter ->                         -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    Int32 ->                                -- count : TBasicType TInt
    IO CInt

-- | Moves /@count@/ visible lines forward, if possible (if /@count@/ would move
-- past the start or end of the buffer, moves to the start or end of
-- the buffer).  The return value indicates whether the iterator moved
-- onto a dereferenceable position; if the iterator didn’t move, or
-- moved onto the end iterator, then 'P.False' is returned. If /@count@/ is 0,
-- the function does nothing and returns 'P.False'. If /@count@/ is negative,
-- moves backward by 0 - /@count@/ lines.
-- 
-- /Since: 2.8/
textIterForwardVisibleLines ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TextIter
    -- ^ /@iter@/: a t'GI.Gtk.Structs.TextIter.TextIter'
    -> Int32
    -- ^ /@count@/: number of lines to move forward
    -> m Bool
    -- ^ __Returns:__ whether /@iter@/ moved and is dereferenceable
textIterForwardVisibleLines iter count = liftIO $ do
    iter' <- unsafeManagedPtrGetPtr iter
    result <- gtk_text_iter_forward_visible_lines iter' count
    let result' = (/= 0) result
    touchManagedPtr iter
    return result'

#if defined(ENABLE_OVERLOADING)
data TextIterForwardVisibleLinesMethodInfo
instance (signature ~ (Int32 -> m Bool), MonadIO m) => O.OverloadedMethod TextIterForwardVisibleLinesMethodInfo TextIter signature where
    overloadedMethod = textIterForwardVisibleLines

instance O.OverloadedMethodInfo TextIterForwardVisibleLinesMethodInfo TextIter where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextIter.textIterForwardVisibleLines",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextIter.html#v:textIterForwardVisibleLines"
        })


#endif

-- method TextIter::forward_visible_word_end
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextIter" , sinceVersion = Nothing }
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

foreign import ccall "gtk_text_iter_forward_visible_word_end" gtk_text_iter_forward_visible_word_end :: 
    Ptr TextIter ->                         -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    IO CInt

-- | Moves forward to the next visible word end. (If /@iter@/ is currently on a
-- word end, moves forward to the next one after that.) Word breaks
-- are determined by Pango and should be correct for nearly any
-- language (if not, the correct fix would be to the Pango word break
-- algorithms).
-- 
-- /Since: 2.4/
textIterForwardVisibleWordEnd ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TextIter
    -- ^ /@iter@/: a t'GI.Gtk.Structs.TextIter.TextIter'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if /@iter@/ moved and is not the end iterator
textIterForwardVisibleWordEnd iter = liftIO $ do
    iter' <- unsafeManagedPtrGetPtr iter
    result <- gtk_text_iter_forward_visible_word_end iter'
    let result' = (/= 0) result
    touchManagedPtr iter
    return result'

#if defined(ENABLE_OVERLOADING)
data TextIterForwardVisibleWordEndMethodInfo
instance (signature ~ (m Bool), MonadIO m) => O.OverloadedMethod TextIterForwardVisibleWordEndMethodInfo TextIter signature where
    overloadedMethod = textIterForwardVisibleWordEnd

instance O.OverloadedMethodInfo TextIterForwardVisibleWordEndMethodInfo TextIter where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextIter.textIterForwardVisibleWordEnd",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextIter.html#v:textIterForwardVisibleWordEnd"
        })


#endif

-- method TextIter::forward_visible_word_ends
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextIter" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "count"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "number of times to move"
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

foreign import ccall "gtk_text_iter_forward_visible_word_ends" gtk_text_iter_forward_visible_word_ends :: 
    Ptr TextIter ->                         -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    Int32 ->                                -- count : TBasicType TInt
    IO CInt

-- | Calls 'GI.Gtk.Structs.TextIter.textIterForwardVisibleWordEnd' up to /@count@/ times.
-- 
-- /Since: 2.4/
textIterForwardVisibleWordEnds ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TextIter
    -- ^ /@iter@/: a t'GI.Gtk.Structs.TextIter.TextIter'
    -> Int32
    -- ^ /@count@/: number of times to move
    -> m Bool
    -- ^ __Returns:__ 'P.True' if /@iter@/ moved and is not the end iterator
textIterForwardVisibleWordEnds iter count = liftIO $ do
    iter' <- unsafeManagedPtrGetPtr iter
    result <- gtk_text_iter_forward_visible_word_ends iter' count
    let result' = (/= 0) result
    touchManagedPtr iter
    return result'

#if defined(ENABLE_OVERLOADING)
data TextIterForwardVisibleWordEndsMethodInfo
instance (signature ~ (Int32 -> m Bool), MonadIO m) => O.OverloadedMethod TextIterForwardVisibleWordEndsMethodInfo TextIter signature where
    overloadedMethod = textIterForwardVisibleWordEnds

instance O.OverloadedMethodInfo TextIterForwardVisibleWordEndsMethodInfo TextIter where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextIter.textIterForwardVisibleWordEnds",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextIter.html#v:textIterForwardVisibleWordEnds"
        })


#endif

-- method TextIter::forward_word_end
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextIter" , sinceVersion = Nothing }
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

foreign import ccall "gtk_text_iter_forward_word_end" gtk_text_iter_forward_word_end :: 
    Ptr TextIter ->                         -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    IO CInt

-- | Moves forward to the next word end. (If /@iter@/ is currently on a
-- word end, moves forward to the next one after that.) Word breaks
-- are determined by Pango and should be correct for nearly any
-- language (if not, the correct fix would be to the Pango word break
-- algorithms).
textIterForwardWordEnd ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TextIter
    -- ^ /@iter@/: a t'GI.Gtk.Structs.TextIter.TextIter'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if /@iter@/ moved and is not the end iterator
textIterForwardWordEnd iter = liftIO $ do
    iter' <- unsafeManagedPtrGetPtr iter
    result <- gtk_text_iter_forward_word_end iter'
    let result' = (/= 0) result
    touchManagedPtr iter
    return result'

#if defined(ENABLE_OVERLOADING)
data TextIterForwardWordEndMethodInfo
instance (signature ~ (m Bool), MonadIO m) => O.OverloadedMethod TextIterForwardWordEndMethodInfo TextIter signature where
    overloadedMethod = textIterForwardWordEnd

instance O.OverloadedMethodInfo TextIterForwardWordEndMethodInfo TextIter where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextIter.textIterForwardWordEnd",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextIter.html#v:textIterForwardWordEnd"
        })


#endif

-- method TextIter::forward_word_ends
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextIter" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "count"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "number of times to move"
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

foreign import ccall "gtk_text_iter_forward_word_ends" gtk_text_iter_forward_word_ends :: 
    Ptr TextIter ->                         -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    Int32 ->                                -- count : TBasicType TInt
    IO CInt

-- | Calls 'GI.Gtk.Structs.TextIter.textIterForwardWordEnd' up to /@count@/ times.
textIterForwardWordEnds ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TextIter
    -- ^ /@iter@/: a t'GI.Gtk.Structs.TextIter.TextIter'
    -> Int32
    -- ^ /@count@/: number of times to move
    -> m Bool
    -- ^ __Returns:__ 'P.True' if /@iter@/ moved and is not the end iterator
textIterForwardWordEnds iter count = liftIO $ do
    iter' <- unsafeManagedPtrGetPtr iter
    result <- gtk_text_iter_forward_word_ends iter' count
    let result' = (/= 0) result
    touchManagedPtr iter
    return result'

#if defined(ENABLE_OVERLOADING)
data TextIterForwardWordEndsMethodInfo
instance (signature ~ (Int32 -> m Bool), MonadIO m) => O.OverloadedMethod TextIterForwardWordEndsMethodInfo TextIter signature where
    overloadedMethod = textIterForwardWordEnds

instance O.OverloadedMethodInfo TextIterForwardWordEndsMethodInfo TextIter where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextIter.textIterForwardWordEnds",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextIter.html#v:textIterForwardWordEnds"
        })


#endif

-- method TextIter::free
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a dynamically-allocated iterator"
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

foreign import ccall "gtk_text_iter_free" gtk_text_iter_free :: 
    Ptr TextIter ->                         -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    IO ()

-- | Free an iterator allocated on the heap. This function
-- is intended for use in language bindings, and is not
-- especially useful for applications, because iterators can
-- simply be allocated on the stack.
textIterFree ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TextIter
    -- ^ /@iter@/: a dynamically-allocated iterator
    -> m ()
textIterFree iter = liftIO $ do
    iter' <- unsafeManagedPtrGetPtr iter
    gtk_text_iter_free iter'
    touchManagedPtr iter
    return ()

#if defined(ENABLE_OVERLOADING)
data TextIterFreeMethodInfo
instance (signature ~ (m ()), MonadIO m) => O.OverloadedMethod TextIterFreeMethodInfo TextIter signature where
    overloadedMethod = textIterFree

instance O.OverloadedMethodInfo TextIterFreeMethodInfo TextIter where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextIter.textIterFree",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextIter.html#v:textIterFree"
        })


#endif

-- method TextIter::get_attributes
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "an iterator" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "values"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextAttributes" }
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextAttributes to be filled in"
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

foreign import ccall "gtk_text_iter_get_attributes" gtk_text_iter_get_attributes :: 
    Ptr TextIter ->                         -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    Ptr Gtk.TextAttributes.TextAttributes -> -- values : TInterface (Name {namespace = "Gtk", name = "TextAttributes"})
    IO CInt

-- | Computes the effect of any tags applied to this spot in the
-- text. The /@values@/ parameter should be initialized to the default
-- settings you wish to use if no tags are in effect. You’d typically
-- obtain the defaults from 'GI.Gtk.Objects.TextView.textViewGetDefaultAttributes'.
-- 
-- 'GI.Gtk.Structs.TextIter.textIterGetAttributes' will modify /@values@/, applying the
-- effects of any tags present at /@iter@/. If any tags affected /@values@/,
-- the function returns 'P.True'.
textIterGetAttributes ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TextIter
    -- ^ /@iter@/: an iterator
    -> m ((Bool, Gtk.TextAttributes.TextAttributes))
    -- ^ __Returns:__ 'P.True' if /@values@/ was modified
textIterGetAttributes iter = liftIO $ do
    iter' <- unsafeManagedPtrGetPtr iter
    values <- SP.callocBoxedBytes 168 :: IO (Ptr Gtk.TextAttributes.TextAttributes)
    result <- gtk_text_iter_get_attributes iter' values
    let result' = (/= 0) result
    values' <- (wrapBoxed Gtk.TextAttributes.TextAttributes) values
    touchManagedPtr iter
    return (result', values')

#if defined(ENABLE_OVERLOADING)
data TextIterGetAttributesMethodInfo
instance (signature ~ (m ((Bool, Gtk.TextAttributes.TextAttributes))), MonadIO m) => O.OverloadedMethod TextIterGetAttributesMethodInfo TextIter signature where
    overloadedMethod = textIterGetAttributes

instance O.OverloadedMethodInfo TextIterGetAttributesMethodInfo TextIter where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextIter.textIterGetAttributes",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextIter.html#v:textIterGetAttributes"
        })


#endif

-- method TextIter::get_buffer
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "an iterator" , sinceVersion = Nothing }
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

foreign import ccall "gtk_text_iter_get_buffer" gtk_text_iter_get_buffer :: 
    Ptr TextIter ->                         -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    IO (Ptr Gtk.TextBuffer.TextBuffer)

-- | Returns the t'GI.Gtk.Objects.TextBuffer.TextBuffer' this iterator is associated with.
textIterGetBuffer ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TextIter
    -- ^ /@iter@/: an iterator
    -> m Gtk.TextBuffer.TextBuffer
    -- ^ __Returns:__ the buffer
textIterGetBuffer iter = liftIO $ do
    iter' <- unsafeManagedPtrGetPtr iter
    result <- gtk_text_iter_get_buffer iter'
    checkUnexpectedReturnNULL "textIterGetBuffer" result
    result' <- (newObject Gtk.TextBuffer.TextBuffer) result
    touchManagedPtr iter
    return result'

#if defined(ENABLE_OVERLOADING)
data TextIterGetBufferMethodInfo
instance (signature ~ (m Gtk.TextBuffer.TextBuffer), MonadIO m) => O.OverloadedMethod TextIterGetBufferMethodInfo TextIter signature where
    overloadedMethod = textIterGetBuffer

instance O.OverloadedMethodInfo TextIterGetBufferMethodInfo TextIter where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextIter.textIterGetBuffer",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextIter.html#v:textIterGetBuffer"
        })


#endif

-- method TextIter::get_bytes_in_line
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "an iterator" , sinceVersion = Nothing }
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

foreign import ccall "gtk_text_iter_get_bytes_in_line" gtk_text_iter_get_bytes_in_line :: 
    Ptr TextIter ->                         -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    IO Int32

-- | Returns the number of bytes in the line containing /@iter@/,
-- including the paragraph delimiters.
textIterGetBytesInLine ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TextIter
    -- ^ /@iter@/: an iterator
    -> m Int32
    -- ^ __Returns:__ number of bytes in the line
textIterGetBytesInLine iter = liftIO $ do
    iter' <- unsafeManagedPtrGetPtr iter
    result <- gtk_text_iter_get_bytes_in_line iter'
    touchManagedPtr iter
    return result

#if defined(ENABLE_OVERLOADING)
data TextIterGetBytesInLineMethodInfo
instance (signature ~ (m Int32), MonadIO m) => O.OverloadedMethod TextIterGetBytesInLineMethodInfo TextIter signature where
    overloadedMethod = textIterGetBytesInLine

instance O.OverloadedMethodInfo TextIterGetBytesInLineMethodInfo TextIter where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextIter.textIterGetBytesInLine",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextIter.html#v:textIterGetBytesInLine"
        })


#endif

-- method TextIter::get_char
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "an iterator" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TBasicType TUniChar)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_text_iter_get_char" gtk_text_iter_get_char :: 
    Ptr TextIter ->                         -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    IO CInt

-- | The Unicode character at this iterator is returned.  (Equivalent to
-- operator* on a C++ iterator.)  If the element at this iterator is a
-- non-character element, such as an image embedded in the buffer, the
-- Unicode “unknown” character 0xFFFC is returned. If invoked on
-- the end iterator, zero is returned; zero is not a valid Unicode character.
-- So you can write a loop which ends when 'GI.Gtk.Structs.TextIter.textIterGetChar'
-- returns 0.
textIterGetChar ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TextIter
    -- ^ /@iter@/: an iterator
    -> m Char
    -- ^ __Returns:__ a Unicode character, or 0 if /@iter@/ is not dereferenceable
textIterGetChar iter = liftIO $ do
    iter' <- unsafeManagedPtrGetPtr iter
    result <- gtk_text_iter_get_char iter'
    let result' = (chr . fromIntegral) result
    touchManagedPtr iter
    return result'

#if defined(ENABLE_OVERLOADING)
data TextIterGetCharMethodInfo
instance (signature ~ (m Char), MonadIO m) => O.OverloadedMethod TextIterGetCharMethodInfo TextIter signature where
    overloadedMethod = textIterGetChar

instance O.OverloadedMethodInfo TextIterGetCharMethodInfo TextIter where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextIter.textIterGetChar",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextIter.html#v:textIterGetChar"
        })


#endif

-- method TextIter::get_chars_in_line
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "an iterator" , sinceVersion = Nothing }
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

foreign import ccall "gtk_text_iter_get_chars_in_line" gtk_text_iter_get_chars_in_line :: 
    Ptr TextIter ->                         -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    IO Int32

-- | Returns the number of characters in the line containing /@iter@/,
-- including the paragraph delimiters.
textIterGetCharsInLine ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TextIter
    -- ^ /@iter@/: an iterator
    -> m Int32
    -- ^ __Returns:__ number of characters in the line
textIterGetCharsInLine iter = liftIO $ do
    iter' <- unsafeManagedPtrGetPtr iter
    result <- gtk_text_iter_get_chars_in_line iter'
    touchManagedPtr iter
    return result

#if defined(ENABLE_OVERLOADING)
data TextIterGetCharsInLineMethodInfo
instance (signature ~ (m Int32), MonadIO m) => O.OverloadedMethod TextIterGetCharsInLineMethodInfo TextIter signature where
    overloadedMethod = textIterGetCharsInLine

instance O.OverloadedMethodInfo TextIterGetCharsInLineMethodInfo TextIter where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextIter.textIterGetCharsInLine",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextIter.html#v:textIterGetCharsInLine"
        })


#endif

-- method TextIter::get_child_anchor
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "an iterator" , sinceVersion = Nothing }
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

foreign import ccall "gtk_text_iter_get_child_anchor" gtk_text_iter_get_child_anchor :: 
    Ptr TextIter ->                         -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    IO (Ptr Gtk.TextChildAnchor.TextChildAnchor)

-- | If the location at /@iter@/ contains a child anchor, the
-- anchor is returned (with no new reference count added). Otherwise,
-- 'P.Nothing' is returned.
textIterGetChildAnchor ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TextIter
    -- ^ /@iter@/: an iterator
    -> m Gtk.TextChildAnchor.TextChildAnchor
    -- ^ __Returns:__ the anchor at /@iter@/
textIterGetChildAnchor iter = liftIO $ do
    iter' <- unsafeManagedPtrGetPtr iter
    result <- gtk_text_iter_get_child_anchor iter'
    checkUnexpectedReturnNULL "textIterGetChildAnchor" result
    result' <- (newObject Gtk.TextChildAnchor.TextChildAnchor) result
    touchManagedPtr iter
    return result'

#if defined(ENABLE_OVERLOADING)
data TextIterGetChildAnchorMethodInfo
instance (signature ~ (m Gtk.TextChildAnchor.TextChildAnchor), MonadIO m) => O.OverloadedMethod TextIterGetChildAnchorMethodInfo TextIter signature where
    overloadedMethod = textIterGetChildAnchor

instance O.OverloadedMethodInfo TextIterGetChildAnchorMethodInfo TextIter where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextIter.textIterGetChildAnchor",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextIter.html#v:textIterGetChildAnchor"
        })


#endif

-- method TextIter::get_language
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "an iterator" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Pango" , name = "Language" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_text_iter_get_language" gtk_text_iter_get_language :: 
    Ptr TextIter ->                         -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    IO (Ptr Pango.Language.Language)

-- | A convenience wrapper around 'GI.Gtk.Structs.TextIter.textIterGetAttributes',
-- which returns the language in effect at /@iter@/. If no tags affecting
-- language apply to /@iter@/, the return value is identical to that of
-- 'GI.Gtk.Functions.getDefaultLanguage'.
textIterGetLanguage ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TextIter
    -- ^ /@iter@/: an iterator
    -> m Pango.Language.Language
    -- ^ __Returns:__ language in effect at /@iter@/
textIterGetLanguage iter = liftIO $ do
    iter' <- unsafeManagedPtrGetPtr iter
    result <- gtk_text_iter_get_language iter'
    checkUnexpectedReturnNULL "textIterGetLanguage" result
    result' <- (wrapBoxed Pango.Language.Language) result
    touchManagedPtr iter
    return result'

#if defined(ENABLE_OVERLOADING)
data TextIterGetLanguageMethodInfo
instance (signature ~ (m Pango.Language.Language), MonadIO m) => O.OverloadedMethod TextIterGetLanguageMethodInfo TextIter signature where
    overloadedMethod = textIterGetLanguage

instance O.OverloadedMethodInfo TextIterGetLanguageMethodInfo TextIter where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextIter.textIterGetLanguage",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextIter.html#v:textIterGetLanguage"
        })


#endif

-- method TextIter::get_line
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "an iterator" , sinceVersion = Nothing }
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

foreign import ccall "gtk_text_iter_get_line" gtk_text_iter_get_line :: 
    Ptr TextIter ->                         -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    IO Int32

-- | Returns the line number containing the iterator. Lines in
-- a t'GI.Gtk.Objects.TextBuffer.TextBuffer' are numbered beginning with 0 for the first
-- line in the buffer.
textIterGetLine ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TextIter
    -- ^ /@iter@/: an iterator
    -> m Int32
    -- ^ __Returns:__ a line number
textIterGetLine iter = liftIO $ do
    iter' <- unsafeManagedPtrGetPtr iter
    result <- gtk_text_iter_get_line iter'
    touchManagedPtr iter
    return result

#if defined(ENABLE_OVERLOADING)
data TextIterGetLineMethodInfo
instance (signature ~ (m Int32), MonadIO m) => O.OverloadedMethod TextIterGetLineMethodInfo TextIter signature where
    overloadedMethod = textIterGetLine

instance O.OverloadedMethodInfo TextIterGetLineMethodInfo TextIter where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextIter.textIterGetLine",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextIter.html#v:textIterGetLine"
        })


#endif

-- method TextIter::get_line_index
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "an iterator" , sinceVersion = Nothing }
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

foreign import ccall "gtk_text_iter_get_line_index" gtk_text_iter_get_line_index :: 
    Ptr TextIter ->                         -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    IO Int32

-- | Returns the byte index of the iterator, counting
-- from the start of a newline-terminated line.
-- Remember that t'GI.Gtk.Objects.TextBuffer.TextBuffer' encodes text in
-- UTF-8, and that characters can require a variable
-- number of bytes to represent.
textIterGetLineIndex ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TextIter
    -- ^ /@iter@/: an iterator
    -> m Int32
    -- ^ __Returns:__ distance from start of line, in bytes
textIterGetLineIndex iter = liftIO $ do
    iter' <- unsafeManagedPtrGetPtr iter
    result <- gtk_text_iter_get_line_index iter'
    touchManagedPtr iter
    return result

#if defined(ENABLE_OVERLOADING)
data TextIterGetLineIndexMethodInfo
instance (signature ~ (m Int32), MonadIO m) => O.OverloadedMethod TextIterGetLineIndexMethodInfo TextIter signature where
    overloadedMethod = textIterGetLineIndex

instance O.OverloadedMethodInfo TextIterGetLineIndexMethodInfo TextIter where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextIter.textIterGetLineIndex",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextIter.html#v:textIterGetLineIndex"
        })


#endif

-- method TextIter::get_line_offset
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "an iterator" , sinceVersion = Nothing }
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

foreign import ccall "gtk_text_iter_get_line_offset" gtk_text_iter_get_line_offset :: 
    Ptr TextIter ->                         -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    IO Int32

-- | Returns the character offset of the iterator,
-- counting from the start of a newline-terminated line.
-- The first character on the line has offset 0.
textIterGetLineOffset ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TextIter
    -- ^ /@iter@/: an iterator
    -> m Int32
    -- ^ __Returns:__ offset from start of line
textIterGetLineOffset iter = liftIO $ do
    iter' <- unsafeManagedPtrGetPtr iter
    result <- gtk_text_iter_get_line_offset iter'
    touchManagedPtr iter
    return result

#if defined(ENABLE_OVERLOADING)
data TextIterGetLineOffsetMethodInfo
instance (signature ~ (m Int32), MonadIO m) => O.OverloadedMethod TextIterGetLineOffsetMethodInfo TextIter signature where
    overloadedMethod = textIterGetLineOffset

instance O.OverloadedMethodInfo TextIterGetLineOffsetMethodInfo TextIter where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextIter.textIterGetLineOffset",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextIter.html#v:textIterGetLineOffset"
        })


#endif

-- method TextIter::get_marks
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "an iterator" , sinceVersion = Nothing }
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
--                  (TInterface Name { namespace = "Gtk" , name = "TextMark" }))
-- throws : False
-- Skip return : False

foreign import ccall "gtk_text_iter_get_marks" gtk_text_iter_get_marks :: 
    Ptr TextIter ->                         -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    IO (Ptr (GSList (Ptr Gtk.TextMark.TextMark)))

-- | Returns a list of all t'GI.Gtk.Objects.TextMark.TextMark' at this location. Because marks
-- are not iterable (they don’t take up any \"space\" in the buffer,
-- they are just marks in between iterable locations), multiple marks
-- can exist in the same place. The returned list is not in any
-- meaningful order.
textIterGetMarks ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TextIter
    -- ^ /@iter@/: an iterator
    -> m [Gtk.TextMark.TextMark]
    -- ^ __Returns:__ list of t'GI.Gtk.Objects.TextMark.TextMark'
textIterGetMarks iter = liftIO $ do
    iter' <- unsafeManagedPtrGetPtr iter
    result <- gtk_text_iter_get_marks iter'
    result' <- unpackGSList result
    result'' <- mapM (newObject Gtk.TextMark.TextMark) result'
    g_slist_free result
    touchManagedPtr iter
    return result''

#if defined(ENABLE_OVERLOADING)
data TextIterGetMarksMethodInfo
instance (signature ~ (m [Gtk.TextMark.TextMark]), MonadIO m) => O.OverloadedMethod TextIterGetMarksMethodInfo TextIter signature where
    overloadedMethod = textIterGetMarks

instance O.OverloadedMethodInfo TextIterGetMarksMethodInfo TextIter where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextIter.textIterGetMarks",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextIter.html#v:textIterGetMarks"
        })


#endif

-- method TextIter::get_offset
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "an iterator" , sinceVersion = Nothing }
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

foreign import ccall "gtk_text_iter_get_offset" gtk_text_iter_get_offset :: 
    Ptr TextIter ->                         -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    IO Int32

-- | Returns the character offset of an iterator.
-- Each character in a t'GI.Gtk.Objects.TextBuffer.TextBuffer' has an offset,
-- starting with 0 for the first character in the buffer.
-- Use 'GI.Gtk.Objects.TextBuffer.textBufferGetIterAtOffset' to convert an
-- offset back into an iterator.
textIterGetOffset ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TextIter
    -- ^ /@iter@/: an iterator
    -> m Int32
    -- ^ __Returns:__ a character offset
textIterGetOffset iter = liftIO $ do
    iter' <- unsafeManagedPtrGetPtr iter
    result <- gtk_text_iter_get_offset iter'
    touchManagedPtr iter
    return result

#if defined(ENABLE_OVERLOADING)
data TextIterGetOffsetMethodInfo
instance (signature ~ (m Int32), MonadIO m) => O.OverloadedMethod TextIterGetOffsetMethodInfo TextIter signature where
    overloadedMethod = textIterGetOffset

instance O.OverloadedMethodInfo TextIterGetOffsetMethodInfo TextIter where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextIter.textIterGetOffset",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextIter.html#v:textIterGetOffset"
        })


#endif

-- method TextIter::get_pixbuf
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "an iterator" , sinceVersion = Nothing }
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

foreign import ccall "gtk_text_iter_get_pixbuf" gtk_text_iter_get_pixbuf :: 
    Ptr TextIter ->                         -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    IO (Ptr GdkPixbuf.Pixbuf.Pixbuf)

-- | If the element at /@iter@/ is a pixbuf, the pixbuf is returned
-- (with no new reference count added). Otherwise,
-- 'P.Nothing' is returned.
textIterGetPixbuf ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TextIter
    -- ^ /@iter@/: an iterator
    -> m GdkPixbuf.Pixbuf.Pixbuf
    -- ^ __Returns:__ the pixbuf at /@iter@/
textIterGetPixbuf iter = liftIO $ do
    iter' <- unsafeManagedPtrGetPtr iter
    result <- gtk_text_iter_get_pixbuf iter'
    checkUnexpectedReturnNULL "textIterGetPixbuf" result
    result' <- (newObject GdkPixbuf.Pixbuf.Pixbuf) result
    touchManagedPtr iter
    return result'

#if defined(ENABLE_OVERLOADING)
data TextIterGetPixbufMethodInfo
instance (signature ~ (m GdkPixbuf.Pixbuf.Pixbuf), MonadIO m) => O.OverloadedMethod TextIterGetPixbufMethodInfo TextIter signature where
    overloadedMethod = textIterGetPixbuf

instance O.OverloadedMethodInfo TextIterGetPixbufMethodInfo TextIter where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextIter.textIterGetPixbuf",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextIter.html#v:textIterGetPixbuf"
        })


#endif

-- method TextIter::get_slice
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "start"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "iterator at start of a range"
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
--                 { rawDocText = Just "iterator at end of a range"
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

foreign import ccall "gtk_text_iter_get_slice" gtk_text_iter_get_slice :: 
    Ptr TextIter ->                         -- start : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    Ptr TextIter ->                         -- end : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    IO CString

-- | Returns the text in the given range. A “slice” is an array of
-- characters encoded in UTF-8 format, including the Unicode “unknown”
-- character 0xFFFC for iterable non-character elements in the buffer,
-- such as images.  Because images are encoded in the slice, byte and
-- character offsets in the returned array will correspond to byte
-- offsets in the text buffer. Note that 0xFFFC can occur in normal
-- text as well, so it is not a reliable indicator that a pixbuf or
-- widget is in the buffer.
textIterGetSlice ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TextIter
    -- ^ /@start@/: iterator at start of a range
    -> TextIter
    -- ^ /@end@/: iterator at end of a range
    -> m T.Text
    -- ^ __Returns:__ slice of text from the buffer
textIterGetSlice start end = liftIO $ do
    start' <- unsafeManagedPtrGetPtr start
    end' <- unsafeManagedPtrGetPtr end
    result <- gtk_text_iter_get_slice start' end'
    checkUnexpectedReturnNULL "textIterGetSlice" result
    result' <- cstringToText result
    freeMem result
    touchManagedPtr start
    touchManagedPtr end
    return result'

#if defined(ENABLE_OVERLOADING)
data TextIterGetSliceMethodInfo
instance (signature ~ (TextIter -> m T.Text), MonadIO m) => O.OverloadedMethod TextIterGetSliceMethodInfo TextIter signature where
    overloadedMethod = textIterGetSlice

instance O.OverloadedMethodInfo TextIterGetSliceMethodInfo TextIter where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextIter.textIterGetSlice",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextIter.html#v:textIterGetSlice"
        })


#endif

-- method TextIter::get_tags
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextIter" , sinceVersion = Nothing }
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
--                  (TInterface Name { namespace = "Gtk" , name = "TextTag" }))
-- throws : False
-- Skip return : False

foreign import ccall "gtk_text_iter_get_tags" gtk_text_iter_get_tags :: 
    Ptr TextIter ->                         -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    IO (Ptr (GSList (Ptr Gtk.TextTag.TextTag)))

-- | Returns a list of tags that apply to /@iter@/, in ascending order of
-- priority (highest-priority tags are last). The t'GI.Gtk.Objects.TextTag.TextTag' in the
-- list don’t have a reference added, but you have to free the list
-- itself.
textIterGetTags ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TextIter
    -- ^ /@iter@/: a t'GI.Gtk.Structs.TextIter.TextIter'
    -> m [Gtk.TextTag.TextTag]
    -- ^ __Returns:__ list of t'GI.Gtk.Objects.TextTag.TextTag'
textIterGetTags iter = liftIO $ do
    iter' <- unsafeManagedPtrGetPtr iter
    result <- gtk_text_iter_get_tags iter'
    result' <- unpackGSList result
    result'' <- mapM (newObject Gtk.TextTag.TextTag) result'
    g_slist_free result
    touchManagedPtr iter
    return result''

#if defined(ENABLE_OVERLOADING)
data TextIterGetTagsMethodInfo
instance (signature ~ (m [Gtk.TextTag.TextTag]), MonadIO m) => O.OverloadedMethod TextIterGetTagsMethodInfo TextIter signature where
    overloadedMethod = textIterGetTags

instance O.OverloadedMethodInfo TextIterGetTagsMethodInfo TextIter where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextIter.textIterGetTags",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextIter.html#v:textIterGetTags"
        })


#endif

-- method TextIter::get_text
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "start"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "iterator at start of a range"
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
--                 { rawDocText = Just "iterator at end of a range"
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

foreign import ccall "gtk_text_iter_get_text" gtk_text_iter_get_text :: 
    Ptr TextIter ->                         -- start : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    Ptr TextIter ->                         -- end : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    IO CString

-- | Returns text in the given range.  If the range
-- contains non-text elements such as images, the character and byte
-- offsets in the returned string will not correspond to character and
-- byte offsets in the buffer. If you want offsets to correspond, see
-- 'GI.Gtk.Structs.TextIter.textIterGetSlice'.
textIterGetText ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TextIter
    -- ^ /@start@/: iterator at start of a range
    -> TextIter
    -- ^ /@end@/: iterator at end of a range
    -> m T.Text
    -- ^ __Returns:__ array of characters from the buffer
textIterGetText start end = liftIO $ do
    start' <- unsafeManagedPtrGetPtr start
    end' <- unsafeManagedPtrGetPtr end
    result <- gtk_text_iter_get_text start' end'
    checkUnexpectedReturnNULL "textIterGetText" result
    result' <- cstringToText result
    freeMem result
    touchManagedPtr start
    touchManagedPtr end
    return result'

#if defined(ENABLE_OVERLOADING)
data TextIterGetTextMethodInfo
instance (signature ~ (TextIter -> m T.Text), MonadIO m) => O.OverloadedMethod TextIterGetTextMethodInfo TextIter signature where
    overloadedMethod = textIterGetText

instance O.OverloadedMethodInfo TextIterGetTextMethodInfo TextIter where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextIter.textIterGetText",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextIter.html#v:textIterGetText"
        })


#endif

-- method TextIter::get_toggled_tags
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "an iterator" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "toggled_on"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "%TRUE to get toggled-on tags"
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
--                  (TInterface Name { namespace = "Gtk" , name = "TextTag" }))
-- throws : False
-- Skip return : False

foreign import ccall "gtk_text_iter_get_toggled_tags" gtk_text_iter_get_toggled_tags :: 
    Ptr TextIter ->                         -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    CInt ->                                 -- toggled_on : TBasicType TBoolean
    IO (Ptr (GSList (Ptr Gtk.TextTag.TextTag)))

-- | Returns a list of t'GI.Gtk.Objects.TextTag.TextTag' that are toggled on or off at this
-- point.  (If /@toggledOn@/ is 'P.True', the list contains tags that are
-- toggled on.) If a tag is toggled on at /@iter@/, then some non-empty
-- range of characters following /@iter@/ has that tag applied to it.  If
-- a tag is toggled off, then some non-empty range following /@iter@/
-- does not have the tag applied to it.
textIterGetToggledTags ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TextIter
    -- ^ /@iter@/: an iterator
    -> Bool
    -- ^ /@toggledOn@/: 'P.True' to get toggled-on tags
    -> m [Gtk.TextTag.TextTag]
    -- ^ __Returns:__ tags toggled at this point
textIterGetToggledTags iter toggledOn = liftIO $ do
    iter' <- unsafeManagedPtrGetPtr iter
    let toggledOn' = (fromIntegral . fromEnum) toggledOn
    result <- gtk_text_iter_get_toggled_tags iter' toggledOn'
    result' <- unpackGSList result
    result'' <- mapM (newObject Gtk.TextTag.TextTag) result'
    g_slist_free result
    touchManagedPtr iter
    return result''

#if defined(ENABLE_OVERLOADING)
data TextIterGetToggledTagsMethodInfo
instance (signature ~ (Bool -> m [Gtk.TextTag.TextTag]), MonadIO m) => O.OverloadedMethod TextIterGetToggledTagsMethodInfo TextIter signature where
    overloadedMethod = textIterGetToggledTags

instance O.OverloadedMethodInfo TextIterGetToggledTagsMethodInfo TextIter where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextIter.textIterGetToggledTags",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextIter.html#v:textIterGetToggledTags"
        })


#endif

-- method TextIter::get_visible_line_index
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextIter" , sinceVersion = Nothing }
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

foreign import ccall "gtk_text_iter_get_visible_line_index" gtk_text_iter_get_visible_line_index :: 
    Ptr TextIter ->                         -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    IO Int32

-- | Returns the number of bytes from the start of the
-- line to the given /@iter@/, not counting bytes that
-- are invisible due to tags with the “invisible” flag
-- toggled on.
textIterGetVisibleLineIndex ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TextIter
    -- ^ /@iter@/: a t'GI.Gtk.Structs.TextIter.TextIter'
    -> m Int32
    -- ^ __Returns:__ byte index of /@iter@/ with respect to the start of the line
textIterGetVisibleLineIndex iter = liftIO $ do
    iter' <- unsafeManagedPtrGetPtr iter
    result <- gtk_text_iter_get_visible_line_index iter'
    touchManagedPtr iter
    return result

#if defined(ENABLE_OVERLOADING)
data TextIterGetVisibleLineIndexMethodInfo
instance (signature ~ (m Int32), MonadIO m) => O.OverloadedMethod TextIterGetVisibleLineIndexMethodInfo TextIter signature where
    overloadedMethod = textIterGetVisibleLineIndex

instance O.OverloadedMethodInfo TextIterGetVisibleLineIndexMethodInfo TextIter where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextIter.textIterGetVisibleLineIndex",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextIter.html#v:textIterGetVisibleLineIndex"
        })


#endif

-- method TextIter::get_visible_line_offset
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextIter" , sinceVersion = Nothing }
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

foreign import ccall "gtk_text_iter_get_visible_line_offset" gtk_text_iter_get_visible_line_offset :: 
    Ptr TextIter ->                         -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    IO Int32

-- | Returns the offset in characters from the start of the
-- line to the given /@iter@/, not counting characters that
-- are invisible due to tags with the “invisible” flag
-- toggled on.
textIterGetVisibleLineOffset ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TextIter
    -- ^ /@iter@/: a t'GI.Gtk.Structs.TextIter.TextIter'
    -> m Int32
    -- ^ __Returns:__ offset in visible characters from the start of the line
textIterGetVisibleLineOffset iter = liftIO $ do
    iter' <- unsafeManagedPtrGetPtr iter
    result <- gtk_text_iter_get_visible_line_offset iter'
    touchManagedPtr iter
    return result

#if defined(ENABLE_OVERLOADING)
data TextIterGetVisibleLineOffsetMethodInfo
instance (signature ~ (m Int32), MonadIO m) => O.OverloadedMethod TextIterGetVisibleLineOffsetMethodInfo TextIter signature where
    overloadedMethod = textIterGetVisibleLineOffset

instance O.OverloadedMethodInfo TextIterGetVisibleLineOffsetMethodInfo TextIter where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextIter.textIterGetVisibleLineOffset",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextIter.html#v:textIterGetVisibleLineOffset"
        })


#endif

-- method TextIter::get_visible_slice
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "start"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "iterator at start of range"
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
--                 { rawDocText = Just "iterator at end of range"
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

foreign import ccall "gtk_text_iter_get_visible_slice" gtk_text_iter_get_visible_slice :: 
    Ptr TextIter ->                         -- start : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    Ptr TextIter ->                         -- end : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    IO CString

-- | Like 'GI.Gtk.Structs.TextIter.textIterGetSlice', but invisible text is not included.
-- Invisible text is usually invisible because a t'GI.Gtk.Objects.TextTag.TextTag' with the
-- “invisible” attribute turned on has been applied to it.
textIterGetVisibleSlice ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TextIter
    -- ^ /@start@/: iterator at start of range
    -> TextIter
    -- ^ /@end@/: iterator at end of range
    -> m T.Text
    -- ^ __Returns:__ slice of text from the buffer
textIterGetVisibleSlice start end = liftIO $ do
    start' <- unsafeManagedPtrGetPtr start
    end' <- unsafeManagedPtrGetPtr end
    result <- gtk_text_iter_get_visible_slice start' end'
    checkUnexpectedReturnNULL "textIterGetVisibleSlice" result
    result' <- cstringToText result
    freeMem result
    touchManagedPtr start
    touchManagedPtr end
    return result'

#if defined(ENABLE_OVERLOADING)
data TextIterGetVisibleSliceMethodInfo
instance (signature ~ (TextIter -> m T.Text), MonadIO m) => O.OverloadedMethod TextIterGetVisibleSliceMethodInfo TextIter signature where
    overloadedMethod = textIterGetVisibleSlice

instance O.OverloadedMethodInfo TextIterGetVisibleSliceMethodInfo TextIter where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextIter.textIterGetVisibleSlice",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextIter.html#v:textIterGetVisibleSlice"
        })


#endif

-- method TextIter::get_visible_text
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "start"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "iterator at start of range"
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
--                 { rawDocText = Just "iterator at end of range"
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

foreign import ccall "gtk_text_iter_get_visible_text" gtk_text_iter_get_visible_text :: 
    Ptr TextIter ->                         -- start : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    Ptr TextIter ->                         -- end : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    IO CString

-- | Like 'GI.Gtk.Structs.TextIter.textIterGetText', but invisible text is not included.
-- Invisible text is usually invisible because a t'GI.Gtk.Objects.TextTag.TextTag' with the
-- “invisible” attribute turned on has been applied to it.
textIterGetVisibleText ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TextIter
    -- ^ /@start@/: iterator at start of range
    -> TextIter
    -- ^ /@end@/: iterator at end of range
    -> m T.Text
    -- ^ __Returns:__ string containing visible text in the
    -- range
textIterGetVisibleText start end = liftIO $ do
    start' <- unsafeManagedPtrGetPtr start
    end' <- unsafeManagedPtrGetPtr end
    result <- gtk_text_iter_get_visible_text start' end'
    checkUnexpectedReturnNULL "textIterGetVisibleText" result
    result' <- cstringToText result
    freeMem result
    touchManagedPtr start
    touchManagedPtr end
    return result'

#if defined(ENABLE_OVERLOADING)
data TextIterGetVisibleTextMethodInfo
instance (signature ~ (TextIter -> m T.Text), MonadIO m) => O.OverloadedMethod TextIterGetVisibleTextMethodInfo TextIter signature where
    overloadedMethod = textIterGetVisibleText

instance O.OverloadedMethodInfo TextIterGetVisibleTextMethodInfo TextIter where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextIter.textIterGetVisibleText",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextIter.html#v:textIterGetVisibleText"
        })


#endif

-- method TextIter::has_tag
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "an iterator" , sinceVersion = Nothing }
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
--       ]
-- Lengths: []
-- returnType: Just (TBasicType TBoolean)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_text_iter_has_tag" gtk_text_iter_has_tag :: 
    Ptr TextIter ->                         -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    Ptr Gtk.TextTag.TextTag ->              -- tag : TInterface (Name {namespace = "Gtk", name = "TextTag"})
    IO CInt

-- | Returns 'P.True' if /@iter@/ points to a character that is part of a range tagged
-- with /@tag@/. See also 'GI.Gtk.Structs.TextIter.textIterStartsTag' and 'GI.Gtk.Structs.TextIter.textIterEndsTag'.
textIterHasTag ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.TextTag.IsTextTag a) =>
    TextIter
    -- ^ /@iter@/: an iterator
    -> a
    -- ^ /@tag@/: a t'GI.Gtk.Objects.TextTag.TextTag'
    -> m Bool
    -- ^ __Returns:__ whether /@iter@/ is tagged with /@tag@/
textIterHasTag iter tag = liftIO $ do
    iter' <- unsafeManagedPtrGetPtr iter
    tag' <- unsafeManagedPtrCastPtr tag
    result <- gtk_text_iter_has_tag iter' tag'
    let result' = (/= 0) result
    touchManagedPtr iter
    touchManagedPtr tag
    return result'

#if defined(ENABLE_OVERLOADING)
data TextIterHasTagMethodInfo
instance (signature ~ (a -> m Bool), MonadIO m, Gtk.TextTag.IsTextTag a) => O.OverloadedMethod TextIterHasTagMethodInfo TextIter signature where
    overloadedMethod = textIterHasTag

instance O.OverloadedMethodInfo TextIterHasTagMethodInfo TextIter where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextIter.textIterHasTag",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextIter.html#v:textIterHasTag"
        })


#endif

-- method TextIter::in_range
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextIter" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "start of range" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "end of range" , sinceVersion = Nothing }
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

foreign import ccall "gtk_text_iter_in_range" gtk_text_iter_in_range :: 
    Ptr TextIter ->                         -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    Ptr TextIter ->                         -- start : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    Ptr TextIter ->                         -- end : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    IO CInt

-- | Checks whether /@iter@/ falls in the range [/@start@/, /@end@/).
-- /@start@/ and /@end@/ must be in ascending order.
textIterInRange ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TextIter
    -- ^ /@iter@/: a t'GI.Gtk.Structs.TextIter.TextIter'
    -> TextIter
    -- ^ /@start@/: start of range
    -> TextIter
    -- ^ /@end@/: end of range
    -> m Bool
    -- ^ __Returns:__ 'P.True' if /@iter@/ is in the range
textIterInRange iter start end = liftIO $ do
    iter' <- unsafeManagedPtrGetPtr iter
    start' <- unsafeManagedPtrGetPtr start
    end' <- unsafeManagedPtrGetPtr end
    result <- gtk_text_iter_in_range iter' start' end'
    let result' = (/= 0) result
    touchManagedPtr iter
    touchManagedPtr start
    touchManagedPtr end
    return result'

#if defined(ENABLE_OVERLOADING)
data TextIterInRangeMethodInfo
instance (signature ~ (TextIter -> TextIter -> m Bool), MonadIO m) => O.OverloadedMethod TextIterInRangeMethodInfo TextIter signature where
    overloadedMethod = textIterInRange

instance O.OverloadedMethodInfo TextIterInRangeMethodInfo TextIter where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextIter.textIterInRange",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextIter.html#v:textIterInRange"
        })


#endif

-- method TextIter::inside_sentence
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextIter" , sinceVersion = Nothing }
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

foreign import ccall "gtk_text_iter_inside_sentence" gtk_text_iter_inside_sentence :: 
    Ptr TextIter ->                         -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    IO CInt

-- | Determines whether /@iter@/ is inside a sentence (as opposed to in
-- between two sentences, e.g. after a period and before the first
-- letter of the next sentence).  Sentence boundaries are determined
-- by Pango and should be correct for nearly any language (if not, the
-- correct fix would be to the Pango text boundary algorithms).
textIterInsideSentence ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TextIter
    -- ^ /@iter@/: a t'GI.Gtk.Structs.TextIter.TextIter'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if /@iter@/ is inside a sentence.
textIterInsideSentence iter = liftIO $ do
    iter' <- unsafeManagedPtrGetPtr iter
    result <- gtk_text_iter_inside_sentence iter'
    let result' = (/= 0) result
    touchManagedPtr iter
    return result'

#if defined(ENABLE_OVERLOADING)
data TextIterInsideSentenceMethodInfo
instance (signature ~ (m Bool), MonadIO m) => O.OverloadedMethod TextIterInsideSentenceMethodInfo TextIter signature where
    overloadedMethod = textIterInsideSentence

instance O.OverloadedMethodInfo TextIterInsideSentenceMethodInfo TextIter where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextIter.textIterInsideSentence",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextIter.html#v:textIterInsideSentence"
        })


#endif

-- method TextIter::inside_word
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextIter" , sinceVersion = Nothing }
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

foreign import ccall "gtk_text_iter_inside_word" gtk_text_iter_inside_word :: 
    Ptr TextIter ->                         -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    IO CInt

-- | Determines whether the character pointed by /@iter@/ is part of a
-- natural-language word (as opposed to say inside some whitespace).  Word
-- breaks are determined by Pango and should be correct for nearly any language
-- (if not, the correct fix would be to the Pango word break algorithms).
-- 
-- Note that if 'GI.Gtk.Structs.TextIter.textIterStartsWord' returns 'P.True', then this function
-- returns 'P.True' too, since /@iter@/ points to the first character of the word.
textIterInsideWord ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TextIter
    -- ^ /@iter@/: a t'GI.Gtk.Structs.TextIter.TextIter'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if /@iter@/ is inside a word
textIterInsideWord iter = liftIO $ do
    iter' <- unsafeManagedPtrGetPtr iter
    result <- gtk_text_iter_inside_word iter'
    let result' = (/= 0) result
    touchManagedPtr iter
    return result'

#if defined(ENABLE_OVERLOADING)
data TextIterInsideWordMethodInfo
instance (signature ~ (m Bool), MonadIO m) => O.OverloadedMethod TextIterInsideWordMethodInfo TextIter signature where
    overloadedMethod = textIterInsideWord

instance O.OverloadedMethodInfo TextIterInsideWordMethodInfo TextIter where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextIter.textIterInsideWord",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextIter.html#v:textIterInsideWord"
        })


#endif

-- method TextIter::is_cursor_position
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextIter" , sinceVersion = Nothing }
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

foreign import ccall "gtk_text_iter_is_cursor_position" gtk_text_iter_is_cursor_position :: 
    Ptr TextIter ->                         -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    IO CInt

-- | See 'GI.Gtk.Structs.TextIter.textIterForwardCursorPosition' or t'GI.Pango.Structs.LogAttr.LogAttr' or
-- 'GI.Pango.Functions.break' for details on what a cursor position is.
textIterIsCursorPosition ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TextIter
    -- ^ /@iter@/: a t'GI.Gtk.Structs.TextIter.TextIter'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the cursor can be placed at /@iter@/
textIterIsCursorPosition iter = liftIO $ do
    iter' <- unsafeManagedPtrGetPtr iter
    result <- gtk_text_iter_is_cursor_position iter'
    let result' = (/= 0) result
    touchManagedPtr iter
    return result'

#if defined(ENABLE_OVERLOADING)
data TextIterIsCursorPositionMethodInfo
instance (signature ~ (m Bool), MonadIO m) => O.OverloadedMethod TextIterIsCursorPositionMethodInfo TextIter signature where
    overloadedMethod = textIterIsCursorPosition

instance O.OverloadedMethodInfo TextIterIsCursorPositionMethodInfo TextIter where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextIter.textIterIsCursorPosition",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextIter.html#v:textIterIsCursorPosition"
        })


#endif

-- method TextIter::is_end
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "an iterator" , sinceVersion = Nothing }
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

foreign import ccall "gtk_text_iter_is_end" gtk_text_iter_is_end :: 
    Ptr TextIter ->                         -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    IO CInt

-- | Returns 'P.True' if /@iter@/ is the end iterator, i.e. one past the last
-- dereferenceable iterator in the buffer. 'GI.Gtk.Structs.TextIter.textIterIsEnd' is
-- the most efficient way to check whether an iterator is the end
-- iterator.
textIterIsEnd ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TextIter
    -- ^ /@iter@/: an iterator
    -> m Bool
    -- ^ __Returns:__ whether /@iter@/ is the end iterator
textIterIsEnd iter = liftIO $ do
    iter' <- unsafeManagedPtrGetPtr iter
    result <- gtk_text_iter_is_end iter'
    let result' = (/= 0) result
    touchManagedPtr iter
    return result'

#if defined(ENABLE_OVERLOADING)
data TextIterIsEndMethodInfo
instance (signature ~ (m Bool), MonadIO m) => O.OverloadedMethod TextIterIsEndMethodInfo TextIter signature where
    overloadedMethod = textIterIsEnd

instance O.OverloadedMethodInfo TextIterIsEndMethodInfo TextIter where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextIter.textIterIsEnd",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextIter.html#v:textIterIsEnd"
        })


#endif

-- method TextIter::is_start
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "an iterator" , sinceVersion = Nothing }
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

foreign import ccall "gtk_text_iter_is_start" gtk_text_iter_is_start :: 
    Ptr TextIter ->                         -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    IO CInt

-- | Returns 'P.True' if /@iter@/ is the first iterator in the buffer, that is
-- if /@iter@/ has a character offset of 0.
textIterIsStart ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TextIter
    -- ^ /@iter@/: an iterator
    -> m Bool
    -- ^ __Returns:__ whether /@iter@/ is the first in the buffer
textIterIsStart iter = liftIO $ do
    iter' <- unsafeManagedPtrGetPtr iter
    result <- gtk_text_iter_is_start iter'
    let result' = (/= 0) result
    touchManagedPtr iter
    return result'

#if defined(ENABLE_OVERLOADING)
data TextIterIsStartMethodInfo
instance (signature ~ (m Bool), MonadIO m) => O.OverloadedMethod TextIterIsStartMethodInfo TextIter signature where
    overloadedMethod = textIterIsStart

instance O.OverloadedMethodInfo TextIterIsStartMethodInfo TextIter where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextIter.textIterIsStart",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextIter.html#v:textIterIsStart"
        })


#endif

-- method TextIter::order
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "first"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextIter" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "second"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "another #GtkTextIter"
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

foreign import ccall "gtk_text_iter_order" gtk_text_iter_order :: 
    Ptr TextIter ->                         -- first : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    Ptr TextIter ->                         -- second : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    IO ()

-- | Swaps the value of /@first@/ and /@second@/ if /@second@/ comes before
-- /@first@/ in the buffer. That is, ensures that /@first@/ and /@second@/ are
-- in sequence. Most text buffer functions that take a range call this
-- automatically on your behalf, so there’s no real reason to call it yourself
-- in those cases. There are some exceptions, such as 'GI.Gtk.Structs.TextIter.textIterInRange',
-- that expect a pre-sorted range.
textIterOrder ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TextIter
    -- ^ /@first@/: a t'GI.Gtk.Structs.TextIter.TextIter'
    -> TextIter
    -- ^ /@second@/: another t'GI.Gtk.Structs.TextIter.TextIter'
    -> m ()
textIterOrder first second = liftIO $ do
    first' <- unsafeManagedPtrGetPtr first
    second' <- unsafeManagedPtrGetPtr second
    gtk_text_iter_order first' second'
    touchManagedPtr first
    touchManagedPtr second
    return ()

#if defined(ENABLE_OVERLOADING)
data TextIterOrderMethodInfo
instance (signature ~ (TextIter -> m ()), MonadIO m) => O.OverloadedMethod TextIterOrderMethodInfo TextIter signature where
    overloadedMethod = textIterOrder

instance O.OverloadedMethodInfo TextIterOrderMethodInfo TextIter where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextIter.textIterOrder",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextIter.html#v:textIterOrder"
        })


#endif

-- method TextIter::set_line
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextIter" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "line_number"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "line number (counted from 0)"
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

foreign import ccall "gtk_text_iter_set_line" gtk_text_iter_set_line :: 
    Ptr TextIter ->                         -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    Int32 ->                                -- line_number : TBasicType TInt
    IO ()

-- | Moves iterator /@iter@/ to the start of the line /@lineNumber@/.  If
-- /@lineNumber@/ is negative or larger than the number of lines in the
-- buffer, moves /@iter@/ to the start of the last line in the buffer.
textIterSetLine ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TextIter
    -- ^ /@iter@/: a t'GI.Gtk.Structs.TextIter.TextIter'
    -> Int32
    -- ^ /@lineNumber@/: line number (counted from 0)
    -> m ()
textIterSetLine iter lineNumber = liftIO $ do
    iter' <- unsafeManagedPtrGetPtr iter
    gtk_text_iter_set_line iter' lineNumber
    touchManagedPtr iter
    return ()

#if defined(ENABLE_OVERLOADING)
data TextIterSetLineMethodInfo
instance (signature ~ (Int32 -> m ()), MonadIO m) => O.OverloadedMethod TextIterSetLineMethodInfo TextIter signature where
    overloadedMethod = textIterSetLine

instance O.OverloadedMethodInfo TextIterSetLineMethodInfo TextIter where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextIter.textIterSetLine",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextIter.html#v:textIterSetLine"
        })


#endif

-- method TextIter::set_line_index
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextIter" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "byte_on_line"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "a byte index relative to the start of @iter\8217s current line"
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

foreign import ccall "gtk_text_iter_set_line_index" gtk_text_iter_set_line_index :: 
    Ptr TextIter ->                         -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    Int32 ->                                -- byte_on_line : TBasicType TInt
    IO ()

-- | Same as 'GI.Gtk.Structs.TextIter.textIterSetLineOffset', but works with a
-- byte index. The given byte index must be at
-- the start of a character, it can’t be in the middle of a UTF-8
-- encoded character.
textIterSetLineIndex ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TextIter
    -- ^ /@iter@/: a t'GI.Gtk.Structs.TextIter.TextIter'
    -> Int32
    -- ^ /@byteOnLine@/: a byte index relative to the start of /@iter@/’s current line
    -> m ()
textIterSetLineIndex iter byteOnLine = liftIO $ do
    iter' <- unsafeManagedPtrGetPtr iter
    gtk_text_iter_set_line_index iter' byteOnLine
    touchManagedPtr iter
    return ()

#if defined(ENABLE_OVERLOADING)
data TextIterSetLineIndexMethodInfo
instance (signature ~ (Int32 -> m ()), MonadIO m) => O.OverloadedMethod TextIterSetLineIndexMethodInfo TextIter signature where
    overloadedMethod = textIterSetLineIndex

instance O.OverloadedMethodInfo TextIterSetLineIndexMethodInfo TextIter where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextIter.textIterSetLineIndex",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextIter.html#v:textIterSetLineIndex"
        })


#endif

-- method TextIter::set_line_offset
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextIter" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "char_on_line"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "a character offset relative to the start of @iter\8217s current line"
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

foreign import ccall "gtk_text_iter_set_line_offset" gtk_text_iter_set_line_offset :: 
    Ptr TextIter ->                         -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    Int32 ->                                -- char_on_line : TBasicType TInt
    IO ()

-- | Moves /@iter@/ within a line, to a new character
-- (not byte) offset. The given character offset must be less than or
-- equal to the number of characters in the line; if equal, /@iter@/
-- moves to the start of the next line. See
-- 'GI.Gtk.Structs.TextIter.textIterSetLineIndex' if you have a byte index rather than
-- a character offset.
textIterSetLineOffset ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TextIter
    -- ^ /@iter@/: a t'GI.Gtk.Structs.TextIter.TextIter'
    -> Int32
    -- ^ /@charOnLine@/: a character offset relative to the start of /@iter@/’s current line
    -> m ()
textIterSetLineOffset iter charOnLine = liftIO $ do
    iter' <- unsafeManagedPtrGetPtr iter
    gtk_text_iter_set_line_offset iter' charOnLine
    touchManagedPtr iter
    return ()

#if defined(ENABLE_OVERLOADING)
data TextIterSetLineOffsetMethodInfo
instance (signature ~ (Int32 -> m ()), MonadIO m) => O.OverloadedMethod TextIterSetLineOffsetMethodInfo TextIter signature where
    overloadedMethod = textIterSetLineOffset

instance O.OverloadedMethodInfo TextIterSetLineOffsetMethodInfo TextIter where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextIter.textIterSetLineOffset",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextIter.html#v:textIterSetLineOffset"
        })


#endif

-- method TextIter::set_offset
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextIter" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "a character number" , sinceVersion = Nothing }
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

foreign import ccall "gtk_text_iter_set_offset" gtk_text_iter_set_offset :: 
    Ptr TextIter ->                         -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    Int32 ->                                -- char_offset : TBasicType TInt
    IO ()

-- | Sets /@iter@/ to point to /@charOffset@/. /@charOffset@/ counts from the start
-- of the entire text buffer, starting with 0.
textIterSetOffset ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TextIter
    -- ^ /@iter@/: a t'GI.Gtk.Structs.TextIter.TextIter'
    -> Int32
    -- ^ /@charOffset@/: a character number
    -> m ()
textIterSetOffset iter charOffset = liftIO $ do
    iter' <- unsafeManagedPtrGetPtr iter
    gtk_text_iter_set_offset iter' charOffset
    touchManagedPtr iter
    return ()

#if defined(ENABLE_OVERLOADING)
data TextIterSetOffsetMethodInfo
instance (signature ~ (Int32 -> m ()), MonadIO m) => O.OverloadedMethod TextIterSetOffsetMethodInfo TextIter signature where
    overloadedMethod = textIterSetOffset

instance O.OverloadedMethodInfo TextIterSetOffsetMethodInfo TextIter where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextIter.textIterSetOffset",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextIter.html#v:textIterSetOffset"
        })


#endif

-- method TextIter::set_visible_line_index
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextIter" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "byte_on_line"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a byte index" , sinceVersion = Nothing }
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

foreign import ccall "gtk_text_iter_set_visible_line_index" gtk_text_iter_set_visible_line_index :: 
    Ptr TextIter ->                         -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    Int32 ->                                -- byte_on_line : TBasicType TInt
    IO ()

-- | Like 'GI.Gtk.Structs.TextIter.textIterSetLineIndex', but the index is in visible
-- bytes, i.e. text with a tag making it invisible is not counted
-- in the index.
textIterSetVisibleLineIndex ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TextIter
    -- ^ /@iter@/: a t'GI.Gtk.Structs.TextIter.TextIter'
    -> Int32
    -- ^ /@byteOnLine@/: a byte index
    -> m ()
textIterSetVisibleLineIndex iter byteOnLine = liftIO $ do
    iter' <- unsafeManagedPtrGetPtr iter
    gtk_text_iter_set_visible_line_index iter' byteOnLine
    touchManagedPtr iter
    return ()

#if defined(ENABLE_OVERLOADING)
data TextIterSetVisibleLineIndexMethodInfo
instance (signature ~ (Int32 -> m ()), MonadIO m) => O.OverloadedMethod TextIterSetVisibleLineIndexMethodInfo TextIter signature where
    overloadedMethod = textIterSetVisibleLineIndex

instance O.OverloadedMethodInfo TextIterSetVisibleLineIndexMethodInfo TextIter where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextIter.textIterSetVisibleLineIndex",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextIter.html#v:textIterSetVisibleLineIndex"
        })


#endif

-- method TextIter::set_visible_line_offset
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextIter" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "char_on_line"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a character offset" , sinceVersion = Nothing }
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

foreign import ccall "gtk_text_iter_set_visible_line_offset" gtk_text_iter_set_visible_line_offset :: 
    Ptr TextIter ->                         -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    Int32 ->                                -- char_on_line : TBasicType TInt
    IO ()

-- | Like 'GI.Gtk.Structs.TextIter.textIterSetLineOffset', but the offset is in visible
-- characters, i.e. text with a tag making it invisible is not
-- counted in the offset.
textIterSetVisibleLineOffset ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TextIter
    -- ^ /@iter@/: a t'GI.Gtk.Structs.TextIter.TextIter'
    -> Int32
    -- ^ /@charOnLine@/: a character offset
    -> m ()
textIterSetVisibleLineOffset iter charOnLine = liftIO $ do
    iter' <- unsafeManagedPtrGetPtr iter
    gtk_text_iter_set_visible_line_offset iter' charOnLine
    touchManagedPtr iter
    return ()

#if defined(ENABLE_OVERLOADING)
data TextIterSetVisibleLineOffsetMethodInfo
instance (signature ~ (Int32 -> m ()), MonadIO m) => O.OverloadedMethod TextIterSetVisibleLineOffsetMethodInfo TextIter signature where
    overloadedMethod = textIterSetVisibleLineOffset

instance O.OverloadedMethodInfo TextIterSetVisibleLineOffsetMethodInfo TextIter where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextIter.textIterSetVisibleLineOffset",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextIter.html#v:textIterSetVisibleLineOffset"
        })


#endif

-- method TextIter::starts_line
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "an iterator" , sinceVersion = Nothing }
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

foreign import ccall "gtk_text_iter_starts_line" gtk_text_iter_starts_line :: 
    Ptr TextIter ->                         -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    IO CInt

-- | Returns 'P.True' if /@iter@/ begins a paragraph,
-- i.e. if 'GI.Gtk.Structs.TextIter.textIterGetLineOffset' would return 0.
-- However this function is potentially more efficient than
-- 'GI.Gtk.Structs.TextIter.textIterGetLineOffset' because it doesn’t have to compute
-- the offset, it just has to see whether it’s 0.
textIterStartsLine ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TextIter
    -- ^ /@iter@/: an iterator
    -> m Bool
    -- ^ __Returns:__ whether /@iter@/ begins a line
textIterStartsLine iter = liftIO $ do
    iter' <- unsafeManagedPtrGetPtr iter
    result <- gtk_text_iter_starts_line iter'
    let result' = (/= 0) result
    touchManagedPtr iter
    return result'

#if defined(ENABLE_OVERLOADING)
data TextIterStartsLineMethodInfo
instance (signature ~ (m Bool), MonadIO m) => O.OverloadedMethod TextIterStartsLineMethodInfo TextIter signature where
    overloadedMethod = textIterStartsLine

instance O.OverloadedMethodInfo TextIterStartsLineMethodInfo TextIter where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextIter.textIterStartsLine",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextIter.html#v:textIterStartsLine"
        })


#endif

-- method TextIter::starts_sentence
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextIter" , sinceVersion = Nothing }
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

foreign import ccall "gtk_text_iter_starts_sentence" gtk_text_iter_starts_sentence :: 
    Ptr TextIter ->                         -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    IO CInt

-- | Determines whether /@iter@/ begins a sentence.  Sentence boundaries are
-- determined by Pango and should be correct for nearly any language
-- (if not, the correct fix would be to the Pango text boundary
-- algorithms).
textIterStartsSentence ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TextIter
    -- ^ /@iter@/: a t'GI.Gtk.Structs.TextIter.TextIter'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if /@iter@/ is at the start of a sentence.
textIterStartsSentence iter = liftIO $ do
    iter' <- unsafeManagedPtrGetPtr iter
    result <- gtk_text_iter_starts_sentence iter'
    let result' = (/= 0) result
    touchManagedPtr iter
    return result'

#if defined(ENABLE_OVERLOADING)
data TextIterStartsSentenceMethodInfo
instance (signature ~ (m Bool), MonadIO m) => O.OverloadedMethod TextIterStartsSentenceMethodInfo TextIter signature where
    overloadedMethod = textIterStartsSentence

instance O.OverloadedMethodInfo TextIterStartsSentenceMethodInfo TextIter where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextIter.textIterStartsSentence",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextIter.html#v:textIterStartsSentence"
        })


#endif

-- method TextIter::starts_tag
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "an iterator" , sinceVersion = Nothing }
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
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextTag, or %NULL"
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

foreign import ccall "gtk_text_iter_starts_tag" gtk_text_iter_starts_tag :: 
    Ptr TextIter ->                         -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    Ptr Gtk.TextTag.TextTag ->              -- tag : TInterface (Name {namespace = "Gtk", name = "TextTag"})
    IO CInt

-- | Returns 'P.True' if /@tag@/ is toggled on at exactly this point. If /@tag@/
-- is 'P.Nothing', returns 'P.True' if any tag is toggled on at this point.
-- 
-- Note that if 'GI.Gtk.Structs.TextIter.textIterStartsTag' returns 'P.True', it means that /@iter@/ is
-- at the beginning of the tagged range, and that the
-- character at /@iter@/ is inside the tagged range. In other
-- words, unlike 'GI.Gtk.Structs.TextIter.textIterEndsTag', if 'GI.Gtk.Structs.TextIter.textIterStartsTag' returns
-- 'P.True', 'GI.Gtk.Structs.TextIter.textIterHasTag' will also return 'P.True' for the same
-- parameters.
-- 
-- /Since: 3.20/
textIterStartsTag ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.TextTag.IsTextTag a) =>
    TextIter
    -- ^ /@iter@/: an iterator
    -> Maybe (a)
    -- ^ /@tag@/: a t'GI.Gtk.Objects.TextTag.TextTag', or 'P.Nothing'
    -> m Bool
    -- ^ __Returns:__ whether /@iter@/ is the start of a range tagged with /@tag@/
textIterStartsTag iter tag = liftIO $ do
    iter' <- unsafeManagedPtrGetPtr iter
    maybeTag <- case tag of
        Nothing -> return nullPtr
        Just jTag -> do
            jTag' <- unsafeManagedPtrCastPtr jTag
            return jTag'
    result <- gtk_text_iter_starts_tag iter' maybeTag
    let result' = (/= 0) result
    touchManagedPtr iter
    whenJust tag touchManagedPtr
    return result'

#if defined(ENABLE_OVERLOADING)
data TextIterStartsTagMethodInfo
instance (signature ~ (Maybe (a) -> m Bool), MonadIO m, Gtk.TextTag.IsTextTag a) => O.OverloadedMethod TextIterStartsTagMethodInfo TextIter signature where
    overloadedMethod = textIterStartsTag

instance O.OverloadedMethodInfo TextIterStartsTagMethodInfo TextIter where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextIter.textIterStartsTag",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextIter.html#v:textIterStartsTag"
        })


#endif

-- method TextIter::starts_word
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextIter" , sinceVersion = Nothing }
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

foreign import ccall "gtk_text_iter_starts_word" gtk_text_iter_starts_word :: 
    Ptr TextIter ->                         -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    IO CInt

-- | Determines whether /@iter@/ begins a natural-language word.  Word
-- breaks are determined by Pango and should be correct for nearly any
-- language (if not, the correct fix would be to the Pango word break
-- algorithms).
textIterStartsWord ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TextIter
    -- ^ /@iter@/: a t'GI.Gtk.Structs.TextIter.TextIter'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if /@iter@/ is at the start of a word
textIterStartsWord iter = liftIO $ do
    iter' <- unsafeManagedPtrGetPtr iter
    result <- gtk_text_iter_starts_word iter'
    let result' = (/= 0) result
    touchManagedPtr iter
    return result'

#if defined(ENABLE_OVERLOADING)
data TextIterStartsWordMethodInfo
instance (signature ~ (m Bool), MonadIO m) => O.OverloadedMethod TextIterStartsWordMethodInfo TextIter signature where
    overloadedMethod = textIterStartsWord

instance O.OverloadedMethodInfo TextIterStartsWordMethodInfo TextIter where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextIter.textIterStartsWord",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextIter.html#v:textIterStartsWord"
        })


#endif

-- method TextIter::toggles_tag
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "an iterator" , sinceVersion = Nothing }
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
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextTag, or %NULL"
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

foreign import ccall "gtk_text_iter_toggles_tag" gtk_text_iter_toggles_tag :: 
    Ptr TextIter ->                         -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    Ptr Gtk.TextTag.TextTag ->              -- tag : TInterface (Name {namespace = "Gtk", name = "TextTag"})
    IO CInt

-- | This is equivalent to ('GI.Gtk.Structs.TextIter.textIterStartsTag' ||
-- 'GI.Gtk.Structs.TextIter.textIterEndsTag'), i.e. it tells you whether a range with
-- /@tag@/ applied to it begins or ends at /@iter@/.
textIterTogglesTag ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.TextTag.IsTextTag a) =>
    TextIter
    -- ^ /@iter@/: an iterator
    -> Maybe (a)
    -- ^ /@tag@/: a t'GI.Gtk.Objects.TextTag.TextTag', or 'P.Nothing'
    -> m Bool
    -- ^ __Returns:__ whether /@tag@/ is toggled on or off at /@iter@/
textIterTogglesTag iter tag = liftIO $ do
    iter' <- unsafeManagedPtrGetPtr iter
    maybeTag <- case tag of
        Nothing -> return nullPtr
        Just jTag -> do
            jTag' <- unsafeManagedPtrCastPtr jTag
            return jTag'
    result <- gtk_text_iter_toggles_tag iter' maybeTag
    let result' = (/= 0) result
    touchManagedPtr iter
    whenJust tag touchManagedPtr
    return result'

#if defined(ENABLE_OVERLOADING)
data TextIterTogglesTagMethodInfo
instance (signature ~ (Maybe (a) -> m Bool), MonadIO m, Gtk.TextTag.IsTextTag a) => O.OverloadedMethod TextIterTogglesTagMethodInfo TextIter signature where
    overloadedMethod = textIterTogglesTag

instance O.OverloadedMethodInfo TextIterTogglesTagMethodInfo TextIter where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TextIter.textIterTogglesTag",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TextIter.html#v:textIterTogglesTag"
        })


#endif

#if defined(ENABLE_OVERLOADING)
type family ResolveTextIterMethod (t :: Symbol) (o :: *) :: * where
    ResolveTextIterMethod "assign" o = TextIterAssignMethodInfo
    ResolveTextIterMethod "backwardChar" o = TextIterBackwardCharMethodInfo
    ResolveTextIterMethod "backwardChars" o = TextIterBackwardCharsMethodInfo
    ResolveTextIterMethod "backwardCursorPosition" o = TextIterBackwardCursorPositionMethodInfo
    ResolveTextIterMethod "backwardCursorPositions" o = TextIterBackwardCursorPositionsMethodInfo
    ResolveTextIterMethod "backwardFindChar" o = TextIterBackwardFindCharMethodInfo
    ResolveTextIterMethod "backwardLine" o = TextIterBackwardLineMethodInfo
    ResolveTextIterMethod "backwardLines" o = TextIterBackwardLinesMethodInfo
    ResolveTextIterMethod "backwardSearch" o = TextIterBackwardSearchMethodInfo
    ResolveTextIterMethod "backwardSentenceStart" o = TextIterBackwardSentenceStartMethodInfo
    ResolveTextIterMethod "backwardSentenceStarts" o = TextIterBackwardSentenceStartsMethodInfo
    ResolveTextIterMethod "backwardToTagToggle" o = TextIterBackwardToTagToggleMethodInfo
    ResolveTextIterMethod "backwardVisibleCursorPosition" o = TextIterBackwardVisibleCursorPositionMethodInfo
    ResolveTextIterMethod "backwardVisibleCursorPositions" o = TextIterBackwardVisibleCursorPositionsMethodInfo
    ResolveTextIterMethod "backwardVisibleLine" o = TextIterBackwardVisibleLineMethodInfo
    ResolveTextIterMethod "backwardVisibleLines" o = TextIterBackwardVisibleLinesMethodInfo
    ResolveTextIterMethod "backwardVisibleWordStart" o = TextIterBackwardVisibleWordStartMethodInfo
    ResolveTextIterMethod "backwardVisibleWordStarts" o = TextIterBackwardVisibleWordStartsMethodInfo
    ResolveTextIterMethod "backwardWordStart" o = TextIterBackwardWordStartMethodInfo
    ResolveTextIterMethod "backwardWordStarts" o = TextIterBackwardWordStartsMethodInfo
    ResolveTextIterMethod "beginsTag" o = TextIterBeginsTagMethodInfo
    ResolveTextIterMethod "canInsert" o = TextIterCanInsertMethodInfo
    ResolveTextIterMethod "compare" o = TextIterCompareMethodInfo
    ResolveTextIterMethod "copy" o = TextIterCopyMethodInfo
    ResolveTextIterMethod "editable" o = TextIterEditableMethodInfo
    ResolveTextIterMethod "endsLine" o = TextIterEndsLineMethodInfo
    ResolveTextIterMethod "endsSentence" o = TextIterEndsSentenceMethodInfo
    ResolveTextIterMethod "endsTag" o = TextIterEndsTagMethodInfo
    ResolveTextIterMethod "endsWord" o = TextIterEndsWordMethodInfo
    ResolveTextIterMethod "equal" o = TextIterEqualMethodInfo
    ResolveTextIterMethod "forwardChar" o = TextIterForwardCharMethodInfo
    ResolveTextIterMethod "forwardChars" o = TextIterForwardCharsMethodInfo
    ResolveTextIterMethod "forwardCursorPosition" o = TextIterForwardCursorPositionMethodInfo
    ResolveTextIterMethod "forwardCursorPositions" o = TextIterForwardCursorPositionsMethodInfo
    ResolveTextIterMethod "forwardFindChar" o = TextIterForwardFindCharMethodInfo
    ResolveTextIterMethod "forwardLine" o = TextIterForwardLineMethodInfo
    ResolveTextIterMethod "forwardLines" o = TextIterForwardLinesMethodInfo
    ResolveTextIterMethod "forwardSearch" o = TextIterForwardSearchMethodInfo
    ResolveTextIterMethod "forwardSentenceEnd" o = TextIterForwardSentenceEndMethodInfo
    ResolveTextIterMethod "forwardSentenceEnds" o = TextIterForwardSentenceEndsMethodInfo
    ResolveTextIterMethod "forwardToEnd" o = TextIterForwardToEndMethodInfo
    ResolveTextIterMethod "forwardToLineEnd" o = TextIterForwardToLineEndMethodInfo
    ResolveTextIterMethod "forwardToTagToggle" o = TextIterForwardToTagToggleMethodInfo
    ResolveTextIterMethod "forwardVisibleCursorPosition" o = TextIterForwardVisibleCursorPositionMethodInfo
    ResolveTextIterMethod "forwardVisibleCursorPositions" o = TextIterForwardVisibleCursorPositionsMethodInfo
    ResolveTextIterMethod "forwardVisibleLine" o = TextIterForwardVisibleLineMethodInfo
    ResolveTextIterMethod "forwardVisibleLines" o = TextIterForwardVisibleLinesMethodInfo
    ResolveTextIterMethod "forwardVisibleWordEnd" o = TextIterForwardVisibleWordEndMethodInfo
    ResolveTextIterMethod "forwardVisibleWordEnds" o = TextIterForwardVisibleWordEndsMethodInfo
    ResolveTextIterMethod "forwardWordEnd" o = TextIterForwardWordEndMethodInfo
    ResolveTextIterMethod "forwardWordEnds" o = TextIterForwardWordEndsMethodInfo
    ResolveTextIterMethod "free" o = TextIterFreeMethodInfo
    ResolveTextIterMethod "hasTag" o = TextIterHasTagMethodInfo
    ResolveTextIterMethod "inRange" o = TextIterInRangeMethodInfo
    ResolveTextIterMethod "insideSentence" o = TextIterInsideSentenceMethodInfo
    ResolveTextIterMethod "insideWord" o = TextIterInsideWordMethodInfo
    ResolveTextIterMethod "isCursorPosition" o = TextIterIsCursorPositionMethodInfo
    ResolveTextIterMethod "isEnd" o = TextIterIsEndMethodInfo
    ResolveTextIterMethod "isStart" o = TextIterIsStartMethodInfo
    ResolveTextIterMethod "order" o = TextIterOrderMethodInfo
    ResolveTextIterMethod "startsLine" o = TextIterStartsLineMethodInfo
    ResolveTextIterMethod "startsSentence" o = TextIterStartsSentenceMethodInfo
    ResolveTextIterMethod "startsTag" o = TextIterStartsTagMethodInfo
    ResolveTextIterMethod "startsWord" o = TextIterStartsWordMethodInfo
    ResolveTextIterMethod "togglesTag" o = TextIterTogglesTagMethodInfo
    ResolveTextIterMethod "getAttributes" o = TextIterGetAttributesMethodInfo
    ResolveTextIterMethod "getBuffer" o = TextIterGetBufferMethodInfo
    ResolveTextIterMethod "getBytesInLine" o = TextIterGetBytesInLineMethodInfo
    ResolveTextIterMethod "getChar" o = TextIterGetCharMethodInfo
    ResolveTextIterMethod "getCharsInLine" o = TextIterGetCharsInLineMethodInfo
    ResolveTextIterMethod "getChildAnchor" o = TextIterGetChildAnchorMethodInfo
    ResolveTextIterMethod "getLanguage" o = TextIterGetLanguageMethodInfo
    ResolveTextIterMethod "getLine" o = TextIterGetLineMethodInfo
    ResolveTextIterMethod "getLineIndex" o = TextIterGetLineIndexMethodInfo
    ResolveTextIterMethod "getLineOffset" o = TextIterGetLineOffsetMethodInfo
    ResolveTextIterMethod "getMarks" o = TextIterGetMarksMethodInfo
    ResolveTextIterMethod "getOffset" o = TextIterGetOffsetMethodInfo
    ResolveTextIterMethod "getPixbuf" o = TextIterGetPixbufMethodInfo
    ResolveTextIterMethod "getSlice" o = TextIterGetSliceMethodInfo
    ResolveTextIterMethod "getTags" o = TextIterGetTagsMethodInfo
    ResolveTextIterMethod "getText" o = TextIterGetTextMethodInfo
    ResolveTextIterMethod "getToggledTags" o = TextIterGetToggledTagsMethodInfo
    ResolveTextIterMethod "getVisibleLineIndex" o = TextIterGetVisibleLineIndexMethodInfo
    ResolveTextIterMethod "getVisibleLineOffset" o = TextIterGetVisibleLineOffsetMethodInfo
    ResolveTextIterMethod "getVisibleSlice" o = TextIterGetVisibleSliceMethodInfo
    ResolveTextIterMethod "getVisibleText" o = TextIterGetVisibleTextMethodInfo
    ResolveTextIterMethod "setLine" o = TextIterSetLineMethodInfo
    ResolveTextIterMethod "setLineIndex" o = TextIterSetLineIndexMethodInfo
    ResolveTextIterMethod "setLineOffset" o = TextIterSetLineOffsetMethodInfo
    ResolveTextIterMethod "setOffset" o = TextIterSetOffsetMethodInfo
    ResolveTextIterMethod "setVisibleLineIndex" o = TextIterSetVisibleLineIndexMethodInfo
    ResolveTextIterMethod "setVisibleLineOffset" o = TextIterSetVisibleLineOffsetMethodInfo
    ResolveTextIterMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveTextIterMethod t TextIter, O.OverloadedMethod info TextIter p) => OL.IsLabel t (TextIter -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveTextIterMethod t TextIter, O.OverloadedMethod info TextIter p, R.HasField t TextIter p) => R.HasField t TextIter p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveTextIterMethod t TextIter, O.OverloadedMethodInfo info TextIter) => OL.IsLabel t (O.MethodProxy info TextIter) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif


