#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif
module GI.Gtk.Structs.TextIter where

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

newtype TextIter = TextIter (SP.ManagedPtr TextIter)
instance SP.ManagedPtrNewtype TextIter where
instance O.HasParentTypes TextIter
instance B.Types.TypedObject TextIter where
instance B.Types.GBoxed TextIter
instance B.GValue.IsGValue (Maybe TextIter) where
#if defined(ENABLE_OVERLOADING)
data TextIterAssignMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextIterBackwardCharMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextIterBackwardCharsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextIterBackwardCursorPositionMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextIterBackwardCursorPositionsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextIterBackwardFindCharMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextIterBackwardLineMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextIterBackwardLinesMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextIterBackwardSearchMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextIterBackwardSentenceStartMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextIterBackwardSentenceStartsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextIterBackwardToTagToggleMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextIterBackwardVisibleCursorPositionMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextIterBackwardVisibleCursorPositionsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextIterBackwardVisibleLineMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextIterBackwardVisibleLinesMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextIterBackwardVisibleWordStartMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextIterBackwardVisibleWordStartsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextIterBackwardWordStartMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextIterBackwardWordStartsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextIterBeginsTagMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextIterCanInsertMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextIterCompareMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextIterCopyMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextIterEditableMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextIterEndsLineMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextIterEndsSentenceMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextIterEndsTagMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextIterEndsWordMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextIterEqualMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextIterForwardCharMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextIterForwardCharsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextIterForwardCursorPositionMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextIterForwardCursorPositionsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextIterForwardFindCharMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextIterForwardLineMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextIterForwardLinesMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextIterForwardSearchMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextIterForwardSentenceEndMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextIterForwardSentenceEndsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextIterForwardToEndMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextIterForwardToLineEndMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextIterForwardToTagToggleMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextIterForwardVisibleCursorPositionMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextIterForwardVisibleCursorPositionsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextIterForwardVisibleLineMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextIterForwardVisibleLinesMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextIterForwardVisibleWordEndMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextIterForwardVisibleWordEndsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextIterForwardWordEndMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextIterForwardWordEndsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextIterFreeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextIterGetAttributesMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextIterGetBufferMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextIterGetBytesInLineMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextIterGetCharMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextIterGetCharsInLineMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextIterGetChildAnchorMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextIterGetLanguageMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextIterGetLineMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextIterGetLineIndexMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextIterGetLineOffsetMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextIterGetMarksMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextIterGetOffsetMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextIterGetPixbufMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextIterGetSliceMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextIterGetTagsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextIterGetTextMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextIterGetToggledTagsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextIterGetVisibleLineIndexMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextIterGetVisibleLineOffsetMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextIterGetVisibleSliceMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextIterGetVisibleTextMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextIterHasTagMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextIterInRangeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextIterInsideSentenceMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextIterInsideWordMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextIterIsCursorPositionMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextIterIsEndMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextIterIsStartMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextIterOrderMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextIterSetLineMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextIterSetLineIndexMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextIterSetLineOffsetMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextIterSetOffsetMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextIterSetVisibleLineIndexMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextIterSetVisibleLineOffsetMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextIterStartsLineMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextIterStartsSentenceMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextIterStartsTagMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextIterStartsWordMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextIterTogglesTagMethodInfo
#endif
