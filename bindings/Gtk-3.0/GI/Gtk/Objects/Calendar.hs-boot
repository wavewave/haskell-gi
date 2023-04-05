#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif
module GI.Gtk.Objects.Calendar where

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

newtype Calendar = Calendar (SP.ManagedPtr Calendar)
instance SP.ManagedPtrNewtype Calendar where
instance B.Types.TypedObject Calendar where
instance B.Types.GObject Calendar
class (SP.GObject o, O.IsDescendantOf Calendar o) => IsCalendar o
instance (SP.GObject o, O.IsDescendantOf Calendar o) => IsCalendar o
instance O.HasParentTypes Calendar
toCalendar :: (MIO.MonadIO m, IsCalendar o) => o -> m Calendar
instance B.GValue.IsGValue (Maybe Calendar) where
#if defined(ENABLE_OVERLOADING)
data CalendarDaySelectedSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CalendarDaySelectedDoubleClickSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CalendarMonthChangedSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CalendarNextMonthSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CalendarNextYearSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CalendarPrevMonthSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CalendarPrevYearSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CalendarDayPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CalendarDetailHeightRowsPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CalendarDetailWidthCharsPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CalendarMonthPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CalendarNoMonthChangePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CalendarShowDayNamesPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CalendarShowDetailsPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CalendarShowHeadingPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CalendarShowWeekNumbersPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CalendarYearPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CalendarClearMarksMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CalendarGetDateMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CalendarGetDayIsMarkedMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CalendarGetDetailHeightRowsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CalendarGetDetailWidthCharsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CalendarGetDisplayOptionsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CalendarMarkDayMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CalendarSelectDayMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CalendarSelectMonthMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CalendarSetDetailFuncMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CalendarSetDetailHeightRowsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CalendarSetDetailWidthCharsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CalendarSetDisplayOptionsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CalendarUnmarkDayMethodInfo
#endif
