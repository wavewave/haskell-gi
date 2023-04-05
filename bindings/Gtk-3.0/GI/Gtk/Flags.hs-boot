#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif
module GI.Gtk.Flags where

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

data UIManagerItemType
instance P.Enum UIManagerItemType where
instance O.HasParentTypes UIManagerItemType
instance B.Types.TypedObject UIManagerItemType where
instance B.Types.BoxedFlags UIManagerItemType
instance IsGFlag UIManagerItemType
data TreeModelFlags
instance P.Enum TreeModelFlags where
instance O.HasParentTypes TreeModelFlags
instance B.Types.TypedObject TreeModelFlags where
instance B.Types.BoxedFlags TreeModelFlags
instance IsGFlag TreeModelFlags
data ToolPaletteDragTargets
instance P.Enum ToolPaletteDragTargets where
instance O.HasParentTypes ToolPaletteDragTargets
instance B.Types.TypedObject ToolPaletteDragTargets where
instance B.Types.BoxedFlags ToolPaletteDragTargets
instance IsGFlag ToolPaletteDragTargets
data TextSearchFlags
instance P.Enum TextSearchFlags where
instance O.HasParentTypes TextSearchFlags
instance B.Types.TypedObject TextSearchFlags where
instance B.Types.BoxedFlags TextSearchFlags
instance IsGFlag TextSearchFlags
data TargetFlags
instance P.Enum TargetFlags where
instance O.HasParentTypes TargetFlags
instance B.Types.TypedObject TargetFlags where
instance B.Types.BoxedFlags TargetFlags
instance IsGFlag TargetFlags
data StyleContextPrintFlags
instance P.Enum StyleContextPrintFlags where
instance O.HasParentTypes StyleContextPrintFlags
instance B.Types.TypedObject StyleContextPrintFlags where
instance B.Types.BoxedFlags StyleContextPrintFlags
instance IsGFlag StyleContextPrintFlags
data StateFlags
instance P.Enum StateFlags where
instance O.HasParentTypes StateFlags
instance B.Types.TypedObject StateFlags where
instance B.Types.BoxedFlags StateFlags
instance IsGFlag StateFlags
data RegionFlags
instance P.Enum RegionFlags where
instance O.HasParentTypes RegionFlags
instance B.Types.TypedObject RegionFlags where
instance B.Types.BoxedFlags RegionFlags
instance IsGFlag RegionFlags
data RecentFilterFlags
instance P.Enum RecentFilterFlags where
instance O.HasParentTypes RecentFilterFlags
instance B.Types.TypedObject RecentFilterFlags where
instance B.Types.BoxedFlags RecentFilterFlags
instance IsGFlag RecentFilterFlags
data RcFlags
instance P.Enum RcFlags where
instance O.HasParentTypes RcFlags
instance B.Types.TypedObject RcFlags where
instance B.Types.BoxedFlags RcFlags
instance IsGFlag RcFlags
data PlacesOpenFlags
instance P.Enum PlacesOpenFlags where
instance O.HasParentTypes PlacesOpenFlags
instance B.Types.TypedObject PlacesOpenFlags where
instance B.Types.BoxedFlags PlacesOpenFlags
instance IsGFlag PlacesOpenFlags
data JunctionSides
instance P.Enum JunctionSides where
instance O.HasParentTypes JunctionSides
instance B.Types.TypedObject JunctionSides where
instance B.Types.BoxedFlags JunctionSides
instance IsGFlag JunctionSides
data InputHints
instance P.Enum InputHints where
instance O.HasParentTypes InputHints
instance B.Types.TypedObject InputHints where
instance B.Types.BoxedFlags InputHints
instance IsGFlag InputHints
data IconLookupFlags
instance P.Enum IconLookupFlags where
instance O.HasParentTypes IconLookupFlags
instance B.Types.TypedObject IconLookupFlags where
instance B.Types.BoxedFlags IconLookupFlags
instance IsGFlag IconLookupFlags
data FontChooserLevel
instance P.Enum FontChooserLevel where
instance O.HasParentTypes FontChooserLevel
instance B.Types.TypedObject FontChooserLevel where
instance B.Types.BoxedFlags FontChooserLevel
instance IsGFlag FontChooserLevel
data FileFilterFlags
instance P.Enum FileFilterFlags where
instance O.HasParentTypes FileFilterFlags
instance B.Types.TypedObject FileFilterFlags where
instance B.Types.BoxedFlags FileFilterFlags
instance IsGFlag FileFilterFlags
data EventControllerScrollFlags
instance P.Enum EventControllerScrollFlags where
instance O.HasParentTypes EventControllerScrollFlags
instance B.Types.TypedObject EventControllerScrollFlags where
instance B.Types.BoxedFlags EventControllerScrollFlags
instance IsGFlag EventControllerScrollFlags
data DialogFlags
instance P.Enum DialogFlags where
instance O.HasParentTypes DialogFlags
instance B.Types.TypedObject DialogFlags where
instance B.Types.BoxedFlags DialogFlags
instance IsGFlag DialogFlags
data DestDefaults
instance P.Enum DestDefaults where
instance O.HasParentTypes DestDefaults
instance B.Types.TypedObject DestDefaults where
instance B.Types.BoxedFlags DestDefaults
instance IsGFlag DestDefaults
data DebugFlag
instance P.Enum DebugFlag where
instance O.HasParentTypes DebugFlag
instance B.Types.TypedObject DebugFlag where
instance B.Types.BoxedFlags DebugFlag
instance IsGFlag DebugFlag
data CellRendererState
instance P.Enum CellRendererState where
instance O.HasParentTypes CellRendererState
instance B.Types.TypedObject CellRendererState where
instance B.Types.BoxedFlags CellRendererState
instance IsGFlag CellRendererState
data CalendarDisplayOptions
instance P.Enum CalendarDisplayOptions where
instance O.HasParentTypes CalendarDisplayOptions
instance B.Types.TypedObject CalendarDisplayOptions where
instance B.Types.BoxedFlags CalendarDisplayOptions
instance IsGFlag CalendarDisplayOptions
data AttachOptions
instance P.Enum AttachOptions where
instance O.HasParentTypes AttachOptions
instance B.Types.TypedObject AttachOptions where
instance B.Types.BoxedFlags AttachOptions
instance IsGFlag AttachOptions
data ApplicationInhibitFlags
instance P.Enum ApplicationInhibitFlags where
instance O.HasParentTypes ApplicationInhibitFlags
instance B.Types.TypedObject ApplicationInhibitFlags where
instance B.Types.BoxedFlags ApplicationInhibitFlags
instance IsGFlag ApplicationInhibitFlags
data AccelFlags
instance P.Enum AccelFlags where
instance O.HasParentTypes AccelFlags
instance B.Types.TypedObject AccelFlags where
instance B.Types.BoxedFlags AccelFlags
instance IsGFlag AccelFlags
