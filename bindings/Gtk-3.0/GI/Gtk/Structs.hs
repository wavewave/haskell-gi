

-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Structs
    (     module GI.Gtk.Structs.AccelGroupEntry   ,
    module GI.Gtk.Structs.AccelKey          ,
    module GI.Gtk.Structs.ActionEntry       ,
    module GI.Gtk.Structs.BindingArg        ,
    module GI.Gtk.Structs.BindingEntry      ,
    module GI.Gtk.Structs.BindingSet        ,
    module GI.Gtk.Structs.BindingSignal     ,
    module GI.Gtk.Structs.Border            ,
    module GI.Gtk.Structs.ContainerClass    ,
    module GI.Gtk.Structs.CssSection        ,
    module GI.Gtk.Structs.FileFilterInfo    ,
    module GI.Gtk.Structs.FixedChild        ,
    module GI.Gtk.Structs.Gradient          ,
    module GI.Gtk.Structs.IMContextInfo     ,
    module GI.Gtk.Structs.IconSet           ,
    module GI.Gtk.Structs.IconSource        ,
    module GI.Gtk.Structs.LabelSelectionInfo,
    module GI.Gtk.Structs.MountOperationHandlerIface_,
    module GI.Gtk.Structs.MountOperationHandlerProxyClass_,
    module GI.Gtk.Structs.MountOperationHandlerProxy_,
    module GI.Gtk.Structs.MountOperationHandlerSkeletonClass_,
    module GI.Gtk.Structs.MountOperationHandlerSkeleton_,
    module GI.Gtk.Structs.MountOperationHandler_,
    module GI.Gtk.Structs.PadActionEntry    ,
    module GI.Gtk.Structs.PageRange         ,
    module GI.Gtk.Structs.PaperSize         ,
    module GI.Gtk.Structs.RadioActionEntry  ,
    module GI.Gtk.Structs.RcContext         ,
    module GI.Gtk.Structs.RcProperty        ,
    module GI.Gtk.Structs.RecentData        ,
    module GI.Gtk.Structs.RecentFilterInfo  ,
    module GI.Gtk.Structs.RecentInfo        ,
    module GI.Gtk.Structs.RequestedSize     ,
    module GI.Gtk.Structs.Requisition       ,
    module GI.Gtk.Structs.SelectionData     ,
    module GI.Gtk.Structs.SettingsValue     ,
    module GI.Gtk.Structs.StockItem         ,
    module GI.Gtk.Structs.SymbolicColor     ,
    module GI.Gtk.Structs.TableChild        ,
    module GI.Gtk.Structs.TableRowCol       ,
    module GI.Gtk.Structs.TargetEntry       ,
    module GI.Gtk.Structs.TargetList        ,
    module GI.Gtk.Structs.TargetPair        ,
    module GI.Gtk.Structs.TextAppearance    ,
    module GI.Gtk.Structs.TextAttributes    ,
    module GI.Gtk.Structs.TextBTree         ,
    module GI.Gtk.Structs.TextIter          ,
    module GI.Gtk.Structs.ThemeEngine       ,
    module GI.Gtk.Structs.ToggleActionEntry ,
    module GI.Gtk.Structs.TreeIter          ,
    module GI.Gtk.Structs.TreePath          ,
    module GI.Gtk.Structs.TreeRowReference  ,
    module GI.Gtk.Structs.WidgetClass       ,
    module GI.Gtk.Structs.WidgetPath        ,
    module GI.Gtk.Structs.WindowGeometryInfo,


    ) where

import GI.Gtk.Structs.AccelGroupEntry
import GI.Gtk.Structs.AccelKey
import GI.Gtk.Structs.ActionEntry
import GI.Gtk.Structs.BindingArg
import GI.Gtk.Structs.BindingEntry
import GI.Gtk.Structs.BindingSet
import GI.Gtk.Structs.BindingSignal
import GI.Gtk.Structs.Border
import GI.Gtk.Structs.ContainerClass
import GI.Gtk.Structs.CssSection
import GI.Gtk.Structs.FileFilterInfo
import GI.Gtk.Structs.FixedChild
import GI.Gtk.Structs.Gradient
import GI.Gtk.Structs.IMContextInfo
import GI.Gtk.Structs.IconSet
import GI.Gtk.Structs.IconSource
import GI.Gtk.Structs.LabelSelectionInfo
import GI.Gtk.Structs.MountOperationHandlerIface_
import GI.Gtk.Structs.MountOperationHandlerProxyClass_
import GI.Gtk.Structs.MountOperationHandlerProxy_
import GI.Gtk.Structs.MountOperationHandlerSkeletonClass_
import GI.Gtk.Structs.MountOperationHandlerSkeleton_
import GI.Gtk.Structs.MountOperationHandler_
import GI.Gtk.Structs.PadActionEntry
import GI.Gtk.Structs.PageRange
import GI.Gtk.Structs.PaperSize
import GI.Gtk.Structs.RadioActionEntry
import GI.Gtk.Structs.RcContext
import GI.Gtk.Structs.RcProperty
import GI.Gtk.Structs.RecentData
import GI.Gtk.Structs.RecentFilterInfo
import GI.Gtk.Structs.RecentInfo
import GI.Gtk.Structs.RequestedSize
import GI.Gtk.Structs.Requisition
import GI.Gtk.Structs.SelectionData
import GI.Gtk.Structs.SettingsValue
import GI.Gtk.Structs.StockItem
import GI.Gtk.Structs.SymbolicColor
import GI.Gtk.Structs.TableChild
import GI.Gtk.Structs.TableRowCol
import GI.Gtk.Structs.TargetEntry
import GI.Gtk.Structs.TargetList
import GI.Gtk.Structs.TargetPair
import GI.Gtk.Structs.TextAppearance
import GI.Gtk.Structs.TextAttributes
import GI.Gtk.Structs.TextBTree
import GI.Gtk.Structs.TextIter
import GI.Gtk.Structs.ThemeEngine
import GI.Gtk.Structs.ToggleActionEntry
import GI.Gtk.Structs.TreeIter
import GI.Gtk.Structs.TreePath
import GI.Gtk.Structs.TreeRowReference
import GI.Gtk.Structs.WidgetClass
import GI.Gtk.Structs.WidgetPath
import GI.Gtk.Structs.WindowGeometryInfo

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



