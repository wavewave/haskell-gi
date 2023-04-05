{-# LANGUAGE ImplicitParams, RankNTypes, TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- A t'GI.Gtk.Objects.UIManager.UIManager' constructs a user interface (menus and toolbars) from
-- one or more UI definitions, which reference actions from one or more
-- action groups.
-- 
-- > GtkUIManager is deprecated since GTK+ 3.10. To construct user interfaces
-- > from XML definitions, you should use t'GI.Gtk.Objects.Builder.Builder', t'GI.Gio.Objects.MenuModel.MenuModel', et al. To
-- > work with actions, use t'GI.Gio.Interfaces.Action.Action', t'GI.Gtk.Interfaces.Actionable.Actionable' et al. These newer classes
-- > support richer functionality and integration with various desktop shells.
-- > It should be possible to migrate most\/all functionality from GtkUIManager.
-- 
-- # UI Definitions # {@/XML/@-UI}
-- 
-- The UI definitions are specified in an XML format which can be
-- roughly described by the following DTD.
-- 
-- > Do not confuse the GtkUIManager UI Definitions described here with
-- > the similarly named [GtkBuilder UI Definitions][BUILDER-UI].
-- 
-- >
-- ><!ELEMENT ui          (menubar|toolbar|popup|accelerator)* >
-- ><!ELEMENT menubar     (menuitem|separator|placeholder|menu)* >
-- ><!ELEMENT menu        (menuitem|separator|placeholder|menu)* >
-- ><!ELEMENT popup       (menuitem|separator|placeholder|menu)* >
-- ><!ELEMENT toolbar     (toolitem|separator|placeholder)* >
-- ><!ELEMENT placeholder (menuitem|toolitem|separator|placeholder|menu)* >
-- ><!ELEMENT menuitem     EMPTY >
-- ><!ELEMENT toolitem     (menu?) >
-- ><!ELEMENT separator    EMPTY >
-- ><!ELEMENT accelerator  EMPTY >
-- ><!ATTLIST menubar      name                      #IMPLIED
-- >                       action                    #IMPLIED >
-- ><!ATTLIST toolbar      name                      #IMPLIED
-- >                       action                    #IMPLIED >
-- ><!ATTLIST popup        name                      #IMPLIED
-- >                       action                    #IMPLIED
-- >                       accelerators (true|false) #IMPLIED >
-- ><!ATTLIST placeholder  name                      #IMPLIED
-- >                       action                    #IMPLIED >
-- ><!ATTLIST separator    name                      #IMPLIED
-- >                       action                    #IMPLIED
-- >                       expand       (true|false) #IMPLIED >
-- ><!ATTLIST menu         name                      #IMPLIED
-- >                       action                    #REQUIRED
-- >                       position     (top|bot)    #IMPLIED >
-- ><!ATTLIST menuitem     name                      #IMPLIED
-- >                       action                    #REQUIRED
-- >                       position     (top|bot)    #IMPLIED
-- >                       always-show-image (true|false) #IMPLIED >
-- ><!ATTLIST toolitem     name                      #IMPLIED
-- >                       action                    #REQUIRED
-- >                       position     (top|bot)    #IMPLIED >
-- ><!ATTLIST accelerator  name                      #IMPLIED
-- >                       action                    #REQUIRED >
-- 
-- 
-- There are some additional restrictions beyond those specified in the
-- DTD, e.g. every toolitem must have a toolbar in its anchestry and
-- every menuitem must have a menubar or popup in its anchestry. Since
-- a t'GI.GLib.Structs.MarkupParser.MarkupParser' is used to parse the UI description, it must not only
-- be valid XML, but valid markup.
-- 
-- If a name is not specified, it defaults to the action. If an action is
-- not specified either, the element name is used. The name and action
-- attributes must not contain “\/” characters after parsing (since that
-- would mess up path lookup) and must be usable as XML attributes when
-- enclosed in doublequotes, thus they must not “\"” characters or references
-- to the &quot; entity.
-- 
-- = A UI definition 
-- 
-- 
-- === /xml code/
-- >
-- ><ui>
-- >  <menubar>
-- >    <menu name="FileMenu" action="FileMenuAction">
-- >      <menuitem name="New" action="New2Action" />
-- >      <placeholder name="FileMenuAdditions" />
-- >    </menu>
-- >    <menu name="JustifyMenu" action="JustifyMenuAction">
-- >      <menuitem name="Left" action="justify-left"/>
-- >      <menuitem name="Centre" action="justify-center"/>
-- >      <menuitem name="Right" action="justify-right"/>
-- >      <menuitem name="Fill" action="justify-fill"/>
-- >    </menu>
-- >  </menubar>
-- >  <toolbar action="toolbar1">
-- >    <placeholder name="JustifyToolItems">
-- >      <separator/>
-- >      <toolitem name="Left" action="justify-left"/>
-- >      <toolitem name="Centre" action="justify-center"/>
-- >      <toolitem name="Right" action="justify-right"/>
-- >      <toolitem name="Fill" action="justify-fill"/>
-- >      <separator/>
-- >    </placeholder>
-- >  </toolbar>
-- ></ui>
-- 
-- 
-- The constructed widget hierarchy is very similar to the element tree
-- of the XML, with the exception that placeholders are merged into their
-- parents. The correspondence of XML elements to widgets should be
-- almost obvious:
-- 
-- * menubar
-- 
-- 
--    a t'GI.Gtk.Objects.MenuBar.MenuBar'
-- 
-- * toolbar
-- 
-- 
--    a t'GI.Gtk.Objects.Toolbar.Toolbar'
-- 
-- * popup
-- 
-- 
--    a toplevel t'GI.Gtk.Objects.Menu.Menu'
-- 
-- * menu
-- 
-- 
--    a t'GI.Gtk.Objects.Menu.Menu' attached to a menuitem
-- 
-- * menuitem
-- 
-- 
--    a t'GI.Gtk.Objects.MenuItem.MenuItem' subclass, the exact type depends on the action
-- 
-- * toolitem
-- 
-- 
--    a t'GI.Gtk.Objects.ToolItem.ToolItem' subclass, the exact type depends on the
--    action. Note that toolitem elements may contain a menu element,
--    but only if their associated action specifies a
--    t'GI.Gtk.Objects.MenuToolButton.MenuToolButton' as proxy.
-- 
-- * separator
-- 
-- 
--    a t'GI.Gtk.Objects.SeparatorMenuItem.SeparatorMenuItem' or t'GI.Gtk.Objects.SeparatorToolItem.SeparatorToolItem'
-- 
-- * accelerator
-- 
-- 
--    a keyboard accelerator
-- 
-- The “position” attribute determines where a constructed widget is positioned
-- wrt. to its siblings in the partially constructed tree. If it is
-- “top”, the widget is prepended, otherwise it is appended.
-- 
-- # UI Merging # {@/UI/@-Merging}
-- 
-- The most remarkable feature of t'GI.Gtk.Objects.UIManager.UIManager' is that it can overlay a set
-- of menuitems and toolitems over another one, and demerge them later.
-- 
-- Merging is done based on the names of the XML elements. Each element is
-- identified by a path which consists of the names of its anchestors, separated
-- by slashes. For example, the menuitem named “Left” in the example above
-- has the path @\/ui\/menubar\/JustifyMenu\/Left@ and the
-- toolitem with the same name has path
-- @\/ui\/toolbar1\/JustifyToolItems\/Left@.
-- 
-- = Accelerators 
-- 
-- Every action has an accelerator path. Accelerators are installed together
-- with menuitem proxies, but they can also be explicitly added with
-- @\<accelerator>@ elements in the UI definition. This makes it possible to
-- have accelerators for actions even if they have no visible proxies.
-- 
-- # Smart Separators # {@/Smart/@-Separators}
-- 
-- The separators created by t'GI.Gtk.Objects.UIManager.UIManager' are “smart”, i.e. they do not show up
-- in the UI unless they end up between two visible menu or tool items. Separators
-- which are located at the very beginning or end of the menu or toolbar
-- containing them, or multiple separators next to each other, are hidden. This
-- is a useful feature, since the merging of UI elements from multiple sources
-- can make it hard or impossible to determine in advance whether a separator
-- will end up in such an unfortunate position.
-- 
-- For separators in toolbars, you can set @expand=\"true\"@ to
-- turn them from a small, visible separator to an expanding, invisible one.
-- Toolitems following an expanding separator are effectively right-aligned.
-- 
-- = Empty Menus
-- 
-- Submenus pose similar problems to separators inconnection with merging. It is
-- impossible to know in advance whether they will end up empty after merging.
-- t'GI.Gtk.Objects.UIManager.UIManager' offers two ways to treat empty submenus:
-- 
-- * make them disappear by hiding the menu item they’re attached to
-- * add an insensitive “Empty” item
-- 
-- 
-- The behaviour is chosen based on the “hide_if_empty” property of the action
-- to which the submenu is associated.
-- 
-- # GtkUIManager as GtkBuildable # {t'GI.Gtk.Objects.UIManager.UIManager'-BUILDER-UI}
-- 
-- The GtkUIManager implementation of the GtkBuildable interface accepts
-- GtkActionGroup objects as @\<child>@ elements in UI definitions.
-- 
-- A GtkUIManager UI definition as described above can be embedded in
-- an GtkUIManager @\<object>@ element in a GtkBuilder UI definition.
-- 
-- The widgets that are constructed by a GtkUIManager can be embedded in
-- other parts of the constructed user interface with the help of the
-- “constructor” attribute. See the example below.
-- 
-- == An embedded GtkUIManager UI definition
-- 
-- 
-- === /xml code/
-- >
-- ><object class="GtkUIManager" id="uiman">
-- >  <child>
-- >    <object class="GtkActionGroup" id="actiongroup">
-- >      <child>
-- >        <object class="GtkAction" id="file">
-- >          <property name="label">_File</property>
-- >        </object>
-- >      </child>
-- >    </object>
-- >  </child>
-- >  <ui>
-- >    <menubar name="menubar1">
-- >      <menu action="file">
-- >      </menu>
-- >    </menubar>
-- >  </ui>
-- ></object>
-- ><object class="GtkWindow" id="main-window">
-- >  <child>
-- >    <object class="GtkMenuBar" id="menubar1" constructor="uiman"/>
-- >  </child>
-- ></object>
-- 

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.UIManager
    ( 

-- * Exported types
    UIManager(..)                           ,
    IsUIManager                             ,
    toUIManager                             ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [addChild]("GI.Gtk.Interfaces.Buildable#g:method:addChild"), [addUi]("GI.Gtk.Objects.UIManager#g:method:addUi"), [addUiFromFile]("GI.Gtk.Objects.UIManager#g:method:addUiFromFile"), [addUiFromResource]("GI.Gtk.Objects.UIManager#g:method:addUiFromResource"), [addUiFromString]("GI.Gtk.Objects.UIManager#g:method:addUiFromString"), [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [constructChild]("GI.Gtk.Interfaces.Buildable#g:method:constructChild"), [customFinished]("GI.Gtk.Interfaces.Buildable#g:method:customFinished"), [customTagEnd]("GI.Gtk.Interfaces.Buildable#g:method:customTagEnd"), [customTagStart]("GI.Gtk.Interfaces.Buildable#g:method:customTagStart"), [ensureUpdate]("GI.Gtk.Objects.UIManager#g:method:ensureUpdate"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [insertActionGroup]("GI.Gtk.Objects.UIManager#g:method:insertActionGroup"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [newMergeId]("GI.Gtk.Objects.UIManager#g:method:newMergeId"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [parserFinished]("GI.Gtk.Interfaces.Buildable#g:method:parserFinished"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [removeActionGroup]("GI.Gtk.Objects.UIManager#g:method:removeActionGroup"), [removeUi]("GI.Gtk.Objects.UIManager#g:method:removeUi"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getAccelGroup]("GI.Gtk.Objects.UIManager#g:method:getAccelGroup"), [getAction]("GI.Gtk.Objects.UIManager#g:method:getAction"), [getActionGroups]("GI.Gtk.Objects.UIManager#g:method:getActionGroups"), [getAddTearoffs]("GI.Gtk.Objects.UIManager#g:method:getAddTearoffs"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getInternalChild]("GI.Gtk.Interfaces.Buildable#g:method:getInternalChild"), [getName]("GI.Gtk.Interfaces.Buildable#g:method:getName"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getToplevels]("GI.Gtk.Objects.UIManager#g:method:getToplevels"), [getUi]("GI.Gtk.Objects.UIManager#g:method:getUi"), [getWidget]("GI.Gtk.Objects.UIManager#g:method:getWidget").
-- 
-- ==== Setters
-- [setAddTearoffs]("GI.Gtk.Objects.UIManager#g:method:setAddTearoffs"), [setBuildableProperty]("GI.Gtk.Interfaces.Buildable#g:method:setBuildableProperty"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setName]("GI.Gtk.Interfaces.Buildable#g:method:setName"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty").

#if defined(ENABLE_OVERLOADING)
    ResolveUIManagerMethod                  ,
#endif

-- ** addUi #method:addUi#

#if defined(ENABLE_OVERLOADING)
    UIManagerAddUiMethodInfo                ,
#endif
    uIManagerAddUi                          ,


-- ** addUiFromFile #method:addUiFromFile#

#if defined(ENABLE_OVERLOADING)
    UIManagerAddUiFromFileMethodInfo        ,
#endif
    uIManagerAddUiFromFile                  ,


-- ** addUiFromResource #method:addUiFromResource#

#if defined(ENABLE_OVERLOADING)
    UIManagerAddUiFromResourceMethodInfo    ,
#endif
    uIManagerAddUiFromResource              ,


-- ** addUiFromString #method:addUiFromString#

#if defined(ENABLE_OVERLOADING)
    UIManagerAddUiFromStringMethodInfo      ,
#endif
    uIManagerAddUiFromString                ,


-- ** ensureUpdate #method:ensureUpdate#

#if defined(ENABLE_OVERLOADING)
    UIManagerEnsureUpdateMethodInfo         ,
#endif
    uIManagerEnsureUpdate                   ,


-- ** getAccelGroup #method:getAccelGroup#

#if defined(ENABLE_OVERLOADING)
    UIManagerGetAccelGroupMethodInfo        ,
#endif
    uIManagerGetAccelGroup                  ,


-- ** getAction #method:getAction#

#if defined(ENABLE_OVERLOADING)
    UIManagerGetActionMethodInfo            ,
#endif
    uIManagerGetAction                      ,


-- ** getActionGroups #method:getActionGroups#

#if defined(ENABLE_OVERLOADING)
    UIManagerGetActionGroupsMethodInfo      ,
#endif
    uIManagerGetActionGroups                ,


-- ** getAddTearoffs #method:getAddTearoffs#

#if defined(ENABLE_OVERLOADING)
    UIManagerGetAddTearoffsMethodInfo       ,
#endif
    uIManagerGetAddTearoffs                 ,


-- ** getToplevels #method:getToplevels#

#if defined(ENABLE_OVERLOADING)
    UIManagerGetToplevelsMethodInfo         ,
#endif
    uIManagerGetToplevels                   ,


-- ** getUi #method:getUi#

#if defined(ENABLE_OVERLOADING)
    UIManagerGetUiMethodInfo                ,
#endif
    uIManagerGetUi                          ,


-- ** getWidget #method:getWidget#

#if defined(ENABLE_OVERLOADING)
    UIManagerGetWidgetMethodInfo            ,
#endif
    uIManagerGetWidget                      ,


-- ** insertActionGroup #method:insertActionGroup#

#if defined(ENABLE_OVERLOADING)
    UIManagerInsertActionGroupMethodInfo    ,
#endif
    uIManagerInsertActionGroup              ,


-- ** new #method:new#

    uIManagerNew                            ,


-- ** newMergeId #method:newMergeId#

#if defined(ENABLE_OVERLOADING)
    UIManagerNewMergeIdMethodInfo           ,
#endif
    uIManagerNewMergeId                     ,


-- ** removeActionGroup #method:removeActionGroup#

#if defined(ENABLE_OVERLOADING)
    UIManagerRemoveActionGroupMethodInfo    ,
#endif
    uIManagerRemoveActionGroup              ,


-- ** removeUi #method:removeUi#

#if defined(ENABLE_OVERLOADING)
    UIManagerRemoveUiMethodInfo             ,
#endif
    uIManagerRemoveUi                       ,


-- ** setAddTearoffs #method:setAddTearoffs#

#if defined(ENABLE_OVERLOADING)
    UIManagerSetAddTearoffsMethodInfo       ,
#endif
    uIManagerSetAddTearoffs                 ,




 -- * Properties


-- ** addTearoffs #attr:addTearoffs#
-- | The \"add-tearoffs\" property controls whether generated menus
-- have tearoff menu items.
-- 
-- Note that this only affects regular menus. Generated popup
-- menus never have tearoff menu items.
-- 
-- /Since: 2.4/

#if defined(ENABLE_OVERLOADING)
    UIManagerAddTearoffsPropertyInfo        ,
#endif
    constructUIManagerAddTearoffs           ,
    getUIManagerAddTearoffs                 ,
    setUIManagerAddTearoffs                 ,
#if defined(ENABLE_OVERLOADING)
    uIManagerAddTearoffs                    ,
#endif


-- ** ui #attr:ui#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    UIManagerUiPropertyInfo                 ,
#endif
    getUIManagerUi                          ,
#if defined(ENABLE_OVERLOADING)
    uIManagerUi                             ,
#endif




 -- * Signals


-- ** actionsChanged #signal:actionsChanged#

    UIManagerActionsChangedCallback         ,
#if defined(ENABLE_OVERLOADING)
    UIManagerActionsChangedSignalInfo       ,
#endif
    afterUIManagerActionsChanged            ,
    onUIManagerActionsChanged               ,


-- ** addWidget #signal:addWidget#

    UIManagerAddWidgetCallback              ,
#if defined(ENABLE_OVERLOADING)
    UIManagerAddWidgetSignalInfo            ,
#endif
    afterUIManagerAddWidget                 ,
    onUIManagerAddWidget                    ,


-- ** connectProxy #signal:connectProxy#

    UIManagerConnectProxyCallback           ,
#if defined(ENABLE_OVERLOADING)
    UIManagerConnectProxySignalInfo         ,
#endif
    afterUIManagerConnectProxy              ,
    onUIManagerConnectProxy                 ,


-- ** disconnectProxy #signal:disconnectProxy#

    UIManagerDisconnectProxyCallback        ,
#if defined(ENABLE_OVERLOADING)
    UIManagerDisconnectProxySignalInfo      ,
#endif
    afterUIManagerDisconnectProxy           ,
    onUIManagerDisconnectProxy              ,


-- ** postActivate #signal:postActivate#

    UIManagerPostActivateCallback           ,
#if defined(ENABLE_OVERLOADING)
    UIManagerPostActivateSignalInfo         ,
#endif
    afterUIManagerPostActivate              ,
    onUIManagerPostActivate                 ,


-- ** preActivate #signal:preActivate#

    UIManagerPreActivateCallback            ,
#if defined(ENABLE_OVERLOADING)
    UIManagerPreActivateSignalInfo          ,
#endif
    afterUIManagerPreActivate               ,
    onUIManagerPreActivate                  ,




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
import {-# SOURCE #-} qualified GI.Gtk.Flags as Gtk.Flags
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.Buildable as Gtk.Buildable
import {-# SOURCE #-} qualified GI.Gtk.Objects.AccelGroup as Gtk.AccelGroup
import {-# SOURCE #-} qualified GI.Gtk.Objects.Action as Gtk.Action
import {-# SOURCE #-} qualified GI.Gtk.Objects.ActionGroup as Gtk.ActionGroup
import {-# SOURCE #-} qualified GI.Gtk.Objects.Widget as Gtk.Widget

-- | Memory-managed wrapper type.
newtype UIManager = UIManager (SP.ManagedPtr UIManager)
    deriving (Eq)

instance SP.ManagedPtrNewtype UIManager where
    toManagedPtr (UIManager p) = p

foreign import ccall "gtk_ui_manager_get_type"
    c_gtk_ui_manager_get_type :: IO B.Types.GType

instance B.Types.TypedObject UIManager where
    glibType = c_gtk_ui_manager_get_type

instance B.Types.GObject UIManager

-- | Type class for types which can be safely cast to `UIManager`, for instance with `toUIManager`.
class (SP.GObject o, O.IsDescendantOf UIManager o) => IsUIManager o
instance (SP.GObject o, O.IsDescendantOf UIManager o) => IsUIManager o

instance O.HasParentTypes UIManager
type instance O.ParentTypes UIManager = '[GObject.Object.Object, Gtk.Buildable.Buildable]

-- | Cast to `UIManager`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toUIManager :: (MIO.MonadIO m, IsUIManager o) => o -> m UIManager
toUIManager = MIO.liftIO . B.ManagedPtr.unsafeCastTo UIManager

-- | Convert 'UIManager' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe UIManager) where
    gvalueGType_ = c_gtk_ui_manager_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr UIManager)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr UIManager)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject UIManager ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveUIManagerMethod (t :: Symbol) (o :: *) :: * where
    ResolveUIManagerMethod "addChild" o = Gtk.Buildable.BuildableAddChildMethodInfo
    ResolveUIManagerMethod "addUi" o = UIManagerAddUiMethodInfo
    ResolveUIManagerMethod "addUiFromFile" o = UIManagerAddUiFromFileMethodInfo
    ResolveUIManagerMethod "addUiFromResource" o = UIManagerAddUiFromResourceMethodInfo
    ResolveUIManagerMethod "addUiFromString" o = UIManagerAddUiFromStringMethodInfo
    ResolveUIManagerMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveUIManagerMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveUIManagerMethod "constructChild" o = Gtk.Buildable.BuildableConstructChildMethodInfo
    ResolveUIManagerMethod "customFinished" o = Gtk.Buildable.BuildableCustomFinishedMethodInfo
    ResolveUIManagerMethod "customTagEnd" o = Gtk.Buildable.BuildableCustomTagEndMethodInfo
    ResolveUIManagerMethod "customTagStart" o = Gtk.Buildable.BuildableCustomTagStartMethodInfo
    ResolveUIManagerMethod "ensureUpdate" o = UIManagerEnsureUpdateMethodInfo
    ResolveUIManagerMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveUIManagerMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveUIManagerMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveUIManagerMethod "insertActionGroup" o = UIManagerInsertActionGroupMethodInfo
    ResolveUIManagerMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveUIManagerMethod "newMergeId" o = UIManagerNewMergeIdMethodInfo
    ResolveUIManagerMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveUIManagerMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveUIManagerMethod "parserFinished" o = Gtk.Buildable.BuildableParserFinishedMethodInfo
    ResolveUIManagerMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveUIManagerMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveUIManagerMethod "removeActionGroup" o = UIManagerRemoveActionGroupMethodInfo
    ResolveUIManagerMethod "removeUi" o = UIManagerRemoveUiMethodInfo
    ResolveUIManagerMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveUIManagerMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveUIManagerMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveUIManagerMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveUIManagerMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveUIManagerMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveUIManagerMethod "getAccelGroup" o = UIManagerGetAccelGroupMethodInfo
    ResolveUIManagerMethod "getAction" o = UIManagerGetActionMethodInfo
    ResolveUIManagerMethod "getActionGroups" o = UIManagerGetActionGroupsMethodInfo
    ResolveUIManagerMethod "getAddTearoffs" o = UIManagerGetAddTearoffsMethodInfo
    ResolveUIManagerMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveUIManagerMethod "getInternalChild" o = Gtk.Buildable.BuildableGetInternalChildMethodInfo
    ResolveUIManagerMethod "getName" o = Gtk.Buildable.BuildableGetNameMethodInfo
    ResolveUIManagerMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveUIManagerMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveUIManagerMethod "getToplevels" o = UIManagerGetToplevelsMethodInfo
    ResolveUIManagerMethod "getUi" o = UIManagerGetUiMethodInfo
    ResolveUIManagerMethod "getWidget" o = UIManagerGetWidgetMethodInfo
    ResolveUIManagerMethod "setAddTearoffs" o = UIManagerSetAddTearoffsMethodInfo
    ResolveUIManagerMethod "setBuildableProperty" o = Gtk.Buildable.BuildableSetBuildablePropertyMethodInfo
    ResolveUIManagerMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveUIManagerMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveUIManagerMethod "setName" o = Gtk.Buildable.BuildableSetNameMethodInfo
    ResolveUIManagerMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveUIManagerMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveUIManagerMethod t UIManager, O.OverloadedMethod info UIManager p) => OL.IsLabel t (UIManager -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveUIManagerMethod t UIManager, O.OverloadedMethod info UIManager p, R.HasField t UIManager p) => R.HasField t UIManager p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveUIManagerMethod t UIManager, O.OverloadedMethodInfo info UIManager) => OL.IsLabel t (O.MethodProxy info UIManager) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- signal UIManager::actions-changed
{-# DEPRECATED UIManagerActionsChangedCallback ["(Since version 3.10)"] #-}
-- | The [actionsChanged](#g:signal:actionsChanged) signal is emitted whenever the set of actions
-- changes.
-- 
-- /Since: 2.4/
type UIManagerActionsChangedCallback =
    IO ()

type C_UIManagerActionsChangedCallback =
    Ptr UIManager ->                        -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_UIManagerActionsChangedCallback`.
foreign import ccall "wrapper"
    mk_UIManagerActionsChangedCallback :: C_UIManagerActionsChangedCallback -> IO (FunPtr C_UIManagerActionsChangedCallback)

wrap_UIManagerActionsChangedCallback :: 
    GObject a => (a -> UIManagerActionsChangedCallback) ->
    C_UIManagerActionsChangedCallback
wrap_UIManagerActionsChangedCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [actionsChanged](#signal:actionsChanged) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' uIManager #actionsChanged callback
-- @
-- 
-- 
onUIManagerActionsChanged :: (IsUIManager a, MonadIO m) => a -> ((?self :: a) => UIManagerActionsChangedCallback) -> m SignalHandlerId
onUIManagerActionsChanged obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_UIManagerActionsChangedCallback wrapped
    wrapped'' <- mk_UIManagerActionsChangedCallback wrapped'
    connectSignalFunPtr obj "actions-changed" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [actionsChanged](#signal:actionsChanged) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' uIManager #actionsChanged callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterUIManagerActionsChanged :: (IsUIManager a, MonadIO m) => a -> ((?self :: a) => UIManagerActionsChangedCallback) -> m SignalHandlerId
afterUIManagerActionsChanged obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_UIManagerActionsChangedCallback wrapped
    wrapped'' <- mk_UIManagerActionsChangedCallback wrapped'
    connectSignalFunPtr obj "actions-changed" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data UIManagerActionsChangedSignalInfo
instance SignalInfo UIManagerActionsChangedSignalInfo where
    type HaskellCallbackType UIManagerActionsChangedSignalInfo = UIManagerActionsChangedCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_UIManagerActionsChangedCallback cb
        cb'' <- mk_UIManagerActionsChangedCallback cb'
        connectSignalFunPtr obj "actions-changed" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.UIManager::actions-changed"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-UIManager.html#g:signal:actionsChanged"})

#endif

-- signal UIManager::add-widget
{-# DEPRECATED UIManagerAddWidgetCallback ["(Since version 3.10)"] #-}
-- | The [addWidget](#g:signal:addWidget) signal is emitted for each generated menubar and toolbar.
-- It is not emitted for generated popup menus, which can be obtained by
-- 'GI.Gtk.Objects.UIManager.uIManagerGetWidget'.
-- 
-- /Since: 2.4/
type UIManagerAddWidgetCallback =
    Gtk.Widget.Widget
    -- ^ /@widget@/: the added widget
    -> IO ()

type C_UIManagerAddWidgetCallback =
    Ptr UIManager ->                        -- object
    Ptr Gtk.Widget.Widget ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_UIManagerAddWidgetCallback`.
foreign import ccall "wrapper"
    mk_UIManagerAddWidgetCallback :: C_UIManagerAddWidgetCallback -> IO (FunPtr C_UIManagerAddWidgetCallback)

wrap_UIManagerAddWidgetCallback :: 
    GObject a => (a -> UIManagerAddWidgetCallback) ->
    C_UIManagerAddWidgetCallback
wrap_UIManagerAddWidgetCallback gi'cb gi'selfPtr widget _ = do
    widget' <- (newObject Gtk.Widget.Widget) widget
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  widget'


-- | Connect a signal handler for the [addWidget](#signal:addWidget) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' uIManager #addWidget callback
-- @
-- 
-- 
onUIManagerAddWidget :: (IsUIManager a, MonadIO m) => a -> ((?self :: a) => UIManagerAddWidgetCallback) -> m SignalHandlerId
onUIManagerAddWidget obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_UIManagerAddWidgetCallback wrapped
    wrapped'' <- mk_UIManagerAddWidgetCallback wrapped'
    connectSignalFunPtr obj "add-widget" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [addWidget](#signal:addWidget) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' uIManager #addWidget callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterUIManagerAddWidget :: (IsUIManager a, MonadIO m) => a -> ((?self :: a) => UIManagerAddWidgetCallback) -> m SignalHandlerId
afterUIManagerAddWidget obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_UIManagerAddWidgetCallback wrapped
    wrapped'' <- mk_UIManagerAddWidgetCallback wrapped'
    connectSignalFunPtr obj "add-widget" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data UIManagerAddWidgetSignalInfo
instance SignalInfo UIManagerAddWidgetSignalInfo where
    type HaskellCallbackType UIManagerAddWidgetSignalInfo = UIManagerAddWidgetCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_UIManagerAddWidgetCallback cb
        cb'' <- mk_UIManagerAddWidgetCallback cb'
        connectSignalFunPtr obj "add-widget" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.UIManager::add-widget"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-UIManager.html#g:signal:addWidget"})

#endif

-- signal UIManager::connect-proxy
{-# DEPRECATED UIManagerConnectProxyCallback ["(Since version 3.10)"] #-}
-- | The [connectProxy](#g:signal:connectProxy) signal is emitted after connecting a proxy to
-- an action in the group.
-- 
-- This is intended for simple customizations for which a custom action
-- class would be too clumsy, e.g. showing tooltips for menuitems in the
-- statusbar.
-- 
-- /Since: 2.4/
type UIManagerConnectProxyCallback =
    Gtk.Action.Action
    -- ^ /@action@/: the action
    -> Gtk.Widget.Widget
    -- ^ /@proxy@/: the proxy
    -> IO ()

type C_UIManagerConnectProxyCallback =
    Ptr UIManager ->                        -- object
    Ptr Gtk.Action.Action ->
    Ptr Gtk.Widget.Widget ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_UIManagerConnectProxyCallback`.
foreign import ccall "wrapper"
    mk_UIManagerConnectProxyCallback :: C_UIManagerConnectProxyCallback -> IO (FunPtr C_UIManagerConnectProxyCallback)

wrap_UIManagerConnectProxyCallback :: 
    GObject a => (a -> UIManagerConnectProxyCallback) ->
    C_UIManagerConnectProxyCallback
wrap_UIManagerConnectProxyCallback gi'cb gi'selfPtr action proxy _ = do
    action' <- (newObject Gtk.Action.Action) action
    proxy' <- (newObject Gtk.Widget.Widget) proxy
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  action' proxy'


-- | Connect a signal handler for the [connectProxy](#signal:connectProxy) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' uIManager #connectProxy callback
-- @
-- 
-- 
onUIManagerConnectProxy :: (IsUIManager a, MonadIO m) => a -> ((?self :: a) => UIManagerConnectProxyCallback) -> m SignalHandlerId
onUIManagerConnectProxy obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_UIManagerConnectProxyCallback wrapped
    wrapped'' <- mk_UIManagerConnectProxyCallback wrapped'
    connectSignalFunPtr obj "connect-proxy" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [connectProxy](#signal:connectProxy) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' uIManager #connectProxy callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterUIManagerConnectProxy :: (IsUIManager a, MonadIO m) => a -> ((?self :: a) => UIManagerConnectProxyCallback) -> m SignalHandlerId
afterUIManagerConnectProxy obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_UIManagerConnectProxyCallback wrapped
    wrapped'' <- mk_UIManagerConnectProxyCallback wrapped'
    connectSignalFunPtr obj "connect-proxy" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data UIManagerConnectProxySignalInfo
instance SignalInfo UIManagerConnectProxySignalInfo where
    type HaskellCallbackType UIManagerConnectProxySignalInfo = UIManagerConnectProxyCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_UIManagerConnectProxyCallback cb
        cb'' <- mk_UIManagerConnectProxyCallback cb'
        connectSignalFunPtr obj "connect-proxy" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.UIManager::connect-proxy"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-UIManager.html#g:signal:connectProxy"})

#endif

-- signal UIManager::disconnect-proxy
{-# DEPRECATED UIManagerDisconnectProxyCallback ["(Since version 3.10)"] #-}
-- | The [disconnectProxy](#g:signal:disconnectProxy) signal is emitted after disconnecting a proxy
-- from an action in the group.
-- 
-- /Since: 2.4/
type UIManagerDisconnectProxyCallback =
    Gtk.Action.Action
    -- ^ /@action@/: the action
    -> Gtk.Widget.Widget
    -- ^ /@proxy@/: the proxy
    -> IO ()

type C_UIManagerDisconnectProxyCallback =
    Ptr UIManager ->                        -- object
    Ptr Gtk.Action.Action ->
    Ptr Gtk.Widget.Widget ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_UIManagerDisconnectProxyCallback`.
foreign import ccall "wrapper"
    mk_UIManagerDisconnectProxyCallback :: C_UIManagerDisconnectProxyCallback -> IO (FunPtr C_UIManagerDisconnectProxyCallback)

wrap_UIManagerDisconnectProxyCallback :: 
    GObject a => (a -> UIManagerDisconnectProxyCallback) ->
    C_UIManagerDisconnectProxyCallback
wrap_UIManagerDisconnectProxyCallback gi'cb gi'selfPtr action proxy _ = do
    action' <- (newObject Gtk.Action.Action) action
    proxy' <- (newObject Gtk.Widget.Widget) proxy
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  action' proxy'


-- | Connect a signal handler for the [disconnectProxy](#signal:disconnectProxy) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' uIManager #disconnectProxy callback
-- @
-- 
-- 
onUIManagerDisconnectProxy :: (IsUIManager a, MonadIO m) => a -> ((?self :: a) => UIManagerDisconnectProxyCallback) -> m SignalHandlerId
onUIManagerDisconnectProxy obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_UIManagerDisconnectProxyCallback wrapped
    wrapped'' <- mk_UIManagerDisconnectProxyCallback wrapped'
    connectSignalFunPtr obj "disconnect-proxy" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [disconnectProxy](#signal:disconnectProxy) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' uIManager #disconnectProxy callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterUIManagerDisconnectProxy :: (IsUIManager a, MonadIO m) => a -> ((?self :: a) => UIManagerDisconnectProxyCallback) -> m SignalHandlerId
afterUIManagerDisconnectProxy obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_UIManagerDisconnectProxyCallback wrapped
    wrapped'' <- mk_UIManagerDisconnectProxyCallback wrapped'
    connectSignalFunPtr obj "disconnect-proxy" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data UIManagerDisconnectProxySignalInfo
instance SignalInfo UIManagerDisconnectProxySignalInfo where
    type HaskellCallbackType UIManagerDisconnectProxySignalInfo = UIManagerDisconnectProxyCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_UIManagerDisconnectProxyCallback cb
        cb'' <- mk_UIManagerDisconnectProxyCallback cb'
        connectSignalFunPtr obj "disconnect-proxy" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.UIManager::disconnect-proxy"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-UIManager.html#g:signal:disconnectProxy"})

#endif

-- signal UIManager::post-activate
{-# DEPRECATED UIManagerPostActivateCallback ["(Since version 3.10)"] #-}
-- | The [postActivate](#g:signal:postActivate) signal is emitted just after the /@action@/
-- is activated.
-- 
-- This is intended for applications to get notification
-- just after any action is activated.
-- 
-- /Since: 2.4/
type UIManagerPostActivateCallback =
    Gtk.Action.Action
    -- ^ /@action@/: the action
    -> IO ()

type C_UIManagerPostActivateCallback =
    Ptr UIManager ->                        -- object
    Ptr Gtk.Action.Action ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_UIManagerPostActivateCallback`.
foreign import ccall "wrapper"
    mk_UIManagerPostActivateCallback :: C_UIManagerPostActivateCallback -> IO (FunPtr C_UIManagerPostActivateCallback)

wrap_UIManagerPostActivateCallback :: 
    GObject a => (a -> UIManagerPostActivateCallback) ->
    C_UIManagerPostActivateCallback
wrap_UIManagerPostActivateCallback gi'cb gi'selfPtr action _ = do
    action' <- (newObject Gtk.Action.Action) action
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  action'


-- | Connect a signal handler for the [postActivate](#signal:postActivate) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' uIManager #postActivate callback
-- @
-- 
-- 
onUIManagerPostActivate :: (IsUIManager a, MonadIO m) => a -> ((?self :: a) => UIManagerPostActivateCallback) -> m SignalHandlerId
onUIManagerPostActivate obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_UIManagerPostActivateCallback wrapped
    wrapped'' <- mk_UIManagerPostActivateCallback wrapped'
    connectSignalFunPtr obj "post-activate" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [postActivate](#signal:postActivate) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' uIManager #postActivate callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterUIManagerPostActivate :: (IsUIManager a, MonadIO m) => a -> ((?self :: a) => UIManagerPostActivateCallback) -> m SignalHandlerId
afterUIManagerPostActivate obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_UIManagerPostActivateCallback wrapped
    wrapped'' <- mk_UIManagerPostActivateCallback wrapped'
    connectSignalFunPtr obj "post-activate" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data UIManagerPostActivateSignalInfo
instance SignalInfo UIManagerPostActivateSignalInfo where
    type HaskellCallbackType UIManagerPostActivateSignalInfo = UIManagerPostActivateCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_UIManagerPostActivateCallback cb
        cb'' <- mk_UIManagerPostActivateCallback cb'
        connectSignalFunPtr obj "post-activate" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.UIManager::post-activate"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-UIManager.html#g:signal:postActivate"})

#endif

-- signal UIManager::pre-activate
{-# DEPRECATED UIManagerPreActivateCallback ["(Since version 3.10)"] #-}
-- | The [preActivate](#g:signal:preActivate) signal is emitted just before the /@action@/
-- is activated.
-- 
-- This is intended for applications to get notification
-- just before any action is activated.
-- 
-- /Since: 2.4/
type UIManagerPreActivateCallback =
    Gtk.Action.Action
    -- ^ /@action@/: the action
    -> IO ()

type C_UIManagerPreActivateCallback =
    Ptr UIManager ->                        -- object
    Ptr Gtk.Action.Action ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_UIManagerPreActivateCallback`.
foreign import ccall "wrapper"
    mk_UIManagerPreActivateCallback :: C_UIManagerPreActivateCallback -> IO (FunPtr C_UIManagerPreActivateCallback)

wrap_UIManagerPreActivateCallback :: 
    GObject a => (a -> UIManagerPreActivateCallback) ->
    C_UIManagerPreActivateCallback
wrap_UIManagerPreActivateCallback gi'cb gi'selfPtr action _ = do
    action' <- (newObject Gtk.Action.Action) action
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  action'


-- | Connect a signal handler for the [preActivate](#signal:preActivate) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' uIManager #preActivate callback
-- @
-- 
-- 
onUIManagerPreActivate :: (IsUIManager a, MonadIO m) => a -> ((?self :: a) => UIManagerPreActivateCallback) -> m SignalHandlerId
onUIManagerPreActivate obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_UIManagerPreActivateCallback wrapped
    wrapped'' <- mk_UIManagerPreActivateCallback wrapped'
    connectSignalFunPtr obj "pre-activate" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [preActivate](#signal:preActivate) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' uIManager #preActivate callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterUIManagerPreActivate :: (IsUIManager a, MonadIO m) => a -> ((?self :: a) => UIManagerPreActivateCallback) -> m SignalHandlerId
afterUIManagerPreActivate obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_UIManagerPreActivateCallback wrapped
    wrapped'' <- mk_UIManagerPreActivateCallback wrapped'
    connectSignalFunPtr obj "pre-activate" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data UIManagerPreActivateSignalInfo
instance SignalInfo UIManagerPreActivateSignalInfo where
    type HaskellCallbackType UIManagerPreActivateSignalInfo = UIManagerPreActivateCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_UIManagerPreActivateCallback cb
        cb'' <- mk_UIManagerPreActivateCallback cb'
        connectSignalFunPtr obj "pre-activate" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.UIManager::pre-activate"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-UIManager.html#g:signal:preActivate"})

#endif

-- VVV Prop "add-tearoffs"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@add-tearoffs@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' uIManager #addTearoffs
-- @
getUIManagerAddTearoffs :: (MonadIO m, IsUIManager o) => o -> m Bool
getUIManagerAddTearoffs obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "add-tearoffs"

-- | Set the value of the “@add-tearoffs@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' uIManager [ #addTearoffs 'Data.GI.Base.Attributes.:=' value ]
-- @
setUIManagerAddTearoffs :: (MonadIO m, IsUIManager o) => o -> Bool -> m ()
setUIManagerAddTearoffs obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "add-tearoffs" val

-- | Construct a `GValueConstruct` with valid value for the “@add-tearoffs@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructUIManagerAddTearoffs :: (IsUIManager o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructUIManagerAddTearoffs val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "add-tearoffs" val

#if defined(ENABLE_OVERLOADING)
data UIManagerAddTearoffsPropertyInfo
instance AttrInfo UIManagerAddTearoffsPropertyInfo where
    type AttrAllowedOps UIManagerAddTearoffsPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint UIManagerAddTearoffsPropertyInfo = IsUIManager
    type AttrSetTypeConstraint UIManagerAddTearoffsPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint UIManagerAddTearoffsPropertyInfo = (~) Bool
    type AttrTransferType UIManagerAddTearoffsPropertyInfo = Bool
    type AttrGetType UIManagerAddTearoffsPropertyInfo = Bool
    type AttrLabel UIManagerAddTearoffsPropertyInfo = "add-tearoffs"
    type AttrOrigin UIManagerAddTearoffsPropertyInfo = UIManager
    attrGet = getUIManagerAddTearoffs
    attrSet = setUIManagerAddTearoffs
    attrTransfer _ v = do
        return v
    attrConstruct = constructUIManagerAddTearoffs
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.UIManager.addTearoffs"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-UIManager.html#g:attr:addTearoffs"
        })
#endif

-- VVV Prop "ui"
   -- Type: TBasicType TUTF8
   -- Flags: [PropertyReadable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@ui@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' uIManager #ui
-- @
getUIManagerUi :: (MonadIO m, IsUIManager o) => o -> m (Maybe T.Text)
getUIManagerUi obj = MIO.liftIO $ B.Properties.getObjectPropertyString obj "ui"

#if defined(ENABLE_OVERLOADING)
data UIManagerUiPropertyInfo
instance AttrInfo UIManagerUiPropertyInfo where
    type AttrAllowedOps UIManagerUiPropertyInfo = '[ 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint UIManagerUiPropertyInfo = IsUIManager
    type AttrSetTypeConstraint UIManagerUiPropertyInfo = (~) ()
    type AttrTransferTypeConstraint UIManagerUiPropertyInfo = (~) ()
    type AttrTransferType UIManagerUiPropertyInfo = ()
    type AttrGetType UIManagerUiPropertyInfo = (Maybe T.Text)
    type AttrLabel UIManagerUiPropertyInfo = "ui"
    type AttrOrigin UIManagerUiPropertyInfo = UIManager
    attrGet = getUIManagerUi
    attrSet = undefined
    attrTransfer _ = undefined
    attrConstruct = undefined
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.UIManager.ui"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-UIManager.html#g:attr:ui"
        })
#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList UIManager
type instance O.AttributeList UIManager = UIManagerAttributeList
type UIManagerAttributeList = ('[ '("addTearoffs", UIManagerAddTearoffsPropertyInfo), '("ui", UIManagerUiPropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
uIManagerAddTearoffs :: AttrLabelProxy "addTearoffs"
uIManagerAddTearoffs = AttrLabelProxy

uIManagerUi :: AttrLabelProxy "ui"
uIManagerUi = AttrLabelProxy

#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList UIManager = UIManagerSignalList
type UIManagerSignalList = ('[ '("actionsChanged", UIManagerActionsChangedSignalInfo), '("addWidget", UIManagerAddWidgetSignalInfo), '("connectProxy", UIManagerConnectProxySignalInfo), '("disconnectProxy", UIManagerDisconnectProxySignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo), '("postActivate", UIManagerPostActivateSignalInfo), '("preActivate", UIManagerPreActivateSignalInfo)] :: [(Symbol, *)])

#endif

-- method UIManager::new
-- method type : Constructor
-- Args: []
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "UIManager" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_ui_manager_new" gtk_ui_manager_new :: 
    IO (Ptr UIManager)

{-# DEPRECATED uIManagerNew ["(Since version 3.10)"] #-}
-- | Creates a new ui manager object.
-- 
-- /Since: 2.4/
uIManagerNew ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m UIManager
    -- ^ __Returns:__ a new ui manager object.
uIManagerNew  = liftIO $ do
    result <- gtk_ui_manager_new
    checkUnexpectedReturnNULL "uIManagerNew" result
    result' <- (wrapObject UIManager) result
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method UIManager::add_ui
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "manager"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "UIManager" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkUIManager" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "merge_id"
--           , argType = TBasicType TUInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the merge id for the merged UI, see gtk_ui_manager_new_merge_id()"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "path"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a path" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "the name for the added UI element"
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
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the name of the action to be proxied, or %NULL to add a separator"
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
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "UIManagerItemType" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the type of UI element to add."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "top"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "if %TRUE, the UI element is added before its siblings, otherwise it\n  is added after its siblings."
--                 , sinceVersion = Nothing
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

foreign import ccall "gtk_ui_manager_add_ui" gtk_ui_manager_add_ui :: 
    Ptr UIManager ->                        -- manager : TInterface (Name {namespace = "Gtk", name = "UIManager"})
    Word32 ->                               -- merge_id : TBasicType TUInt
    CString ->                              -- path : TBasicType TUTF8
    CString ->                              -- name : TBasicType TUTF8
    CString ->                              -- action : TBasicType TUTF8
    CUInt ->                                -- type : TInterface (Name {namespace = "Gtk", name = "UIManagerItemType"})
    CInt ->                                 -- top : TBasicType TBoolean
    IO ()

{-# DEPRECATED uIManagerAddUi ["(Since version 3.10)"] #-}
-- | Adds a UI element to the current contents of /@manager@/.
-- 
-- If /@type@/ is 'GI.Gtk.Flags.UIManagerItemTypeAuto', GTK+ inserts a menuitem, toolitem or
-- separator if such an element can be inserted at the place determined by
-- /@path@/. Otherwise /@type@/ must indicate an element that can be inserted at
-- the place determined by /@path@/.
-- 
-- If /@path@/ points to a menuitem or toolitem, the new element will be inserted
-- before or after this item, depending on /@top@/.
-- 
-- /Since: 2.4/
uIManagerAddUi ::
    (B.CallStack.HasCallStack, MonadIO m, IsUIManager a) =>
    a
    -- ^ /@manager@/: a t'GI.Gtk.Objects.UIManager.UIManager'
    -> Word32
    -- ^ /@mergeId@/: the merge id for the merged UI, see 'GI.Gtk.Objects.UIManager.uIManagerNewMergeId'
    -> T.Text
    -- ^ /@path@/: a path
    -> T.Text
    -- ^ /@name@/: the name for the added UI element
    -> Maybe (T.Text)
    -- ^ /@action@/: the name of the action to be proxied, or 'P.Nothing' to add a separator
    -> [Gtk.Flags.UIManagerItemType]
    -- ^ /@type@/: the type of UI element to add.
    -> Bool
    -- ^ /@top@/: if 'P.True', the UI element is added before its siblings, otherwise it
    --   is added after its siblings.
    -> m ()
uIManagerAddUi manager mergeId path name action type_ top = liftIO $ do
    manager' <- unsafeManagedPtrCastPtr manager
    path' <- textToCString path
    name' <- textToCString name
    maybeAction <- case action of
        Nothing -> return nullPtr
        Just jAction -> do
            jAction' <- textToCString jAction
            return jAction'
    let type_' = gflagsToWord type_
    let top' = (fromIntegral . fromEnum) top
    gtk_ui_manager_add_ui manager' mergeId path' name' maybeAction type_' top'
    touchManagedPtr manager
    freeMem path'
    freeMem name'
    freeMem maybeAction
    return ()

#if defined(ENABLE_OVERLOADING)
data UIManagerAddUiMethodInfo
instance (signature ~ (Word32 -> T.Text -> T.Text -> Maybe (T.Text) -> [Gtk.Flags.UIManagerItemType] -> Bool -> m ()), MonadIO m, IsUIManager a) => O.OverloadedMethod UIManagerAddUiMethodInfo a signature where
    overloadedMethod = uIManagerAddUi

instance O.OverloadedMethodInfo UIManagerAddUiMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.UIManager.uIManagerAddUi",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-UIManager.html#v:uIManagerAddUi"
        })


#endif

-- method UIManager::add_ui_from_file
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "manager"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "UIManager" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkUIManager object"
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
--                 { rawDocText = Just "the name of the file to parse"
--                 , sinceVersion = Nothing
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
-- throws : True
-- Skip return : False

foreign import ccall "gtk_ui_manager_add_ui_from_file" gtk_ui_manager_add_ui_from_file :: 
    Ptr UIManager ->                        -- manager : TInterface (Name {namespace = "Gtk", name = "UIManager"})
    CString ->                              -- filename : TBasicType TFileName
    Ptr (Ptr GError) ->                     -- error
    IO Word32

{-# DEPRECATED uIManagerAddUiFromFile ["(Since version 3.10)"] #-}
-- | Parses a file containing a [UI definition][XML-UI] and
-- merges it with the current contents of /@manager@/.
-- 
-- /Since: 2.4/
uIManagerAddUiFromFile ::
    (B.CallStack.HasCallStack, MonadIO m, IsUIManager a) =>
    a
    -- ^ /@manager@/: a t'GI.Gtk.Objects.UIManager.UIManager' object
    -> [Char]
    -- ^ /@filename@/: the name of the file to parse
    -> m Word32
    -- ^ __Returns:__ The merge id for the merged UI. The merge id can be used
    --   to unmerge the UI with 'GI.Gtk.Objects.UIManager.uIManagerRemoveUi'. If an error occurred,
    --   the return value is 0. /(Can throw 'Data.GI.Base.GError.GError')/
uIManagerAddUiFromFile manager filename = liftIO $ do
    manager' <- unsafeManagedPtrCastPtr manager
    filename' <- stringToCString filename
    onException (do
        result <- propagateGError $ gtk_ui_manager_add_ui_from_file manager' filename'
        touchManagedPtr manager
        freeMem filename'
        return result
     ) (do
        freeMem filename'
     )

#if defined(ENABLE_OVERLOADING)
data UIManagerAddUiFromFileMethodInfo
instance (signature ~ ([Char] -> m Word32), MonadIO m, IsUIManager a) => O.OverloadedMethod UIManagerAddUiFromFileMethodInfo a signature where
    overloadedMethod = uIManagerAddUiFromFile

instance O.OverloadedMethodInfo UIManagerAddUiFromFileMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.UIManager.uIManagerAddUiFromFile",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-UIManager.html#v:uIManagerAddUiFromFile"
        })


#endif

-- method UIManager::add_ui_from_resource
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "manager"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "UIManager" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkUIManager object"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "resource_path"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the resource path of the file to parse"
--                 , sinceVersion = Nothing
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
-- throws : True
-- Skip return : False

foreign import ccall "gtk_ui_manager_add_ui_from_resource" gtk_ui_manager_add_ui_from_resource :: 
    Ptr UIManager ->                        -- manager : TInterface (Name {namespace = "Gtk", name = "UIManager"})
    CString ->                              -- resource_path : TBasicType TUTF8
    Ptr (Ptr GError) ->                     -- error
    IO Word32

{-# DEPRECATED uIManagerAddUiFromResource ["(Since version 3.10)"] #-}
-- | Parses a resource file containing a [UI definition][XML-UI] and
-- merges it with the current contents of /@manager@/.
-- 
-- /Since: 3.4/
uIManagerAddUiFromResource ::
    (B.CallStack.HasCallStack, MonadIO m, IsUIManager a) =>
    a
    -- ^ /@manager@/: a t'GI.Gtk.Objects.UIManager.UIManager' object
    -> T.Text
    -- ^ /@resourcePath@/: the resource path of the file to parse
    -> m Word32
    -- ^ __Returns:__ The merge id for the merged UI. The merge id can be used
    --   to unmerge the UI with 'GI.Gtk.Objects.UIManager.uIManagerRemoveUi'. If an error occurred,
    --   the return value is 0. /(Can throw 'Data.GI.Base.GError.GError')/
uIManagerAddUiFromResource manager resourcePath = liftIO $ do
    manager' <- unsafeManagedPtrCastPtr manager
    resourcePath' <- textToCString resourcePath
    onException (do
        result <- propagateGError $ gtk_ui_manager_add_ui_from_resource manager' resourcePath'
        touchManagedPtr manager
        freeMem resourcePath'
        return result
     ) (do
        freeMem resourcePath'
     )

#if defined(ENABLE_OVERLOADING)
data UIManagerAddUiFromResourceMethodInfo
instance (signature ~ (T.Text -> m Word32), MonadIO m, IsUIManager a) => O.OverloadedMethod UIManagerAddUiFromResourceMethodInfo a signature where
    overloadedMethod = uIManagerAddUiFromResource

instance O.OverloadedMethodInfo UIManagerAddUiFromResourceMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.UIManager.uIManagerAddUiFromResource",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-UIManager.html#v:uIManagerAddUiFromResource"
        })


#endif

-- method UIManager::add_ui_from_string
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "manager"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "UIManager" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkUIManager object"
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
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the string to parse"
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
--           , argType = TBasicType TInt64
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the length of @buffer (may be -1 if @buffer is nul-terminated)"
--                 , sinceVersion = Nothing
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
-- throws : True
-- Skip return : False

foreign import ccall "gtk_ui_manager_add_ui_from_string" gtk_ui_manager_add_ui_from_string :: 
    Ptr UIManager ->                        -- manager : TInterface (Name {namespace = "Gtk", name = "UIManager"})
    CString ->                              -- buffer : TBasicType TUTF8
    Int64 ->                                -- length : TBasicType TInt64
    Ptr (Ptr GError) ->                     -- error
    IO Word32

{-# DEPRECATED uIManagerAddUiFromString ["(Since version 3.10)"] #-}
-- | Parses a string containing a [UI definition][XML-UI] and merges it with
-- the current contents of /@manager@/. An enclosing @\<ui>@ element is added if
-- it is missing.
-- 
-- /Since: 2.4/
uIManagerAddUiFromString ::
    (B.CallStack.HasCallStack, MonadIO m, IsUIManager a) =>
    a
    -- ^ /@manager@/: a t'GI.Gtk.Objects.UIManager.UIManager' object
    -> T.Text
    -- ^ /@buffer@/: the string to parse
    -> Int64
    -- ^ /@length@/: the length of /@buffer@/ (may be -1 if /@buffer@/ is nul-terminated)
    -> m Word32
    -- ^ __Returns:__ The merge id for the merged UI. The merge id can be used
    --   to unmerge the UI with 'GI.Gtk.Objects.UIManager.uIManagerRemoveUi'. If an error occurred,
    --   the return value is 0. /(Can throw 'Data.GI.Base.GError.GError')/
uIManagerAddUiFromString manager buffer length_ = liftIO $ do
    manager' <- unsafeManagedPtrCastPtr manager
    buffer' <- textToCString buffer
    onException (do
        result <- propagateGError $ gtk_ui_manager_add_ui_from_string manager' buffer' length_
        touchManagedPtr manager
        freeMem buffer'
        return result
     ) (do
        freeMem buffer'
     )

#if defined(ENABLE_OVERLOADING)
data UIManagerAddUiFromStringMethodInfo
instance (signature ~ (T.Text -> Int64 -> m Word32), MonadIO m, IsUIManager a) => O.OverloadedMethod UIManagerAddUiFromStringMethodInfo a signature where
    overloadedMethod = uIManagerAddUiFromString

instance O.OverloadedMethodInfo UIManagerAddUiFromStringMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.UIManager.uIManagerAddUiFromString",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-UIManager.html#v:uIManagerAddUiFromString"
        })


#endif

-- method UIManager::ensure_update
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "manager"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "UIManager" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkUIManager" , sinceVersion = Nothing }
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

foreign import ccall "gtk_ui_manager_ensure_update" gtk_ui_manager_ensure_update :: 
    Ptr UIManager ->                        -- manager : TInterface (Name {namespace = "Gtk", name = "UIManager"})
    IO ()

{-# DEPRECATED uIManagerEnsureUpdate ["(Since version 3.10)"] #-}
-- | Makes sure that all pending updates to the UI have been completed.
-- 
-- This may occasionally be necessary, since t'GI.Gtk.Objects.UIManager.UIManager' updates the
-- UI in an idle function. A typical example where this function is
-- useful is to enforce that the menubar and toolbar have been added to
-- the main window before showing it:
-- 
-- === /C code/
-- >
-- >gtk_container_add (GTK_CONTAINER (window), vbox);
-- >g_signal_connect (merge, "add-widget",
-- >                  G_CALLBACK (add_widget), vbox);
-- >gtk_ui_manager_add_ui_from_file (merge, "my-menus");
-- >gtk_ui_manager_add_ui_from_file (merge, "my-toolbars");
-- >gtk_ui_manager_ensure_update (merge);
-- >gtk_widget_show (window);
-- 
-- 
-- /Since: 2.4/
uIManagerEnsureUpdate ::
    (B.CallStack.HasCallStack, MonadIO m, IsUIManager a) =>
    a
    -- ^ /@manager@/: a t'GI.Gtk.Objects.UIManager.UIManager'
    -> m ()
uIManagerEnsureUpdate manager = liftIO $ do
    manager' <- unsafeManagedPtrCastPtr manager
    gtk_ui_manager_ensure_update manager'
    touchManagedPtr manager
    return ()

#if defined(ENABLE_OVERLOADING)
data UIManagerEnsureUpdateMethodInfo
instance (signature ~ (m ()), MonadIO m, IsUIManager a) => O.OverloadedMethod UIManagerEnsureUpdateMethodInfo a signature where
    overloadedMethod = uIManagerEnsureUpdate

instance O.OverloadedMethodInfo UIManagerEnsureUpdateMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.UIManager.uIManagerEnsureUpdate",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-UIManager.html#v:uIManagerEnsureUpdate"
        })


#endif

-- method UIManager::get_accel_group
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "manager"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "UIManager" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkUIManager object"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "AccelGroup" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_ui_manager_get_accel_group" gtk_ui_manager_get_accel_group :: 
    Ptr UIManager ->                        -- manager : TInterface (Name {namespace = "Gtk", name = "UIManager"})
    IO (Ptr Gtk.AccelGroup.AccelGroup)

{-# DEPRECATED uIManagerGetAccelGroup ["(Since version 3.10)"] #-}
-- | Returns the t'GI.Gtk.Objects.AccelGroup.AccelGroup' associated with /@manager@/.
-- 
-- /Since: 2.4/
uIManagerGetAccelGroup ::
    (B.CallStack.HasCallStack, MonadIO m, IsUIManager a) =>
    a
    -- ^ /@manager@/: a t'GI.Gtk.Objects.UIManager.UIManager' object
    -> m Gtk.AccelGroup.AccelGroup
    -- ^ __Returns:__ the t'GI.Gtk.Objects.AccelGroup.AccelGroup'.
uIManagerGetAccelGroup manager = liftIO $ do
    manager' <- unsafeManagedPtrCastPtr manager
    result <- gtk_ui_manager_get_accel_group manager'
    checkUnexpectedReturnNULL "uIManagerGetAccelGroup" result
    result' <- (newObject Gtk.AccelGroup.AccelGroup) result
    touchManagedPtr manager
    return result'

#if defined(ENABLE_OVERLOADING)
data UIManagerGetAccelGroupMethodInfo
instance (signature ~ (m Gtk.AccelGroup.AccelGroup), MonadIO m, IsUIManager a) => O.OverloadedMethod UIManagerGetAccelGroupMethodInfo a signature where
    overloadedMethod = uIManagerGetAccelGroup

instance O.OverloadedMethodInfo UIManagerGetAccelGroupMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.UIManager.uIManagerGetAccelGroup",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-UIManager.html#v:uIManagerGetAccelGroup"
        })


#endif

-- method UIManager::get_action
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "manager"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "UIManager" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkUIManager" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "path"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a path" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "Action" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_ui_manager_get_action" gtk_ui_manager_get_action :: 
    Ptr UIManager ->                        -- manager : TInterface (Name {namespace = "Gtk", name = "UIManager"})
    CString ->                              -- path : TBasicType TUTF8
    IO (Ptr Gtk.Action.Action)

{-# DEPRECATED uIManagerGetAction ["(Since version 3.10)"] #-}
-- | Looks up an action by following a path. See 'GI.Gtk.Objects.UIManager.uIManagerGetWidget'
-- for more information about paths.
-- 
-- /Since: 2.4/
uIManagerGetAction ::
    (B.CallStack.HasCallStack, MonadIO m, IsUIManager a) =>
    a
    -- ^ /@manager@/: a t'GI.Gtk.Objects.UIManager.UIManager'
    -> T.Text
    -- ^ /@path@/: a path
    -> m (Maybe Gtk.Action.Action)
    -- ^ __Returns:__ the action whose proxy widget is found by following the path,
    --     or 'P.Nothing' if no widget was found.
uIManagerGetAction manager path = liftIO $ do
    manager' <- unsafeManagedPtrCastPtr manager
    path' <- textToCString path
    result <- gtk_ui_manager_get_action manager' path'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (newObject Gtk.Action.Action) result'
        return result''
    touchManagedPtr manager
    freeMem path'
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data UIManagerGetActionMethodInfo
instance (signature ~ (T.Text -> m (Maybe Gtk.Action.Action)), MonadIO m, IsUIManager a) => O.OverloadedMethod UIManagerGetActionMethodInfo a signature where
    overloadedMethod = uIManagerGetAction

instance O.OverloadedMethodInfo UIManagerGetActionMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.UIManager.uIManagerGetAction",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-UIManager.html#v:uIManagerGetAction"
        })


#endif

-- method UIManager::get_action_groups
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "manager"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "UIManager" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkUIManager object"
--                 , sinceVersion = Nothing
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
--               (TGList
--                  (TInterface Name { namespace = "Gtk" , name = "ActionGroup" }))
-- throws : False
-- Skip return : False

foreign import ccall "gtk_ui_manager_get_action_groups" gtk_ui_manager_get_action_groups :: 
    Ptr UIManager ->                        -- manager : TInterface (Name {namespace = "Gtk", name = "UIManager"})
    IO (Ptr (GList (Ptr Gtk.ActionGroup.ActionGroup)))

{-# DEPRECATED uIManagerGetActionGroups ["(Since version 3.10)"] #-}
-- | Returns the list of action groups associated with /@manager@/.
-- 
-- /Since: 2.4/
uIManagerGetActionGroups ::
    (B.CallStack.HasCallStack, MonadIO m, IsUIManager a) =>
    a
    -- ^ /@manager@/: a t'GI.Gtk.Objects.UIManager.UIManager' object
    -> m [Gtk.ActionGroup.ActionGroup]
    -- ^ __Returns:__ a t'GI.GLib.Structs.List.List' of
    --   action groups. The list is owned by GTK+
    --   and should not be modified.
uIManagerGetActionGroups manager = liftIO $ do
    manager' <- unsafeManagedPtrCastPtr manager
    result <- gtk_ui_manager_get_action_groups manager'
    result' <- unpackGList result
    result'' <- mapM (newObject Gtk.ActionGroup.ActionGroup) result'
    touchManagedPtr manager
    return result''

#if defined(ENABLE_OVERLOADING)
data UIManagerGetActionGroupsMethodInfo
instance (signature ~ (m [Gtk.ActionGroup.ActionGroup]), MonadIO m, IsUIManager a) => O.OverloadedMethod UIManagerGetActionGroupsMethodInfo a signature where
    overloadedMethod = uIManagerGetActionGroups

instance O.OverloadedMethodInfo UIManagerGetActionGroupsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.UIManager.uIManagerGetActionGroups",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-UIManager.html#v:uIManagerGetActionGroups"
        })


#endif

-- method UIManager::get_add_tearoffs
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "manager"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "UIManager" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkUIManager" , sinceVersion = Nothing }
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

foreign import ccall "gtk_ui_manager_get_add_tearoffs" gtk_ui_manager_get_add_tearoffs :: 
    Ptr UIManager ->                        -- manager : TInterface (Name {namespace = "Gtk", name = "UIManager"})
    IO CInt

{-# DEPRECATED uIManagerGetAddTearoffs ["(Since version 3.4)","Tearoff menus are deprecated and should not","    be used in newly written code."] #-}
-- | Returns whether menus generated by this t'GI.Gtk.Objects.UIManager.UIManager'
-- will have tearoff menu items.
-- 
-- /Since: 2.4/
uIManagerGetAddTearoffs ::
    (B.CallStack.HasCallStack, MonadIO m, IsUIManager a) =>
    a
    -- ^ /@manager@/: a t'GI.Gtk.Objects.UIManager.UIManager'
    -> m Bool
    -- ^ __Returns:__ whether tearoff menu items are added
uIManagerGetAddTearoffs manager = liftIO $ do
    manager' <- unsafeManagedPtrCastPtr manager
    result <- gtk_ui_manager_get_add_tearoffs manager'
    let result' = (/= 0) result
    touchManagedPtr manager
    return result'

#if defined(ENABLE_OVERLOADING)
data UIManagerGetAddTearoffsMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsUIManager a) => O.OverloadedMethod UIManagerGetAddTearoffsMethodInfo a signature where
    overloadedMethod = uIManagerGetAddTearoffs

instance O.OverloadedMethodInfo UIManagerGetAddTearoffsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.UIManager.uIManagerGetAddTearoffs",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-UIManager.html#v:uIManagerGetAddTearoffs"
        })


#endif

-- method UIManager::get_toplevels
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "manager"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "UIManager" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkUIManager" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "types"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "UIManagerItemType" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "specifies the types of toplevel widgets to include. Allowed\n  types are #GTK_UI_MANAGER_MENUBAR, #GTK_UI_MANAGER_TOOLBAR and\n  #GTK_UI_MANAGER_POPUP."
--                 , sinceVersion = Nothing
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
--               (TGSList (TInterface Name { namespace = "Gtk" , name = "Widget" }))
-- throws : False
-- Skip return : False

foreign import ccall "gtk_ui_manager_get_toplevels" gtk_ui_manager_get_toplevels :: 
    Ptr UIManager ->                        -- manager : TInterface (Name {namespace = "Gtk", name = "UIManager"})
    CUInt ->                                -- types : TInterface (Name {namespace = "Gtk", name = "UIManagerItemType"})
    IO (Ptr (GSList (Ptr Gtk.Widget.Widget)))

{-# DEPRECATED uIManagerGetToplevels ["(Since version 3.10)"] #-}
-- | Obtains a list of all toplevel widgets of the requested types.
-- 
-- /Since: 2.4/
uIManagerGetToplevels ::
    (B.CallStack.HasCallStack, MonadIO m, IsUIManager a) =>
    a
    -- ^ /@manager@/: a t'GI.Gtk.Objects.UIManager.UIManager'
    -> [Gtk.Flags.UIManagerItemType]
    -- ^ /@types@/: specifies the types of toplevel widgets to include. Allowed
    --   types are @/GTK_UI_MANAGER_MENUBAR/@, @/GTK_UI_MANAGER_TOOLBAR/@ and
    --   @/GTK_UI_MANAGER_POPUP/@.
    -> m [Gtk.Widget.Widget]
    -- ^ __Returns:__ a newly-allocated t'GI.GLib.Structs.SList.SList' of
    -- all toplevel widgets of the requested types.  Free the returned list with @/g_slist_free()/@.
uIManagerGetToplevels manager types = liftIO $ do
    manager' <- unsafeManagedPtrCastPtr manager
    let types' = gflagsToWord types
    result <- gtk_ui_manager_get_toplevels manager' types'
    result' <- unpackGSList result
    result'' <- mapM (newObject Gtk.Widget.Widget) result'
    g_slist_free result
    touchManagedPtr manager
    return result''

#if defined(ENABLE_OVERLOADING)
data UIManagerGetToplevelsMethodInfo
instance (signature ~ ([Gtk.Flags.UIManagerItemType] -> m [Gtk.Widget.Widget]), MonadIO m, IsUIManager a) => O.OverloadedMethod UIManagerGetToplevelsMethodInfo a signature where
    overloadedMethod = uIManagerGetToplevels

instance O.OverloadedMethodInfo UIManagerGetToplevelsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.UIManager.uIManagerGetToplevels",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-UIManager.html#v:uIManagerGetToplevels"
        })


#endif

-- method UIManager::get_ui
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "manager"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "UIManager" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkUIManager" , sinceVersion = Nothing }
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

foreign import ccall "gtk_ui_manager_get_ui" gtk_ui_manager_get_ui :: 
    Ptr UIManager ->                        -- manager : TInterface (Name {namespace = "Gtk", name = "UIManager"})
    IO CString

{-# DEPRECATED uIManagerGetUi ["(Since version 3.10)"] #-}
-- | Creates a [UI definition][XML-UI] of the merged UI.
-- 
-- /Since: 2.4/
uIManagerGetUi ::
    (B.CallStack.HasCallStack, MonadIO m, IsUIManager a) =>
    a
    -- ^ /@manager@/: a t'GI.Gtk.Objects.UIManager.UIManager'
    -> m T.Text
    -- ^ __Returns:__ A newly allocated string containing an XML representation of
    -- the merged UI.
uIManagerGetUi manager = liftIO $ do
    manager' <- unsafeManagedPtrCastPtr manager
    result <- gtk_ui_manager_get_ui manager'
    checkUnexpectedReturnNULL "uIManagerGetUi" result
    result' <- cstringToText result
    freeMem result
    touchManagedPtr manager
    return result'

#if defined(ENABLE_OVERLOADING)
data UIManagerGetUiMethodInfo
instance (signature ~ (m T.Text), MonadIO m, IsUIManager a) => O.OverloadedMethod UIManagerGetUiMethodInfo a signature where
    overloadedMethod = uIManagerGetUi

instance O.OverloadedMethodInfo UIManagerGetUiMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.UIManager.uIManagerGetUi",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-UIManager.html#v:uIManagerGetUi"
        })


#endif

-- method UIManager::get_widget
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "manager"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "UIManager" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkUIManager" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "path"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a path" , sinceVersion = Nothing }
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

foreign import ccall "gtk_ui_manager_get_widget" gtk_ui_manager_get_widget :: 
    Ptr UIManager ->                        -- manager : TInterface (Name {namespace = "Gtk", name = "UIManager"})
    CString ->                              -- path : TBasicType TUTF8
    IO (Ptr Gtk.Widget.Widget)

{-# DEPRECATED uIManagerGetWidget ["(Since version 3.10)"] #-}
-- | Looks up a widget by following a path.
-- The path consists of the names specified in the XML description of the UI.
-- separated by “\/”. Elements which don’t have a name or action attribute in
-- the XML (e.g. @\<popup>@) can be addressed by their XML element name
-- (e.g. \"popup\"). The root element (\"\/ui\") can be omitted in the path.
-- 
-- Note that the widget found by following a path that ends in a @\<menu>@;
-- element is the menuitem to which the menu is attached, not the menu it
-- manages.
-- 
-- Also note that the widgets constructed by a ui manager are not tied to
-- the lifecycle of the ui manager. If you add the widgets returned by this
-- function to some container or explicitly ref them, they will survive the
-- destruction of the ui manager.
-- 
-- /Since: 2.4/
uIManagerGetWidget ::
    (B.CallStack.HasCallStack, MonadIO m, IsUIManager a) =>
    a
    -- ^ /@manager@/: a t'GI.Gtk.Objects.UIManager.UIManager'
    -> T.Text
    -- ^ /@path@/: a path
    -> m (Maybe Gtk.Widget.Widget)
    -- ^ __Returns:__ the widget found by following the path,
    --     or 'P.Nothing' if no widget was found
uIManagerGetWidget manager path = liftIO $ do
    manager' <- unsafeManagedPtrCastPtr manager
    path' <- textToCString path
    result <- gtk_ui_manager_get_widget manager' path'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (newObject Gtk.Widget.Widget) result'
        return result''
    touchManagedPtr manager
    freeMem path'
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data UIManagerGetWidgetMethodInfo
instance (signature ~ (T.Text -> m (Maybe Gtk.Widget.Widget)), MonadIO m, IsUIManager a) => O.OverloadedMethod UIManagerGetWidgetMethodInfo a signature where
    overloadedMethod = uIManagerGetWidget

instance O.OverloadedMethodInfo UIManagerGetWidgetMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.UIManager.uIManagerGetWidget",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-UIManager.html#v:uIManagerGetWidget"
        })


#endif

-- method UIManager::insert_action_group
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "manager"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "UIManager" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkUIManager object"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "action_group"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ActionGroup" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the action group to be inserted"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "pos"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "the position at which the group will be inserted."
--                 , sinceVersion = Nothing
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

foreign import ccall "gtk_ui_manager_insert_action_group" gtk_ui_manager_insert_action_group :: 
    Ptr UIManager ->                        -- manager : TInterface (Name {namespace = "Gtk", name = "UIManager"})
    Ptr Gtk.ActionGroup.ActionGroup ->      -- action_group : TInterface (Name {namespace = "Gtk", name = "ActionGroup"})
    Int32 ->                                -- pos : TBasicType TInt
    IO ()

{-# DEPRECATED uIManagerInsertActionGroup ["(Since version 3.10)"] #-}
-- | Inserts an action group into the list of action groups associated
-- with /@manager@/. Actions in earlier groups hide actions with the same
-- name in later groups.
-- 
-- If /@pos@/ is larger than the number of action groups in /@manager@/, or
-- negative, /@actionGroup@/ will be inserted at the end of the internal
-- list.
-- 
-- /Since: 2.4/
uIManagerInsertActionGroup ::
    (B.CallStack.HasCallStack, MonadIO m, IsUIManager a, Gtk.ActionGroup.IsActionGroup b) =>
    a
    -- ^ /@manager@/: a t'GI.Gtk.Objects.UIManager.UIManager' object
    -> b
    -- ^ /@actionGroup@/: the action group to be inserted
    -> Int32
    -- ^ /@pos@/: the position at which the group will be inserted.
    -> m ()
uIManagerInsertActionGroup manager actionGroup pos = liftIO $ do
    manager' <- unsafeManagedPtrCastPtr manager
    actionGroup' <- unsafeManagedPtrCastPtr actionGroup
    gtk_ui_manager_insert_action_group manager' actionGroup' pos
    touchManagedPtr manager
    touchManagedPtr actionGroup
    return ()

#if defined(ENABLE_OVERLOADING)
data UIManagerInsertActionGroupMethodInfo
instance (signature ~ (b -> Int32 -> m ()), MonadIO m, IsUIManager a, Gtk.ActionGroup.IsActionGroup b) => O.OverloadedMethod UIManagerInsertActionGroupMethodInfo a signature where
    overloadedMethod = uIManagerInsertActionGroup

instance O.OverloadedMethodInfo UIManagerInsertActionGroupMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.UIManager.uIManagerInsertActionGroup",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-UIManager.html#v:uIManagerInsertActionGroup"
        })


#endif

-- method UIManager::new_merge_id
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "manager"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "UIManager" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkUIManager" , sinceVersion = Nothing }
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

foreign import ccall "gtk_ui_manager_new_merge_id" gtk_ui_manager_new_merge_id :: 
    Ptr UIManager ->                        -- manager : TInterface (Name {namespace = "Gtk", name = "UIManager"})
    IO Word32

{-# DEPRECATED uIManagerNewMergeId ["(Since version 3.10)"] #-}
-- | Returns an unused merge id, suitable for use with
-- 'GI.Gtk.Objects.UIManager.uIManagerAddUi'.
-- 
-- /Since: 2.4/
uIManagerNewMergeId ::
    (B.CallStack.HasCallStack, MonadIO m, IsUIManager a) =>
    a
    -- ^ /@manager@/: a t'GI.Gtk.Objects.UIManager.UIManager'
    -> m Word32
    -- ^ __Returns:__ an unused merge id.
uIManagerNewMergeId manager = liftIO $ do
    manager' <- unsafeManagedPtrCastPtr manager
    result <- gtk_ui_manager_new_merge_id manager'
    touchManagedPtr manager
    return result

#if defined(ENABLE_OVERLOADING)
data UIManagerNewMergeIdMethodInfo
instance (signature ~ (m Word32), MonadIO m, IsUIManager a) => O.OverloadedMethod UIManagerNewMergeIdMethodInfo a signature where
    overloadedMethod = uIManagerNewMergeId

instance O.OverloadedMethodInfo UIManagerNewMergeIdMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.UIManager.uIManagerNewMergeId",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-UIManager.html#v:uIManagerNewMergeId"
        })


#endif

-- method UIManager::remove_action_group
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "manager"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "UIManager" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkUIManager object"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "action_group"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ActionGroup" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the action group to be removed"
--                 , sinceVersion = Nothing
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

foreign import ccall "gtk_ui_manager_remove_action_group" gtk_ui_manager_remove_action_group :: 
    Ptr UIManager ->                        -- manager : TInterface (Name {namespace = "Gtk", name = "UIManager"})
    Ptr Gtk.ActionGroup.ActionGroup ->      -- action_group : TInterface (Name {namespace = "Gtk", name = "ActionGroup"})
    IO ()

{-# DEPRECATED uIManagerRemoveActionGroup ["(Since version 3.10)"] #-}
-- | Removes an action group from the list of action groups associated
-- with /@manager@/.
-- 
-- /Since: 2.4/
uIManagerRemoveActionGroup ::
    (B.CallStack.HasCallStack, MonadIO m, IsUIManager a, Gtk.ActionGroup.IsActionGroup b) =>
    a
    -- ^ /@manager@/: a t'GI.Gtk.Objects.UIManager.UIManager' object
    -> b
    -- ^ /@actionGroup@/: the action group to be removed
    -> m ()
uIManagerRemoveActionGroup manager actionGroup = liftIO $ do
    manager' <- unsafeManagedPtrCastPtr manager
    actionGroup' <- unsafeManagedPtrCastPtr actionGroup
    gtk_ui_manager_remove_action_group manager' actionGroup'
    touchManagedPtr manager
    touchManagedPtr actionGroup
    return ()

#if defined(ENABLE_OVERLOADING)
data UIManagerRemoveActionGroupMethodInfo
instance (signature ~ (b -> m ()), MonadIO m, IsUIManager a, Gtk.ActionGroup.IsActionGroup b) => O.OverloadedMethod UIManagerRemoveActionGroupMethodInfo a signature where
    overloadedMethod = uIManagerRemoveActionGroup

instance O.OverloadedMethodInfo UIManagerRemoveActionGroupMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.UIManager.uIManagerRemoveActionGroup",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-UIManager.html#v:uIManagerRemoveActionGroup"
        })


#endif

-- method UIManager::remove_ui
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "manager"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "UIManager" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkUIManager object"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "merge_id"
--           , argType = TBasicType TUInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "a merge id as returned by gtk_ui_manager_add_ui_from_string()"
--                 , sinceVersion = Nothing
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

foreign import ccall "gtk_ui_manager_remove_ui" gtk_ui_manager_remove_ui :: 
    Ptr UIManager ->                        -- manager : TInterface (Name {namespace = "Gtk", name = "UIManager"})
    Word32 ->                               -- merge_id : TBasicType TUInt
    IO ()

{-# DEPRECATED uIManagerRemoveUi ["(Since version 3.10)"] #-}
-- | Unmerges the part of /@manager@/\'s content identified by /@mergeId@/.
-- 
-- /Since: 2.4/
uIManagerRemoveUi ::
    (B.CallStack.HasCallStack, MonadIO m, IsUIManager a) =>
    a
    -- ^ /@manager@/: a t'GI.Gtk.Objects.UIManager.UIManager' object
    -> Word32
    -- ^ /@mergeId@/: a merge id as returned by 'GI.Gtk.Objects.UIManager.uIManagerAddUiFromString'
    -> m ()
uIManagerRemoveUi manager mergeId = liftIO $ do
    manager' <- unsafeManagedPtrCastPtr manager
    gtk_ui_manager_remove_ui manager' mergeId
    touchManagedPtr manager
    return ()

#if defined(ENABLE_OVERLOADING)
data UIManagerRemoveUiMethodInfo
instance (signature ~ (Word32 -> m ()), MonadIO m, IsUIManager a) => O.OverloadedMethod UIManagerRemoveUiMethodInfo a signature where
    overloadedMethod = uIManagerRemoveUi

instance O.OverloadedMethodInfo UIManagerRemoveUiMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.UIManager.uIManagerRemoveUi",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-UIManager.html#v:uIManagerRemoveUi"
        })


#endif

-- method UIManager::set_add_tearoffs
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "manager"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "UIManager" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkUIManager" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "add_tearoffs"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "whether tearoff menu items are added"
--                 , sinceVersion = Nothing
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

foreign import ccall "gtk_ui_manager_set_add_tearoffs" gtk_ui_manager_set_add_tearoffs :: 
    Ptr UIManager ->                        -- manager : TInterface (Name {namespace = "Gtk", name = "UIManager"})
    CInt ->                                 -- add_tearoffs : TBasicType TBoolean
    IO ()

{-# DEPRECATED uIManagerSetAddTearoffs ["(Since version 3.4)","Tearoff menus are deprecated and should not","    be used in newly written code."] #-}
-- | Sets the “add_tearoffs” property, which controls whether menus
-- generated by this t'GI.Gtk.Objects.UIManager.UIManager' will have tearoff menu items.
-- 
-- Note that this only affects regular menus. Generated popup
-- menus never have tearoff menu items.
-- 
-- /Since: 2.4/
uIManagerSetAddTearoffs ::
    (B.CallStack.HasCallStack, MonadIO m, IsUIManager a) =>
    a
    -- ^ /@manager@/: a t'GI.Gtk.Objects.UIManager.UIManager'
    -> Bool
    -- ^ /@addTearoffs@/: whether tearoff menu items are added
    -> m ()
uIManagerSetAddTearoffs manager addTearoffs = liftIO $ do
    manager' <- unsafeManagedPtrCastPtr manager
    let addTearoffs' = (fromIntegral . fromEnum) addTearoffs
    gtk_ui_manager_set_add_tearoffs manager' addTearoffs'
    touchManagedPtr manager
    return ()

#if defined(ENABLE_OVERLOADING)
data UIManagerSetAddTearoffsMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsUIManager a) => O.OverloadedMethod UIManagerSetAddTearoffsMethodInfo a signature where
    overloadedMethod = uIManagerSetAddTearoffs

instance O.OverloadedMethodInfo UIManagerSetAddTearoffsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.UIManager.uIManagerSetAddTearoffs",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-UIManager.html#v:uIManagerSetAddTearoffs"
        })


#endif


