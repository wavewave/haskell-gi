{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- A GtkBuilder is an auxiliary object that reads textual descriptions
-- of a user interface and instantiates the described objects. To create
-- a GtkBuilder from a user interface description, call
-- 'GI.Gtk.Objects.Builder.builderNewFromFile', 'GI.Gtk.Objects.Builder.builderNewFromResource' or
-- 'GI.Gtk.Objects.Builder.builderNewFromString'.
-- 
-- In the (unusual) case that you want to add user interface
-- descriptions from multiple sources to the same GtkBuilder you can
-- call 'GI.Gtk.Objects.Builder.builderNew' to get an empty builder and populate it by
-- (multiple) calls to 'GI.Gtk.Objects.Builder.builderAddFromFile',
-- 'GI.Gtk.Objects.Builder.builderAddFromResource' or 'GI.Gtk.Objects.Builder.builderAddFromString'.
-- 
-- A GtkBuilder holds a reference to all objects that it has constructed
-- and drops these references when it is finalized. This finalization can
-- cause the destruction of non-widget objects or widgets which are not
-- contained in a toplevel window. For toplevel windows constructed by a
-- builder, it is the responsibility of the user to call 'GI.Gtk.Objects.Widget.widgetDestroy'
-- to get rid of them and all the widgets they contain.
-- 
-- The functions 'GI.Gtk.Objects.Builder.builderGetObject' and 'GI.Gtk.Objects.Builder.builderGetObjects'
-- can be used to access the widgets in the interface by the names assigned
-- to them inside the UI description. Toplevel windows returned by these
-- functions will stay around until the user explicitly destroys them
-- with 'GI.Gtk.Objects.Widget.widgetDestroy'. Other widgets will either be part of a
-- larger hierarchy constructed by the builder (in which case you should
-- not have to worry about their lifecycle), or without a parent, in which
-- case they have to be added to some container to make use of them.
-- Non-widget objects need to be reffed with 'GI.GObject.Objects.Object.objectRef' to keep them
-- beyond the lifespan of the builder.
-- 
-- The function 'GI.Gtk.Objects.Builder.builderConnectSignals' and variants thereof can be
-- used to connect handlers to the named signals in the description.
-- 
-- # GtkBuilder UI Definitions # {@/BUILDER/@-UI}
-- 
-- GtkBuilder parses textual descriptions of user interfaces which are
-- specified in an XML format which can be roughly described by the
-- RELAX NG schema below. We refer to these descriptions as “GtkBuilder
-- UI definitions” or just “UI definitions” if the context is clear.
-- Do not confuse GtkBuilder UI Definitions with
-- [GtkUIManager UI Definitions][XML-UI], which are more limited in scope.
-- It is common to use @.ui@ as the filename extension for files containing
-- GtkBuilder UI definitions.
-- 
-- <https://gitlab.gnome.org/GNOME/gtk/-/blob/gtk-3-24/gtk/gtkbuilder.rnc RELAX NG Compact Syntax>
-- 
-- The toplevel element is @\<interface>@. It optionally takes a “domain”
-- attribute, which will make the builder look for translated strings
-- using @/dgettext()/@ in the domain specified. This can also be done by
-- calling 'GI.Gtk.Objects.Builder.builderSetTranslationDomain' on the builder.
-- Objects are described by @\<object>@ elements, which can contain
-- @\<property>@ elements to set properties, @\<signal>@ elements which
-- connect signals to handlers, and @\<child>@ elements, which describe
-- child objects (most often widgets inside a container, but also e.g.
-- actions in an action group, or columns in a tree model). A @\<child>@
-- element contains an @\<object>@ element which describes the child object.
-- The target toolkit version(s) are described by @\<requires>@ elements,
-- the “lib” attribute specifies the widget library in question (currently
-- the only supported value is “gtk+”) and the “version” attribute specifies
-- the target version in the form @\<major>.\<minor>@. The builder will error
-- out if the version requirements are not met.
-- 
-- Typically, the specific kind of object represented by an @\<object>@
-- element is specified by the “class” attribute. If the type has not
-- been loaded yet, GTK+ tries to find the @get_type()@ function from the
-- class name by applying heuristics. This works in most cases, but if
-- necessary, it is possible to specify the name of the @/get_type()/@ function
-- explictly with the \"type-func\" attribute. As a special case, GtkBuilder
-- allows to use an object that has been constructed by a t'GI.Gtk.Objects.UIManager.UIManager' in
-- another part of the UI definition by specifying the id of the t'GI.Gtk.Objects.UIManager.UIManager'
-- in the “constructor” attribute and the name of the object in the “id”
-- attribute.
-- 
-- Objects may be given a name with the “id” attribute, which allows the
-- application to retrieve them from the builder with 'GI.Gtk.Objects.Builder.builderGetObject'.
-- An id is also necessary to use the object as property value in other
-- parts of the UI definition. GTK+ reserves ids starting and ending
-- with @___@ (3 underscores) for its own purposes.
-- 
-- Setting properties of objects is pretty straightforward with the
-- @\<property>@ element: the “name” attribute specifies the name of the
-- property, and the content of the element specifies the value.
-- If the “translatable” attribute is set to a true value, GTK+ uses
-- @/gettext()/@ (or @/dgettext()/@ if the builder has a translation domain set)
-- to find a translation for the value. This happens before the value
-- is parsed, so it can be used for properties of any type, but it is
-- probably most useful for string properties. It is also possible to
-- specify a context to disambiguate short strings, and comments which
-- may help the translators.
-- 
-- GtkBuilder can parse textual representations for the most common
-- property types: characters, strings, integers, floating-point numbers,
-- booleans (strings like “TRUE”, “t”, “yes”, “y”, “1” are interpreted
-- as 'P.True', strings like “FALSE”, “f”, “no”, “n”, “0” are interpreted
-- as 'P.False'), enumerations (can be specified by their name, nick or
-- integer value), flags (can be specified by their name, nick, integer
-- value, optionally combined with “|”, e.g. “GTK_VISIBLE|GTK_REALIZED”)
-- and colors (in a format understood by 'GI.Gdk.Structs.RGBA.rGBAParse').
-- 
-- GVariants can be specified in the format understood by 'GI.GLib.Functions.variantParse',
-- and pixbufs can be specified as a filename of an image file to load.
-- 
-- Objects can be referred to by their name and by default refer to
-- objects declared in the local xml fragment and objects exposed via
-- 'GI.Gtk.Objects.Builder.builderExposeObject'. In general, GtkBuilder allows forward
-- references to objects — declared in the local xml; an object doesn’t
-- have to be constructed before it can be referred to. The exception
-- to this rule is that an object has to be constructed before it can
-- be used as the value of a construct-only property.
-- 
-- It is also possible to bind a property value to another object\'s
-- property value using the attributes
-- \"bind-source\" to specify the source object of the binding,
-- \"bind-property\" to specify the source property and optionally
-- \"bind-flags\" to specify the binding flags.
-- Internally builder implements this using GBinding objects.
-- For more information see 'GI.GObject.Objects.Object.objectBindProperty'
-- 
-- Signal handlers are set up with the @\<signal>@ element. The “name”
-- attribute specifies the name of the signal, and the “handler” attribute
-- specifies the function to connect to the signal. By default, GTK+ tries
-- to find the handler using 'GI.GModule.Structs.Module.moduleSymbol', but this can be changed by
-- passing a custom t'GI.Gtk.Callbacks.BuilderConnectFunc' to
-- 'GI.Gtk.Objects.Builder.builderConnectSignalsFull'. The remaining attributes, “after”,
-- “swapped” and “object”, have the same meaning as the corresponding
-- parameters of the @/g_signal_connect_object()/@ or
-- @/g_signal_connect_data()/@ functions. A “last_modification_time”
-- attribute is also allowed, but it does not have a meaning to the
-- builder.
-- 
-- Sometimes it is necessary to refer to widgets which have implicitly
-- been constructed by GTK+ as part of a composite widget, to set
-- properties on them or to add further children (e.g. the /@vbox@/ of
-- a t'GI.Gtk.Objects.Dialog.Dialog'). This can be achieved by setting the “internal-child”
-- property of the @\<child>@ element to a true value. Note that GtkBuilder
-- still requires an @\<object>@ element for the internal child, even if it
-- has already been constructed.
-- 
-- A number of widgets have different places where a child can be added
-- (e.g. tabs vs. page content in notebooks). This can be reflected in
-- a UI definition by specifying the “type” attribute on a @\<child>@
-- The possible values for the “type” attribute are described in the
-- sections describing the widget-specific portions of UI definitions.
-- 
-- = A GtkBuilder UI Definition
-- 
-- 
-- === /xml code/
-- >
-- ><interface>
-- >  <object class="GtkDialog" id="dialog1">
-- >    <child internal-child="vbox">
-- >      <object class="GtkBox" id="vbox1">
-- >        <property name="border-width">10</property>
-- >        <child internal-child="action_area">
-- >          <object class="GtkButtonBox" id="hbuttonbox1">
-- >            <property name="border-width">20</property>
-- >            <child>
-- >              <object class="GtkButton" id="ok_button">
-- >                <property name="label">gtk-ok</property>
-- >                <property name="use-stock">TRUE</property>
-- >                <signal name="clicked" handler="ok_button_clicked"/>
-- >              </object>
-- >            </child>
-- >          </object>
-- >        </child>
-- >      </object>
-- >    </child>
-- >  </object>
-- ></interface>
-- 
-- 
-- Beyond this general structure, several object classes define their
-- own XML DTD fragments for filling in the ANY placeholders in the DTD
-- above. Note that a custom element in a @\<child>@ element gets parsed by
-- the custom tag handler of the parent object, while a custom element in
-- an @\<object>@ element gets parsed by the custom tag handler of the object.
-- 
-- These XML fragments are explained in the documentation of the
-- respective objects.
-- 
-- Additionally, since 3.10 a special @\<template>@ tag has been added
-- to the format allowing one to define a widget class’s components.
-- See the [GtkWidget documentation][composite-templates] for details.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.Builder
    ( 

-- * Exported types
    Builder(..)                             ,
    IsBuilder                               ,
    toBuilder                               ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [addCallbackSymbol]("GI.Gtk.Objects.Builder#g:method:addCallbackSymbol"), [addFromFile]("GI.Gtk.Objects.Builder#g:method:addFromFile"), [addFromResource]("GI.Gtk.Objects.Builder#g:method:addFromResource"), [addFromString]("GI.Gtk.Objects.Builder#g:method:addFromString"), [addObjectsFromFile]("GI.Gtk.Objects.Builder#g:method:addObjectsFromFile"), [addObjectsFromResource]("GI.Gtk.Objects.Builder#g:method:addObjectsFromResource"), [addObjectsFromString]("GI.Gtk.Objects.Builder#g:method:addObjectsFromString"), [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [connectSignals]("GI.Gtk.Objects.Builder#g:method:connectSignals"), [connectSignalsFull]("GI.Gtk.Objects.Builder#g:method:connectSignalsFull"), [exposeObject]("GI.Gtk.Objects.Builder#g:method:exposeObject"), [extendWithTemplate]("GI.Gtk.Objects.Builder#g:method:extendWithTemplate"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [valueFromString]("GI.Gtk.Objects.Builder#g:method:valueFromString"), [valueFromStringType]("GI.Gtk.Objects.Builder#g:method:valueFromStringType"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getApplication]("GI.Gtk.Objects.Builder#g:method:getApplication"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getObject]("GI.Gtk.Objects.Builder#g:method:getObject"), [getObjects]("GI.Gtk.Objects.Builder#g:method:getObjects"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getTranslationDomain]("GI.Gtk.Objects.Builder#g:method:getTranslationDomain"), [getTypeFromName]("GI.Gtk.Objects.Builder#g:method:getTypeFromName").
-- 
-- ==== Setters
-- [setApplication]("GI.Gtk.Objects.Builder#g:method:setApplication"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setTranslationDomain]("GI.Gtk.Objects.Builder#g:method:setTranslationDomain").

#if defined(ENABLE_OVERLOADING)
    ResolveBuilderMethod                    ,
#endif

-- ** addCallbackSymbol #method:addCallbackSymbol#

#if defined(ENABLE_OVERLOADING)
    BuilderAddCallbackSymbolMethodInfo      ,
#endif
    builderAddCallbackSymbol                ,


-- ** addFromFile #method:addFromFile#

#if defined(ENABLE_OVERLOADING)
    BuilderAddFromFileMethodInfo            ,
#endif
    builderAddFromFile                      ,


-- ** addFromResource #method:addFromResource#

#if defined(ENABLE_OVERLOADING)
    BuilderAddFromResourceMethodInfo        ,
#endif
    builderAddFromResource                  ,


-- ** addFromString #method:addFromString#

#if defined(ENABLE_OVERLOADING)
    BuilderAddFromStringMethodInfo          ,
#endif
    builderAddFromString                    ,


-- ** addObjectsFromFile #method:addObjectsFromFile#

#if defined(ENABLE_OVERLOADING)
    BuilderAddObjectsFromFileMethodInfo     ,
#endif
    builderAddObjectsFromFile               ,


-- ** addObjectsFromResource #method:addObjectsFromResource#

#if defined(ENABLE_OVERLOADING)
    BuilderAddObjectsFromResourceMethodInfo ,
#endif
    builderAddObjectsFromResource           ,


-- ** addObjectsFromString #method:addObjectsFromString#

#if defined(ENABLE_OVERLOADING)
    BuilderAddObjectsFromStringMethodInfo   ,
#endif
    builderAddObjectsFromString             ,


-- ** connectSignals #method:connectSignals#

#if defined(ENABLE_OVERLOADING)
    BuilderConnectSignalsMethodInfo         ,
#endif
    builderConnectSignals                   ,


-- ** connectSignalsFull #method:connectSignalsFull#

#if defined(ENABLE_OVERLOADING)
    BuilderConnectSignalsFullMethodInfo     ,
#endif
    builderConnectSignalsFull               ,


-- ** exposeObject #method:exposeObject#

#if defined(ENABLE_OVERLOADING)
    BuilderExposeObjectMethodInfo           ,
#endif
    builderExposeObject                     ,


-- ** extendWithTemplate #method:extendWithTemplate#

#if defined(ENABLE_OVERLOADING)
    BuilderExtendWithTemplateMethodInfo     ,
#endif
    builderExtendWithTemplate               ,


-- ** getApplication #method:getApplication#

#if defined(ENABLE_OVERLOADING)
    BuilderGetApplicationMethodInfo         ,
#endif
    builderGetApplication                   ,


-- ** getObject #method:getObject#

#if defined(ENABLE_OVERLOADING)
    BuilderGetObjectMethodInfo              ,
#endif
    builderGetObject                        ,


-- ** getObjects #method:getObjects#

#if defined(ENABLE_OVERLOADING)
    BuilderGetObjectsMethodInfo             ,
#endif
    builderGetObjects                       ,


-- ** getTranslationDomain #method:getTranslationDomain#

#if defined(ENABLE_OVERLOADING)
    BuilderGetTranslationDomainMethodInfo   ,
#endif
    builderGetTranslationDomain             ,


-- ** getTypeFromName #method:getTypeFromName#

#if defined(ENABLE_OVERLOADING)
    BuilderGetTypeFromNameMethodInfo        ,
#endif
    builderGetTypeFromName                  ,


-- ** new #method:new#

    builderNew                              ,


-- ** newFromFile #method:newFromFile#

    builderNewFromFile                      ,


-- ** newFromResource #method:newFromResource#

    builderNewFromResource                  ,


-- ** newFromString #method:newFromString#

    builderNewFromString                    ,


-- ** setApplication #method:setApplication#

#if defined(ENABLE_OVERLOADING)
    BuilderSetApplicationMethodInfo         ,
#endif
    builderSetApplication                   ,


-- ** setTranslationDomain #method:setTranslationDomain#

#if defined(ENABLE_OVERLOADING)
    BuilderSetTranslationDomainMethodInfo   ,
#endif
    builderSetTranslationDomain             ,


-- ** valueFromString #method:valueFromString#

#if defined(ENABLE_OVERLOADING)
    BuilderValueFromStringMethodInfo        ,
#endif
    builderValueFromString                  ,


-- ** valueFromStringType #method:valueFromStringType#

#if defined(ENABLE_OVERLOADING)
    BuilderValueFromStringTypeMethodInfo    ,
#endif
    builderValueFromStringType              ,




 -- * Properties


-- ** translationDomain #attr:translationDomain#
-- | The translation domain used when translating property values that
-- have been marked as translatable in interface descriptions.
-- If the translation domain is 'P.Nothing', t'GI.Gtk.Objects.Builder.Builder' uses @/gettext()/@,
-- otherwise 'GI.GLib.Functions.dgettext'.
-- 
-- /Since: 2.12/

#if defined(ENABLE_OVERLOADING)
    BuilderTranslationDomainPropertyInfo    ,
#endif
#if defined(ENABLE_OVERLOADING)
    builderTranslationDomain                ,
#endif
    clearBuilderTranslationDomain           ,
    constructBuilderTranslationDomain       ,
    getBuilderTranslationDomain             ,
    setBuilderTranslationDomain             ,




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

import qualified GI.GObject.Callbacks as GObject.Callbacks
import qualified GI.GObject.Objects.Object as GObject.Object
import qualified GI.Gtk.Callbacks as Gtk.Callbacks
import {-# SOURCE #-} qualified GI.Gtk.Objects.Application as Gtk.Application
import {-# SOURCE #-} qualified GI.Gtk.Objects.Widget as Gtk.Widget

-- | Memory-managed wrapper type.
newtype Builder = Builder (SP.ManagedPtr Builder)
    deriving (Eq)

instance SP.ManagedPtrNewtype Builder where
    toManagedPtr (Builder p) = p

foreign import ccall "gtk_builder_get_type"
    c_gtk_builder_get_type :: IO B.Types.GType

instance B.Types.TypedObject Builder where
    glibType = c_gtk_builder_get_type

instance B.Types.GObject Builder

-- | Type class for types which can be safely cast to `Builder`, for instance with `toBuilder`.
class (SP.GObject o, O.IsDescendantOf Builder o) => IsBuilder o
instance (SP.GObject o, O.IsDescendantOf Builder o) => IsBuilder o

instance O.HasParentTypes Builder
type instance O.ParentTypes Builder = '[GObject.Object.Object]

-- | Cast to `Builder`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toBuilder :: (MIO.MonadIO m, IsBuilder o) => o -> m Builder
toBuilder = MIO.liftIO . B.ManagedPtr.unsafeCastTo Builder

-- | Convert 'Builder' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe Builder) where
    gvalueGType_ = c_gtk_builder_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr Builder)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr Builder)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject Builder ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveBuilderMethod (t :: Symbol) (o :: *) :: * where
    ResolveBuilderMethod "addCallbackSymbol" o = BuilderAddCallbackSymbolMethodInfo
    ResolveBuilderMethod "addFromFile" o = BuilderAddFromFileMethodInfo
    ResolveBuilderMethod "addFromResource" o = BuilderAddFromResourceMethodInfo
    ResolveBuilderMethod "addFromString" o = BuilderAddFromStringMethodInfo
    ResolveBuilderMethod "addObjectsFromFile" o = BuilderAddObjectsFromFileMethodInfo
    ResolveBuilderMethod "addObjectsFromResource" o = BuilderAddObjectsFromResourceMethodInfo
    ResolveBuilderMethod "addObjectsFromString" o = BuilderAddObjectsFromStringMethodInfo
    ResolveBuilderMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveBuilderMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveBuilderMethod "connectSignals" o = BuilderConnectSignalsMethodInfo
    ResolveBuilderMethod "connectSignalsFull" o = BuilderConnectSignalsFullMethodInfo
    ResolveBuilderMethod "exposeObject" o = BuilderExposeObjectMethodInfo
    ResolveBuilderMethod "extendWithTemplate" o = BuilderExtendWithTemplateMethodInfo
    ResolveBuilderMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveBuilderMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveBuilderMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveBuilderMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveBuilderMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveBuilderMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveBuilderMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveBuilderMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveBuilderMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveBuilderMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveBuilderMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveBuilderMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveBuilderMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveBuilderMethod "valueFromString" o = BuilderValueFromStringMethodInfo
    ResolveBuilderMethod "valueFromStringType" o = BuilderValueFromStringTypeMethodInfo
    ResolveBuilderMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveBuilderMethod "getApplication" o = BuilderGetApplicationMethodInfo
    ResolveBuilderMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveBuilderMethod "getObject" o = BuilderGetObjectMethodInfo
    ResolveBuilderMethod "getObjects" o = BuilderGetObjectsMethodInfo
    ResolveBuilderMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveBuilderMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveBuilderMethod "getTranslationDomain" o = BuilderGetTranslationDomainMethodInfo
    ResolveBuilderMethod "getTypeFromName" o = BuilderGetTypeFromNameMethodInfo
    ResolveBuilderMethod "setApplication" o = BuilderSetApplicationMethodInfo
    ResolveBuilderMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveBuilderMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveBuilderMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveBuilderMethod "setTranslationDomain" o = BuilderSetTranslationDomainMethodInfo
    ResolveBuilderMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveBuilderMethod t Builder, O.OverloadedMethod info Builder p) => OL.IsLabel t (Builder -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveBuilderMethod t Builder, O.OverloadedMethod info Builder p, R.HasField t Builder p) => R.HasField t Builder p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveBuilderMethod t Builder, O.OverloadedMethodInfo info Builder) => OL.IsLabel t (O.MethodProxy info Builder) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- VVV Prop "translation-domain"
   -- Type: TBasicType TUTF8
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just True)

-- | Get the value of the “@translation-domain@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' builder #translationDomain
-- @
getBuilderTranslationDomain :: (MonadIO m, IsBuilder o) => o -> m T.Text
getBuilderTranslationDomain obj = MIO.liftIO $ checkUnexpectedNothing "getBuilderTranslationDomain" $ B.Properties.getObjectPropertyString obj "translation-domain"

-- | Set the value of the “@translation-domain@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' builder [ #translationDomain 'Data.GI.Base.Attributes.:=' value ]
-- @
setBuilderTranslationDomain :: (MonadIO m, IsBuilder o) => o -> T.Text -> m ()
setBuilderTranslationDomain obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "translation-domain" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@translation-domain@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructBuilderTranslationDomain :: (IsBuilder o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructBuilderTranslationDomain val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "translation-domain" (P.Just val)

-- | Set the value of the “@translation-domain@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #translationDomain
-- @
clearBuilderTranslationDomain :: (MonadIO m, IsBuilder o) => o -> m ()
clearBuilderTranslationDomain obj = liftIO $ B.Properties.setObjectPropertyString obj "translation-domain" (Nothing :: Maybe T.Text)

#if defined(ENABLE_OVERLOADING)
data BuilderTranslationDomainPropertyInfo
instance AttrInfo BuilderTranslationDomainPropertyInfo where
    type AttrAllowedOps BuilderTranslationDomainPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint BuilderTranslationDomainPropertyInfo = IsBuilder
    type AttrSetTypeConstraint BuilderTranslationDomainPropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint BuilderTranslationDomainPropertyInfo = (~) T.Text
    type AttrTransferType BuilderTranslationDomainPropertyInfo = T.Text
    type AttrGetType BuilderTranslationDomainPropertyInfo = T.Text
    type AttrLabel BuilderTranslationDomainPropertyInfo = "translation-domain"
    type AttrOrigin BuilderTranslationDomainPropertyInfo = Builder
    attrGet = getBuilderTranslationDomain
    attrSet = setBuilderTranslationDomain
    attrTransfer _ v = do
        return v
    attrConstruct = constructBuilderTranslationDomain
    attrClear = clearBuilderTranslationDomain
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Builder.translationDomain"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Builder.html#g:attr:translationDomain"
        })
#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList Builder
type instance O.AttributeList Builder = BuilderAttributeList
type BuilderAttributeList = ('[ '("translationDomain", BuilderTranslationDomainPropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
builderTranslationDomain :: AttrLabelProxy "translationDomain"
builderTranslationDomain = AttrLabelProxy

#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList Builder = BuilderSignalList
type BuilderSignalList = ('[ '("notify", GObject.Object.ObjectNotifySignalInfo)] :: [(Symbol, *)])

#endif

-- method Builder::new
-- method type : Constructor
-- Args: []
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "Builder" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_builder_new" gtk_builder_new :: 
    IO (Ptr Builder)

-- | Creates a new empty builder object.
-- 
-- This function is only useful if you intend to make multiple calls
-- to 'GI.Gtk.Objects.Builder.builderAddFromFile', 'GI.Gtk.Objects.Builder.builderAddFromResource'
-- or 'GI.Gtk.Objects.Builder.builderAddFromString' in order to merge multiple UI
-- descriptions into a single builder.
-- 
-- Most users will probably want to use 'GI.Gtk.Objects.Builder.builderNewFromFile',
-- 'GI.Gtk.Objects.Builder.builderNewFromResource' or 'GI.Gtk.Objects.Builder.builderNewFromString'.
-- 
-- /Since: 2.12/
builderNew ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m Builder
    -- ^ __Returns:__ a new (empty) t'GI.Gtk.Objects.Builder.Builder' object
builderNew  = liftIO $ do
    result <- gtk_builder_new
    checkUnexpectedReturnNULL "builderNew" result
    result' <- (wrapObject Builder) result
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method Builder::new_from_file
-- method type : Constructor
-- Args: [ Arg
--           { argCName = "filename"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "filename of user interface description file"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "Builder" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_builder_new_from_file" gtk_builder_new_from_file :: 
    CString ->                              -- filename : TBasicType TUTF8
    IO (Ptr Builder)

-- | Builds the [GtkBuilder UI definition][BUILDER-UI]
-- in the file /@filename@/.
-- 
-- If there is an error opening the file or parsing the description then
-- the program will be aborted.  You should only ever attempt to parse
-- user interface descriptions that are shipped as part of your program.
-- 
-- /Since: 3.10/
builderNewFromFile ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    T.Text
    -- ^ /@filename@/: filename of user interface description file
    -> m Builder
    -- ^ __Returns:__ a t'GI.Gtk.Objects.Builder.Builder' containing the described interface
builderNewFromFile filename = liftIO $ do
    filename' <- textToCString filename
    result <- gtk_builder_new_from_file filename'
    checkUnexpectedReturnNULL "builderNewFromFile" result
    result' <- (wrapObject Builder) result
    freeMem filename'
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method Builder::new_from_resource
-- method type : Constructor
-- Args: [ Arg
--           { argCName = "resource_path"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GResource resource path"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "Builder" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_builder_new_from_resource" gtk_builder_new_from_resource :: 
    CString ->                              -- resource_path : TBasicType TUTF8
    IO (Ptr Builder)

-- | Builds the [GtkBuilder UI definition][BUILDER-UI]
-- at /@resourcePath@/.
-- 
-- If there is an error locating the resource or parsing the
-- description, then the program will be aborted.
-- 
-- /Since: 3.10/
builderNewFromResource ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    T.Text
    -- ^ /@resourcePath@/: a t'GI.Gio.Structs.Resource.Resource' resource path
    -> m Builder
    -- ^ __Returns:__ a t'GI.Gtk.Objects.Builder.Builder' containing the described interface
builderNewFromResource resourcePath = liftIO $ do
    resourcePath' <- textToCString resourcePath
    result <- gtk_builder_new_from_resource resourcePath'
    checkUnexpectedReturnNULL "builderNewFromResource" result
    result' <- (wrapObject Builder) result
    freeMem resourcePath'
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method Builder::new_from_string
-- method type : Constructor
-- Args: [ Arg
--           { argCName = "string"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a user interface (XML) description"
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
--                 { rawDocText = Just "the length of @string, or -1"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "Builder" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_builder_new_from_string" gtk_builder_new_from_string :: 
    CString ->                              -- string : TBasicType TUTF8
    Int64 ->                                -- length : TBasicType TInt64
    IO (Ptr Builder)

-- | Builds the user interface described by /@string@/ (in the
-- [GtkBuilder UI definition][BUILDER-UI] format).
-- 
-- If /@string@/ is 'P.Nothing'-terminated, then /@length@/ should be -1.
-- If /@length@/ is not -1, then it is the length of /@string@/.
-- 
-- If there is an error parsing /@string@/ then the program will be
-- aborted. You should not attempt to parse user interface description
-- from untrusted sources.
-- 
-- /Since: 3.10/
builderNewFromString ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    T.Text
    -- ^ /@string@/: a user interface (XML) description
    -> Int64
    -- ^ /@length@/: the length of /@string@/, or -1
    -> m Builder
    -- ^ __Returns:__ a t'GI.Gtk.Objects.Builder.Builder' containing the interface described by /@string@/
builderNewFromString string length_ = liftIO $ do
    string' <- textToCString string
    result <- gtk_builder_new_from_string string' length_
    checkUnexpectedReturnNULL "builderNewFromString" result
    result' <- (wrapObject Builder) result
    freeMem string'
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method Builder::add_callback_symbol
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "builder"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Builder" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkBuilder" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "callback_name"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "The name of the callback, as expected in the XML"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "callback_symbol"
--           , argType =
--               TInterface Name { namespace = "GObject" , name = "Callback" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "The callback pointer"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeNotified
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

foreign import ccall "gtk_builder_add_callback_symbol" gtk_builder_add_callback_symbol :: 
    Ptr Builder ->                          -- builder : TInterface (Name {namespace = "Gtk", name = "Builder"})
    CString ->                              -- callback_name : TBasicType TUTF8
    FunPtr GObject.Callbacks.C_Callback ->  -- callback_symbol : TInterface (Name {namespace = "GObject", name = "Callback"})
    IO ()

-- | Adds the /@callbackSymbol@/ to the scope of /@builder@/ under the given /@callbackName@/.
-- 
-- Using this function overrides the behavior of 'GI.Gtk.Objects.Builder.builderConnectSignals'
-- for any callback symbols that are added. Using this method allows for better
-- encapsulation as it does not require that callback symbols be declared in
-- the global namespace.
-- 
-- /Since: 3.10/
builderAddCallbackSymbol ::
    (B.CallStack.HasCallStack, MonadIO m, IsBuilder a) =>
    a
    -- ^ /@builder@/: a t'GI.Gtk.Objects.Builder.Builder'
    -> T.Text
    -- ^ /@callbackName@/: The name of the callback, as expected in the XML
    -> GObject.Callbacks.Callback
    -- ^ /@callbackSymbol@/: The callback pointer
    -> m ()
builderAddCallbackSymbol builder callbackName callbackSymbol = liftIO $ do
    builder' <- unsafeManagedPtrCastPtr builder
    callbackName' <- textToCString callbackName
    callbackSymbol' <- GObject.Callbacks.mk_Callback (GObject.Callbacks.wrap_Callback Nothing callbackSymbol)
    gtk_builder_add_callback_symbol builder' callbackName' callbackSymbol'
    touchManagedPtr builder
    freeMem callbackName'
    return ()

#if defined(ENABLE_OVERLOADING)
data BuilderAddCallbackSymbolMethodInfo
instance (signature ~ (T.Text -> GObject.Callbacks.Callback -> m ()), MonadIO m, IsBuilder a) => O.OverloadedMethod BuilderAddCallbackSymbolMethodInfo a signature where
    overloadedMethod = builderAddCallbackSymbol

instance O.OverloadedMethodInfo BuilderAddCallbackSymbolMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Builder.builderAddCallbackSymbol",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Builder.html#v:builderAddCallbackSymbol"
        })


#endif

-- method Builder::add_from_file
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "builder"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Builder" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkBuilder" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "filename"
--           , argType = TBasicType TUTF8
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

foreign import ccall "gtk_builder_add_from_file" gtk_builder_add_from_file :: 
    Ptr Builder ->                          -- builder : TInterface (Name {namespace = "Gtk", name = "Builder"})
    CString ->                              -- filename : TBasicType TUTF8
    Ptr (Ptr GError) ->                     -- error
    IO Word32

-- | Parses a file containing a [GtkBuilder UI definition][BUILDER-UI]
-- and merges it with the current contents of /@builder@/.
-- 
-- Most users will probably want to use 'GI.Gtk.Objects.Builder.builderNewFromFile'.
-- 
-- If an error occurs, 0 will be returned and /@error@/ will be assigned a
-- t'GError' from the @/GTK_BUILDER_ERROR/@, @/G_MARKUP_ERROR/@ or @/G_FILE_ERROR/@
-- domain.
-- 
-- It’s not really reasonable to attempt to handle failures of this
-- call. You should not use this function with untrusted files (ie:
-- files that are not part of your application). Broken t'GI.Gtk.Objects.Builder.Builder'
-- files can easily crash your program, and it’s possible that memory
-- was leaked leading up to the reported failure. The only reasonable
-- thing to do when an error is detected is to call @/g_error()/@.
-- 
-- /Since: 2.12/
builderAddFromFile ::
    (B.CallStack.HasCallStack, MonadIO m, IsBuilder a) =>
    a
    -- ^ /@builder@/: a t'GI.Gtk.Objects.Builder.Builder'
    -> T.Text
    -- ^ /@filename@/: the name of the file to parse
    -> m Word32
    -- ^ __Returns:__ A positive value on success, 0 if an error occurred /(Can throw 'Data.GI.Base.GError.GError')/
builderAddFromFile builder filename = liftIO $ do
    builder' <- unsafeManagedPtrCastPtr builder
    filename' <- textToCString filename
    onException (do
        result <- propagateGError $ gtk_builder_add_from_file builder' filename'
        touchManagedPtr builder
        freeMem filename'
        return result
     ) (do
        freeMem filename'
     )

#if defined(ENABLE_OVERLOADING)
data BuilderAddFromFileMethodInfo
instance (signature ~ (T.Text -> m Word32), MonadIO m, IsBuilder a) => O.OverloadedMethod BuilderAddFromFileMethodInfo a signature where
    overloadedMethod = builderAddFromFile

instance O.OverloadedMethodInfo BuilderAddFromFileMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Builder.builderAddFromFile",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Builder.html#v:builderAddFromFile"
        })


#endif

-- method Builder::add_from_resource
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "builder"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Builder" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkBuilder" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "the path of the resource file to parse"
--                 , sinceVersion = Nothing
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

foreign import ccall "gtk_builder_add_from_resource" gtk_builder_add_from_resource :: 
    Ptr Builder ->                          -- builder : TInterface (Name {namespace = "Gtk", name = "Builder"})
    CString ->                              -- resource_path : TBasicType TUTF8
    Ptr (Ptr GError) ->                     -- error
    IO Word32

-- | Parses a resource file containing a [GtkBuilder UI definition][BUILDER-UI]
-- and merges it with the current contents of /@builder@/.
-- 
-- Most users will probably want to use 'GI.Gtk.Objects.Builder.builderNewFromResource'.
-- 
-- If an error occurs, 0 will be returned and /@error@/ will be assigned a
-- t'GError' from the @/GTK_BUILDER_ERROR/@, @/G_MARKUP_ERROR/@ or @/G_RESOURCE_ERROR/@
-- domain.
-- 
-- It’s not really reasonable to attempt to handle failures of this
-- call.  The only reasonable thing to do when an error is detected is
-- to call @/g_error()/@.
-- 
-- /Since: 3.4/
builderAddFromResource ::
    (B.CallStack.HasCallStack, MonadIO m, IsBuilder a) =>
    a
    -- ^ /@builder@/: a t'GI.Gtk.Objects.Builder.Builder'
    -> T.Text
    -- ^ /@resourcePath@/: the path of the resource file to parse
    -> m Word32
    -- ^ __Returns:__ A positive value on success, 0 if an error occurred /(Can throw 'Data.GI.Base.GError.GError')/
builderAddFromResource builder resourcePath = liftIO $ do
    builder' <- unsafeManagedPtrCastPtr builder
    resourcePath' <- textToCString resourcePath
    onException (do
        result <- propagateGError $ gtk_builder_add_from_resource builder' resourcePath'
        touchManagedPtr builder
        freeMem resourcePath'
        return result
     ) (do
        freeMem resourcePath'
     )

#if defined(ENABLE_OVERLOADING)
data BuilderAddFromResourceMethodInfo
instance (signature ~ (T.Text -> m Word32), MonadIO m, IsBuilder a) => O.OverloadedMethod BuilderAddFromResourceMethodInfo a signature where
    overloadedMethod = builderAddFromResource

instance O.OverloadedMethodInfo BuilderAddFromResourceMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Builder.builderAddFromResource",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Builder.html#v:builderAddFromResource"
        })


#endif

-- method Builder::add_from_string
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "builder"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Builder" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkBuilder" , sinceVersion = Nothing }
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

foreign import ccall "gtk_builder_add_from_string" gtk_builder_add_from_string :: 
    Ptr Builder ->                          -- builder : TInterface (Name {namespace = "Gtk", name = "Builder"})
    CString ->                              -- buffer : TBasicType TUTF8
    Int64 ->                                -- length : TBasicType TInt64
    Ptr (Ptr GError) ->                     -- error
    IO Word32

-- | Parses a string containing a [GtkBuilder UI definition][BUILDER-UI]
-- and merges it with the current contents of /@builder@/.
-- 
-- Most users will probably want to use 'GI.Gtk.Objects.Builder.builderNewFromString'.
-- 
-- Upon errors 0 will be returned and /@error@/ will be assigned a
-- t'GError' from the @/GTK_BUILDER_ERROR/@, @/G_MARKUP_ERROR/@ or
-- @/G_VARIANT_PARSE_ERROR/@ domain.
-- 
-- It’s not really reasonable to attempt to handle failures of this
-- call.  The only reasonable thing to do when an error is detected is
-- to call @/g_error()/@.
-- 
-- /Since: 2.12/
builderAddFromString ::
    (B.CallStack.HasCallStack, MonadIO m, IsBuilder a) =>
    a
    -- ^ /@builder@/: a t'GI.Gtk.Objects.Builder.Builder'
    -> T.Text
    -- ^ /@buffer@/: the string to parse
    -> Int64
    -- ^ /@length@/: the length of /@buffer@/ (may be -1 if /@buffer@/ is nul-terminated)
    -> m Word32
    -- ^ __Returns:__ A positive value on success, 0 if an error occurred /(Can throw 'Data.GI.Base.GError.GError')/
builderAddFromString builder buffer length_ = liftIO $ do
    builder' <- unsafeManagedPtrCastPtr builder
    buffer' <- textToCString buffer
    onException (do
        result <- propagateGError $ gtk_builder_add_from_string builder' buffer' length_
        touchManagedPtr builder
        freeMem buffer'
        return result
     ) (do
        freeMem buffer'
     )

#if defined(ENABLE_OVERLOADING)
data BuilderAddFromStringMethodInfo
instance (signature ~ (T.Text -> Int64 -> m Word32), MonadIO m, IsBuilder a) => O.OverloadedMethod BuilderAddFromStringMethodInfo a signature where
    overloadedMethod = builderAddFromString

instance O.OverloadedMethodInfo BuilderAddFromStringMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Builder.builderAddFromString",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Builder.html#v:builderAddFromString"
        })


#endif

-- method Builder::add_objects_from_file
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "builder"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Builder" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkBuilder" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "filename"
--           , argType = TBasicType TUTF8
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
--       , Arg
--           { argCName = "object_ids"
--           , argType = TCArray True (-1) (-1) (TBasicType TUTF8)
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "nul-terminated array of objects to build"
--                 , sinceVersion = Nothing
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

foreign import ccall "gtk_builder_add_objects_from_file" gtk_builder_add_objects_from_file :: 
    Ptr Builder ->                          -- builder : TInterface (Name {namespace = "Gtk", name = "Builder"})
    CString ->                              -- filename : TBasicType TUTF8
    Ptr CString ->                          -- object_ids : TCArray True (-1) (-1) (TBasicType TUTF8)
    Ptr (Ptr GError) ->                     -- error
    IO Word32

-- | Parses a file containing a [GtkBuilder UI definition][BUILDER-UI]
-- building only the requested objects and merges
-- them with the current contents of /@builder@/.
-- 
-- Upon errors 0 will be returned and /@error@/ will be assigned a
-- t'GError' from the @/GTK_BUILDER_ERROR/@, @/G_MARKUP_ERROR/@ or @/G_FILE_ERROR/@
-- domain.
-- 
-- If you are adding an object that depends on an object that is not
-- its child (for instance a t'GI.Gtk.Objects.TreeView.TreeView' that depends on its
-- t'GI.Gtk.Interfaces.TreeModel.TreeModel'), you have to explicitly list all of them in /@objectIds@/.
-- 
-- /Since: 2.14/
builderAddObjectsFromFile ::
    (B.CallStack.HasCallStack, MonadIO m, IsBuilder a) =>
    a
    -- ^ /@builder@/: a t'GI.Gtk.Objects.Builder.Builder'
    -> T.Text
    -- ^ /@filename@/: the name of the file to parse
    -> [T.Text]
    -- ^ /@objectIds@/: nul-terminated array of objects to build
    -> m Word32
    -- ^ __Returns:__ A positive value on success, 0 if an error occurred /(Can throw 'Data.GI.Base.GError.GError')/
builderAddObjectsFromFile builder filename objectIds = liftIO $ do
    builder' <- unsafeManagedPtrCastPtr builder
    filename' <- textToCString filename
    objectIds' <- packZeroTerminatedUTF8CArray objectIds
    onException (do
        result <- propagateGError $ gtk_builder_add_objects_from_file builder' filename' objectIds'
        touchManagedPtr builder
        freeMem filename'
        mapZeroTerminatedCArray freeMem objectIds'
        freeMem objectIds'
        return result
     ) (do
        freeMem filename'
        mapZeroTerminatedCArray freeMem objectIds'
        freeMem objectIds'
     )

#if defined(ENABLE_OVERLOADING)
data BuilderAddObjectsFromFileMethodInfo
instance (signature ~ (T.Text -> [T.Text] -> m Word32), MonadIO m, IsBuilder a) => O.OverloadedMethod BuilderAddObjectsFromFileMethodInfo a signature where
    overloadedMethod = builderAddObjectsFromFile

instance O.OverloadedMethodInfo BuilderAddObjectsFromFileMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Builder.builderAddObjectsFromFile",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Builder.html#v:builderAddObjectsFromFile"
        })


#endif

-- method Builder::add_objects_from_resource
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "builder"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Builder" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkBuilder" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "the path of the resource file to parse"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "object_ids"
--           , argType = TCArray True (-1) (-1) (TBasicType TUTF8)
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "nul-terminated array of objects to build"
--                 , sinceVersion = Nothing
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

foreign import ccall "gtk_builder_add_objects_from_resource" gtk_builder_add_objects_from_resource :: 
    Ptr Builder ->                          -- builder : TInterface (Name {namespace = "Gtk", name = "Builder"})
    CString ->                              -- resource_path : TBasicType TUTF8
    Ptr CString ->                          -- object_ids : TCArray True (-1) (-1) (TBasicType TUTF8)
    Ptr (Ptr GError) ->                     -- error
    IO Word32

-- | Parses a resource file containing a [GtkBuilder UI definition][BUILDER-UI]
-- building only the requested objects and merges
-- them with the current contents of /@builder@/.
-- 
-- Upon errors 0 will be returned and /@error@/ will be assigned a
-- t'GError' from the @/GTK_BUILDER_ERROR/@, @/G_MARKUP_ERROR/@ or @/G_RESOURCE_ERROR/@
-- domain.
-- 
-- If you are adding an object that depends on an object that is not
-- its child (for instance a t'GI.Gtk.Objects.TreeView.TreeView' that depends on its
-- t'GI.Gtk.Interfaces.TreeModel.TreeModel'), you have to explicitly list all of them in /@objectIds@/.
-- 
-- /Since: 3.4/
builderAddObjectsFromResource ::
    (B.CallStack.HasCallStack, MonadIO m, IsBuilder a) =>
    a
    -- ^ /@builder@/: a t'GI.Gtk.Objects.Builder.Builder'
    -> T.Text
    -- ^ /@resourcePath@/: the path of the resource file to parse
    -> [T.Text]
    -- ^ /@objectIds@/: nul-terminated array of objects to build
    -> m Word32
    -- ^ __Returns:__ A positive value on success, 0 if an error occurred /(Can throw 'Data.GI.Base.GError.GError')/
builderAddObjectsFromResource builder resourcePath objectIds = liftIO $ do
    builder' <- unsafeManagedPtrCastPtr builder
    resourcePath' <- textToCString resourcePath
    objectIds' <- packZeroTerminatedUTF8CArray objectIds
    onException (do
        result <- propagateGError $ gtk_builder_add_objects_from_resource builder' resourcePath' objectIds'
        touchManagedPtr builder
        freeMem resourcePath'
        mapZeroTerminatedCArray freeMem objectIds'
        freeMem objectIds'
        return result
     ) (do
        freeMem resourcePath'
        mapZeroTerminatedCArray freeMem objectIds'
        freeMem objectIds'
     )

#if defined(ENABLE_OVERLOADING)
data BuilderAddObjectsFromResourceMethodInfo
instance (signature ~ (T.Text -> [T.Text] -> m Word32), MonadIO m, IsBuilder a) => O.OverloadedMethod BuilderAddObjectsFromResourceMethodInfo a signature where
    overloadedMethod = builderAddObjectsFromResource

instance O.OverloadedMethodInfo BuilderAddObjectsFromResourceMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Builder.builderAddObjectsFromResource",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Builder.html#v:builderAddObjectsFromResource"
        })


#endif

-- method Builder::add_objects_from_string
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "builder"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Builder" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkBuilder" , sinceVersion = Nothing }
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
--           , argType = TBasicType TUInt64
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
--       , Arg
--           { argCName = "object_ids"
--           , argType = TCArray True (-1) (-1) (TBasicType TUTF8)
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "nul-terminated array of objects to build"
--                 , sinceVersion = Nothing
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

foreign import ccall "gtk_builder_add_objects_from_string" gtk_builder_add_objects_from_string :: 
    Ptr Builder ->                          -- builder : TInterface (Name {namespace = "Gtk", name = "Builder"})
    CString ->                              -- buffer : TBasicType TUTF8
    Word64 ->                               -- length : TBasicType TUInt64
    Ptr CString ->                          -- object_ids : TCArray True (-1) (-1) (TBasicType TUTF8)
    Ptr (Ptr GError) ->                     -- error
    IO Word32

-- | Parses a string containing a [GtkBuilder UI definition][BUILDER-UI]
-- building only the requested objects and merges
-- them with the current contents of /@builder@/.
-- 
-- Upon errors 0 will be returned and /@error@/ will be assigned a
-- t'GError' from the @/GTK_BUILDER_ERROR/@ or @/G_MARKUP_ERROR/@ domain.
-- 
-- If you are adding an object that depends on an object that is not
-- its child (for instance a t'GI.Gtk.Objects.TreeView.TreeView' that depends on its
-- t'GI.Gtk.Interfaces.TreeModel.TreeModel'), you have to explicitly list all of them in /@objectIds@/.
-- 
-- /Since: 2.14/
builderAddObjectsFromString ::
    (B.CallStack.HasCallStack, MonadIO m, IsBuilder a) =>
    a
    -- ^ /@builder@/: a t'GI.Gtk.Objects.Builder.Builder'
    -> T.Text
    -- ^ /@buffer@/: the string to parse
    -> Word64
    -- ^ /@length@/: the length of /@buffer@/ (may be -1 if /@buffer@/ is nul-terminated)
    -> [T.Text]
    -- ^ /@objectIds@/: nul-terminated array of objects to build
    -> m Word32
    -- ^ __Returns:__ A positive value on success, 0 if an error occurred /(Can throw 'Data.GI.Base.GError.GError')/
builderAddObjectsFromString builder buffer length_ objectIds = liftIO $ do
    builder' <- unsafeManagedPtrCastPtr builder
    buffer' <- textToCString buffer
    objectIds' <- packZeroTerminatedUTF8CArray objectIds
    onException (do
        result <- propagateGError $ gtk_builder_add_objects_from_string builder' buffer' length_ objectIds'
        touchManagedPtr builder
        freeMem buffer'
        mapZeroTerminatedCArray freeMem objectIds'
        freeMem objectIds'
        return result
     ) (do
        freeMem buffer'
        mapZeroTerminatedCArray freeMem objectIds'
        freeMem objectIds'
     )

#if defined(ENABLE_OVERLOADING)
data BuilderAddObjectsFromStringMethodInfo
instance (signature ~ (T.Text -> Word64 -> [T.Text] -> m Word32), MonadIO m, IsBuilder a) => O.OverloadedMethod BuilderAddObjectsFromStringMethodInfo a signature where
    overloadedMethod = builderAddObjectsFromString

instance O.OverloadedMethodInfo BuilderAddObjectsFromStringMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Builder.builderAddObjectsFromString",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Builder.html#v:builderAddObjectsFromString"
        })


#endif

-- method Builder::connect_signals
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "builder"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Builder" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkBuilder" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
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
--                 { rawDocText = Just "user data to pass back with all signals"
--                 , sinceVersion = Nothing
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

foreign import ccall "gtk_builder_connect_signals" gtk_builder_connect_signals :: 
    Ptr Builder ->                          -- builder : TInterface (Name {namespace = "Gtk", name = "Builder"})
    Ptr () ->                               -- user_data : TBasicType TPtr
    IO ()

-- | This method is a simpler variation of 'GI.Gtk.Objects.Builder.builderConnectSignalsFull'.
-- It uses symbols explicitly added to /@builder@/ with prior calls to
-- 'GI.Gtk.Objects.Builder.builderAddCallbackSymbol'. In the case that symbols are not
-- explicitly added; it uses t'GI.GModule.Structs.Module.Module'’s introspective features (by opening the module 'P.Nothing')
-- to look at the application’s symbol table. From here it tries to match
-- the signal handler names given in the interface description with
-- symbols in the application and connects the signals. Note that this
-- function can only be called once, subsequent calls will do nothing.
-- 
-- Note that unless 'GI.Gtk.Objects.Builder.builderAddCallbackSymbol' is called for
-- all signal callbacks which are referenced by the loaded XML, this
-- function will require that t'GI.GModule.Structs.Module.Module' be supported on the platform.
-- 
-- If you rely on t'GI.GModule.Structs.Module.Module' support to lookup callbacks in the symbol table,
-- the following details should be noted:
-- 
-- When compiling applications for Windows, you must declare signal callbacks
-- with @/G_MODULE_EXPORT/@, or they will not be put in the symbol table.
-- On Linux and Unices, this is not necessary; applications should instead
-- be compiled with the -Wl,--export-dynamic CFLAGS, and linked against
-- gmodule-export-2.0.
-- 
-- /Since: 2.12/
builderConnectSignals ::
    (B.CallStack.HasCallStack, MonadIO m, IsBuilder a) =>
    a
    -- ^ /@builder@/: a t'GI.Gtk.Objects.Builder.Builder'
    -> Ptr ()
    -- ^ /@userData@/: user data to pass back with all signals
    -> m ()
builderConnectSignals builder userData = liftIO $ do
    builder' <- unsafeManagedPtrCastPtr builder
    gtk_builder_connect_signals builder' userData
    touchManagedPtr builder
    return ()

#if defined(ENABLE_OVERLOADING)
data BuilderConnectSignalsMethodInfo
instance (signature ~ (Ptr () -> m ()), MonadIO m, IsBuilder a) => O.OverloadedMethod BuilderConnectSignalsMethodInfo a signature where
    overloadedMethod = builderConnectSignals

instance O.OverloadedMethodInfo BuilderConnectSignalsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Builder.builderConnectSignals",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Builder.html#v:builderConnectSignals"
        })


#endif

-- method Builder::connect_signals_full
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "builder"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Builder" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkBuilder" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "func"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "BuilderConnectFunc" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the function used to connect the signals"
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
--                 { rawDocText =
--                     Just
--                       "arbitrary data that will be passed to the connection function"
--                 , sinceVersion = Nothing
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

foreign import ccall "gtk_builder_connect_signals_full" gtk_builder_connect_signals_full :: 
    Ptr Builder ->                          -- builder : TInterface (Name {namespace = "Gtk", name = "Builder"})
    FunPtr Gtk.Callbacks.C_BuilderConnectFunc -> -- func : TInterface (Name {namespace = "Gtk", name = "BuilderConnectFunc"})
    Ptr () ->                               -- user_data : TBasicType TPtr
    IO ()

-- | This function can be thought of the interpreted language binding
-- version of 'GI.Gtk.Objects.Builder.builderConnectSignals', except that it does not
-- require GModule to function correctly.
-- 
-- /Since: 2.12/
builderConnectSignalsFull ::
    (B.CallStack.HasCallStack, MonadIO m, IsBuilder a) =>
    a
    -- ^ /@builder@/: a t'GI.Gtk.Objects.Builder.Builder'
    -> Gtk.Callbacks.BuilderConnectFunc
    -- ^ /@func@/: the function used to connect the signals
    -> m ()
builderConnectSignalsFull builder func = liftIO $ do
    builder' <- unsafeManagedPtrCastPtr builder
    func' <- Gtk.Callbacks.mk_BuilderConnectFunc (Gtk.Callbacks.wrap_BuilderConnectFunc Nothing (Gtk.Callbacks.drop_closures_BuilderConnectFunc func))
    let userData = nullPtr
    gtk_builder_connect_signals_full builder' func' userData
    safeFreeFunPtr $ castFunPtrToPtr func'
    touchManagedPtr builder
    return ()

#if defined(ENABLE_OVERLOADING)
data BuilderConnectSignalsFullMethodInfo
instance (signature ~ (Gtk.Callbacks.BuilderConnectFunc -> m ()), MonadIO m, IsBuilder a) => O.OverloadedMethod BuilderConnectSignalsFullMethodInfo a signature where
    overloadedMethod = builderConnectSignalsFull

instance O.OverloadedMethodInfo BuilderConnectSignalsFullMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Builder.builderConnectSignalsFull",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Builder.html#v:builderConnectSignalsFull"
        })


#endif

-- method Builder::expose_object
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "builder"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Builder" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkBuilder" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "the name of the object exposed to the builder"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "object"
--           , argType =
--               TInterface Name { namespace = "GObject" , name = "Object" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the object to expose"
--                 , sinceVersion = Nothing
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

foreign import ccall "gtk_builder_expose_object" gtk_builder_expose_object :: 
    Ptr Builder ->                          -- builder : TInterface (Name {namespace = "Gtk", name = "Builder"})
    CString ->                              -- name : TBasicType TUTF8
    Ptr GObject.Object.Object ->            -- object : TInterface (Name {namespace = "GObject", name = "Object"})
    IO ()

-- | Add /@object@/ to the /@builder@/ object pool so it can be referenced just like any
-- other object built by builder.
-- 
-- /Since: 3.8/
builderExposeObject ::
    (B.CallStack.HasCallStack, MonadIO m, IsBuilder a, GObject.Object.IsObject b) =>
    a
    -- ^ /@builder@/: a t'GI.Gtk.Objects.Builder.Builder'
    -> T.Text
    -- ^ /@name@/: the name of the object exposed to the builder
    -> b
    -- ^ /@object@/: the object to expose
    -> m ()
builderExposeObject builder name object = liftIO $ do
    builder' <- unsafeManagedPtrCastPtr builder
    name' <- textToCString name
    object' <- unsafeManagedPtrCastPtr object
    gtk_builder_expose_object builder' name' object'
    touchManagedPtr builder
    touchManagedPtr object
    freeMem name'
    return ()

#if defined(ENABLE_OVERLOADING)
data BuilderExposeObjectMethodInfo
instance (signature ~ (T.Text -> b -> m ()), MonadIO m, IsBuilder a, GObject.Object.IsObject b) => O.OverloadedMethod BuilderExposeObjectMethodInfo a signature where
    overloadedMethod = builderExposeObject

instance O.OverloadedMethodInfo BuilderExposeObjectMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Builder.builderExposeObject",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Builder.html#v:builderExposeObject"
        })


#endif

-- method Builder::extend_with_template
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "builder"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Builder" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkBuilder" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "the widget that is being extended"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "template_type"
--           , argType = TBasicType TGType
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the type that the template is for"
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
--           , argType = TBasicType TUInt64
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

foreign import ccall "gtk_builder_extend_with_template" gtk_builder_extend_with_template :: 
    Ptr Builder ->                          -- builder : TInterface (Name {namespace = "Gtk", name = "Builder"})
    Ptr Gtk.Widget.Widget ->                -- widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    CGType ->                               -- template_type : TBasicType TGType
    CString ->                              -- buffer : TBasicType TUTF8
    Word64 ->                               -- length : TBasicType TUInt64
    Ptr (Ptr GError) ->                     -- error
    IO Word32

-- | Main private entry point for building composite container
-- components from template XML.
-- 
-- This is exported purely to let gtk-builder-tool validate
-- templates, applications have no need to call this function.
builderExtendWithTemplate ::
    (B.CallStack.HasCallStack, MonadIO m, IsBuilder a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@builder@/: a t'GI.Gtk.Objects.Builder.Builder'
    -> b
    -- ^ /@widget@/: the widget that is being extended
    -> GType
    -- ^ /@templateType@/: the type that the template is for
    -> T.Text
    -- ^ /@buffer@/: the string to parse
    -> Word64
    -- ^ /@length@/: the length of /@buffer@/ (may be -1 if /@buffer@/ is nul-terminated)
    -> m Word32
    -- ^ __Returns:__ A positive value on success, 0 if an error occurred /(Can throw 'Data.GI.Base.GError.GError')/
builderExtendWithTemplate builder widget templateType buffer length_ = liftIO $ do
    builder' <- unsafeManagedPtrCastPtr builder
    widget' <- unsafeManagedPtrCastPtr widget
    let templateType' = gtypeToCGType templateType
    buffer' <- textToCString buffer
    onException (do
        result <- propagateGError $ gtk_builder_extend_with_template builder' widget' templateType' buffer' length_
        touchManagedPtr builder
        touchManagedPtr widget
        freeMem buffer'
        return result
     ) (do
        freeMem buffer'
     )

#if defined(ENABLE_OVERLOADING)
data BuilderExtendWithTemplateMethodInfo
instance (signature ~ (b -> GType -> T.Text -> Word64 -> m Word32), MonadIO m, IsBuilder a, Gtk.Widget.IsWidget b) => O.OverloadedMethod BuilderExtendWithTemplateMethodInfo a signature where
    overloadedMethod = builderExtendWithTemplate

instance O.OverloadedMethodInfo BuilderExtendWithTemplateMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Builder.builderExtendWithTemplate",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Builder.html#v:builderExtendWithTemplate"
        })


#endif

-- method Builder::get_application
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "builder"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Builder" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkBuilder" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "Application" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_builder_get_application" gtk_builder_get_application :: 
    Ptr Builder ->                          -- builder : TInterface (Name {namespace = "Gtk", name = "Builder"})
    IO (Ptr Gtk.Application.Application)

-- | Gets the t'GI.Gtk.Objects.Application.Application' associated with the builder.
-- 
-- The t'GI.Gtk.Objects.Application.Application' is used for creating action proxies as requested
-- from XML that the builder is loading.
-- 
-- By default, the builder uses the default application: the one from
-- 'GI.Gio.Objects.Application.applicationGetDefault'. If you want to use another application
-- for constructing proxies, use 'GI.Gtk.Objects.Builder.builderSetApplication'.
-- 
-- /Since: 3.10/
builderGetApplication ::
    (B.CallStack.HasCallStack, MonadIO m, IsBuilder a) =>
    a
    -- ^ /@builder@/: a t'GI.Gtk.Objects.Builder.Builder'
    -> m (Maybe Gtk.Application.Application)
    -- ^ __Returns:__ the application being used by the builder,
    --     or 'P.Nothing'
builderGetApplication builder = liftIO $ do
    builder' <- unsafeManagedPtrCastPtr builder
    result <- gtk_builder_get_application builder'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (newObject Gtk.Application.Application) result'
        return result''
    touchManagedPtr builder
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data BuilderGetApplicationMethodInfo
instance (signature ~ (m (Maybe Gtk.Application.Application)), MonadIO m, IsBuilder a) => O.OverloadedMethod BuilderGetApplicationMethodInfo a signature where
    overloadedMethod = builderGetApplication

instance O.OverloadedMethodInfo BuilderGetApplicationMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Builder.builderGetApplication",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Builder.html#v:builderGetApplication"
        })


#endif

-- method Builder::get_object
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "builder"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Builder" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkBuilder" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "name of object to get"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "GObject" , name = "Object" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_builder_get_object" gtk_builder_get_object :: 
    Ptr Builder ->                          -- builder : TInterface (Name {namespace = "Gtk", name = "Builder"})
    CString ->                              -- name : TBasicType TUTF8
    IO (Ptr GObject.Object.Object)

-- | Gets the object named /@name@/. Note that this function does not
-- increment the reference count of the returned object.
-- 
-- /Since: 2.12/
builderGetObject ::
    (B.CallStack.HasCallStack, MonadIO m, IsBuilder a) =>
    a
    -- ^ /@builder@/: a t'GI.Gtk.Objects.Builder.Builder'
    -> T.Text
    -- ^ /@name@/: name of object to get
    -> m (Maybe GObject.Object.Object)
    -- ^ __Returns:__ the object named /@name@/ or 'P.Nothing' if
    --    it could not be found in the object tree.
builderGetObject builder name = liftIO $ do
    builder' <- unsafeManagedPtrCastPtr builder
    name' <- textToCString name
    result <- gtk_builder_get_object builder' name'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (newObject GObject.Object.Object) result'
        return result''
    touchManagedPtr builder
    freeMem name'
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data BuilderGetObjectMethodInfo
instance (signature ~ (T.Text -> m (Maybe GObject.Object.Object)), MonadIO m, IsBuilder a) => O.OverloadedMethod BuilderGetObjectMethodInfo a signature where
    overloadedMethod = builderGetObject

instance O.OverloadedMethodInfo BuilderGetObjectMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Builder.builderGetObject",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Builder.html#v:builderGetObject"
        })


#endif

-- method Builder::get_objects
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "builder"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Builder" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkBuilder" , sinceVersion = Nothing }
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
--                  (TInterface Name { namespace = "GObject" , name = "Object" }))
-- throws : False
-- Skip return : False

foreign import ccall "gtk_builder_get_objects" gtk_builder_get_objects :: 
    Ptr Builder ->                          -- builder : TInterface (Name {namespace = "Gtk", name = "Builder"})
    IO (Ptr (GSList (Ptr GObject.Object.Object)))

-- | Gets all objects that have been constructed by /@builder@/. Note that
-- this function does not increment the reference counts of the returned
-- objects.
-- 
-- /Since: 2.12/
builderGetObjects ::
    (B.CallStack.HasCallStack, MonadIO m, IsBuilder a) =>
    a
    -- ^ /@builder@/: a t'GI.Gtk.Objects.Builder.Builder'
    -> m [GObject.Object.Object]
    -- ^ __Returns:__ a newly-allocated t'GI.GLib.Structs.SList.SList' containing all the objects
    --   constructed by the t'GI.Gtk.Objects.Builder.Builder' instance. It should be freed by
    --   @/g_slist_free()/@
builderGetObjects builder = liftIO $ do
    builder' <- unsafeManagedPtrCastPtr builder
    result <- gtk_builder_get_objects builder'
    result' <- unpackGSList result
    result'' <- mapM (newObject GObject.Object.Object) result'
    g_slist_free result
    touchManagedPtr builder
    return result''

#if defined(ENABLE_OVERLOADING)
data BuilderGetObjectsMethodInfo
instance (signature ~ (m [GObject.Object.Object]), MonadIO m, IsBuilder a) => O.OverloadedMethod BuilderGetObjectsMethodInfo a signature where
    overloadedMethod = builderGetObjects

instance O.OverloadedMethodInfo BuilderGetObjectsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Builder.builderGetObjects",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Builder.html#v:builderGetObjects"
        })


#endif

-- method Builder::get_translation_domain
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "builder"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Builder" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkBuilder" , sinceVersion = Nothing }
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

foreign import ccall "gtk_builder_get_translation_domain" gtk_builder_get_translation_domain :: 
    Ptr Builder ->                          -- builder : TInterface (Name {namespace = "Gtk", name = "Builder"})
    IO CString

-- | Gets the translation domain of /@builder@/.
-- 
-- /Since: 2.12/
builderGetTranslationDomain ::
    (B.CallStack.HasCallStack, MonadIO m, IsBuilder a) =>
    a
    -- ^ /@builder@/: a t'GI.Gtk.Objects.Builder.Builder'
    -> m T.Text
    -- ^ __Returns:__ the translation domain. This string is owned
    -- by the builder object and must not be modified or freed.
builderGetTranslationDomain builder = liftIO $ do
    builder' <- unsafeManagedPtrCastPtr builder
    result <- gtk_builder_get_translation_domain builder'
    checkUnexpectedReturnNULL "builderGetTranslationDomain" result
    result' <- cstringToText result
    touchManagedPtr builder
    return result'

#if defined(ENABLE_OVERLOADING)
data BuilderGetTranslationDomainMethodInfo
instance (signature ~ (m T.Text), MonadIO m, IsBuilder a) => O.OverloadedMethod BuilderGetTranslationDomainMethodInfo a signature where
    overloadedMethod = builderGetTranslationDomain

instance O.OverloadedMethodInfo BuilderGetTranslationDomainMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Builder.builderGetTranslationDomain",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Builder.html#v:builderGetTranslationDomain"
        })


#endif

-- method Builder::get_type_from_name
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "builder"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Builder" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkBuilder" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "type_name"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "type name to lookup"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TBasicType TGType)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_builder_get_type_from_name" gtk_builder_get_type_from_name :: 
    Ptr Builder ->                          -- builder : TInterface (Name {namespace = "Gtk", name = "Builder"})
    CString ->                              -- type_name : TBasicType TUTF8
    IO CGType

-- | Looks up a type by name, using the virtual function that
-- t'GI.Gtk.Objects.Builder.Builder' has for that purpose. This is mainly used when
-- implementing the t'GI.Gtk.Interfaces.Buildable.Buildable' interface on a type.
-- 
-- /Since: 2.12/
builderGetTypeFromName ::
    (B.CallStack.HasCallStack, MonadIO m, IsBuilder a) =>
    a
    -- ^ /@builder@/: a t'GI.Gtk.Objects.Builder.Builder'
    -> T.Text
    -- ^ /@typeName@/: type name to lookup
    -> m GType
    -- ^ __Returns:__ the t'GType' found for /@typeName@/ or @/G_TYPE_INVALID/@
    --   if no type was found
builderGetTypeFromName builder typeName = liftIO $ do
    builder' <- unsafeManagedPtrCastPtr builder
    typeName' <- textToCString typeName
    result <- gtk_builder_get_type_from_name builder' typeName'
    let result' = GType result
    touchManagedPtr builder
    freeMem typeName'
    return result'

#if defined(ENABLE_OVERLOADING)
data BuilderGetTypeFromNameMethodInfo
instance (signature ~ (T.Text -> m GType), MonadIO m, IsBuilder a) => O.OverloadedMethod BuilderGetTypeFromNameMethodInfo a signature where
    overloadedMethod = builderGetTypeFromName

instance O.OverloadedMethodInfo BuilderGetTypeFromNameMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Builder.builderGetTypeFromName",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Builder.html#v:builderGetTypeFromName"
        })


#endif

-- method Builder::set_application
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "builder"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Builder" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkBuilder" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "application"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Application" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkApplication" , sinceVersion = Nothing }
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

foreign import ccall "gtk_builder_set_application" gtk_builder_set_application :: 
    Ptr Builder ->                          -- builder : TInterface (Name {namespace = "Gtk", name = "Builder"})
    Ptr Gtk.Application.Application ->      -- application : TInterface (Name {namespace = "Gtk", name = "Application"})
    IO ()

-- | Sets the application associated with /@builder@/.
-- 
-- You only need this function if there is more than one t'GI.Gio.Objects.Application.Application'
-- in your process. /@application@/ cannot be 'P.Nothing'.
-- 
-- /Since: 3.10/
builderSetApplication ::
    (B.CallStack.HasCallStack, MonadIO m, IsBuilder a, Gtk.Application.IsApplication b) =>
    a
    -- ^ /@builder@/: a t'GI.Gtk.Objects.Builder.Builder'
    -> b
    -- ^ /@application@/: a t'GI.Gtk.Objects.Application.Application'
    -> m ()
builderSetApplication builder application = liftIO $ do
    builder' <- unsafeManagedPtrCastPtr builder
    application' <- unsafeManagedPtrCastPtr application
    gtk_builder_set_application builder' application'
    touchManagedPtr builder
    touchManagedPtr application
    return ()

#if defined(ENABLE_OVERLOADING)
data BuilderSetApplicationMethodInfo
instance (signature ~ (b -> m ()), MonadIO m, IsBuilder a, Gtk.Application.IsApplication b) => O.OverloadedMethod BuilderSetApplicationMethodInfo a signature where
    overloadedMethod = builderSetApplication

instance O.OverloadedMethodInfo BuilderSetApplicationMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Builder.builderSetApplication",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Builder.html#v:builderSetApplication"
        })


#endif

-- method Builder::set_translation_domain
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "builder"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Builder" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkBuilder" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "domain"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the translation domain or %NULL"
--                 , sinceVersion = Nothing
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

foreign import ccall "gtk_builder_set_translation_domain" gtk_builder_set_translation_domain :: 
    Ptr Builder ->                          -- builder : TInterface (Name {namespace = "Gtk", name = "Builder"})
    CString ->                              -- domain : TBasicType TUTF8
    IO ()

-- | Sets the translation domain of /@builder@/.
-- See [Builder:translationDomain]("GI.Gtk.Objects.Builder#g:attr:translationDomain").
-- 
-- /Since: 2.12/
builderSetTranslationDomain ::
    (B.CallStack.HasCallStack, MonadIO m, IsBuilder a) =>
    a
    -- ^ /@builder@/: a t'GI.Gtk.Objects.Builder.Builder'
    -> Maybe (T.Text)
    -- ^ /@domain@/: the translation domain or 'P.Nothing'
    -> m ()
builderSetTranslationDomain builder domain = liftIO $ do
    builder' <- unsafeManagedPtrCastPtr builder
    maybeDomain <- case domain of
        Nothing -> return nullPtr
        Just jDomain -> do
            jDomain' <- textToCString jDomain
            return jDomain'
    gtk_builder_set_translation_domain builder' maybeDomain
    touchManagedPtr builder
    freeMem maybeDomain
    return ()

#if defined(ENABLE_OVERLOADING)
data BuilderSetTranslationDomainMethodInfo
instance (signature ~ (Maybe (T.Text) -> m ()), MonadIO m, IsBuilder a) => O.OverloadedMethod BuilderSetTranslationDomainMethodInfo a signature where
    overloadedMethod = builderSetTranslationDomain

instance O.OverloadedMethodInfo BuilderSetTranslationDomainMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Builder.builderSetTranslationDomain",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Builder.html#v:builderSetTranslationDomain"
        })


#endif

-- method Builder::value_from_string
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "builder"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Builder" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkBuilder" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "pspec"
--           , argType = TParamSpec
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the #GParamSpec for the property"
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
--                 { rawDocText = Just "the string representation of the value"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "value"
--           , argType = TGValue
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the #GValue to store the result in"
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
-- throws : True
-- Skip return : False

foreign import ccall "gtk_builder_value_from_string" gtk_builder_value_from_string :: 
    Ptr Builder ->                          -- builder : TInterface (Name {namespace = "Gtk", name = "Builder"})
    Ptr GParamSpec ->                       -- pspec : TParamSpec
    CString ->                              -- string : TBasicType TUTF8
    Ptr GValue ->                           -- value : TGValue
    Ptr (Ptr GError) ->                     -- error
    IO CInt

-- | This function demarshals a value from a string. This function
-- calls 'GI.GObject.Structs.Value.valueInit' on the /@value@/ argument, so it need not be
-- initialised beforehand.
-- 
-- This function can handle char, uchar, boolean, int, uint, long,
-- ulong, enum, flags, float, double, string, t'GI.Gdk.Structs.Color.Color', t'GI.Gdk.Structs.RGBA.RGBA' and
-- t'GI.Gtk.Objects.Adjustment.Adjustment' type values. Support for t'GI.Gtk.Objects.Widget.Widget' type values is
-- still to come.
-- 
-- Upon errors 'P.False' will be returned and /@error@/ will be assigned a
-- t'GError' from the @/GTK_BUILDER_ERROR/@ domain.
-- 
-- /Since: 2.12/
builderValueFromString ::
    (B.CallStack.HasCallStack, MonadIO m, IsBuilder a) =>
    a
    -- ^ /@builder@/: a t'GI.Gtk.Objects.Builder.Builder'
    -> GParamSpec
    -- ^ /@pspec@/: the t'GI.GObject.Objects.ParamSpec.ParamSpec' for the property
    -> T.Text
    -- ^ /@string@/: the string representation of the value
    -> m (GValue)
    -- ^ /(Can throw 'Data.GI.Base.GError.GError')/
builderValueFromString builder pspec string = liftIO $ do
    builder' <- unsafeManagedPtrCastPtr builder
    pspec' <- unsafeManagedPtrGetPtr pspec
    string' <- textToCString string
    value <- SP.callocBytes 24 :: IO (Ptr GValue)
    onException (do
        _ <- propagateGError $ gtk_builder_value_from_string builder' pspec' string' value
        value' <- B.GValue.wrapGValuePtr value
        touchManagedPtr builder
        touchManagedPtr pspec
        freeMem string'
        return value'
     ) (do
        freeMem string'
        freeMem value
     )

#if defined(ENABLE_OVERLOADING)
data BuilderValueFromStringMethodInfo
instance (signature ~ (GParamSpec -> T.Text -> m (GValue)), MonadIO m, IsBuilder a) => O.OverloadedMethod BuilderValueFromStringMethodInfo a signature where
    overloadedMethod = builderValueFromString

instance O.OverloadedMethodInfo BuilderValueFromStringMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Builder.builderValueFromString",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Builder.html#v:builderValueFromString"
        })


#endif

-- method Builder::value_from_string_type
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "builder"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Builder" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkBuilder" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "the #GType of the value"
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
--                 { rawDocText = Just "the string representation of the value"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "value"
--           , argType = TGValue
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the #GValue to store the result in"
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
-- throws : True
-- Skip return : False

foreign import ccall "gtk_builder_value_from_string_type" gtk_builder_value_from_string_type :: 
    Ptr Builder ->                          -- builder : TInterface (Name {namespace = "Gtk", name = "Builder"})
    CGType ->                               -- type : TBasicType TGType
    CString ->                              -- string : TBasicType TUTF8
    Ptr GValue ->                           -- value : TGValue
    Ptr (Ptr GError) ->                     -- error
    IO CInt

-- | Like 'GI.Gtk.Objects.Builder.builderValueFromString', this function demarshals
-- a value from a string, but takes a t'GType' instead of t'GI.GObject.Objects.ParamSpec.ParamSpec'.
-- This function calls 'GI.GObject.Structs.Value.valueInit' on the /@value@/ argument, so it
-- need not be initialised beforehand.
-- 
-- Upon errors 'P.False' will be returned and /@error@/ will be assigned a
-- t'GError' from the @/GTK_BUILDER_ERROR/@ domain.
-- 
-- /Since: 2.12/
builderValueFromStringType ::
    (B.CallStack.HasCallStack, MonadIO m, IsBuilder a) =>
    a
    -- ^ /@builder@/: a t'GI.Gtk.Objects.Builder.Builder'
    -> GType
    -- ^ /@type@/: the t'GType' of the value
    -> T.Text
    -- ^ /@string@/: the string representation of the value
    -> m (GValue)
    -- ^ /(Can throw 'Data.GI.Base.GError.GError')/
builderValueFromStringType builder type_ string = liftIO $ do
    builder' <- unsafeManagedPtrCastPtr builder
    let type_' = gtypeToCGType type_
    string' <- textToCString string
    value <- SP.callocBytes 24 :: IO (Ptr GValue)
    onException (do
        _ <- propagateGError $ gtk_builder_value_from_string_type builder' type_' string' value
        value' <- B.GValue.wrapGValuePtr value
        touchManagedPtr builder
        freeMem string'
        return value'
     ) (do
        freeMem string'
        freeMem value
     )

#if defined(ENABLE_OVERLOADING)
data BuilderValueFromStringTypeMethodInfo
instance (signature ~ (GType -> T.Text -> m (GValue)), MonadIO m, IsBuilder a) => O.OverloadedMethod BuilderValueFromStringTypeMethodInfo a signature where
    overloadedMethod = builderValueFromStringType

instance O.OverloadedMethodInfo BuilderValueFromStringTypeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Builder.builderValueFromStringType",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Builder.html#v:builderValueFromStringType"
        })


#endif


