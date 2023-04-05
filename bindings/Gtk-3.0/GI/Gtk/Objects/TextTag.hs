{-# LANGUAGE ImplicitParams, RankNTypes, TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- You may wish to begin by reading the
-- [text widget conceptual overview][TextWidget]
-- which gives an overview of all the objects and
-- data types related to the text widget and how they work together.
-- 
-- Tags should be in the t'GI.Gtk.Objects.TextTagTable.TextTagTable' for a given t'GI.Gtk.Objects.TextBuffer.TextBuffer'
-- before using them with that buffer.
-- 
-- @/gtk_text_buffer_create_tag()/@ is the best way to create tags.
-- See “gtk3-demo” for numerous examples.
-- 
-- For each property of t'GI.Gtk.Objects.TextTag.TextTag', there is a “set” property, e.g.
-- “font-set” corresponds to “font”. These “set” properties reflect
-- whether a property has been set or not.
-- They are maintained by GTK+ and you should not set them independently.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.TextTag
    ( 

-- * Exported types
    TextTag(..)                             ,
    IsTextTag                               ,
    toTextTag                               ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [changed]("GI.Gtk.Objects.TextTag#g:method:changed"), [event]("GI.Gtk.Objects.TextTag#g:method:event"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getData]("GI.GObject.Objects.Object#g:method:getData"), [getPriority]("GI.Gtk.Objects.TextTag#g:method:getPriority"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata").
-- 
-- ==== Setters
-- [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setPriority]("GI.Gtk.Objects.TextTag#g:method:setPriority"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty").

#if defined(ENABLE_OVERLOADING)
    ResolveTextTagMethod                    ,
#endif

-- ** changed #method:changed#

#if defined(ENABLE_OVERLOADING)
    TextTagChangedMethodInfo                ,
#endif
    textTagChanged                          ,


-- ** event #method:event#

#if defined(ENABLE_OVERLOADING)
    TextTagEventMethodInfo                  ,
#endif
    textTagEvent                            ,


-- ** getPriority #method:getPriority#

#if defined(ENABLE_OVERLOADING)
    TextTagGetPriorityMethodInfo            ,
#endif
    textTagGetPriority                      ,


-- ** new #method:new#

    textTagNew                              ,


-- ** setPriority #method:setPriority#

#if defined(ENABLE_OVERLOADING)
    TextTagSetPriorityMethodInfo            ,
#endif
    textTagSetPriority                      ,




 -- * Properties


-- ** accumulativeMargin #attr:accumulativeMargin#
-- | Whether the margins accumulate or override each other.
-- 
-- When set to 'P.True' the margins of this tag are added to the margins
-- of any other non-accumulative margins present. When set to 'P.False'
-- the margins override one another (the default).
-- 
-- /Since: 2.12/

#if defined(ENABLE_OVERLOADING)
    TextTagAccumulativeMarginPropertyInfo   ,
#endif
    constructTextTagAccumulativeMargin      ,
    getTextTagAccumulativeMargin            ,
    setTextTagAccumulativeMargin            ,
#if defined(ENABLE_OVERLOADING)
    textTagAccumulativeMargin               ,
#endif


-- ** background #attr:background#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TextTagBackgroundPropertyInfo           ,
#endif
    clearTextTagBackground                  ,
    constructTextTagBackground              ,
    setTextTagBackground                    ,
#if defined(ENABLE_OVERLOADING)
    textTagBackground                       ,
#endif


-- ** backgroundFullHeight #attr:backgroundFullHeight#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TextTagBackgroundFullHeightPropertyInfo ,
#endif
    constructTextTagBackgroundFullHeight    ,
    getTextTagBackgroundFullHeight          ,
    setTextTagBackgroundFullHeight          ,
#if defined(ENABLE_OVERLOADING)
    textTagBackgroundFullHeight             ,
#endif


-- ** backgroundFullHeightSet #attr:backgroundFullHeightSet#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TextTagBackgroundFullHeightSetPropertyInfo,
#endif
    constructTextTagBackgroundFullHeightSet ,
    getTextTagBackgroundFullHeightSet       ,
    setTextTagBackgroundFullHeightSet       ,
#if defined(ENABLE_OVERLOADING)
    textTagBackgroundFullHeightSet          ,
#endif


-- ** backgroundGdk #attr:backgroundGdk#
-- | Background color as a t'GI.Gdk.Structs.Color.Color'.

#if defined(ENABLE_OVERLOADING)
    TextTagBackgroundGdkPropertyInfo        ,
#endif
    clearTextTagBackgroundGdk               ,
    constructTextTagBackgroundGdk           ,
    getTextTagBackgroundGdk                 ,
    setTextTagBackgroundGdk                 ,
#if defined(ENABLE_OVERLOADING)
    textTagBackgroundGdk                    ,
#endif


-- ** backgroundRgba #attr:backgroundRgba#
-- | Background color as a t'GI.Gdk.Structs.RGBA.RGBA'.
-- 
-- /Since: 3.2/

#if defined(ENABLE_OVERLOADING)
    TextTagBackgroundRgbaPropertyInfo       ,
#endif
    clearTextTagBackgroundRgba              ,
    constructTextTagBackgroundRgba          ,
    getTextTagBackgroundRgba                ,
    setTextTagBackgroundRgba                ,
#if defined(ENABLE_OVERLOADING)
    textTagBackgroundRgba                   ,
#endif


-- ** backgroundSet #attr:backgroundSet#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TextTagBackgroundSetPropertyInfo        ,
#endif
    constructTextTagBackgroundSet           ,
    getTextTagBackgroundSet                 ,
    setTextTagBackgroundSet                 ,
#if defined(ENABLE_OVERLOADING)
    textTagBackgroundSet                    ,
#endif


-- ** direction #attr:direction#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TextTagDirectionPropertyInfo            ,
#endif
    constructTextTagDirection               ,
    getTextTagDirection                     ,
    setTextTagDirection                     ,
#if defined(ENABLE_OVERLOADING)
    textTagDirection                        ,
#endif


-- ** editable #attr:editable#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TextTagEditablePropertyInfo             ,
#endif
    constructTextTagEditable                ,
    getTextTagEditable                      ,
    setTextTagEditable                      ,
#if defined(ENABLE_OVERLOADING)
    textTagEditable                         ,
#endif


-- ** editableSet #attr:editableSet#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TextTagEditableSetPropertyInfo          ,
#endif
    constructTextTagEditableSet             ,
    getTextTagEditableSet                   ,
    setTextTagEditableSet                   ,
#if defined(ENABLE_OVERLOADING)
    textTagEditableSet                      ,
#endif


-- ** fallback #attr:fallback#
-- | Whether font fallback is enabled.
-- 
-- When set to 'P.True', other fonts will be substituted
-- where the current font is missing glyphs.
-- 
-- /Since: 3.16/

#if defined(ENABLE_OVERLOADING)
    TextTagFallbackPropertyInfo             ,
#endif
    constructTextTagFallback                ,
    getTextTagFallback                      ,
    setTextTagFallback                      ,
#if defined(ENABLE_OVERLOADING)
    textTagFallback                         ,
#endif


-- ** fallbackSet #attr:fallbackSet#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TextTagFallbackSetPropertyInfo          ,
#endif
    constructTextTagFallbackSet             ,
    getTextTagFallbackSet                   ,
    setTextTagFallbackSet                   ,
#if defined(ENABLE_OVERLOADING)
    textTagFallbackSet                      ,
#endif


-- ** family #attr:family#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TextTagFamilyPropertyInfo               ,
#endif
    clearTextTagFamily                      ,
    constructTextTagFamily                  ,
    getTextTagFamily                        ,
    setTextTagFamily                        ,
#if defined(ENABLE_OVERLOADING)
    textTagFamily                           ,
#endif


-- ** familySet #attr:familySet#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TextTagFamilySetPropertyInfo            ,
#endif
    constructTextTagFamilySet               ,
    getTextTagFamilySet                     ,
    setTextTagFamilySet                     ,
#if defined(ENABLE_OVERLOADING)
    textTagFamilySet                        ,
#endif


-- ** font #attr:font#
-- | Font description as string, e.g. \\\"Sans Italic 12\\\".
-- 
-- Note that the initial value of this property depends on
-- the internals of t'GI.Pango.Structs.FontDescription.FontDescription'.

#if defined(ENABLE_OVERLOADING)
    TextTagFontPropertyInfo                 ,
#endif
    clearTextTagFont                        ,
    constructTextTagFont                    ,
    getTextTagFont                          ,
    setTextTagFont                          ,
#if defined(ENABLE_OVERLOADING)
    textTagFont                             ,
#endif


-- ** fontDesc #attr:fontDesc#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TextTagFontDescPropertyInfo             ,
#endif
    clearTextTagFontDesc                    ,
    constructTextTagFontDesc                ,
    getTextTagFontDesc                      ,
    setTextTagFontDesc                      ,
#if defined(ENABLE_OVERLOADING)
    textTagFontDesc                         ,
#endif


-- ** fontFeatures #attr:fontFeatures#
-- | OpenType font features, as a string.
-- 
-- /Since: 3.18/

#if defined(ENABLE_OVERLOADING)
    TextTagFontFeaturesPropertyInfo         ,
#endif
    clearTextTagFontFeatures                ,
    constructTextTagFontFeatures            ,
    getTextTagFontFeatures                  ,
    setTextTagFontFeatures                  ,
#if defined(ENABLE_OVERLOADING)
    textTagFontFeatures                     ,
#endif


-- ** fontFeaturesSet #attr:fontFeaturesSet#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TextTagFontFeaturesSetPropertyInfo      ,
#endif
    constructTextTagFontFeaturesSet         ,
    getTextTagFontFeaturesSet               ,
    setTextTagFontFeaturesSet               ,
#if defined(ENABLE_OVERLOADING)
    textTagFontFeaturesSet                  ,
#endif


-- ** foreground #attr:foreground#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TextTagForegroundPropertyInfo           ,
#endif
    clearTextTagForeground                  ,
    constructTextTagForeground              ,
    setTextTagForeground                    ,
#if defined(ENABLE_OVERLOADING)
    textTagForeground                       ,
#endif


-- ** foregroundGdk #attr:foregroundGdk#
-- | Foreground color as a t'GI.Gdk.Structs.Color.Color'.

#if defined(ENABLE_OVERLOADING)
    TextTagForegroundGdkPropertyInfo        ,
#endif
    clearTextTagForegroundGdk               ,
    constructTextTagForegroundGdk           ,
    getTextTagForegroundGdk                 ,
    setTextTagForegroundGdk                 ,
#if defined(ENABLE_OVERLOADING)
    textTagForegroundGdk                    ,
#endif


-- ** foregroundRgba #attr:foregroundRgba#
-- | Foreground color as a t'GI.Gdk.Structs.RGBA.RGBA'.
-- 
-- /Since: 3.2/

#if defined(ENABLE_OVERLOADING)
    TextTagForegroundRgbaPropertyInfo       ,
#endif
    clearTextTagForegroundRgba              ,
    constructTextTagForegroundRgba          ,
    getTextTagForegroundRgba                ,
    setTextTagForegroundRgba                ,
#if defined(ENABLE_OVERLOADING)
    textTagForegroundRgba                   ,
#endif


-- ** foregroundSet #attr:foregroundSet#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TextTagForegroundSetPropertyInfo        ,
#endif
    constructTextTagForegroundSet           ,
    getTextTagForegroundSet                 ,
    setTextTagForegroundSet                 ,
#if defined(ENABLE_OVERLOADING)
    textTagForegroundSet                    ,
#endif


-- ** indent #attr:indent#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TextTagIndentPropertyInfo               ,
#endif
    constructTextTagIndent                  ,
    getTextTagIndent                        ,
    setTextTagIndent                        ,
#if defined(ENABLE_OVERLOADING)
    textTagIndent                           ,
#endif


-- ** indentSet #attr:indentSet#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TextTagIndentSetPropertyInfo            ,
#endif
    constructTextTagIndentSet               ,
    getTextTagIndentSet                     ,
    setTextTagIndentSet                     ,
#if defined(ENABLE_OVERLOADING)
    textTagIndentSet                        ,
#endif


-- ** invisible #attr:invisible#
-- | Whether this text is hidden.
-- 
-- Note that there may still be problems with the support for invisible
-- text, in particular when navigating programmatically inside a buffer
-- containing invisible segments.
-- 
-- /Since: 2.8/

#if defined(ENABLE_OVERLOADING)
    TextTagInvisiblePropertyInfo            ,
#endif
    constructTextTagInvisible               ,
    getTextTagInvisible                     ,
    setTextTagInvisible                     ,
#if defined(ENABLE_OVERLOADING)
    textTagInvisible                        ,
#endif


-- ** invisibleSet #attr:invisibleSet#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TextTagInvisibleSetPropertyInfo         ,
#endif
    constructTextTagInvisibleSet            ,
    getTextTagInvisibleSet                  ,
    setTextTagInvisibleSet                  ,
#if defined(ENABLE_OVERLOADING)
    textTagInvisibleSet                     ,
#endif


-- ** justification #attr:justification#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TextTagJustificationPropertyInfo        ,
#endif
    constructTextTagJustification           ,
    getTextTagJustification                 ,
    setTextTagJustification                 ,
#if defined(ENABLE_OVERLOADING)
    textTagJustification                    ,
#endif


-- ** justificationSet #attr:justificationSet#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TextTagJustificationSetPropertyInfo     ,
#endif
    constructTextTagJustificationSet        ,
    getTextTagJustificationSet              ,
    setTextTagJustificationSet              ,
#if defined(ENABLE_OVERLOADING)
    textTagJustificationSet                 ,
#endif


-- ** language #attr:language#
-- | The language this text is in, as an ISO code. Pango can use this as a
-- hint when rendering the text. If not set, an appropriate default will be
-- used.
-- 
-- Note that the initial value of this property depends on the current
-- locale, see also 'GI.Gtk.Functions.getDefaultLanguage'.

#if defined(ENABLE_OVERLOADING)
    TextTagLanguagePropertyInfo             ,
#endif
    clearTextTagLanguage                    ,
    constructTextTagLanguage                ,
    getTextTagLanguage                      ,
    setTextTagLanguage                      ,
#if defined(ENABLE_OVERLOADING)
    textTagLanguage                         ,
#endif


-- ** languageSet #attr:languageSet#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TextTagLanguageSetPropertyInfo          ,
#endif
    constructTextTagLanguageSet             ,
    getTextTagLanguageSet                   ,
    setTextTagLanguageSet                   ,
#if defined(ENABLE_OVERLOADING)
    textTagLanguageSet                      ,
#endif


-- ** leftMargin #attr:leftMargin#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TextTagLeftMarginPropertyInfo           ,
#endif
    constructTextTagLeftMargin              ,
    getTextTagLeftMargin                    ,
    setTextTagLeftMargin                    ,
#if defined(ENABLE_OVERLOADING)
    textTagLeftMargin                       ,
#endif


-- ** leftMarginSet #attr:leftMarginSet#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TextTagLeftMarginSetPropertyInfo        ,
#endif
    constructTextTagLeftMarginSet           ,
    getTextTagLeftMarginSet                 ,
    setTextTagLeftMarginSet                 ,
#if defined(ENABLE_OVERLOADING)
    textTagLeftMarginSet                    ,
#endif


-- ** letterSpacing #attr:letterSpacing#
-- | Extra spacing between graphemes, in Pango units.
-- 
-- /Since: 3.16/

#if defined(ENABLE_OVERLOADING)
    TextTagLetterSpacingPropertyInfo        ,
#endif
    constructTextTagLetterSpacing           ,
    getTextTagLetterSpacing                 ,
    setTextTagLetterSpacing                 ,
#if defined(ENABLE_OVERLOADING)
    textTagLetterSpacing                    ,
#endif


-- ** letterSpacingSet #attr:letterSpacingSet#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TextTagLetterSpacingSetPropertyInfo     ,
#endif
    constructTextTagLetterSpacingSet        ,
    getTextTagLetterSpacingSet              ,
    setTextTagLetterSpacingSet              ,
#if defined(ENABLE_OVERLOADING)
    textTagLetterSpacingSet                 ,
#endif


-- ** name #attr:name#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TextTagNamePropertyInfo                 ,
#endif
    constructTextTagName                    ,
    getTextTagName                          ,
#if defined(ENABLE_OVERLOADING)
    textTagName                             ,
#endif


-- ** paragraphBackground #attr:paragraphBackground#
-- | The paragraph background color as a string.
-- 
-- /Since: 2.8/

#if defined(ENABLE_OVERLOADING)
    TextTagParagraphBackgroundPropertyInfo  ,
#endif
    clearTextTagParagraphBackground         ,
    constructTextTagParagraphBackground     ,
    setTextTagParagraphBackground           ,
#if defined(ENABLE_OVERLOADING)
    textTagParagraphBackground              ,
#endif


-- ** paragraphBackgroundGdk #attr:paragraphBackgroundGdk#
-- | The paragraph background color as a t'GI.Gdk.Structs.Color.Color'.
-- 
-- /Since: 2.8/

#if defined(ENABLE_OVERLOADING)
    TextTagParagraphBackgroundGdkPropertyInfo,
#endif
    clearTextTagParagraphBackgroundGdk      ,
    constructTextTagParagraphBackgroundGdk  ,
    getTextTagParagraphBackgroundGdk        ,
    setTextTagParagraphBackgroundGdk        ,
#if defined(ENABLE_OVERLOADING)
    textTagParagraphBackgroundGdk           ,
#endif


-- ** paragraphBackgroundRgba #attr:paragraphBackgroundRgba#
-- | The paragraph background color as a t'GI.Gdk.Structs.RGBA.RGBA'.
-- 
-- /Since: 3.2/

#if defined(ENABLE_OVERLOADING)
    TextTagParagraphBackgroundRgbaPropertyInfo,
#endif
    clearTextTagParagraphBackgroundRgba     ,
    constructTextTagParagraphBackgroundRgba ,
    getTextTagParagraphBackgroundRgba       ,
    setTextTagParagraphBackgroundRgba       ,
#if defined(ENABLE_OVERLOADING)
    textTagParagraphBackgroundRgba          ,
#endif


-- ** paragraphBackgroundSet #attr:paragraphBackgroundSet#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TextTagParagraphBackgroundSetPropertyInfo,
#endif
    constructTextTagParagraphBackgroundSet  ,
    getTextTagParagraphBackgroundSet        ,
    setTextTagParagraphBackgroundSet        ,
#if defined(ENABLE_OVERLOADING)
    textTagParagraphBackgroundSet           ,
#endif


-- ** pixelsAboveLines #attr:pixelsAboveLines#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TextTagPixelsAboveLinesPropertyInfo     ,
#endif
    constructTextTagPixelsAboveLines        ,
    getTextTagPixelsAboveLines              ,
    setTextTagPixelsAboveLines              ,
#if defined(ENABLE_OVERLOADING)
    textTagPixelsAboveLines                 ,
#endif


-- ** pixelsAboveLinesSet #attr:pixelsAboveLinesSet#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TextTagPixelsAboveLinesSetPropertyInfo  ,
#endif
    constructTextTagPixelsAboveLinesSet     ,
    getTextTagPixelsAboveLinesSet           ,
    setTextTagPixelsAboveLinesSet           ,
#if defined(ENABLE_OVERLOADING)
    textTagPixelsAboveLinesSet              ,
#endif


-- ** pixelsBelowLines #attr:pixelsBelowLines#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TextTagPixelsBelowLinesPropertyInfo     ,
#endif
    constructTextTagPixelsBelowLines        ,
    getTextTagPixelsBelowLines              ,
    setTextTagPixelsBelowLines              ,
#if defined(ENABLE_OVERLOADING)
    textTagPixelsBelowLines                 ,
#endif


-- ** pixelsBelowLinesSet #attr:pixelsBelowLinesSet#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TextTagPixelsBelowLinesSetPropertyInfo  ,
#endif
    constructTextTagPixelsBelowLinesSet     ,
    getTextTagPixelsBelowLinesSet           ,
    setTextTagPixelsBelowLinesSet           ,
#if defined(ENABLE_OVERLOADING)
    textTagPixelsBelowLinesSet              ,
#endif


-- ** pixelsInsideWrap #attr:pixelsInsideWrap#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TextTagPixelsInsideWrapPropertyInfo     ,
#endif
    constructTextTagPixelsInsideWrap        ,
    getTextTagPixelsInsideWrap              ,
    setTextTagPixelsInsideWrap              ,
#if defined(ENABLE_OVERLOADING)
    textTagPixelsInsideWrap                 ,
#endif


-- ** pixelsInsideWrapSet #attr:pixelsInsideWrapSet#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TextTagPixelsInsideWrapSetPropertyInfo  ,
#endif
    constructTextTagPixelsInsideWrapSet     ,
    getTextTagPixelsInsideWrapSet           ,
    setTextTagPixelsInsideWrapSet           ,
#if defined(ENABLE_OVERLOADING)
    textTagPixelsInsideWrapSet              ,
#endif


-- ** rightMargin #attr:rightMargin#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TextTagRightMarginPropertyInfo          ,
#endif
    constructTextTagRightMargin             ,
    getTextTagRightMargin                   ,
    setTextTagRightMargin                   ,
#if defined(ENABLE_OVERLOADING)
    textTagRightMargin                      ,
#endif


-- ** rightMarginSet #attr:rightMarginSet#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TextTagRightMarginSetPropertyInfo       ,
#endif
    constructTextTagRightMarginSet          ,
    getTextTagRightMarginSet                ,
    setTextTagRightMarginSet                ,
#if defined(ENABLE_OVERLOADING)
    textTagRightMarginSet                   ,
#endif


-- ** rise #attr:rise#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TextTagRisePropertyInfo                 ,
#endif
    constructTextTagRise                    ,
    getTextTagRise                          ,
    setTextTagRise                          ,
#if defined(ENABLE_OVERLOADING)
    textTagRise                             ,
#endif


-- ** riseSet #attr:riseSet#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TextTagRiseSetPropertyInfo              ,
#endif
    constructTextTagRiseSet                 ,
    getTextTagRiseSet                       ,
    setTextTagRiseSet                       ,
#if defined(ENABLE_OVERLOADING)
    textTagRiseSet                          ,
#endif


-- ** scale #attr:scale#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TextTagScalePropertyInfo                ,
#endif
    constructTextTagScale                   ,
    getTextTagScale                         ,
    setTextTagScale                         ,
#if defined(ENABLE_OVERLOADING)
    textTagScale                            ,
#endif


-- ** scaleSet #attr:scaleSet#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TextTagScaleSetPropertyInfo             ,
#endif
    constructTextTagScaleSet                ,
    getTextTagScaleSet                      ,
    setTextTagScaleSet                      ,
#if defined(ENABLE_OVERLOADING)
    textTagScaleSet                         ,
#endif


-- ** size #attr:size#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TextTagSizePropertyInfo                 ,
#endif
    constructTextTagSize                    ,
    getTextTagSize                          ,
    setTextTagSize                          ,
#if defined(ENABLE_OVERLOADING)
    textTagSize                             ,
#endif


-- ** sizePoints #attr:sizePoints#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TextTagSizePointsPropertyInfo           ,
#endif
    constructTextTagSizePoints              ,
    getTextTagSizePoints                    ,
    setTextTagSizePoints                    ,
#if defined(ENABLE_OVERLOADING)
    textTagSizePoints                       ,
#endif


-- ** sizeSet #attr:sizeSet#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TextTagSizeSetPropertyInfo              ,
#endif
    constructTextTagSizeSet                 ,
    getTextTagSizeSet                       ,
    setTextTagSizeSet                       ,
#if defined(ENABLE_OVERLOADING)
    textTagSizeSet                          ,
#endif


-- ** stretch #attr:stretch#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TextTagStretchPropertyInfo              ,
#endif
    constructTextTagStretch                 ,
    getTextTagStretch                       ,
    setTextTagStretch                       ,
#if defined(ENABLE_OVERLOADING)
    textTagStretch                          ,
#endif


-- ** stretchSet #attr:stretchSet#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TextTagStretchSetPropertyInfo           ,
#endif
    constructTextTagStretchSet              ,
    getTextTagStretchSet                    ,
    setTextTagStretchSet                    ,
#if defined(ENABLE_OVERLOADING)
    textTagStretchSet                       ,
#endif


-- ** strikethrough #attr:strikethrough#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TextTagStrikethroughPropertyInfo        ,
#endif
    constructTextTagStrikethrough           ,
    getTextTagStrikethrough                 ,
    setTextTagStrikethrough                 ,
#if defined(ENABLE_OVERLOADING)
    textTagStrikethrough                    ,
#endif


-- ** strikethroughRgba #attr:strikethroughRgba#
-- | This property modifies the color of strikeouts. If not set, strikeouts
-- will use the forground color.
-- 
-- /Since: 3.16/

#if defined(ENABLE_OVERLOADING)
    TextTagStrikethroughRgbaPropertyInfo    ,
#endif
    clearTextTagStrikethroughRgba           ,
    constructTextTagStrikethroughRgba       ,
    getTextTagStrikethroughRgba             ,
    setTextTagStrikethroughRgba             ,
#if defined(ENABLE_OVERLOADING)
    textTagStrikethroughRgba                ,
#endif


-- ** strikethroughRgbaSet #attr:strikethroughRgbaSet#
-- | If the [TextTag:strikethroughRgba]("GI.Gtk.Objects.TextTag#g:attr:strikethroughRgba") property has been set.
-- 
-- /Since: 3.16/

#if defined(ENABLE_OVERLOADING)
    TextTagStrikethroughRgbaSetPropertyInfo ,
#endif
    constructTextTagStrikethroughRgbaSet    ,
    getTextTagStrikethroughRgbaSet          ,
    setTextTagStrikethroughRgbaSet          ,
#if defined(ENABLE_OVERLOADING)
    textTagStrikethroughRgbaSet             ,
#endif


-- ** strikethroughSet #attr:strikethroughSet#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TextTagStrikethroughSetPropertyInfo     ,
#endif
    constructTextTagStrikethroughSet        ,
    getTextTagStrikethroughSet              ,
    setTextTagStrikethroughSet              ,
#if defined(ENABLE_OVERLOADING)
    textTagStrikethroughSet                 ,
#endif


-- ** style #attr:style#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TextTagStylePropertyInfo                ,
#endif
    constructTextTagStyle                   ,
    getTextTagStyle                         ,
    setTextTagStyle                         ,
#if defined(ENABLE_OVERLOADING)
    textTagStyle                            ,
#endif


-- ** styleSet #attr:styleSet#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TextTagStyleSetPropertyInfo             ,
#endif
    constructTextTagStyleSet                ,
    getTextTagStyleSet                      ,
    setTextTagStyleSet                      ,
#if defined(ENABLE_OVERLOADING)
    textTagStyleSet                         ,
#endif


-- ** tabs #attr:tabs#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TextTagTabsPropertyInfo                 ,
#endif
    clearTextTagTabs                        ,
    constructTextTagTabs                    ,
    getTextTagTabs                          ,
    setTextTagTabs                          ,
#if defined(ENABLE_OVERLOADING)
    textTagTabs                             ,
#endif


-- ** tabsSet #attr:tabsSet#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TextTagTabsSetPropertyInfo              ,
#endif
    constructTextTagTabsSet                 ,
    getTextTagTabsSet                       ,
    setTextTagTabsSet                       ,
#if defined(ENABLE_OVERLOADING)
    textTagTabsSet                          ,
#endif


-- ** underline #attr:underline#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TextTagUnderlinePropertyInfo            ,
#endif
    constructTextTagUnderline               ,
    getTextTagUnderline                     ,
    setTextTagUnderline                     ,
#if defined(ENABLE_OVERLOADING)
    textTagUnderline                        ,
#endif


-- ** underlineRgba #attr:underlineRgba#
-- | This property modifies the color of underlines. If not set, underlines
-- will use the forground color.
-- 
-- If [TextTag:underline]("GI.Gtk.Objects.TextTag#g:attr:underline") is set to 'GI.Pango.Enums.UnderlineError', an alternate
-- color may be applied instead of the foreground. Setting this property
-- will always override those defaults.
-- 
-- /Since: 3.16/

#if defined(ENABLE_OVERLOADING)
    TextTagUnderlineRgbaPropertyInfo        ,
#endif
    clearTextTagUnderlineRgba               ,
    constructTextTagUnderlineRgba           ,
    getTextTagUnderlineRgba                 ,
    setTextTagUnderlineRgba                 ,
#if defined(ENABLE_OVERLOADING)
    textTagUnderlineRgba                    ,
#endif


-- ** underlineRgbaSet #attr:underlineRgbaSet#
-- | If the [TextTag:underlineRgba]("GI.Gtk.Objects.TextTag#g:attr:underlineRgba") property has been set.
-- 
-- /Since: 3.16/

#if defined(ENABLE_OVERLOADING)
    TextTagUnderlineRgbaSetPropertyInfo     ,
#endif
    constructTextTagUnderlineRgbaSet        ,
    getTextTagUnderlineRgbaSet              ,
    setTextTagUnderlineRgbaSet              ,
#if defined(ENABLE_OVERLOADING)
    textTagUnderlineRgbaSet                 ,
#endif


-- ** underlineSet #attr:underlineSet#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TextTagUnderlineSetPropertyInfo         ,
#endif
    constructTextTagUnderlineSet            ,
    getTextTagUnderlineSet                  ,
    setTextTagUnderlineSet                  ,
#if defined(ENABLE_OVERLOADING)
    textTagUnderlineSet                     ,
#endif


-- ** variant #attr:variant#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TextTagVariantPropertyInfo              ,
#endif
    constructTextTagVariant                 ,
    getTextTagVariant                       ,
    setTextTagVariant                       ,
#if defined(ENABLE_OVERLOADING)
    textTagVariant                          ,
#endif


-- ** variantSet #attr:variantSet#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TextTagVariantSetPropertyInfo           ,
#endif
    constructTextTagVariantSet              ,
    getTextTagVariantSet                    ,
    setTextTagVariantSet                    ,
#if defined(ENABLE_OVERLOADING)
    textTagVariantSet                       ,
#endif


-- ** weight #attr:weight#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TextTagWeightPropertyInfo               ,
#endif
    constructTextTagWeight                  ,
    getTextTagWeight                        ,
    setTextTagWeight                        ,
#if defined(ENABLE_OVERLOADING)
    textTagWeight                           ,
#endif


-- ** weightSet #attr:weightSet#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TextTagWeightSetPropertyInfo            ,
#endif
    constructTextTagWeightSet               ,
    getTextTagWeightSet                     ,
    setTextTagWeightSet                     ,
#if defined(ENABLE_OVERLOADING)
    textTagWeightSet                        ,
#endif


-- ** wrapMode #attr:wrapMode#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TextTagWrapModePropertyInfo             ,
#endif
    constructTextTagWrapMode                ,
    getTextTagWrapMode                      ,
    setTextTagWrapMode                      ,
#if defined(ENABLE_OVERLOADING)
    textTagWrapMode                         ,
#endif


-- ** wrapModeSet #attr:wrapModeSet#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TextTagWrapModeSetPropertyInfo          ,
#endif
    constructTextTagWrapModeSet             ,
    getTextTagWrapModeSet                   ,
    setTextTagWrapModeSet                   ,
#if defined(ENABLE_OVERLOADING)
    textTagWrapModeSet                      ,
#endif




 -- * Signals


-- ** event #signal:event#

    TextTagEventCallback                    ,
#if defined(ENABLE_OVERLOADING)
    TextTagEventSignalInfo                  ,
#endif
    afterTextTagEvent                       ,
    onTextTagEvent                          ,




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
import qualified GI.Gdk.Structs.Color as Gdk.Color
import qualified GI.Gdk.Structs.RGBA as Gdk.RGBA
import qualified GI.Gdk.Unions.Event as Gdk.Event
import {-# SOURCE #-} qualified GI.Gtk.Enums as Gtk.Enums
import {-# SOURCE #-} qualified GI.Gtk.Structs.TextIter as Gtk.TextIter
import qualified GI.Pango.Enums as Pango.Enums
import qualified GI.Pango.Structs.FontDescription as Pango.FontDescription
import qualified GI.Pango.Structs.TabArray as Pango.TabArray

-- | Memory-managed wrapper type.
newtype TextTag = TextTag (SP.ManagedPtr TextTag)
    deriving (Eq)

instance SP.ManagedPtrNewtype TextTag where
    toManagedPtr (TextTag p) = p

foreign import ccall "gtk_text_tag_get_type"
    c_gtk_text_tag_get_type :: IO B.Types.GType

instance B.Types.TypedObject TextTag where
    glibType = c_gtk_text_tag_get_type

instance B.Types.GObject TextTag

-- | Type class for types which can be safely cast to `TextTag`, for instance with `toTextTag`.
class (SP.GObject o, O.IsDescendantOf TextTag o) => IsTextTag o
instance (SP.GObject o, O.IsDescendantOf TextTag o) => IsTextTag o

instance O.HasParentTypes TextTag
type instance O.ParentTypes TextTag = '[GObject.Object.Object]

-- | Cast to `TextTag`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toTextTag :: (MIO.MonadIO m, IsTextTag o) => o -> m TextTag
toTextTag = MIO.liftIO . B.ManagedPtr.unsafeCastTo TextTag

-- | Convert 'TextTag' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe TextTag) where
    gvalueGType_ = c_gtk_text_tag_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr TextTag)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr TextTag)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject TextTag ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveTextTagMethod (t :: Symbol) (o :: *) :: * where
    ResolveTextTagMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveTextTagMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveTextTagMethod "changed" o = TextTagChangedMethodInfo
    ResolveTextTagMethod "event" o = TextTagEventMethodInfo
    ResolveTextTagMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveTextTagMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveTextTagMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveTextTagMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveTextTagMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveTextTagMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveTextTagMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveTextTagMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveTextTagMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveTextTagMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveTextTagMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveTextTagMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveTextTagMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveTextTagMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveTextTagMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveTextTagMethod "getPriority" o = TextTagGetPriorityMethodInfo
    ResolveTextTagMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveTextTagMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveTextTagMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveTextTagMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveTextTagMethod "setPriority" o = TextTagSetPriorityMethodInfo
    ResolveTextTagMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveTextTagMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveTextTagMethod t TextTag, O.OverloadedMethod info TextTag p) => OL.IsLabel t (TextTag -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveTextTagMethod t TextTag, O.OverloadedMethod info TextTag p, R.HasField t TextTag p) => R.HasField t TextTag p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveTextTagMethod t TextTag, O.OverloadedMethodInfo info TextTag) => OL.IsLabel t (O.MethodProxy info TextTag) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- signal TextTag::event
-- | The [event](#g:signal:event) signal is emitted when an event occurs on a region of the
-- buffer marked with this tag.
type TextTagEventCallback =
    GObject.Object.Object
    -- ^ /@object@/: the object the event was fired from (typically a t'GI.Gtk.Objects.TextView.TextView')
    -> Gdk.Event.Event
    -- ^ /@event@/: the event which triggered the signal
    -> Gtk.TextIter.TextIter
    -- ^ /@iter@/: a t'GI.Gtk.Structs.TextIter.TextIter' pointing at the location the event occurred
    -> IO Bool
    -- ^ __Returns:__ 'P.True' to stop other handlers from being invoked for the
    -- event. 'P.False' to propagate the event further.

type C_TextTagEventCallback =
    Ptr TextTag ->                          -- object
    Ptr GObject.Object.Object ->
    Ptr Gdk.Event.Event ->
    Ptr Gtk.TextIter.TextIter ->
    Ptr () ->                               -- user_data
    IO CInt

-- | Generate a function pointer callable from C code, from a `C_TextTagEventCallback`.
foreign import ccall "wrapper"
    mk_TextTagEventCallback :: C_TextTagEventCallback -> IO (FunPtr C_TextTagEventCallback)

wrap_TextTagEventCallback :: 
    GObject a => (a -> TextTagEventCallback) ->
    C_TextTagEventCallback
wrap_TextTagEventCallback gi'cb gi'selfPtr object event iter _ = do
    object' <- (newObject GObject.Object.Object) object
    B.ManagedPtr.withTransient  event $ \event' -> do
        B.ManagedPtr.withTransient  iter $ \iter' -> do
            result <- B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  object' event' iter'
            let result' = (fromIntegral . fromEnum) result
            return result'


-- | Connect a signal handler for the [event](#signal:event) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' textTag #event callback
-- @
-- 
-- 
onTextTagEvent :: (IsTextTag a, MonadIO m) => a -> ((?self :: a) => TextTagEventCallback) -> m SignalHandlerId
onTextTagEvent obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TextTagEventCallback wrapped
    wrapped'' <- mk_TextTagEventCallback wrapped'
    connectSignalFunPtr obj "event" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [event](#signal:event) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' textTag #event callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterTextTagEvent :: (IsTextTag a, MonadIO m) => a -> ((?self :: a) => TextTagEventCallback) -> m SignalHandlerId
afterTextTagEvent obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TextTagEventCallback wrapped
    wrapped'' <- mk_TextTagEventCallback wrapped'
    connectSignalFunPtr obj "event" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data TextTagEventSignalInfo
instance SignalInfo TextTagEventSignalInfo where
    type HaskellCallbackType TextTagEventSignalInfo = TextTagEventCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_TextTagEventCallback cb
        cb'' <- mk_TextTagEventCallback cb'
        connectSignalFunPtr obj "event" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextTag::event"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextTag.html#g:signal:event"})

#endif

-- VVV Prop "accumulative-margin"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@accumulative-margin@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textTag #accumulativeMargin
-- @
getTextTagAccumulativeMargin :: (MonadIO m, IsTextTag o) => o -> m Bool
getTextTagAccumulativeMargin obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "accumulative-margin"

-- | Set the value of the “@accumulative-margin@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textTag [ #accumulativeMargin 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextTagAccumulativeMargin :: (MonadIO m, IsTextTag o) => o -> Bool -> m ()
setTextTagAccumulativeMargin obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "accumulative-margin" val

-- | Construct a `GValueConstruct` with valid value for the “@accumulative-margin@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextTagAccumulativeMargin :: (IsTextTag o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructTextTagAccumulativeMargin val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "accumulative-margin" val

#if defined(ENABLE_OVERLOADING)
data TextTagAccumulativeMarginPropertyInfo
instance AttrInfo TextTagAccumulativeMarginPropertyInfo where
    type AttrAllowedOps TextTagAccumulativeMarginPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TextTagAccumulativeMarginPropertyInfo = IsTextTag
    type AttrSetTypeConstraint TextTagAccumulativeMarginPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint TextTagAccumulativeMarginPropertyInfo = (~) Bool
    type AttrTransferType TextTagAccumulativeMarginPropertyInfo = Bool
    type AttrGetType TextTagAccumulativeMarginPropertyInfo = Bool
    type AttrLabel TextTagAccumulativeMarginPropertyInfo = "accumulative-margin"
    type AttrOrigin TextTagAccumulativeMarginPropertyInfo = TextTag
    attrGet = getTextTagAccumulativeMargin
    attrSet = setTextTagAccumulativeMargin
    attrTransfer _ v = do
        return v
    attrConstruct = constructTextTagAccumulativeMargin
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextTag.accumulativeMargin"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextTag.html#g:attr:accumulativeMargin"
        })
#endif

-- VVV Prop "background"
   -- Type: TBasicType TUTF8
   -- Flags: [PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Set the value of the “@background@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textTag [ #background 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextTagBackground :: (MonadIO m, IsTextTag o) => o -> T.Text -> m ()
setTextTagBackground obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "background" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@background@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextTagBackground :: (IsTextTag o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructTextTagBackground val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "background" (P.Just val)

-- | Set the value of the “@background@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #background
-- @
clearTextTagBackground :: (MonadIO m, IsTextTag o) => o -> m ()
clearTextTagBackground obj = liftIO $ B.Properties.setObjectPropertyString obj "background" (Nothing :: Maybe T.Text)

#if defined(ENABLE_OVERLOADING)
data TextTagBackgroundPropertyInfo
instance AttrInfo TextTagBackgroundPropertyInfo where
    type AttrAllowedOps TextTagBackgroundPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrClear]
    type AttrBaseTypeConstraint TextTagBackgroundPropertyInfo = IsTextTag
    type AttrSetTypeConstraint TextTagBackgroundPropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint TextTagBackgroundPropertyInfo = (~) T.Text
    type AttrTransferType TextTagBackgroundPropertyInfo = T.Text
    type AttrGetType TextTagBackgroundPropertyInfo = ()
    type AttrLabel TextTagBackgroundPropertyInfo = "background"
    type AttrOrigin TextTagBackgroundPropertyInfo = TextTag
    attrGet = undefined
    attrSet = setTextTagBackground
    attrTransfer _ v = do
        return v
    attrConstruct = constructTextTagBackground
    attrClear = clearTextTagBackground
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextTag.background"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextTag.html#g:attr:background"
        })
#endif

-- VVV Prop "background-full-height"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@background-full-height@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textTag #backgroundFullHeight
-- @
getTextTagBackgroundFullHeight :: (MonadIO m, IsTextTag o) => o -> m Bool
getTextTagBackgroundFullHeight obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "background-full-height"

-- | Set the value of the “@background-full-height@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textTag [ #backgroundFullHeight 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextTagBackgroundFullHeight :: (MonadIO m, IsTextTag o) => o -> Bool -> m ()
setTextTagBackgroundFullHeight obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "background-full-height" val

-- | Construct a `GValueConstruct` with valid value for the “@background-full-height@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextTagBackgroundFullHeight :: (IsTextTag o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructTextTagBackgroundFullHeight val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "background-full-height" val

#if defined(ENABLE_OVERLOADING)
data TextTagBackgroundFullHeightPropertyInfo
instance AttrInfo TextTagBackgroundFullHeightPropertyInfo where
    type AttrAllowedOps TextTagBackgroundFullHeightPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TextTagBackgroundFullHeightPropertyInfo = IsTextTag
    type AttrSetTypeConstraint TextTagBackgroundFullHeightPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint TextTagBackgroundFullHeightPropertyInfo = (~) Bool
    type AttrTransferType TextTagBackgroundFullHeightPropertyInfo = Bool
    type AttrGetType TextTagBackgroundFullHeightPropertyInfo = Bool
    type AttrLabel TextTagBackgroundFullHeightPropertyInfo = "background-full-height"
    type AttrOrigin TextTagBackgroundFullHeightPropertyInfo = TextTag
    attrGet = getTextTagBackgroundFullHeight
    attrSet = setTextTagBackgroundFullHeight
    attrTransfer _ v = do
        return v
    attrConstruct = constructTextTagBackgroundFullHeight
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextTag.backgroundFullHeight"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextTag.html#g:attr:backgroundFullHeight"
        })
#endif

-- VVV Prop "background-full-height-set"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@background-full-height-set@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textTag #backgroundFullHeightSet
-- @
getTextTagBackgroundFullHeightSet :: (MonadIO m, IsTextTag o) => o -> m Bool
getTextTagBackgroundFullHeightSet obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "background-full-height-set"

-- | Set the value of the “@background-full-height-set@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textTag [ #backgroundFullHeightSet 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextTagBackgroundFullHeightSet :: (MonadIO m, IsTextTag o) => o -> Bool -> m ()
setTextTagBackgroundFullHeightSet obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "background-full-height-set" val

-- | Construct a `GValueConstruct` with valid value for the “@background-full-height-set@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextTagBackgroundFullHeightSet :: (IsTextTag o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructTextTagBackgroundFullHeightSet val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "background-full-height-set" val

#if defined(ENABLE_OVERLOADING)
data TextTagBackgroundFullHeightSetPropertyInfo
instance AttrInfo TextTagBackgroundFullHeightSetPropertyInfo where
    type AttrAllowedOps TextTagBackgroundFullHeightSetPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TextTagBackgroundFullHeightSetPropertyInfo = IsTextTag
    type AttrSetTypeConstraint TextTagBackgroundFullHeightSetPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint TextTagBackgroundFullHeightSetPropertyInfo = (~) Bool
    type AttrTransferType TextTagBackgroundFullHeightSetPropertyInfo = Bool
    type AttrGetType TextTagBackgroundFullHeightSetPropertyInfo = Bool
    type AttrLabel TextTagBackgroundFullHeightSetPropertyInfo = "background-full-height-set"
    type AttrOrigin TextTagBackgroundFullHeightSetPropertyInfo = TextTag
    attrGet = getTextTagBackgroundFullHeightSet
    attrSet = setTextTagBackgroundFullHeightSet
    attrTransfer _ v = do
        return v
    attrConstruct = constructTextTagBackgroundFullHeightSet
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextTag.backgroundFullHeightSet"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextTag.html#g:attr:backgroundFullHeightSet"
        })
#endif

-- VVV Prop "background-gdk"
   -- Type: TInterface (Name {namespace = "Gdk", name = "Color"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@background-gdk@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textTag #backgroundGdk
-- @
getTextTagBackgroundGdk :: (MonadIO m, IsTextTag o) => o -> m (Maybe Gdk.Color.Color)
getTextTagBackgroundGdk obj = MIO.liftIO $ B.Properties.getObjectPropertyBoxed obj "background-gdk" Gdk.Color.Color

-- | Set the value of the “@background-gdk@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textTag [ #backgroundGdk 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextTagBackgroundGdk :: (MonadIO m, IsTextTag o) => o -> Gdk.Color.Color -> m ()
setTextTagBackgroundGdk obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBoxed obj "background-gdk" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@background-gdk@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextTagBackgroundGdk :: (IsTextTag o, MIO.MonadIO m) => Gdk.Color.Color -> m (GValueConstruct o)
constructTextTagBackgroundGdk val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBoxed "background-gdk" (P.Just val)

-- | Set the value of the “@background-gdk@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #backgroundGdk
-- @
clearTextTagBackgroundGdk :: (MonadIO m, IsTextTag o) => o -> m ()
clearTextTagBackgroundGdk obj = liftIO $ B.Properties.setObjectPropertyBoxed obj "background-gdk" (Nothing :: Maybe Gdk.Color.Color)

#if defined(ENABLE_OVERLOADING)
data TextTagBackgroundGdkPropertyInfo
instance AttrInfo TextTagBackgroundGdkPropertyInfo where
    type AttrAllowedOps TextTagBackgroundGdkPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint TextTagBackgroundGdkPropertyInfo = IsTextTag
    type AttrSetTypeConstraint TextTagBackgroundGdkPropertyInfo = (~) Gdk.Color.Color
    type AttrTransferTypeConstraint TextTagBackgroundGdkPropertyInfo = (~) Gdk.Color.Color
    type AttrTransferType TextTagBackgroundGdkPropertyInfo = Gdk.Color.Color
    type AttrGetType TextTagBackgroundGdkPropertyInfo = (Maybe Gdk.Color.Color)
    type AttrLabel TextTagBackgroundGdkPropertyInfo = "background-gdk"
    type AttrOrigin TextTagBackgroundGdkPropertyInfo = TextTag
    attrGet = getTextTagBackgroundGdk
    attrSet = setTextTagBackgroundGdk
    attrTransfer _ v = do
        return v
    attrConstruct = constructTextTagBackgroundGdk
    attrClear = clearTextTagBackgroundGdk
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextTag.backgroundGdk"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextTag.html#g:attr:backgroundGdk"
        })
#endif

-- VVV Prop "background-rgba"
   -- Type: TInterface (Name {namespace = "Gdk", name = "RGBA"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@background-rgba@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textTag #backgroundRgba
-- @
getTextTagBackgroundRgba :: (MonadIO m, IsTextTag o) => o -> m (Maybe Gdk.RGBA.RGBA)
getTextTagBackgroundRgba obj = MIO.liftIO $ B.Properties.getObjectPropertyBoxed obj "background-rgba" Gdk.RGBA.RGBA

-- | Set the value of the “@background-rgba@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textTag [ #backgroundRgba 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextTagBackgroundRgba :: (MonadIO m, IsTextTag o) => o -> Gdk.RGBA.RGBA -> m ()
setTextTagBackgroundRgba obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBoxed obj "background-rgba" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@background-rgba@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextTagBackgroundRgba :: (IsTextTag o, MIO.MonadIO m) => Gdk.RGBA.RGBA -> m (GValueConstruct o)
constructTextTagBackgroundRgba val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBoxed "background-rgba" (P.Just val)

-- | Set the value of the “@background-rgba@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #backgroundRgba
-- @
clearTextTagBackgroundRgba :: (MonadIO m, IsTextTag o) => o -> m ()
clearTextTagBackgroundRgba obj = liftIO $ B.Properties.setObjectPropertyBoxed obj "background-rgba" (Nothing :: Maybe Gdk.RGBA.RGBA)

#if defined(ENABLE_OVERLOADING)
data TextTagBackgroundRgbaPropertyInfo
instance AttrInfo TextTagBackgroundRgbaPropertyInfo where
    type AttrAllowedOps TextTagBackgroundRgbaPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint TextTagBackgroundRgbaPropertyInfo = IsTextTag
    type AttrSetTypeConstraint TextTagBackgroundRgbaPropertyInfo = (~) Gdk.RGBA.RGBA
    type AttrTransferTypeConstraint TextTagBackgroundRgbaPropertyInfo = (~) Gdk.RGBA.RGBA
    type AttrTransferType TextTagBackgroundRgbaPropertyInfo = Gdk.RGBA.RGBA
    type AttrGetType TextTagBackgroundRgbaPropertyInfo = (Maybe Gdk.RGBA.RGBA)
    type AttrLabel TextTagBackgroundRgbaPropertyInfo = "background-rgba"
    type AttrOrigin TextTagBackgroundRgbaPropertyInfo = TextTag
    attrGet = getTextTagBackgroundRgba
    attrSet = setTextTagBackgroundRgba
    attrTransfer _ v = do
        return v
    attrConstruct = constructTextTagBackgroundRgba
    attrClear = clearTextTagBackgroundRgba
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextTag.backgroundRgba"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextTag.html#g:attr:backgroundRgba"
        })
#endif

-- VVV Prop "background-set"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@background-set@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textTag #backgroundSet
-- @
getTextTagBackgroundSet :: (MonadIO m, IsTextTag o) => o -> m Bool
getTextTagBackgroundSet obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "background-set"

-- | Set the value of the “@background-set@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textTag [ #backgroundSet 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextTagBackgroundSet :: (MonadIO m, IsTextTag o) => o -> Bool -> m ()
setTextTagBackgroundSet obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "background-set" val

-- | Construct a `GValueConstruct` with valid value for the “@background-set@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextTagBackgroundSet :: (IsTextTag o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructTextTagBackgroundSet val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "background-set" val

#if defined(ENABLE_OVERLOADING)
data TextTagBackgroundSetPropertyInfo
instance AttrInfo TextTagBackgroundSetPropertyInfo where
    type AttrAllowedOps TextTagBackgroundSetPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TextTagBackgroundSetPropertyInfo = IsTextTag
    type AttrSetTypeConstraint TextTagBackgroundSetPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint TextTagBackgroundSetPropertyInfo = (~) Bool
    type AttrTransferType TextTagBackgroundSetPropertyInfo = Bool
    type AttrGetType TextTagBackgroundSetPropertyInfo = Bool
    type AttrLabel TextTagBackgroundSetPropertyInfo = "background-set"
    type AttrOrigin TextTagBackgroundSetPropertyInfo = TextTag
    attrGet = getTextTagBackgroundSet
    attrSet = setTextTagBackgroundSet
    attrTransfer _ v = do
        return v
    attrConstruct = constructTextTagBackgroundSet
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextTag.backgroundSet"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextTag.html#g:attr:backgroundSet"
        })
#endif

-- VVV Prop "direction"
   -- Type: TInterface (Name {namespace = "Gtk", name = "TextDirection"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@direction@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textTag #direction
-- @
getTextTagDirection :: (MonadIO m, IsTextTag o) => o -> m Gtk.Enums.TextDirection
getTextTagDirection obj = MIO.liftIO $ B.Properties.getObjectPropertyEnum obj "direction"

-- | Set the value of the “@direction@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textTag [ #direction 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextTagDirection :: (MonadIO m, IsTextTag o) => o -> Gtk.Enums.TextDirection -> m ()
setTextTagDirection obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyEnum obj "direction" val

-- | Construct a `GValueConstruct` with valid value for the “@direction@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextTagDirection :: (IsTextTag o, MIO.MonadIO m) => Gtk.Enums.TextDirection -> m (GValueConstruct o)
constructTextTagDirection val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyEnum "direction" val

#if defined(ENABLE_OVERLOADING)
data TextTagDirectionPropertyInfo
instance AttrInfo TextTagDirectionPropertyInfo where
    type AttrAllowedOps TextTagDirectionPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TextTagDirectionPropertyInfo = IsTextTag
    type AttrSetTypeConstraint TextTagDirectionPropertyInfo = (~) Gtk.Enums.TextDirection
    type AttrTransferTypeConstraint TextTagDirectionPropertyInfo = (~) Gtk.Enums.TextDirection
    type AttrTransferType TextTagDirectionPropertyInfo = Gtk.Enums.TextDirection
    type AttrGetType TextTagDirectionPropertyInfo = Gtk.Enums.TextDirection
    type AttrLabel TextTagDirectionPropertyInfo = "direction"
    type AttrOrigin TextTagDirectionPropertyInfo = TextTag
    attrGet = getTextTagDirection
    attrSet = setTextTagDirection
    attrTransfer _ v = do
        return v
    attrConstruct = constructTextTagDirection
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextTag.direction"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextTag.html#g:attr:direction"
        })
#endif

-- VVV Prop "editable"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@editable@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textTag #editable
-- @
getTextTagEditable :: (MonadIO m, IsTextTag o) => o -> m Bool
getTextTagEditable obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "editable"

-- | Set the value of the “@editable@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textTag [ #editable 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextTagEditable :: (MonadIO m, IsTextTag o) => o -> Bool -> m ()
setTextTagEditable obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "editable" val

-- | Construct a `GValueConstruct` with valid value for the “@editable@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextTagEditable :: (IsTextTag o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructTextTagEditable val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "editable" val

#if defined(ENABLE_OVERLOADING)
data TextTagEditablePropertyInfo
instance AttrInfo TextTagEditablePropertyInfo where
    type AttrAllowedOps TextTagEditablePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TextTagEditablePropertyInfo = IsTextTag
    type AttrSetTypeConstraint TextTagEditablePropertyInfo = (~) Bool
    type AttrTransferTypeConstraint TextTagEditablePropertyInfo = (~) Bool
    type AttrTransferType TextTagEditablePropertyInfo = Bool
    type AttrGetType TextTagEditablePropertyInfo = Bool
    type AttrLabel TextTagEditablePropertyInfo = "editable"
    type AttrOrigin TextTagEditablePropertyInfo = TextTag
    attrGet = getTextTagEditable
    attrSet = setTextTagEditable
    attrTransfer _ v = do
        return v
    attrConstruct = constructTextTagEditable
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextTag.editable"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextTag.html#g:attr:editable"
        })
#endif

-- VVV Prop "editable-set"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@editable-set@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textTag #editableSet
-- @
getTextTagEditableSet :: (MonadIO m, IsTextTag o) => o -> m Bool
getTextTagEditableSet obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "editable-set"

-- | Set the value of the “@editable-set@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textTag [ #editableSet 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextTagEditableSet :: (MonadIO m, IsTextTag o) => o -> Bool -> m ()
setTextTagEditableSet obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "editable-set" val

-- | Construct a `GValueConstruct` with valid value for the “@editable-set@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextTagEditableSet :: (IsTextTag o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructTextTagEditableSet val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "editable-set" val

#if defined(ENABLE_OVERLOADING)
data TextTagEditableSetPropertyInfo
instance AttrInfo TextTagEditableSetPropertyInfo where
    type AttrAllowedOps TextTagEditableSetPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TextTagEditableSetPropertyInfo = IsTextTag
    type AttrSetTypeConstraint TextTagEditableSetPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint TextTagEditableSetPropertyInfo = (~) Bool
    type AttrTransferType TextTagEditableSetPropertyInfo = Bool
    type AttrGetType TextTagEditableSetPropertyInfo = Bool
    type AttrLabel TextTagEditableSetPropertyInfo = "editable-set"
    type AttrOrigin TextTagEditableSetPropertyInfo = TextTag
    attrGet = getTextTagEditableSet
    attrSet = setTextTagEditableSet
    attrTransfer _ v = do
        return v
    attrConstruct = constructTextTagEditableSet
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextTag.editableSet"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextTag.html#g:attr:editableSet"
        })
#endif

-- VVV Prop "fallback"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@fallback@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textTag #fallback
-- @
getTextTagFallback :: (MonadIO m, IsTextTag o) => o -> m Bool
getTextTagFallback obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "fallback"

-- | Set the value of the “@fallback@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textTag [ #fallback 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextTagFallback :: (MonadIO m, IsTextTag o) => o -> Bool -> m ()
setTextTagFallback obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "fallback" val

-- | Construct a `GValueConstruct` with valid value for the “@fallback@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextTagFallback :: (IsTextTag o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructTextTagFallback val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "fallback" val

#if defined(ENABLE_OVERLOADING)
data TextTagFallbackPropertyInfo
instance AttrInfo TextTagFallbackPropertyInfo where
    type AttrAllowedOps TextTagFallbackPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TextTagFallbackPropertyInfo = IsTextTag
    type AttrSetTypeConstraint TextTagFallbackPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint TextTagFallbackPropertyInfo = (~) Bool
    type AttrTransferType TextTagFallbackPropertyInfo = Bool
    type AttrGetType TextTagFallbackPropertyInfo = Bool
    type AttrLabel TextTagFallbackPropertyInfo = "fallback"
    type AttrOrigin TextTagFallbackPropertyInfo = TextTag
    attrGet = getTextTagFallback
    attrSet = setTextTagFallback
    attrTransfer _ v = do
        return v
    attrConstruct = constructTextTagFallback
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextTag.fallback"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextTag.html#g:attr:fallback"
        })
#endif

-- VVV Prop "fallback-set"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@fallback-set@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textTag #fallbackSet
-- @
getTextTagFallbackSet :: (MonadIO m, IsTextTag o) => o -> m Bool
getTextTagFallbackSet obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "fallback-set"

-- | Set the value of the “@fallback-set@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textTag [ #fallbackSet 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextTagFallbackSet :: (MonadIO m, IsTextTag o) => o -> Bool -> m ()
setTextTagFallbackSet obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "fallback-set" val

-- | Construct a `GValueConstruct` with valid value for the “@fallback-set@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextTagFallbackSet :: (IsTextTag o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructTextTagFallbackSet val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "fallback-set" val

#if defined(ENABLE_OVERLOADING)
data TextTagFallbackSetPropertyInfo
instance AttrInfo TextTagFallbackSetPropertyInfo where
    type AttrAllowedOps TextTagFallbackSetPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TextTagFallbackSetPropertyInfo = IsTextTag
    type AttrSetTypeConstraint TextTagFallbackSetPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint TextTagFallbackSetPropertyInfo = (~) Bool
    type AttrTransferType TextTagFallbackSetPropertyInfo = Bool
    type AttrGetType TextTagFallbackSetPropertyInfo = Bool
    type AttrLabel TextTagFallbackSetPropertyInfo = "fallback-set"
    type AttrOrigin TextTagFallbackSetPropertyInfo = TextTag
    attrGet = getTextTagFallbackSet
    attrSet = setTextTagFallbackSet
    attrTransfer _ v = do
        return v
    attrConstruct = constructTextTagFallbackSet
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextTag.fallbackSet"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextTag.html#g:attr:fallbackSet"
        })
#endif

-- VVV Prop "family"
   -- Type: TBasicType TUTF8
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@family@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textTag #family
-- @
getTextTagFamily :: (MonadIO m, IsTextTag o) => o -> m (Maybe T.Text)
getTextTagFamily obj = MIO.liftIO $ B.Properties.getObjectPropertyString obj "family"

-- | Set the value of the “@family@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textTag [ #family 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextTagFamily :: (MonadIO m, IsTextTag o) => o -> T.Text -> m ()
setTextTagFamily obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "family" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@family@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextTagFamily :: (IsTextTag o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructTextTagFamily val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "family" (P.Just val)

-- | Set the value of the “@family@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #family
-- @
clearTextTagFamily :: (MonadIO m, IsTextTag o) => o -> m ()
clearTextTagFamily obj = liftIO $ B.Properties.setObjectPropertyString obj "family" (Nothing :: Maybe T.Text)

#if defined(ENABLE_OVERLOADING)
data TextTagFamilyPropertyInfo
instance AttrInfo TextTagFamilyPropertyInfo where
    type AttrAllowedOps TextTagFamilyPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint TextTagFamilyPropertyInfo = IsTextTag
    type AttrSetTypeConstraint TextTagFamilyPropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint TextTagFamilyPropertyInfo = (~) T.Text
    type AttrTransferType TextTagFamilyPropertyInfo = T.Text
    type AttrGetType TextTagFamilyPropertyInfo = (Maybe T.Text)
    type AttrLabel TextTagFamilyPropertyInfo = "family"
    type AttrOrigin TextTagFamilyPropertyInfo = TextTag
    attrGet = getTextTagFamily
    attrSet = setTextTagFamily
    attrTransfer _ v = do
        return v
    attrConstruct = constructTextTagFamily
    attrClear = clearTextTagFamily
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextTag.family"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextTag.html#g:attr:family"
        })
#endif

-- VVV Prop "family-set"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@family-set@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textTag #familySet
-- @
getTextTagFamilySet :: (MonadIO m, IsTextTag o) => o -> m Bool
getTextTagFamilySet obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "family-set"

-- | Set the value of the “@family-set@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textTag [ #familySet 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextTagFamilySet :: (MonadIO m, IsTextTag o) => o -> Bool -> m ()
setTextTagFamilySet obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "family-set" val

-- | Construct a `GValueConstruct` with valid value for the “@family-set@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextTagFamilySet :: (IsTextTag o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructTextTagFamilySet val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "family-set" val

#if defined(ENABLE_OVERLOADING)
data TextTagFamilySetPropertyInfo
instance AttrInfo TextTagFamilySetPropertyInfo where
    type AttrAllowedOps TextTagFamilySetPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TextTagFamilySetPropertyInfo = IsTextTag
    type AttrSetTypeConstraint TextTagFamilySetPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint TextTagFamilySetPropertyInfo = (~) Bool
    type AttrTransferType TextTagFamilySetPropertyInfo = Bool
    type AttrGetType TextTagFamilySetPropertyInfo = Bool
    type AttrLabel TextTagFamilySetPropertyInfo = "family-set"
    type AttrOrigin TextTagFamilySetPropertyInfo = TextTag
    attrGet = getTextTagFamilySet
    attrSet = setTextTagFamilySet
    attrTransfer _ v = do
        return v
    attrConstruct = constructTextTagFamilySet
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextTag.familySet"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextTag.html#g:attr:familySet"
        })
#endif

-- VVV Prop "font"
   -- Type: TBasicType TUTF8
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@font@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textTag #font
-- @
getTextTagFont :: (MonadIO m, IsTextTag o) => o -> m (Maybe T.Text)
getTextTagFont obj = MIO.liftIO $ B.Properties.getObjectPropertyString obj "font"

-- | Set the value of the “@font@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textTag [ #font 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextTagFont :: (MonadIO m, IsTextTag o) => o -> T.Text -> m ()
setTextTagFont obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "font" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@font@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextTagFont :: (IsTextTag o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructTextTagFont val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "font" (P.Just val)

-- | Set the value of the “@font@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #font
-- @
clearTextTagFont :: (MonadIO m, IsTextTag o) => o -> m ()
clearTextTagFont obj = liftIO $ B.Properties.setObjectPropertyString obj "font" (Nothing :: Maybe T.Text)

#if defined(ENABLE_OVERLOADING)
data TextTagFontPropertyInfo
instance AttrInfo TextTagFontPropertyInfo where
    type AttrAllowedOps TextTagFontPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint TextTagFontPropertyInfo = IsTextTag
    type AttrSetTypeConstraint TextTagFontPropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint TextTagFontPropertyInfo = (~) T.Text
    type AttrTransferType TextTagFontPropertyInfo = T.Text
    type AttrGetType TextTagFontPropertyInfo = (Maybe T.Text)
    type AttrLabel TextTagFontPropertyInfo = "font"
    type AttrOrigin TextTagFontPropertyInfo = TextTag
    attrGet = getTextTagFont
    attrSet = setTextTagFont
    attrTransfer _ v = do
        return v
    attrConstruct = constructTextTagFont
    attrClear = clearTextTagFont
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextTag.font"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextTag.html#g:attr:font"
        })
#endif

-- VVV Prop "font-desc"
   -- Type: TInterface (Name {namespace = "Pango", name = "FontDescription"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@font-desc@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textTag #fontDesc
-- @
getTextTagFontDesc :: (MonadIO m, IsTextTag o) => o -> m (Maybe Pango.FontDescription.FontDescription)
getTextTagFontDesc obj = MIO.liftIO $ B.Properties.getObjectPropertyBoxed obj "font-desc" Pango.FontDescription.FontDescription

-- | Set the value of the “@font-desc@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textTag [ #fontDesc 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextTagFontDesc :: (MonadIO m, IsTextTag o) => o -> Pango.FontDescription.FontDescription -> m ()
setTextTagFontDesc obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBoxed obj "font-desc" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@font-desc@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextTagFontDesc :: (IsTextTag o, MIO.MonadIO m) => Pango.FontDescription.FontDescription -> m (GValueConstruct o)
constructTextTagFontDesc val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBoxed "font-desc" (P.Just val)

-- | Set the value of the “@font-desc@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #fontDesc
-- @
clearTextTagFontDesc :: (MonadIO m, IsTextTag o) => o -> m ()
clearTextTagFontDesc obj = liftIO $ B.Properties.setObjectPropertyBoxed obj "font-desc" (Nothing :: Maybe Pango.FontDescription.FontDescription)

#if defined(ENABLE_OVERLOADING)
data TextTagFontDescPropertyInfo
instance AttrInfo TextTagFontDescPropertyInfo where
    type AttrAllowedOps TextTagFontDescPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint TextTagFontDescPropertyInfo = IsTextTag
    type AttrSetTypeConstraint TextTagFontDescPropertyInfo = (~) Pango.FontDescription.FontDescription
    type AttrTransferTypeConstraint TextTagFontDescPropertyInfo = (~) Pango.FontDescription.FontDescription
    type AttrTransferType TextTagFontDescPropertyInfo = Pango.FontDescription.FontDescription
    type AttrGetType TextTagFontDescPropertyInfo = (Maybe Pango.FontDescription.FontDescription)
    type AttrLabel TextTagFontDescPropertyInfo = "font-desc"
    type AttrOrigin TextTagFontDescPropertyInfo = TextTag
    attrGet = getTextTagFontDesc
    attrSet = setTextTagFontDesc
    attrTransfer _ v = do
        return v
    attrConstruct = constructTextTagFontDesc
    attrClear = clearTextTagFontDesc
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextTag.fontDesc"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextTag.html#g:attr:fontDesc"
        })
#endif

-- VVV Prop "font-features"
   -- Type: TBasicType TUTF8
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@font-features@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textTag #fontFeatures
-- @
getTextTagFontFeatures :: (MonadIO m, IsTextTag o) => o -> m (Maybe T.Text)
getTextTagFontFeatures obj = MIO.liftIO $ B.Properties.getObjectPropertyString obj "font-features"

-- | Set the value of the “@font-features@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textTag [ #fontFeatures 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextTagFontFeatures :: (MonadIO m, IsTextTag o) => o -> T.Text -> m ()
setTextTagFontFeatures obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "font-features" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@font-features@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextTagFontFeatures :: (IsTextTag o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructTextTagFontFeatures val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "font-features" (P.Just val)

-- | Set the value of the “@font-features@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #fontFeatures
-- @
clearTextTagFontFeatures :: (MonadIO m, IsTextTag o) => o -> m ()
clearTextTagFontFeatures obj = liftIO $ B.Properties.setObjectPropertyString obj "font-features" (Nothing :: Maybe T.Text)

#if defined(ENABLE_OVERLOADING)
data TextTagFontFeaturesPropertyInfo
instance AttrInfo TextTagFontFeaturesPropertyInfo where
    type AttrAllowedOps TextTagFontFeaturesPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint TextTagFontFeaturesPropertyInfo = IsTextTag
    type AttrSetTypeConstraint TextTagFontFeaturesPropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint TextTagFontFeaturesPropertyInfo = (~) T.Text
    type AttrTransferType TextTagFontFeaturesPropertyInfo = T.Text
    type AttrGetType TextTagFontFeaturesPropertyInfo = (Maybe T.Text)
    type AttrLabel TextTagFontFeaturesPropertyInfo = "font-features"
    type AttrOrigin TextTagFontFeaturesPropertyInfo = TextTag
    attrGet = getTextTagFontFeatures
    attrSet = setTextTagFontFeatures
    attrTransfer _ v = do
        return v
    attrConstruct = constructTextTagFontFeatures
    attrClear = clearTextTagFontFeatures
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextTag.fontFeatures"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextTag.html#g:attr:fontFeatures"
        })
#endif

-- VVV Prop "font-features-set"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@font-features-set@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textTag #fontFeaturesSet
-- @
getTextTagFontFeaturesSet :: (MonadIO m, IsTextTag o) => o -> m Bool
getTextTagFontFeaturesSet obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "font-features-set"

-- | Set the value of the “@font-features-set@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textTag [ #fontFeaturesSet 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextTagFontFeaturesSet :: (MonadIO m, IsTextTag o) => o -> Bool -> m ()
setTextTagFontFeaturesSet obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "font-features-set" val

-- | Construct a `GValueConstruct` with valid value for the “@font-features-set@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextTagFontFeaturesSet :: (IsTextTag o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructTextTagFontFeaturesSet val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "font-features-set" val

#if defined(ENABLE_OVERLOADING)
data TextTagFontFeaturesSetPropertyInfo
instance AttrInfo TextTagFontFeaturesSetPropertyInfo where
    type AttrAllowedOps TextTagFontFeaturesSetPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TextTagFontFeaturesSetPropertyInfo = IsTextTag
    type AttrSetTypeConstraint TextTagFontFeaturesSetPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint TextTagFontFeaturesSetPropertyInfo = (~) Bool
    type AttrTransferType TextTagFontFeaturesSetPropertyInfo = Bool
    type AttrGetType TextTagFontFeaturesSetPropertyInfo = Bool
    type AttrLabel TextTagFontFeaturesSetPropertyInfo = "font-features-set"
    type AttrOrigin TextTagFontFeaturesSetPropertyInfo = TextTag
    attrGet = getTextTagFontFeaturesSet
    attrSet = setTextTagFontFeaturesSet
    attrTransfer _ v = do
        return v
    attrConstruct = constructTextTagFontFeaturesSet
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextTag.fontFeaturesSet"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextTag.html#g:attr:fontFeaturesSet"
        })
#endif

-- VVV Prop "foreground"
   -- Type: TBasicType TUTF8
   -- Flags: [PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Set the value of the “@foreground@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textTag [ #foreground 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextTagForeground :: (MonadIO m, IsTextTag o) => o -> T.Text -> m ()
setTextTagForeground obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "foreground" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@foreground@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextTagForeground :: (IsTextTag o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructTextTagForeground val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "foreground" (P.Just val)

-- | Set the value of the “@foreground@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #foreground
-- @
clearTextTagForeground :: (MonadIO m, IsTextTag o) => o -> m ()
clearTextTagForeground obj = liftIO $ B.Properties.setObjectPropertyString obj "foreground" (Nothing :: Maybe T.Text)

#if defined(ENABLE_OVERLOADING)
data TextTagForegroundPropertyInfo
instance AttrInfo TextTagForegroundPropertyInfo where
    type AttrAllowedOps TextTagForegroundPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrClear]
    type AttrBaseTypeConstraint TextTagForegroundPropertyInfo = IsTextTag
    type AttrSetTypeConstraint TextTagForegroundPropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint TextTagForegroundPropertyInfo = (~) T.Text
    type AttrTransferType TextTagForegroundPropertyInfo = T.Text
    type AttrGetType TextTagForegroundPropertyInfo = ()
    type AttrLabel TextTagForegroundPropertyInfo = "foreground"
    type AttrOrigin TextTagForegroundPropertyInfo = TextTag
    attrGet = undefined
    attrSet = setTextTagForeground
    attrTransfer _ v = do
        return v
    attrConstruct = constructTextTagForeground
    attrClear = clearTextTagForeground
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextTag.foreground"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextTag.html#g:attr:foreground"
        })
#endif

-- VVV Prop "foreground-gdk"
   -- Type: TInterface (Name {namespace = "Gdk", name = "Color"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@foreground-gdk@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textTag #foregroundGdk
-- @
getTextTagForegroundGdk :: (MonadIO m, IsTextTag o) => o -> m (Maybe Gdk.Color.Color)
getTextTagForegroundGdk obj = MIO.liftIO $ B.Properties.getObjectPropertyBoxed obj "foreground-gdk" Gdk.Color.Color

-- | Set the value of the “@foreground-gdk@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textTag [ #foregroundGdk 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextTagForegroundGdk :: (MonadIO m, IsTextTag o) => o -> Gdk.Color.Color -> m ()
setTextTagForegroundGdk obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBoxed obj "foreground-gdk" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@foreground-gdk@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextTagForegroundGdk :: (IsTextTag o, MIO.MonadIO m) => Gdk.Color.Color -> m (GValueConstruct o)
constructTextTagForegroundGdk val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBoxed "foreground-gdk" (P.Just val)

-- | Set the value of the “@foreground-gdk@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #foregroundGdk
-- @
clearTextTagForegroundGdk :: (MonadIO m, IsTextTag o) => o -> m ()
clearTextTagForegroundGdk obj = liftIO $ B.Properties.setObjectPropertyBoxed obj "foreground-gdk" (Nothing :: Maybe Gdk.Color.Color)

#if defined(ENABLE_OVERLOADING)
data TextTagForegroundGdkPropertyInfo
instance AttrInfo TextTagForegroundGdkPropertyInfo where
    type AttrAllowedOps TextTagForegroundGdkPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint TextTagForegroundGdkPropertyInfo = IsTextTag
    type AttrSetTypeConstraint TextTagForegroundGdkPropertyInfo = (~) Gdk.Color.Color
    type AttrTransferTypeConstraint TextTagForegroundGdkPropertyInfo = (~) Gdk.Color.Color
    type AttrTransferType TextTagForegroundGdkPropertyInfo = Gdk.Color.Color
    type AttrGetType TextTagForegroundGdkPropertyInfo = (Maybe Gdk.Color.Color)
    type AttrLabel TextTagForegroundGdkPropertyInfo = "foreground-gdk"
    type AttrOrigin TextTagForegroundGdkPropertyInfo = TextTag
    attrGet = getTextTagForegroundGdk
    attrSet = setTextTagForegroundGdk
    attrTransfer _ v = do
        return v
    attrConstruct = constructTextTagForegroundGdk
    attrClear = clearTextTagForegroundGdk
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextTag.foregroundGdk"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextTag.html#g:attr:foregroundGdk"
        })
#endif

-- VVV Prop "foreground-rgba"
   -- Type: TInterface (Name {namespace = "Gdk", name = "RGBA"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@foreground-rgba@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textTag #foregroundRgba
-- @
getTextTagForegroundRgba :: (MonadIO m, IsTextTag o) => o -> m (Maybe Gdk.RGBA.RGBA)
getTextTagForegroundRgba obj = MIO.liftIO $ B.Properties.getObjectPropertyBoxed obj "foreground-rgba" Gdk.RGBA.RGBA

-- | Set the value of the “@foreground-rgba@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textTag [ #foregroundRgba 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextTagForegroundRgba :: (MonadIO m, IsTextTag o) => o -> Gdk.RGBA.RGBA -> m ()
setTextTagForegroundRgba obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBoxed obj "foreground-rgba" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@foreground-rgba@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextTagForegroundRgba :: (IsTextTag o, MIO.MonadIO m) => Gdk.RGBA.RGBA -> m (GValueConstruct o)
constructTextTagForegroundRgba val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBoxed "foreground-rgba" (P.Just val)

-- | Set the value of the “@foreground-rgba@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #foregroundRgba
-- @
clearTextTagForegroundRgba :: (MonadIO m, IsTextTag o) => o -> m ()
clearTextTagForegroundRgba obj = liftIO $ B.Properties.setObjectPropertyBoxed obj "foreground-rgba" (Nothing :: Maybe Gdk.RGBA.RGBA)

#if defined(ENABLE_OVERLOADING)
data TextTagForegroundRgbaPropertyInfo
instance AttrInfo TextTagForegroundRgbaPropertyInfo where
    type AttrAllowedOps TextTagForegroundRgbaPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint TextTagForegroundRgbaPropertyInfo = IsTextTag
    type AttrSetTypeConstraint TextTagForegroundRgbaPropertyInfo = (~) Gdk.RGBA.RGBA
    type AttrTransferTypeConstraint TextTagForegroundRgbaPropertyInfo = (~) Gdk.RGBA.RGBA
    type AttrTransferType TextTagForegroundRgbaPropertyInfo = Gdk.RGBA.RGBA
    type AttrGetType TextTagForegroundRgbaPropertyInfo = (Maybe Gdk.RGBA.RGBA)
    type AttrLabel TextTagForegroundRgbaPropertyInfo = "foreground-rgba"
    type AttrOrigin TextTagForegroundRgbaPropertyInfo = TextTag
    attrGet = getTextTagForegroundRgba
    attrSet = setTextTagForegroundRgba
    attrTransfer _ v = do
        return v
    attrConstruct = constructTextTagForegroundRgba
    attrClear = clearTextTagForegroundRgba
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextTag.foregroundRgba"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextTag.html#g:attr:foregroundRgba"
        })
#endif

-- VVV Prop "foreground-set"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@foreground-set@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textTag #foregroundSet
-- @
getTextTagForegroundSet :: (MonadIO m, IsTextTag o) => o -> m Bool
getTextTagForegroundSet obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "foreground-set"

-- | Set the value of the “@foreground-set@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textTag [ #foregroundSet 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextTagForegroundSet :: (MonadIO m, IsTextTag o) => o -> Bool -> m ()
setTextTagForegroundSet obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "foreground-set" val

-- | Construct a `GValueConstruct` with valid value for the “@foreground-set@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextTagForegroundSet :: (IsTextTag o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructTextTagForegroundSet val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "foreground-set" val

#if defined(ENABLE_OVERLOADING)
data TextTagForegroundSetPropertyInfo
instance AttrInfo TextTagForegroundSetPropertyInfo where
    type AttrAllowedOps TextTagForegroundSetPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TextTagForegroundSetPropertyInfo = IsTextTag
    type AttrSetTypeConstraint TextTagForegroundSetPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint TextTagForegroundSetPropertyInfo = (~) Bool
    type AttrTransferType TextTagForegroundSetPropertyInfo = Bool
    type AttrGetType TextTagForegroundSetPropertyInfo = Bool
    type AttrLabel TextTagForegroundSetPropertyInfo = "foreground-set"
    type AttrOrigin TextTagForegroundSetPropertyInfo = TextTag
    attrGet = getTextTagForegroundSet
    attrSet = setTextTagForegroundSet
    attrTransfer _ v = do
        return v
    attrConstruct = constructTextTagForegroundSet
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextTag.foregroundSet"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextTag.html#g:attr:foregroundSet"
        })
#endif

-- VVV Prop "indent"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@indent@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textTag #indent
-- @
getTextTagIndent :: (MonadIO m, IsTextTag o) => o -> m Int32
getTextTagIndent obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "indent"

-- | Set the value of the “@indent@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textTag [ #indent 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextTagIndent :: (MonadIO m, IsTextTag o) => o -> Int32 -> m ()
setTextTagIndent obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyInt32 obj "indent" val

-- | Construct a `GValueConstruct` with valid value for the “@indent@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextTagIndent :: (IsTextTag o, MIO.MonadIO m) => Int32 -> m (GValueConstruct o)
constructTextTagIndent val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyInt32 "indent" val

#if defined(ENABLE_OVERLOADING)
data TextTagIndentPropertyInfo
instance AttrInfo TextTagIndentPropertyInfo where
    type AttrAllowedOps TextTagIndentPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TextTagIndentPropertyInfo = IsTextTag
    type AttrSetTypeConstraint TextTagIndentPropertyInfo = (~) Int32
    type AttrTransferTypeConstraint TextTagIndentPropertyInfo = (~) Int32
    type AttrTransferType TextTagIndentPropertyInfo = Int32
    type AttrGetType TextTagIndentPropertyInfo = Int32
    type AttrLabel TextTagIndentPropertyInfo = "indent"
    type AttrOrigin TextTagIndentPropertyInfo = TextTag
    attrGet = getTextTagIndent
    attrSet = setTextTagIndent
    attrTransfer _ v = do
        return v
    attrConstruct = constructTextTagIndent
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextTag.indent"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextTag.html#g:attr:indent"
        })
#endif

-- VVV Prop "indent-set"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@indent-set@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textTag #indentSet
-- @
getTextTagIndentSet :: (MonadIO m, IsTextTag o) => o -> m Bool
getTextTagIndentSet obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "indent-set"

-- | Set the value of the “@indent-set@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textTag [ #indentSet 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextTagIndentSet :: (MonadIO m, IsTextTag o) => o -> Bool -> m ()
setTextTagIndentSet obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "indent-set" val

-- | Construct a `GValueConstruct` with valid value for the “@indent-set@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextTagIndentSet :: (IsTextTag o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructTextTagIndentSet val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "indent-set" val

#if defined(ENABLE_OVERLOADING)
data TextTagIndentSetPropertyInfo
instance AttrInfo TextTagIndentSetPropertyInfo where
    type AttrAllowedOps TextTagIndentSetPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TextTagIndentSetPropertyInfo = IsTextTag
    type AttrSetTypeConstraint TextTagIndentSetPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint TextTagIndentSetPropertyInfo = (~) Bool
    type AttrTransferType TextTagIndentSetPropertyInfo = Bool
    type AttrGetType TextTagIndentSetPropertyInfo = Bool
    type AttrLabel TextTagIndentSetPropertyInfo = "indent-set"
    type AttrOrigin TextTagIndentSetPropertyInfo = TextTag
    attrGet = getTextTagIndentSet
    attrSet = setTextTagIndentSet
    attrTransfer _ v = do
        return v
    attrConstruct = constructTextTagIndentSet
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextTag.indentSet"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextTag.html#g:attr:indentSet"
        })
#endif

-- VVV Prop "invisible"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@invisible@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textTag #invisible
-- @
getTextTagInvisible :: (MonadIO m, IsTextTag o) => o -> m Bool
getTextTagInvisible obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "invisible"

-- | Set the value of the “@invisible@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textTag [ #invisible 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextTagInvisible :: (MonadIO m, IsTextTag o) => o -> Bool -> m ()
setTextTagInvisible obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "invisible" val

-- | Construct a `GValueConstruct` with valid value for the “@invisible@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextTagInvisible :: (IsTextTag o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructTextTagInvisible val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "invisible" val

#if defined(ENABLE_OVERLOADING)
data TextTagInvisiblePropertyInfo
instance AttrInfo TextTagInvisiblePropertyInfo where
    type AttrAllowedOps TextTagInvisiblePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TextTagInvisiblePropertyInfo = IsTextTag
    type AttrSetTypeConstraint TextTagInvisiblePropertyInfo = (~) Bool
    type AttrTransferTypeConstraint TextTagInvisiblePropertyInfo = (~) Bool
    type AttrTransferType TextTagInvisiblePropertyInfo = Bool
    type AttrGetType TextTagInvisiblePropertyInfo = Bool
    type AttrLabel TextTagInvisiblePropertyInfo = "invisible"
    type AttrOrigin TextTagInvisiblePropertyInfo = TextTag
    attrGet = getTextTagInvisible
    attrSet = setTextTagInvisible
    attrTransfer _ v = do
        return v
    attrConstruct = constructTextTagInvisible
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextTag.invisible"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextTag.html#g:attr:invisible"
        })
#endif

-- VVV Prop "invisible-set"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@invisible-set@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textTag #invisibleSet
-- @
getTextTagInvisibleSet :: (MonadIO m, IsTextTag o) => o -> m Bool
getTextTagInvisibleSet obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "invisible-set"

-- | Set the value of the “@invisible-set@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textTag [ #invisibleSet 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextTagInvisibleSet :: (MonadIO m, IsTextTag o) => o -> Bool -> m ()
setTextTagInvisibleSet obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "invisible-set" val

-- | Construct a `GValueConstruct` with valid value for the “@invisible-set@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextTagInvisibleSet :: (IsTextTag o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructTextTagInvisibleSet val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "invisible-set" val

#if defined(ENABLE_OVERLOADING)
data TextTagInvisibleSetPropertyInfo
instance AttrInfo TextTagInvisibleSetPropertyInfo where
    type AttrAllowedOps TextTagInvisibleSetPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TextTagInvisibleSetPropertyInfo = IsTextTag
    type AttrSetTypeConstraint TextTagInvisibleSetPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint TextTagInvisibleSetPropertyInfo = (~) Bool
    type AttrTransferType TextTagInvisibleSetPropertyInfo = Bool
    type AttrGetType TextTagInvisibleSetPropertyInfo = Bool
    type AttrLabel TextTagInvisibleSetPropertyInfo = "invisible-set"
    type AttrOrigin TextTagInvisibleSetPropertyInfo = TextTag
    attrGet = getTextTagInvisibleSet
    attrSet = setTextTagInvisibleSet
    attrTransfer _ v = do
        return v
    attrConstruct = constructTextTagInvisibleSet
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextTag.invisibleSet"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextTag.html#g:attr:invisibleSet"
        })
#endif

-- VVV Prop "justification"
   -- Type: TInterface (Name {namespace = "Gtk", name = "Justification"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@justification@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textTag #justification
-- @
getTextTagJustification :: (MonadIO m, IsTextTag o) => o -> m Gtk.Enums.Justification
getTextTagJustification obj = MIO.liftIO $ B.Properties.getObjectPropertyEnum obj "justification"

-- | Set the value of the “@justification@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textTag [ #justification 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextTagJustification :: (MonadIO m, IsTextTag o) => o -> Gtk.Enums.Justification -> m ()
setTextTagJustification obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyEnum obj "justification" val

-- | Construct a `GValueConstruct` with valid value for the “@justification@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextTagJustification :: (IsTextTag o, MIO.MonadIO m) => Gtk.Enums.Justification -> m (GValueConstruct o)
constructTextTagJustification val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyEnum "justification" val

#if defined(ENABLE_OVERLOADING)
data TextTagJustificationPropertyInfo
instance AttrInfo TextTagJustificationPropertyInfo where
    type AttrAllowedOps TextTagJustificationPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TextTagJustificationPropertyInfo = IsTextTag
    type AttrSetTypeConstraint TextTagJustificationPropertyInfo = (~) Gtk.Enums.Justification
    type AttrTransferTypeConstraint TextTagJustificationPropertyInfo = (~) Gtk.Enums.Justification
    type AttrTransferType TextTagJustificationPropertyInfo = Gtk.Enums.Justification
    type AttrGetType TextTagJustificationPropertyInfo = Gtk.Enums.Justification
    type AttrLabel TextTagJustificationPropertyInfo = "justification"
    type AttrOrigin TextTagJustificationPropertyInfo = TextTag
    attrGet = getTextTagJustification
    attrSet = setTextTagJustification
    attrTransfer _ v = do
        return v
    attrConstruct = constructTextTagJustification
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextTag.justification"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextTag.html#g:attr:justification"
        })
#endif

-- VVV Prop "justification-set"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@justification-set@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textTag #justificationSet
-- @
getTextTagJustificationSet :: (MonadIO m, IsTextTag o) => o -> m Bool
getTextTagJustificationSet obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "justification-set"

-- | Set the value of the “@justification-set@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textTag [ #justificationSet 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextTagJustificationSet :: (MonadIO m, IsTextTag o) => o -> Bool -> m ()
setTextTagJustificationSet obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "justification-set" val

-- | Construct a `GValueConstruct` with valid value for the “@justification-set@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextTagJustificationSet :: (IsTextTag o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructTextTagJustificationSet val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "justification-set" val

#if defined(ENABLE_OVERLOADING)
data TextTagJustificationSetPropertyInfo
instance AttrInfo TextTagJustificationSetPropertyInfo where
    type AttrAllowedOps TextTagJustificationSetPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TextTagJustificationSetPropertyInfo = IsTextTag
    type AttrSetTypeConstraint TextTagJustificationSetPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint TextTagJustificationSetPropertyInfo = (~) Bool
    type AttrTransferType TextTagJustificationSetPropertyInfo = Bool
    type AttrGetType TextTagJustificationSetPropertyInfo = Bool
    type AttrLabel TextTagJustificationSetPropertyInfo = "justification-set"
    type AttrOrigin TextTagJustificationSetPropertyInfo = TextTag
    attrGet = getTextTagJustificationSet
    attrSet = setTextTagJustificationSet
    attrTransfer _ v = do
        return v
    attrConstruct = constructTextTagJustificationSet
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextTag.justificationSet"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextTag.html#g:attr:justificationSet"
        })
#endif

-- VVV Prop "language"
   -- Type: TBasicType TUTF8
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@language@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textTag #language
-- @
getTextTagLanguage :: (MonadIO m, IsTextTag o) => o -> m (Maybe T.Text)
getTextTagLanguage obj = MIO.liftIO $ B.Properties.getObjectPropertyString obj "language"

-- | Set the value of the “@language@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textTag [ #language 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextTagLanguage :: (MonadIO m, IsTextTag o) => o -> T.Text -> m ()
setTextTagLanguage obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "language" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@language@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextTagLanguage :: (IsTextTag o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructTextTagLanguage val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "language" (P.Just val)

-- | Set the value of the “@language@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #language
-- @
clearTextTagLanguage :: (MonadIO m, IsTextTag o) => o -> m ()
clearTextTagLanguage obj = liftIO $ B.Properties.setObjectPropertyString obj "language" (Nothing :: Maybe T.Text)

#if defined(ENABLE_OVERLOADING)
data TextTagLanguagePropertyInfo
instance AttrInfo TextTagLanguagePropertyInfo where
    type AttrAllowedOps TextTagLanguagePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint TextTagLanguagePropertyInfo = IsTextTag
    type AttrSetTypeConstraint TextTagLanguagePropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint TextTagLanguagePropertyInfo = (~) T.Text
    type AttrTransferType TextTagLanguagePropertyInfo = T.Text
    type AttrGetType TextTagLanguagePropertyInfo = (Maybe T.Text)
    type AttrLabel TextTagLanguagePropertyInfo = "language"
    type AttrOrigin TextTagLanguagePropertyInfo = TextTag
    attrGet = getTextTagLanguage
    attrSet = setTextTagLanguage
    attrTransfer _ v = do
        return v
    attrConstruct = constructTextTagLanguage
    attrClear = clearTextTagLanguage
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextTag.language"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextTag.html#g:attr:language"
        })
#endif

-- VVV Prop "language-set"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@language-set@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textTag #languageSet
-- @
getTextTagLanguageSet :: (MonadIO m, IsTextTag o) => o -> m Bool
getTextTagLanguageSet obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "language-set"

-- | Set the value of the “@language-set@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textTag [ #languageSet 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextTagLanguageSet :: (MonadIO m, IsTextTag o) => o -> Bool -> m ()
setTextTagLanguageSet obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "language-set" val

-- | Construct a `GValueConstruct` with valid value for the “@language-set@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextTagLanguageSet :: (IsTextTag o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructTextTagLanguageSet val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "language-set" val

#if defined(ENABLE_OVERLOADING)
data TextTagLanguageSetPropertyInfo
instance AttrInfo TextTagLanguageSetPropertyInfo where
    type AttrAllowedOps TextTagLanguageSetPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TextTagLanguageSetPropertyInfo = IsTextTag
    type AttrSetTypeConstraint TextTagLanguageSetPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint TextTagLanguageSetPropertyInfo = (~) Bool
    type AttrTransferType TextTagLanguageSetPropertyInfo = Bool
    type AttrGetType TextTagLanguageSetPropertyInfo = Bool
    type AttrLabel TextTagLanguageSetPropertyInfo = "language-set"
    type AttrOrigin TextTagLanguageSetPropertyInfo = TextTag
    attrGet = getTextTagLanguageSet
    attrSet = setTextTagLanguageSet
    attrTransfer _ v = do
        return v
    attrConstruct = constructTextTagLanguageSet
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextTag.languageSet"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextTag.html#g:attr:languageSet"
        })
#endif

-- VVV Prop "left-margin"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@left-margin@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textTag #leftMargin
-- @
getTextTagLeftMargin :: (MonadIO m, IsTextTag o) => o -> m Int32
getTextTagLeftMargin obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "left-margin"

-- | Set the value of the “@left-margin@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textTag [ #leftMargin 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextTagLeftMargin :: (MonadIO m, IsTextTag o) => o -> Int32 -> m ()
setTextTagLeftMargin obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyInt32 obj "left-margin" val

-- | Construct a `GValueConstruct` with valid value for the “@left-margin@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextTagLeftMargin :: (IsTextTag o, MIO.MonadIO m) => Int32 -> m (GValueConstruct o)
constructTextTagLeftMargin val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyInt32 "left-margin" val

#if defined(ENABLE_OVERLOADING)
data TextTagLeftMarginPropertyInfo
instance AttrInfo TextTagLeftMarginPropertyInfo where
    type AttrAllowedOps TextTagLeftMarginPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TextTagLeftMarginPropertyInfo = IsTextTag
    type AttrSetTypeConstraint TextTagLeftMarginPropertyInfo = (~) Int32
    type AttrTransferTypeConstraint TextTagLeftMarginPropertyInfo = (~) Int32
    type AttrTransferType TextTagLeftMarginPropertyInfo = Int32
    type AttrGetType TextTagLeftMarginPropertyInfo = Int32
    type AttrLabel TextTagLeftMarginPropertyInfo = "left-margin"
    type AttrOrigin TextTagLeftMarginPropertyInfo = TextTag
    attrGet = getTextTagLeftMargin
    attrSet = setTextTagLeftMargin
    attrTransfer _ v = do
        return v
    attrConstruct = constructTextTagLeftMargin
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextTag.leftMargin"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextTag.html#g:attr:leftMargin"
        })
#endif

-- VVV Prop "left-margin-set"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@left-margin-set@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textTag #leftMarginSet
-- @
getTextTagLeftMarginSet :: (MonadIO m, IsTextTag o) => o -> m Bool
getTextTagLeftMarginSet obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "left-margin-set"

-- | Set the value of the “@left-margin-set@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textTag [ #leftMarginSet 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextTagLeftMarginSet :: (MonadIO m, IsTextTag o) => o -> Bool -> m ()
setTextTagLeftMarginSet obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "left-margin-set" val

-- | Construct a `GValueConstruct` with valid value for the “@left-margin-set@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextTagLeftMarginSet :: (IsTextTag o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructTextTagLeftMarginSet val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "left-margin-set" val

#if defined(ENABLE_OVERLOADING)
data TextTagLeftMarginSetPropertyInfo
instance AttrInfo TextTagLeftMarginSetPropertyInfo where
    type AttrAllowedOps TextTagLeftMarginSetPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TextTagLeftMarginSetPropertyInfo = IsTextTag
    type AttrSetTypeConstraint TextTagLeftMarginSetPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint TextTagLeftMarginSetPropertyInfo = (~) Bool
    type AttrTransferType TextTagLeftMarginSetPropertyInfo = Bool
    type AttrGetType TextTagLeftMarginSetPropertyInfo = Bool
    type AttrLabel TextTagLeftMarginSetPropertyInfo = "left-margin-set"
    type AttrOrigin TextTagLeftMarginSetPropertyInfo = TextTag
    attrGet = getTextTagLeftMarginSet
    attrSet = setTextTagLeftMarginSet
    attrTransfer _ v = do
        return v
    attrConstruct = constructTextTagLeftMarginSet
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextTag.leftMarginSet"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextTag.html#g:attr:leftMarginSet"
        })
#endif

-- VVV Prop "letter-spacing"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@letter-spacing@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textTag #letterSpacing
-- @
getTextTagLetterSpacing :: (MonadIO m, IsTextTag o) => o -> m Int32
getTextTagLetterSpacing obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "letter-spacing"

-- | Set the value of the “@letter-spacing@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textTag [ #letterSpacing 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextTagLetterSpacing :: (MonadIO m, IsTextTag o) => o -> Int32 -> m ()
setTextTagLetterSpacing obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyInt32 obj "letter-spacing" val

-- | Construct a `GValueConstruct` with valid value for the “@letter-spacing@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextTagLetterSpacing :: (IsTextTag o, MIO.MonadIO m) => Int32 -> m (GValueConstruct o)
constructTextTagLetterSpacing val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyInt32 "letter-spacing" val

#if defined(ENABLE_OVERLOADING)
data TextTagLetterSpacingPropertyInfo
instance AttrInfo TextTagLetterSpacingPropertyInfo where
    type AttrAllowedOps TextTagLetterSpacingPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TextTagLetterSpacingPropertyInfo = IsTextTag
    type AttrSetTypeConstraint TextTagLetterSpacingPropertyInfo = (~) Int32
    type AttrTransferTypeConstraint TextTagLetterSpacingPropertyInfo = (~) Int32
    type AttrTransferType TextTagLetterSpacingPropertyInfo = Int32
    type AttrGetType TextTagLetterSpacingPropertyInfo = Int32
    type AttrLabel TextTagLetterSpacingPropertyInfo = "letter-spacing"
    type AttrOrigin TextTagLetterSpacingPropertyInfo = TextTag
    attrGet = getTextTagLetterSpacing
    attrSet = setTextTagLetterSpacing
    attrTransfer _ v = do
        return v
    attrConstruct = constructTextTagLetterSpacing
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextTag.letterSpacing"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextTag.html#g:attr:letterSpacing"
        })
#endif

-- VVV Prop "letter-spacing-set"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@letter-spacing-set@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textTag #letterSpacingSet
-- @
getTextTagLetterSpacingSet :: (MonadIO m, IsTextTag o) => o -> m Bool
getTextTagLetterSpacingSet obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "letter-spacing-set"

-- | Set the value of the “@letter-spacing-set@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textTag [ #letterSpacingSet 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextTagLetterSpacingSet :: (MonadIO m, IsTextTag o) => o -> Bool -> m ()
setTextTagLetterSpacingSet obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "letter-spacing-set" val

-- | Construct a `GValueConstruct` with valid value for the “@letter-spacing-set@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextTagLetterSpacingSet :: (IsTextTag o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructTextTagLetterSpacingSet val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "letter-spacing-set" val

#if defined(ENABLE_OVERLOADING)
data TextTagLetterSpacingSetPropertyInfo
instance AttrInfo TextTagLetterSpacingSetPropertyInfo where
    type AttrAllowedOps TextTagLetterSpacingSetPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TextTagLetterSpacingSetPropertyInfo = IsTextTag
    type AttrSetTypeConstraint TextTagLetterSpacingSetPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint TextTagLetterSpacingSetPropertyInfo = (~) Bool
    type AttrTransferType TextTagLetterSpacingSetPropertyInfo = Bool
    type AttrGetType TextTagLetterSpacingSetPropertyInfo = Bool
    type AttrLabel TextTagLetterSpacingSetPropertyInfo = "letter-spacing-set"
    type AttrOrigin TextTagLetterSpacingSetPropertyInfo = TextTag
    attrGet = getTextTagLetterSpacingSet
    attrSet = setTextTagLetterSpacingSet
    attrTransfer _ v = do
        return v
    attrConstruct = constructTextTagLetterSpacingSet
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextTag.letterSpacingSet"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextTag.html#g:attr:letterSpacingSet"
        })
#endif

-- VVV Prop "name"
   -- Type: TBasicType TUTF8
   -- Flags: [PropertyReadable,PropertyWritable,PropertyConstructOnly]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@name@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textTag #name
-- @
getTextTagName :: (MonadIO m, IsTextTag o) => o -> m (Maybe T.Text)
getTextTagName obj = MIO.liftIO $ B.Properties.getObjectPropertyString obj "name"

-- | Construct a `GValueConstruct` with valid value for the “@name@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextTagName :: (IsTextTag o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructTextTagName val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "name" (P.Just val)

#if defined(ENABLE_OVERLOADING)
data TextTagNamePropertyInfo
instance AttrInfo TextTagNamePropertyInfo where
    type AttrAllowedOps TextTagNamePropertyInfo = '[ 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint TextTagNamePropertyInfo = IsTextTag
    type AttrSetTypeConstraint TextTagNamePropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint TextTagNamePropertyInfo = (~) T.Text
    type AttrTransferType TextTagNamePropertyInfo = T.Text
    type AttrGetType TextTagNamePropertyInfo = (Maybe T.Text)
    type AttrLabel TextTagNamePropertyInfo = "name"
    type AttrOrigin TextTagNamePropertyInfo = TextTag
    attrGet = getTextTagName
    attrSet = undefined
    attrTransfer _ v = do
        return v
    attrConstruct = constructTextTagName
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextTag.name"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextTag.html#g:attr:name"
        })
#endif

-- VVV Prop "paragraph-background"
   -- Type: TBasicType TUTF8
   -- Flags: [PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Set the value of the “@paragraph-background@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textTag [ #paragraphBackground 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextTagParagraphBackground :: (MonadIO m, IsTextTag o) => o -> T.Text -> m ()
setTextTagParagraphBackground obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "paragraph-background" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@paragraph-background@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextTagParagraphBackground :: (IsTextTag o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructTextTagParagraphBackground val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "paragraph-background" (P.Just val)

-- | Set the value of the “@paragraph-background@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #paragraphBackground
-- @
clearTextTagParagraphBackground :: (MonadIO m, IsTextTag o) => o -> m ()
clearTextTagParagraphBackground obj = liftIO $ B.Properties.setObjectPropertyString obj "paragraph-background" (Nothing :: Maybe T.Text)

#if defined(ENABLE_OVERLOADING)
data TextTagParagraphBackgroundPropertyInfo
instance AttrInfo TextTagParagraphBackgroundPropertyInfo where
    type AttrAllowedOps TextTagParagraphBackgroundPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrClear]
    type AttrBaseTypeConstraint TextTagParagraphBackgroundPropertyInfo = IsTextTag
    type AttrSetTypeConstraint TextTagParagraphBackgroundPropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint TextTagParagraphBackgroundPropertyInfo = (~) T.Text
    type AttrTransferType TextTagParagraphBackgroundPropertyInfo = T.Text
    type AttrGetType TextTagParagraphBackgroundPropertyInfo = ()
    type AttrLabel TextTagParagraphBackgroundPropertyInfo = "paragraph-background"
    type AttrOrigin TextTagParagraphBackgroundPropertyInfo = TextTag
    attrGet = undefined
    attrSet = setTextTagParagraphBackground
    attrTransfer _ v = do
        return v
    attrConstruct = constructTextTagParagraphBackground
    attrClear = clearTextTagParagraphBackground
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextTag.paragraphBackground"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextTag.html#g:attr:paragraphBackground"
        })
#endif

-- VVV Prop "paragraph-background-gdk"
   -- Type: TInterface (Name {namespace = "Gdk", name = "Color"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@paragraph-background-gdk@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textTag #paragraphBackgroundGdk
-- @
getTextTagParagraphBackgroundGdk :: (MonadIO m, IsTextTag o) => o -> m (Maybe Gdk.Color.Color)
getTextTagParagraphBackgroundGdk obj = MIO.liftIO $ B.Properties.getObjectPropertyBoxed obj "paragraph-background-gdk" Gdk.Color.Color

-- | Set the value of the “@paragraph-background-gdk@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textTag [ #paragraphBackgroundGdk 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextTagParagraphBackgroundGdk :: (MonadIO m, IsTextTag o) => o -> Gdk.Color.Color -> m ()
setTextTagParagraphBackgroundGdk obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBoxed obj "paragraph-background-gdk" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@paragraph-background-gdk@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextTagParagraphBackgroundGdk :: (IsTextTag o, MIO.MonadIO m) => Gdk.Color.Color -> m (GValueConstruct o)
constructTextTagParagraphBackgroundGdk val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBoxed "paragraph-background-gdk" (P.Just val)

-- | Set the value of the “@paragraph-background-gdk@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #paragraphBackgroundGdk
-- @
clearTextTagParagraphBackgroundGdk :: (MonadIO m, IsTextTag o) => o -> m ()
clearTextTagParagraphBackgroundGdk obj = liftIO $ B.Properties.setObjectPropertyBoxed obj "paragraph-background-gdk" (Nothing :: Maybe Gdk.Color.Color)

#if defined(ENABLE_OVERLOADING)
data TextTagParagraphBackgroundGdkPropertyInfo
instance AttrInfo TextTagParagraphBackgroundGdkPropertyInfo where
    type AttrAllowedOps TextTagParagraphBackgroundGdkPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint TextTagParagraphBackgroundGdkPropertyInfo = IsTextTag
    type AttrSetTypeConstraint TextTagParagraphBackgroundGdkPropertyInfo = (~) Gdk.Color.Color
    type AttrTransferTypeConstraint TextTagParagraphBackgroundGdkPropertyInfo = (~) Gdk.Color.Color
    type AttrTransferType TextTagParagraphBackgroundGdkPropertyInfo = Gdk.Color.Color
    type AttrGetType TextTagParagraphBackgroundGdkPropertyInfo = (Maybe Gdk.Color.Color)
    type AttrLabel TextTagParagraphBackgroundGdkPropertyInfo = "paragraph-background-gdk"
    type AttrOrigin TextTagParagraphBackgroundGdkPropertyInfo = TextTag
    attrGet = getTextTagParagraphBackgroundGdk
    attrSet = setTextTagParagraphBackgroundGdk
    attrTransfer _ v = do
        return v
    attrConstruct = constructTextTagParagraphBackgroundGdk
    attrClear = clearTextTagParagraphBackgroundGdk
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextTag.paragraphBackgroundGdk"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextTag.html#g:attr:paragraphBackgroundGdk"
        })
#endif

-- VVV Prop "paragraph-background-rgba"
   -- Type: TInterface (Name {namespace = "Gdk", name = "RGBA"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@paragraph-background-rgba@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textTag #paragraphBackgroundRgba
-- @
getTextTagParagraphBackgroundRgba :: (MonadIO m, IsTextTag o) => o -> m (Maybe Gdk.RGBA.RGBA)
getTextTagParagraphBackgroundRgba obj = MIO.liftIO $ B.Properties.getObjectPropertyBoxed obj "paragraph-background-rgba" Gdk.RGBA.RGBA

-- | Set the value of the “@paragraph-background-rgba@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textTag [ #paragraphBackgroundRgba 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextTagParagraphBackgroundRgba :: (MonadIO m, IsTextTag o) => o -> Gdk.RGBA.RGBA -> m ()
setTextTagParagraphBackgroundRgba obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBoxed obj "paragraph-background-rgba" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@paragraph-background-rgba@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextTagParagraphBackgroundRgba :: (IsTextTag o, MIO.MonadIO m) => Gdk.RGBA.RGBA -> m (GValueConstruct o)
constructTextTagParagraphBackgroundRgba val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBoxed "paragraph-background-rgba" (P.Just val)

-- | Set the value of the “@paragraph-background-rgba@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #paragraphBackgroundRgba
-- @
clearTextTagParagraphBackgroundRgba :: (MonadIO m, IsTextTag o) => o -> m ()
clearTextTagParagraphBackgroundRgba obj = liftIO $ B.Properties.setObjectPropertyBoxed obj "paragraph-background-rgba" (Nothing :: Maybe Gdk.RGBA.RGBA)

#if defined(ENABLE_OVERLOADING)
data TextTagParagraphBackgroundRgbaPropertyInfo
instance AttrInfo TextTagParagraphBackgroundRgbaPropertyInfo where
    type AttrAllowedOps TextTagParagraphBackgroundRgbaPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint TextTagParagraphBackgroundRgbaPropertyInfo = IsTextTag
    type AttrSetTypeConstraint TextTagParagraphBackgroundRgbaPropertyInfo = (~) Gdk.RGBA.RGBA
    type AttrTransferTypeConstraint TextTagParagraphBackgroundRgbaPropertyInfo = (~) Gdk.RGBA.RGBA
    type AttrTransferType TextTagParagraphBackgroundRgbaPropertyInfo = Gdk.RGBA.RGBA
    type AttrGetType TextTagParagraphBackgroundRgbaPropertyInfo = (Maybe Gdk.RGBA.RGBA)
    type AttrLabel TextTagParagraphBackgroundRgbaPropertyInfo = "paragraph-background-rgba"
    type AttrOrigin TextTagParagraphBackgroundRgbaPropertyInfo = TextTag
    attrGet = getTextTagParagraphBackgroundRgba
    attrSet = setTextTagParagraphBackgroundRgba
    attrTransfer _ v = do
        return v
    attrConstruct = constructTextTagParagraphBackgroundRgba
    attrClear = clearTextTagParagraphBackgroundRgba
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextTag.paragraphBackgroundRgba"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextTag.html#g:attr:paragraphBackgroundRgba"
        })
#endif

-- VVV Prop "paragraph-background-set"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@paragraph-background-set@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textTag #paragraphBackgroundSet
-- @
getTextTagParagraphBackgroundSet :: (MonadIO m, IsTextTag o) => o -> m Bool
getTextTagParagraphBackgroundSet obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "paragraph-background-set"

-- | Set the value of the “@paragraph-background-set@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textTag [ #paragraphBackgroundSet 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextTagParagraphBackgroundSet :: (MonadIO m, IsTextTag o) => o -> Bool -> m ()
setTextTagParagraphBackgroundSet obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "paragraph-background-set" val

-- | Construct a `GValueConstruct` with valid value for the “@paragraph-background-set@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextTagParagraphBackgroundSet :: (IsTextTag o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructTextTagParagraphBackgroundSet val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "paragraph-background-set" val

#if defined(ENABLE_OVERLOADING)
data TextTagParagraphBackgroundSetPropertyInfo
instance AttrInfo TextTagParagraphBackgroundSetPropertyInfo where
    type AttrAllowedOps TextTagParagraphBackgroundSetPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TextTagParagraphBackgroundSetPropertyInfo = IsTextTag
    type AttrSetTypeConstraint TextTagParagraphBackgroundSetPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint TextTagParagraphBackgroundSetPropertyInfo = (~) Bool
    type AttrTransferType TextTagParagraphBackgroundSetPropertyInfo = Bool
    type AttrGetType TextTagParagraphBackgroundSetPropertyInfo = Bool
    type AttrLabel TextTagParagraphBackgroundSetPropertyInfo = "paragraph-background-set"
    type AttrOrigin TextTagParagraphBackgroundSetPropertyInfo = TextTag
    attrGet = getTextTagParagraphBackgroundSet
    attrSet = setTextTagParagraphBackgroundSet
    attrTransfer _ v = do
        return v
    attrConstruct = constructTextTagParagraphBackgroundSet
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextTag.paragraphBackgroundSet"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextTag.html#g:attr:paragraphBackgroundSet"
        })
#endif

-- VVV Prop "pixels-above-lines"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@pixels-above-lines@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textTag #pixelsAboveLines
-- @
getTextTagPixelsAboveLines :: (MonadIO m, IsTextTag o) => o -> m Int32
getTextTagPixelsAboveLines obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "pixels-above-lines"

-- | Set the value of the “@pixels-above-lines@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textTag [ #pixelsAboveLines 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextTagPixelsAboveLines :: (MonadIO m, IsTextTag o) => o -> Int32 -> m ()
setTextTagPixelsAboveLines obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyInt32 obj "pixels-above-lines" val

-- | Construct a `GValueConstruct` with valid value for the “@pixels-above-lines@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextTagPixelsAboveLines :: (IsTextTag o, MIO.MonadIO m) => Int32 -> m (GValueConstruct o)
constructTextTagPixelsAboveLines val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyInt32 "pixels-above-lines" val

#if defined(ENABLE_OVERLOADING)
data TextTagPixelsAboveLinesPropertyInfo
instance AttrInfo TextTagPixelsAboveLinesPropertyInfo where
    type AttrAllowedOps TextTagPixelsAboveLinesPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TextTagPixelsAboveLinesPropertyInfo = IsTextTag
    type AttrSetTypeConstraint TextTagPixelsAboveLinesPropertyInfo = (~) Int32
    type AttrTransferTypeConstraint TextTagPixelsAboveLinesPropertyInfo = (~) Int32
    type AttrTransferType TextTagPixelsAboveLinesPropertyInfo = Int32
    type AttrGetType TextTagPixelsAboveLinesPropertyInfo = Int32
    type AttrLabel TextTagPixelsAboveLinesPropertyInfo = "pixels-above-lines"
    type AttrOrigin TextTagPixelsAboveLinesPropertyInfo = TextTag
    attrGet = getTextTagPixelsAboveLines
    attrSet = setTextTagPixelsAboveLines
    attrTransfer _ v = do
        return v
    attrConstruct = constructTextTagPixelsAboveLines
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextTag.pixelsAboveLines"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextTag.html#g:attr:pixelsAboveLines"
        })
#endif

-- VVV Prop "pixels-above-lines-set"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@pixels-above-lines-set@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textTag #pixelsAboveLinesSet
-- @
getTextTagPixelsAboveLinesSet :: (MonadIO m, IsTextTag o) => o -> m Bool
getTextTagPixelsAboveLinesSet obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "pixels-above-lines-set"

-- | Set the value of the “@pixels-above-lines-set@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textTag [ #pixelsAboveLinesSet 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextTagPixelsAboveLinesSet :: (MonadIO m, IsTextTag o) => o -> Bool -> m ()
setTextTagPixelsAboveLinesSet obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "pixels-above-lines-set" val

-- | Construct a `GValueConstruct` with valid value for the “@pixels-above-lines-set@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextTagPixelsAboveLinesSet :: (IsTextTag o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructTextTagPixelsAboveLinesSet val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "pixels-above-lines-set" val

#if defined(ENABLE_OVERLOADING)
data TextTagPixelsAboveLinesSetPropertyInfo
instance AttrInfo TextTagPixelsAboveLinesSetPropertyInfo where
    type AttrAllowedOps TextTagPixelsAboveLinesSetPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TextTagPixelsAboveLinesSetPropertyInfo = IsTextTag
    type AttrSetTypeConstraint TextTagPixelsAboveLinesSetPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint TextTagPixelsAboveLinesSetPropertyInfo = (~) Bool
    type AttrTransferType TextTagPixelsAboveLinesSetPropertyInfo = Bool
    type AttrGetType TextTagPixelsAboveLinesSetPropertyInfo = Bool
    type AttrLabel TextTagPixelsAboveLinesSetPropertyInfo = "pixels-above-lines-set"
    type AttrOrigin TextTagPixelsAboveLinesSetPropertyInfo = TextTag
    attrGet = getTextTagPixelsAboveLinesSet
    attrSet = setTextTagPixelsAboveLinesSet
    attrTransfer _ v = do
        return v
    attrConstruct = constructTextTagPixelsAboveLinesSet
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextTag.pixelsAboveLinesSet"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextTag.html#g:attr:pixelsAboveLinesSet"
        })
#endif

-- VVV Prop "pixels-below-lines"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@pixels-below-lines@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textTag #pixelsBelowLines
-- @
getTextTagPixelsBelowLines :: (MonadIO m, IsTextTag o) => o -> m Int32
getTextTagPixelsBelowLines obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "pixels-below-lines"

-- | Set the value of the “@pixels-below-lines@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textTag [ #pixelsBelowLines 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextTagPixelsBelowLines :: (MonadIO m, IsTextTag o) => o -> Int32 -> m ()
setTextTagPixelsBelowLines obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyInt32 obj "pixels-below-lines" val

-- | Construct a `GValueConstruct` with valid value for the “@pixels-below-lines@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextTagPixelsBelowLines :: (IsTextTag o, MIO.MonadIO m) => Int32 -> m (GValueConstruct o)
constructTextTagPixelsBelowLines val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyInt32 "pixels-below-lines" val

#if defined(ENABLE_OVERLOADING)
data TextTagPixelsBelowLinesPropertyInfo
instance AttrInfo TextTagPixelsBelowLinesPropertyInfo where
    type AttrAllowedOps TextTagPixelsBelowLinesPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TextTagPixelsBelowLinesPropertyInfo = IsTextTag
    type AttrSetTypeConstraint TextTagPixelsBelowLinesPropertyInfo = (~) Int32
    type AttrTransferTypeConstraint TextTagPixelsBelowLinesPropertyInfo = (~) Int32
    type AttrTransferType TextTagPixelsBelowLinesPropertyInfo = Int32
    type AttrGetType TextTagPixelsBelowLinesPropertyInfo = Int32
    type AttrLabel TextTagPixelsBelowLinesPropertyInfo = "pixels-below-lines"
    type AttrOrigin TextTagPixelsBelowLinesPropertyInfo = TextTag
    attrGet = getTextTagPixelsBelowLines
    attrSet = setTextTagPixelsBelowLines
    attrTransfer _ v = do
        return v
    attrConstruct = constructTextTagPixelsBelowLines
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextTag.pixelsBelowLines"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextTag.html#g:attr:pixelsBelowLines"
        })
#endif

-- VVV Prop "pixels-below-lines-set"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@pixels-below-lines-set@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textTag #pixelsBelowLinesSet
-- @
getTextTagPixelsBelowLinesSet :: (MonadIO m, IsTextTag o) => o -> m Bool
getTextTagPixelsBelowLinesSet obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "pixels-below-lines-set"

-- | Set the value of the “@pixels-below-lines-set@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textTag [ #pixelsBelowLinesSet 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextTagPixelsBelowLinesSet :: (MonadIO m, IsTextTag o) => o -> Bool -> m ()
setTextTagPixelsBelowLinesSet obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "pixels-below-lines-set" val

-- | Construct a `GValueConstruct` with valid value for the “@pixels-below-lines-set@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextTagPixelsBelowLinesSet :: (IsTextTag o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructTextTagPixelsBelowLinesSet val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "pixels-below-lines-set" val

#if defined(ENABLE_OVERLOADING)
data TextTagPixelsBelowLinesSetPropertyInfo
instance AttrInfo TextTagPixelsBelowLinesSetPropertyInfo where
    type AttrAllowedOps TextTagPixelsBelowLinesSetPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TextTagPixelsBelowLinesSetPropertyInfo = IsTextTag
    type AttrSetTypeConstraint TextTagPixelsBelowLinesSetPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint TextTagPixelsBelowLinesSetPropertyInfo = (~) Bool
    type AttrTransferType TextTagPixelsBelowLinesSetPropertyInfo = Bool
    type AttrGetType TextTagPixelsBelowLinesSetPropertyInfo = Bool
    type AttrLabel TextTagPixelsBelowLinesSetPropertyInfo = "pixels-below-lines-set"
    type AttrOrigin TextTagPixelsBelowLinesSetPropertyInfo = TextTag
    attrGet = getTextTagPixelsBelowLinesSet
    attrSet = setTextTagPixelsBelowLinesSet
    attrTransfer _ v = do
        return v
    attrConstruct = constructTextTagPixelsBelowLinesSet
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextTag.pixelsBelowLinesSet"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextTag.html#g:attr:pixelsBelowLinesSet"
        })
#endif

-- VVV Prop "pixels-inside-wrap"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@pixels-inside-wrap@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textTag #pixelsInsideWrap
-- @
getTextTagPixelsInsideWrap :: (MonadIO m, IsTextTag o) => o -> m Int32
getTextTagPixelsInsideWrap obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "pixels-inside-wrap"

-- | Set the value of the “@pixels-inside-wrap@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textTag [ #pixelsInsideWrap 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextTagPixelsInsideWrap :: (MonadIO m, IsTextTag o) => o -> Int32 -> m ()
setTextTagPixelsInsideWrap obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyInt32 obj "pixels-inside-wrap" val

-- | Construct a `GValueConstruct` with valid value for the “@pixels-inside-wrap@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextTagPixelsInsideWrap :: (IsTextTag o, MIO.MonadIO m) => Int32 -> m (GValueConstruct o)
constructTextTagPixelsInsideWrap val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyInt32 "pixels-inside-wrap" val

#if defined(ENABLE_OVERLOADING)
data TextTagPixelsInsideWrapPropertyInfo
instance AttrInfo TextTagPixelsInsideWrapPropertyInfo where
    type AttrAllowedOps TextTagPixelsInsideWrapPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TextTagPixelsInsideWrapPropertyInfo = IsTextTag
    type AttrSetTypeConstraint TextTagPixelsInsideWrapPropertyInfo = (~) Int32
    type AttrTransferTypeConstraint TextTagPixelsInsideWrapPropertyInfo = (~) Int32
    type AttrTransferType TextTagPixelsInsideWrapPropertyInfo = Int32
    type AttrGetType TextTagPixelsInsideWrapPropertyInfo = Int32
    type AttrLabel TextTagPixelsInsideWrapPropertyInfo = "pixels-inside-wrap"
    type AttrOrigin TextTagPixelsInsideWrapPropertyInfo = TextTag
    attrGet = getTextTagPixelsInsideWrap
    attrSet = setTextTagPixelsInsideWrap
    attrTransfer _ v = do
        return v
    attrConstruct = constructTextTagPixelsInsideWrap
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextTag.pixelsInsideWrap"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextTag.html#g:attr:pixelsInsideWrap"
        })
#endif

-- VVV Prop "pixels-inside-wrap-set"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@pixels-inside-wrap-set@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textTag #pixelsInsideWrapSet
-- @
getTextTagPixelsInsideWrapSet :: (MonadIO m, IsTextTag o) => o -> m Bool
getTextTagPixelsInsideWrapSet obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "pixels-inside-wrap-set"

-- | Set the value of the “@pixels-inside-wrap-set@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textTag [ #pixelsInsideWrapSet 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextTagPixelsInsideWrapSet :: (MonadIO m, IsTextTag o) => o -> Bool -> m ()
setTextTagPixelsInsideWrapSet obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "pixels-inside-wrap-set" val

-- | Construct a `GValueConstruct` with valid value for the “@pixels-inside-wrap-set@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextTagPixelsInsideWrapSet :: (IsTextTag o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructTextTagPixelsInsideWrapSet val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "pixels-inside-wrap-set" val

#if defined(ENABLE_OVERLOADING)
data TextTagPixelsInsideWrapSetPropertyInfo
instance AttrInfo TextTagPixelsInsideWrapSetPropertyInfo where
    type AttrAllowedOps TextTagPixelsInsideWrapSetPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TextTagPixelsInsideWrapSetPropertyInfo = IsTextTag
    type AttrSetTypeConstraint TextTagPixelsInsideWrapSetPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint TextTagPixelsInsideWrapSetPropertyInfo = (~) Bool
    type AttrTransferType TextTagPixelsInsideWrapSetPropertyInfo = Bool
    type AttrGetType TextTagPixelsInsideWrapSetPropertyInfo = Bool
    type AttrLabel TextTagPixelsInsideWrapSetPropertyInfo = "pixels-inside-wrap-set"
    type AttrOrigin TextTagPixelsInsideWrapSetPropertyInfo = TextTag
    attrGet = getTextTagPixelsInsideWrapSet
    attrSet = setTextTagPixelsInsideWrapSet
    attrTransfer _ v = do
        return v
    attrConstruct = constructTextTagPixelsInsideWrapSet
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextTag.pixelsInsideWrapSet"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextTag.html#g:attr:pixelsInsideWrapSet"
        })
#endif

-- VVV Prop "right-margin"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@right-margin@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textTag #rightMargin
-- @
getTextTagRightMargin :: (MonadIO m, IsTextTag o) => o -> m Int32
getTextTagRightMargin obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "right-margin"

-- | Set the value of the “@right-margin@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textTag [ #rightMargin 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextTagRightMargin :: (MonadIO m, IsTextTag o) => o -> Int32 -> m ()
setTextTagRightMargin obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyInt32 obj "right-margin" val

-- | Construct a `GValueConstruct` with valid value for the “@right-margin@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextTagRightMargin :: (IsTextTag o, MIO.MonadIO m) => Int32 -> m (GValueConstruct o)
constructTextTagRightMargin val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyInt32 "right-margin" val

#if defined(ENABLE_OVERLOADING)
data TextTagRightMarginPropertyInfo
instance AttrInfo TextTagRightMarginPropertyInfo where
    type AttrAllowedOps TextTagRightMarginPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TextTagRightMarginPropertyInfo = IsTextTag
    type AttrSetTypeConstraint TextTagRightMarginPropertyInfo = (~) Int32
    type AttrTransferTypeConstraint TextTagRightMarginPropertyInfo = (~) Int32
    type AttrTransferType TextTagRightMarginPropertyInfo = Int32
    type AttrGetType TextTagRightMarginPropertyInfo = Int32
    type AttrLabel TextTagRightMarginPropertyInfo = "right-margin"
    type AttrOrigin TextTagRightMarginPropertyInfo = TextTag
    attrGet = getTextTagRightMargin
    attrSet = setTextTagRightMargin
    attrTransfer _ v = do
        return v
    attrConstruct = constructTextTagRightMargin
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextTag.rightMargin"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextTag.html#g:attr:rightMargin"
        })
#endif

-- VVV Prop "right-margin-set"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@right-margin-set@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textTag #rightMarginSet
-- @
getTextTagRightMarginSet :: (MonadIO m, IsTextTag o) => o -> m Bool
getTextTagRightMarginSet obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "right-margin-set"

-- | Set the value of the “@right-margin-set@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textTag [ #rightMarginSet 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextTagRightMarginSet :: (MonadIO m, IsTextTag o) => o -> Bool -> m ()
setTextTagRightMarginSet obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "right-margin-set" val

-- | Construct a `GValueConstruct` with valid value for the “@right-margin-set@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextTagRightMarginSet :: (IsTextTag o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructTextTagRightMarginSet val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "right-margin-set" val

#if defined(ENABLE_OVERLOADING)
data TextTagRightMarginSetPropertyInfo
instance AttrInfo TextTagRightMarginSetPropertyInfo where
    type AttrAllowedOps TextTagRightMarginSetPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TextTagRightMarginSetPropertyInfo = IsTextTag
    type AttrSetTypeConstraint TextTagRightMarginSetPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint TextTagRightMarginSetPropertyInfo = (~) Bool
    type AttrTransferType TextTagRightMarginSetPropertyInfo = Bool
    type AttrGetType TextTagRightMarginSetPropertyInfo = Bool
    type AttrLabel TextTagRightMarginSetPropertyInfo = "right-margin-set"
    type AttrOrigin TextTagRightMarginSetPropertyInfo = TextTag
    attrGet = getTextTagRightMarginSet
    attrSet = setTextTagRightMarginSet
    attrTransfer _ v = do
        return v
    attrConstruct = constructTextTagRightMarginSet
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextTag.rightMarginSet"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextTag.html#g:attr:rightMarginSet"
        })
#endif

-- VVV Prop "rise"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@rise@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textTag #rise
-- @
getTextTagRise :: (MonadIO m, IsTextTag o) => o -> m Int32
getTextTagRise obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "rise"

-- | Set the value of the “@rise@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textTag [ #rise 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextTagRise :: (MonadIO m, IsTextTag o) => o -> Int32 -> m ()
setTextTagRise obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyInt32 obj "rise" val

-- | Construct a `GValueConstruct` with valid value for the “@rise@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextTagRise :: (IsTextTag o, MIO.MonadIO m) => Int32 -> m (GValueConstruct o)
constructTextTagRise val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyInt32 "rise" val

#if defined(ENABLE_OVERLOADING)
data TextTagRisePropertyInfo
instance AttrInfo TextTagRisePropertyInfo where
    type AttrAllowedOps TextTagRisePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TextTagRisePropertyInfo = IsTextTag
    type AttrSetTypeConstraint TextTagRisePropertyInfo = (~) Int32
    type AttrTransferTypeConstraint TextTagRisePropertyInfo = (~) Int32
    type AttrTransferType TextTagRisePropertyInfo = Int32
    type AttrGetType TextTagRisePropertyInfo = Int32
    type AttrLabel TextTagRisePropertyInfo = "rise"
    type AttrOrigin TextTagRisePropertyInfo = TextTag
    attrGet = getTextTagRise
    attrSet = setTextTagRise
    attrTransfer _ v = do
        return v
    attrConstruct = constructTextTagRise
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextTag.rise"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextTag.html#g:attr:rise"
        })
#endif

-- VVV Prop "rise-set"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@rise-set@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textTag #riseSet
-- @
getTextTagRiseSet :: (MonadIO m, IsTextTag o) => o -> m Bool
getTextTagRiseSet obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "rise-set"

-- | Set the value of the “@rise-set@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textTag [ #riseSet 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextTagRiseSet :: (MonadIO m, IsTextTag o) => o -> Bool -> m ()
setTextTagRiseSet obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "rise-set" val

-- | Construct a `GValueConstruct` with valid value for the “@rise-set@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextTagRiseSet :: (IsTextTag o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructTextTagRiseSet val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "rise-set" val

#if defined(ENABLE_OVERLOADING)
data TextTagRiseSetPropertyInfo
instance AttrInfo TextTagRiseSetPropertyInfo where
    type AttrAllowedOps TextTagRiseSetPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TextTagRiseSetPropertyInfo = IsTextTag
    type AttrSetTypeConstraint TextTagRiseSetPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint TextTagRiseSetPropertyInfo = (~) Bool
    type AttrTransferType TextTagRiseSetPropertyInfo = Bool
    type AttrGetType TextTagRiseSetPropertyInfo = Bool
    type AttrLabel TextTagRiseSetPropertyInfo = "rise-set"
    type AttrOrigin TextTagRiseSetPropertyInfo = TextTag
    attrGet = getTextTagRiseSet
    attrSet = setTextTagRiseSet
    attrTransfer _ v = do
        return v
    attrConstruct = constructTextTagRiseSet
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextTag.riseSet"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextTag.html#g:attr:riseSet"
        })
#endif

-- VVV Prop "scale"
   -- Type: TBasicType TDouble
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@scale@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textTag #scale
-- @
getTextTagScale :: (MonadIO m, IsTextTag o) => o -> m Double
getTextTagScale obj = MIO.liftIO $ B.Properties.getObjectPropertyDouble obj "scale"

-- | Set the value of the “@scale@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textTag [ #scale 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextTagScale :: (MonadIO m, IsTextTag o) => o -> Double -> m ()
setTextTagScale obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyDouble obj "scale" val

-- | Construct a `GValueConstruct` with valid value for the “@scale@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextTagScale :: (IsTextTag o, MIO.MonadIO m) => Double -> m (GValueConstruct o)
constructTextTagScale val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyDouble "scale" val

#if defined(ENABLE_OVERLOADING)
data TextTagScalePropertyInfo
instance AttrInfo TextTagScalePropertyInfo where
    type AttrAllowedOps TextTagScalePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TextTagScalePropertyInfo = IsTextTag
    type AttrSetTypeConstraint TextTagScalePropertyInfo = (~) Double
    type AttrTransferTypeConstraint TextTagScalePropertyInfo = (~) Double
    type AttrTransferType TextTagScalePropertyInfo = Double
    type AttrGetType TextTagScalePropertyInfo = Double
    type AttrLabel TextTagScalePropertyInfo = "scale"
    type AttrOrigin TextTagScalePropertyInfo = TextTag
    attrGet = getTextTagScale
    attrSet = setTextTagScale
    attrTransfer _ v = do
        return v
    attrConstruct = constructTextTagScale
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextTag.scale"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextTag.html#g:attr:scale"
        })
#endif

-- VVV Prop "scale-set"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@scale-set@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textTag #scaleSet
-- @
getTextTagScaleSet :: (MonadIO m, IsTextTag o) => o -> m Bool
getTextTagScaleSet obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "scale-set"

-- | Set the value of the “@scale-set@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textTag [ #scaleSet 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextTagScaleSet :: (MonadIO m, IsTextTag o) => o -> Bool -> m ()
setTextTagScaleSet obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "scale-set" val

-- | Construct a `GValueConstruct` with valid value for the “@scale-set@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextTagScaleSet :: (IsTextTag o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructTextTagScaleSet val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "scale-set" val

#if defined(ENABLE_OVERLOADING)
data TextTagScaleSetPropertyInfo
instance AttrInfo TextTagScaleSetPropertyInfo where
    type AttrAllowedOps TextTagScaleSetPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TextTagScaleSetPropertyInfo = IsTextTag
    type AttrSetTypeConstraint TextTagScaleSetPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint TextTagScaleSetPropertyInfo = (~) Bool
    type AttrTransferType TextTagScaleSetPropertyInfo = Bool
    type AttrGetType TextTagScaleSetPropertyInfo = Bool
    type AttrLabel TextTagScaleSetPropertyInfo = "scale-set"
    type AttrOrigin TextTagScaleSetPropertyInfo = TextTag
    attrGet = getTextTagScaleSet
    attrSet = setTextTagScaleSet
    attrTransfer _ v = do
        return v
    attrConstruct = constructTextTagScaleSet
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextTag.scaleSet"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextTag.html#g:attr:scaleSet"
        })
#endif

-- VVV Prop "size"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@size@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textTag #size
-- @
getTextTagSize :: (MonadIO m, IsTextTag o) => o -> m Int32
getTextTagSize obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "size"

-- | Set the value of the “@size@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textTag [ #size 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextTagSize :: (MonadIO m, IsTextTag o) => o -> Int32 -> m ()
setTextTagSize obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyInt32 obj "size" val

-- | Construct a `GValueConstruct` with valid value for the “@size@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextTagSize :: (IsTextTag o, MIO.MonadIO m) => Int32 -> m (GValueConstruct o)
constructTextTagSize val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyInt32 "size" val

#if defined(ENABLE_OVERLOADING)
data TextTagSizePropertyInfo
instance AttrInfo TextTagSizePropertyInfo where
    type AttrAllowedOps TextTagSizePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TextTagSizePropertyInfo = IsTextTag
    type AttrSetTypeConstraint TextTagSizePropertyInfo = (~) Int32
    type AttrTransferTypeConstraint TextTagSizePropertyInfo = (~) Int32
    type AttrTransferType TextTagSizePropertyInfo = Int32
    type AttrGetType TextTagSizePropertyInfo = Int32
    type AttrLabel TextTagSizePropertyInfo = "size"
    type AttrOrigin TextTagSizePropertyInfo = TextTag
    attrGet = getTextTagSize
    attrSet = setTextTagSize
    attrTransfer _ v = do
        return v
    attrConstruct = constructTextTagSize
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextTag.size"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextTag.html#g:attr:size"
        })
#endif

-- VVV Prop "size-points"
   -- Type: TBasicType TDouble
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@size-points@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textTag #sizePoints
-- @
getTextTagSizePoints :: (MonadIO m, IsTextTag o) => o -> m Double
getTextTagSizePoints obj = MIO.liftIO $ B.Properties.getObjectPropertyDouble obj "size-points"

-- | Set the value of the “@size-points@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textTag [ #sizePoints 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextTagSizePoints :: (MonadIO m, IsTextTag o) => o -> Double -> m ()
setTextTagSizePoints obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyDouble obj "size-points" val

-- | Construct a `GValueConstruct` with valid value for the “@size-points@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextTagSizePoints :: (IsTextTag o, MIO.MonadIO m) => Double -> m (GValueConstruct o)
constructTextTagSizePoints val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyDouble "size-points" val

#if defined(ENABLE_OVERLOADING)
data TextTagSizePointsPropertyInfo
instance AttrInfo TextTagSizePointsPropertyInfo where
    type AttrAllowedOps TextTagSizePointsPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TextTagSizePointsPropertyInfo = IsTextTag
    type AttrSetTypeConstraint TextTagSizePointsPropertyInfo = (~) Double
    type AttrTransferTypeConstraint TextTagSizePointsPropertyInfo = (~) Double
    type AttrTransferType TextTagSizePointsPropertyInfo = Double
    type AttrGetType TextTagSizePointsPropertyInfo = Double
    type AttrLabel TextTagSizePointsPropertyInfo = "size-points"
    type AttrOrigin TextTagSizePointsPropertyInfo = TextTag
    attrGet = getTextTagSizePoints
    attrSet = setTextTagSizePoints
    attrTransfer _ v = do
        return v
    attrConstruct = constructTextTagSizePoints
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextTag.sizePoints"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextTag.html#g:attr:sizePoints"
        })
#endif

-- VVV Prop "size-set"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@size-set@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textTag #sizeSet
-- @
getTextTagSizeSet :: (MonadIO m, IsTextTag o) => o -> m Bool
getTextTagSizeSet obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "size-set"

-- | Set the value of the “@size-set@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textTag [ #sizeSet 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextTagSizeSet :: (MonadIO m, IsTextTag o) => o -> Bool -> m ()
setTextTagSizeSet obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "size-set" val

-- | Construct a `GValueConstruct` with valid value for the “@size-set@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextTagSizeSet :: (IsTextTag o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructTextTagSizeSet val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "size-set" val

#if defined(ENABLE_OVERLOADING)
data TextTagSizeSetPropertyInfo
instance AttrInfo TextTagSizeSetPropertyInfo where
    type AttrAllowedOps TextTagSizeSetPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TextTagSizeSetPropertyInfo = IsTextTag
    type AttrSetTypeConstraint TextTagSizeSetPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint TextTagSizeSetPropertyInfo = (~) Bool
    type AttrTransferType TextTagSizeSetPropertyInfo = Bool
    type AttrGetType TextTagSizeSetPropertyInfo = Bool
    type AttrLabel TextTagSizeSetPropertyInfo = "size-set"
    type AttrOrigin TextTagSizeSetPropertyInfo = TextTag
    attrGet = getTextTagSizeSet
    attrSet = setTextTagSizeSet
    attrTransfer _ v = do
        return v
    attrConstruct = constructTextTagSizeSet
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextTag.sizeSet"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextTag.html#g:attr:sizeSet"
        })
#endif

-- VVV Prop "stretch"
   -- Type: TInterface (Name {namespace = "Pango", name = "Stretch"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@stretch@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textTag #stretch
-- @
getTextTagStretch :: (MonadIO m, IsTextTag o) => o -> m Pango.Enums.Stretch
getTextTagStretch obj = MIO.liftIO $ B.Properties.getObjectPropertyEnum obj "stretch"

-- | Set the value of the “@stretch@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textTag [ #stretch 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextTagStretch :: (MonadIO m, IsTextTag o) => o -> Pango.Enums.Stretch -> m ()
setTextTagStretch obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyEnum obj "stretch" val

-- | Construct a `GValueConstruct` with valid value for the “@stretch@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextTagStretch :: (IsTextTag o, MIO.MonadIO m) => Pango.Enums.Stretch -> m (GValueConstruct o)
constructTextTagStretch val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyEnum "stretch" val

#if defined(ENABLE_OVERLOADING)
data TextTagStretchPropertyInfo
instance AttrInfo TextTagStretchPropertyInfo where
    type AttrAllowedOps TextTagStretchPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TextTagStretchPropertyInfo = IsTextTag
    type AttrSetTypeConstraint TextTagStretchPropertyInfo = (~) Pango.Enums.Stretch
    type AttrTransferTypeConstraint TextTagStretchPropertyInfo = (~) Pango.Enums.Stretch
    type AttrTransferType TextTagStretchPropertyInfo = Pango.Enums.Stretch
    type AttrGetType TextTagStretchPropertyInfo = Pango.Enums.Stretch
    type AttrLabel TextTagStretchPropertyInfo = "stretch"
    type AttrOrigin TextTagStretchPropertyInfo = TextTag
    attrGet = getTextTagStretch
    attrSet = setTextTagStretch
    attrTransfer _ v = do
        return v
    attrConstruct = constructTextTagStretch
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextTag.stretch"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextTag.html#g:attr:stretch"
        })
#endif

-- VVV Prop "stretch-set"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@stretch-set@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textTag #stretchSet
-- @
getTextTagStretchSet :: (MonadIO m, IsTextTag o) => o -> m Bool
getTextTagStretchSet obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "stretch-set"

-- | Set the value of the “@stretch-set@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textTag [ #stretchSet 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextTagStretchSet :: (MonadIO m, IsTextTag o) => o -> Bool -> m ()
setTextTagStretchSet obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "stretch-set" val

-- | Construct a `GValueConstruct` with valid value for the “@stretch-set@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextTagStretchSet :: (IsTextTag o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructTextTagStretchSet val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "stretch-set" val

#if defined(ENABLE_OVERLOADING)
data TextTagStretchSetPropertyInfo
instance AttrInfo TextTagStretchSetPropertyInfo where
    type AttrAllowedOps TextTagStretchSetPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TextTagStretchSetPropertyInfo = IsTextTag
    type AttrSetTypeConstraint TextTagStretchSetPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint TextTagStretchSetPropertyInfo = (~) Bool
    type AttrTransferType TextTagStretchSetPropertyInfo = Bool
    type AttrGetType TextTagStretchSetPropertyInfo = Bool
    type AttrLabel TextTagStretchSetPropertyInfo = "stretch-set"
    type AttrOrigin TextTagStretchSetPropertyInfo = TextTag
    attrGet = getTextTagStretchSet
    attrSet = setTextTagStretchSet
    attrTransfer _ v = do
        return v
    attrConstruct = constructTextTagStretchSet
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextTag.stretchSet"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextTag.html#g:attr:stretchSet"
        })
#endif

-- VVV Prop "strikethrough"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@strikethrough@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textTag #strikethrough
-- @
getTextTagStrikethrough :: (MonadIO m, IsTextTag o) => o -> m Bool
getTextTagStrikethrough obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "strikethrough"

-- | Set the value of the “@strikethrough@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textTag [ #strikethrough 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextTagStrikethrough :: (MonadIO m, IsTextTag o) => o -> Bool -> m ()
setTextTagStrikethrough obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "strikethrough" val

-- | Construct a `GValueConstruct` with valid value for the “@strikethrough@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextTagStrikethrough :: (IsTextTag o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructTextTagStrikethrough val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "strikethrough" val

#if defined(ENABLE_OVERLOADING)
data TextTagStrikethroughPropertyInfo
instance AttrInfo TextTagStrikethroughPropertyInfo where
    type AttrAllowedOps TextTagStrikethroughPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TextTagStrikethroughPropertyInfo = IsTextTag
    type AttrSetTypeConstraint TextTagStrikethroughPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint TextTagStrikethroughPropertyInfo = (~) Bool
    type AttrTransferType TextTagStrikethroughPropertyInfo = Bool
    type AttrGetType TextTagStrikethroughPropertyInfo = Bool
    type AttrLabel TextTagStrikethroughPropertyInfo = "strikethrough"
    type AttrOrigin TextTagStrikethroughPropertyInfo = TextTag
    attrGet = getTextTagStrikethrough
    attrSet = setTextTagStrikethrough
    attrTransfer _ v = do
        return v
    attrConstruct = constructTextTagStrikethrough
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextTag.strikethrough"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextTag.html#g:attr:strikethrough"
        })
#endif

-- VVV Prop "strikethrough-rgba"
   -- Type: TInterface (Name {namespace = "Gdk", name = "RGBA"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@strikethrough-rgba@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textTag #strikethroughRgba
-- @
getTextTagStrikethroughRgba :: (MonadIO m, IsTextTag o) => o -> m (Maybe Gdk.RGBA.RGBA)
getTextTagStrikethroughRgba obj = MIO.liftIO $ B.Properties.getObjectPropertyBoxed obj "strikethrough-rgba" Gdk.RGBA.RGBA

-- | Set the value of the “@strikethrough-rgba@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textTag [ #strikethroughRgba 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextTagStrikethroughRgba :: (MonadIO m, IsTextTag o) => o -> Gdk.RGBA.RGBA -> m ()
setTextTagStrikethroughRgba obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBoxed obj "strikethrough-rgba" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@strikethrough-rgba@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextTagStrikethroughRgba :: (IsTextTag o, MIO.MonadIO m) => Gdk.RGBA.RGBA -> m (GValueConstruct o)
constructTextTagStrikethroughRgba val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBoxed "strikethrough-rgba" (P.Just val)

-- | Set the value of the “@strikethrough-rgba@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #strikethroughRgba
-- @
clearTextTagStrikethroughRgba :: (MonadIO m, IsTextTag o) => o -> m ()
clearTextTagStrikethroughRgba obj = liftIO $ B.Properties.setObjectPropertyBoxed obj "strikethrough-rgba" (Nothing :: Maybe Gdk.RGBA.RGBA)

#if defined(ENABLE_OVERLOADING)
data TextTagStrikethroughRgbaPropertyInfo
instance AttrInfo TextTagStrikethroughRgbaPropertyInfo where
    type AttrAllowedOps TextTagStrikethroughRgbaPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint TextTagStrikethroughRgbaPropertyInfo = IsTextTag
    type AttrSetTypeConstraint TextTagStrikethroughRgbaPropertyInfo = (~) Gdk.RGBA.RGBA
    type AttrTransferTypeConstraint TextTagStrikethroughRgbaPropertyInfo = (~) Gdk.RGBA.RGBA
    type AttrTransferType TextTagStrikethroughRgbaPropertyInfo = Gdk.RGBA.RGBA
    type AttrGetType TextTagStrikethroughRgbaPropertyInfo = (Maybe Gdk.RGBA.RGBA)
    type AttrLabel TextTagStrikethroughRgbaPropertyInfo = "strikethrough-rgba"
    type AttrOrigin TextTagStrikethroughRgbaPropertyInfo = TextTag
    attrGet = getTextTagStrikethroughRgba
    attrSet = setTextTagStrikethroughRgba
    attrTransfer _ v = do
        return v
    attrConstruct = constructTextTagStrikethroughRgba
    attrClear = clearTextTagStrikethroughRgba
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextTag.strikethroughRgba"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextTag.html#g:attr:strikethroughRgba"
        })
#endif

-- VVV Prop "strikethrough-rgba-set"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@strikethrough-rgba-set@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textTag #strikethroughRgbaSet
-- @
getTextTagStrikethroughRgbaSet :: (MonadIO m, IsTextTag o) => o -> m Bool
getTextTagStrikethroughRgbaSet obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "strikethrough-rgba-set"

-- | Set the value of the “@strikethrough-rgba-set@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textTag [ #strikethroughRgbaSet 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextTagStrikethroughRgbaSet :: (MonadIO m, IsTextTag o) => o -> Bool -> m ()
setTextTagStrikethroughRgbaSet obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "strikethrough-rgba-set" val

-- | Construct a `GValueConstruct` with valid value for the “@strikethrough-rgba-set@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextTagStrikethroughRgbaSet :: (IsTextTag o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructTextTagStrikethroughRgbaSet val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "strikethrough-rgba-set" val

#if defined(ENABLE_OVERLOADING)
data TextTagStrikethroughRgbaSetPropertyInfo
instance AttrInfo TextTagStrikethroughRgbaSetPropertyInfo where
    type AttrAllowedOps TextTagStrikethroughRgbaSetPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TextTagStrikethroughRgbaSetPropertyInfo = IsTextTag
    type AttrSetTypeConstraint TextTagStrikethroughRgbaSetPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint TextTagStrikethroughRgbaSetPropertyInfo = (~) Bool
    type AttrTransferType TextTagStrikethroughRgbaSetPropertyInfo = Bool
    type AttrGetType TextTagStrikethroughRgbaSetPropertyInfo = Bool
    type AttrLabel TextTagStrikethroughRgbaSetPropertyInfo = "strikethrough-rgba-set"
    type AttrOrigin TextTagStrikethroughRgbaSetPropertyInfo = TextTag
    attrGet = getTextTagStrikethroughRgbaSet
    attrSet = setTextTagStrikethroughRgbaSet
    attrTransfer _ v = do
        return v
    attrConstruct = constructTextTagStrikethroughRgbaSet
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextTag.strikethroughRgbaSet"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextTag.html#g:attr:strikethroughRgbaSet"
        })
#endif

-- VVV Prop "strikethrough-set"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@strikethrough-set@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textTag #strikethroughSet
-- @
getTextTagStrikethroughSet :: (MonadIO m, IsTextTag o) => o -> m Bool
getTextTagStrikethroughSet obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "strikethrough-set"

-- | Set the value of the “@strikethrough-set@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textTag [ #strikethroughSet 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextTagStrikethroughSet :: (MonadIO m, IsTextTag o) => o -> Bool -> m ()
setTextTagStrikethroughSet obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "strikethrough-set" val

-- | Construct a `GValueConstruct` with valid value for the “@strikethrough-set@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextTagStrikethroughSet :: (IsTextTag o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructTextTagStrikethroughSet val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "strikethrough-set" val

#if defined(ENABLE_OVERLOADING)
data TextTagStrikethroughSetPropertyInfo
instance AttrInfo TextTagStrikethroughSetPropertyInfo where
    type AttrAllowedOps TextTagStrikethroughSetPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TextTagStrikethroughSetPropertyInfo = IsTextTag
    type AttrSetTypeConstraint TextTagStrikethroughSetPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint TextTagStrikethroughSetPropertyInfo = (~) Bool
    type AttrTransferType TextTagStrikethroughSetPropertyInfo = Bool
    type AttrGetType TextTagStrikethroughSetPropertyInfo = Bool
    type AttrLabel TextTagStrikethroughSetPropertyInfo = "strikethrough-set"
    type AttrOrigin TextTagStrikethroughSetPropertyInfo = TextTag
    attrGet = getTextTagStrikethroughSet
    attrSet = setTextTagStrikethroughSet
    attrTransfer _ v = do
        return v
    attrConstruct = constructTextTagStrikethroughSet
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextTag.strikethroughSet"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextTag.html#g:attr:strikethroughSet"
        })
#endif

-- VVV Prop "style"
   -- Type: TInterface (Name {namespace = "Pango", name = "Style"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@style@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textTag #style
-- @
getTextTagStyle :: (MonadIO m, IsTextTag o) => o -> m Pango.Enums.Style
getTextTagStyle obj = MIO.liftIO $ B.Properties.getObjectPropertyEnum obj "style"

-- | Set the value of the “@style@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textTag [ #style 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextTagStyle :: (MonadIO m, IsTextTag o) => o -> Pango.Enums.Style -> m ()
setTextTagStyle obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyEnum obj "style" val

-- | Construct a `GValueConstruct` with valid value for the “@style@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextTagStyle :: (IsTextTag o, MIO.MonadIO m) => Pango.Enums.Style -> m (GValueConstruct o)
constructTextTagStyle val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyEnum "style" val

#if defined(ENABLE_OVERLOADING)
data TextTagStylePropertyInfo
instance AttrInfo TextTagStylePropertyInfo where
    type AttrAllowedOps TextTagStylePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TextTagStylePropertyInfo = IsTextTag
    type AttrSetTypeConstraint TextTagStylePropertyInfo = (~) Pango.Enums.Style
    type AttrTransferTypeConstraint TextTagStylePropertyInfo = (~) Pango.Enums.Style
    type AttrTransferType TextTagStylePropertyInfo = Pango.Enums.Style
    type AttrGetType TextTagStylePropertyInfo = Pango.Enums.Style
    type AttrLabel TextTagStylePropertyInfo = "style"
    type AttrOrigin TextTagStylePropertyInfo = TextTag
    attrGet = getTextTagStyle
    attrSet = setTextTagStyle
    attrTransfer _ v = do
        return v
    attrConstruct = constructTextTagStyle
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextTag.style"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextTag.html#g:attr:style"
        })
#endif

-- VVV Prop "style-set"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@style-set@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textTag #styleSet
-- @
getTextTagStyleSet :: (MonadIO m, IsTextTag o) => o -> m Bool
getTextTagStyleSet obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "style-set"

-- | Set the value of the “@style-set@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textTag [ #styleSet 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextTagStyleSet :: (MonadIO m, IsTextTag o) => o -> Bool -> m ()
setTextTagStyleSet obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "style-set" val

-- | Construct a `GValueConstruct` with valid value for the “@style-set@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextTagStyleSet :: (IsTextTag o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructTextTagStyleSet val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "style-set" val

#if defined(ENABLE_OVERLOADING)
data TextTagStyleSetPropertyInfo
instance AttrInfo TextTagStyleSetPropertyInfo where
    type AttrAllowedOps TextTagStyleSetPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TextTagStyleSetPropertyInfo = IsTextTag
    type AttrSetTypeConstraint TextTagStyleSetPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint TextTagStyleSetPropertyInfo = (~) Bool
    type AttrTransferType TextTagStyleSetPropertyInfo = Bool
    type AttrGetType TextTagStyleSetPropertyInfo = Bool
    type AttrLabel TextTagStyleSetPropertyInfo = "style-set"
    type AttrOrigin TextTagStyleSetPropertyInfo = TextTag
    attrGet = getTextTagStyleSet
    attrSet = setTextTagStyleSet
    attrTransfer _ v = do
        return v
    attrConstruct = constructTextTagStyleSet
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextTag.styleSet"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextTag.html#g:attr:styleSet"
        })
#endif

-- VVV Prop "tabs"
   -- Type: TInterface (Name {namespace = "Pango", name = "TabArray"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@tabs@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textTag #tabs
-- @
getTextTagTabs :: (MonadIO m, IsTextTag o) => o -> m (Maybe Pango.TabArray.TabArray)
getTextTagTabs obj = MIO.liftIO $ B.Properties.getObjectPropertyBoxed obj "tabs" Pango.TabArray.TabArray

-- | Set the value of the “@tabs@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textTag [ #tabs 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextTagTabs :: (MonadIO m, IsTextTag o) => o -> Pango.TabArray.TabArray -> m ()
setTextTagTabs obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBoxed obj "tabs" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@tabs@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextTagTabs :: (IsTextTag o, MIO.MonadIO m) => Pango.TabArray.TabArray -> m (GValueConstruct o)
constructTextTagTabs val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBoxed "tabs" (P.Just val)

-- | Set the value of the “@tabs@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #tabs
-- @
clearTextTagTabs :: (MonadIO m, IsTextTag o) => o -> m ()
clearTextTagTabs obj = liftIO $ B.Properties.setObjectPropertyBoxed obj "tabs" (Nothing :: Maybe Pango.TabArray.TabArray)

#if defined(ENABLE_OVERLOADING)
data TextTagTabsPropertyInfo
instance AttrInfo TextTagTabsPropertyInfo where
    type AttrAllowedOps TextTagTabsPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint TextTagTabsPropertyInfo = IsTextTag
    type AttrSetTypeConstraint TextTagTabsPropertyInfo = (~) Pango.TabArray.TabArray
    type AttrTransferTypeConstraint TextTagTabsPropertyInfo = (~) Pango.TabArray.TabArray
    type AttrTransferType TextTagTabsPropertyInfo = Pango.TabArray.TabArray
    type AttrGetType TextTagTabsPropertyInfo = (Maybe Pango.TabArray.TabArray)
    type AttrLabel TextTagTabsPropertyInfo = "tabs"
    type AttrOrigin TextTagTabsPropertyInfo = TextTag
    attrGet = getTextTagTabs
    attrSet = setTextTagTabs
    attrTransfer _ v = do
        return v
    attrConstruct = constructTextTagTabs
    attrClear = clearTextTagTabs
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextTag.tabs"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextTag.html#g:attr:tabs"
        })
#endif

-- VVV Prop "tabs-set"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@tabs-set@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textTag #tabsSet
-- @
getTextTagTabsSet :: (MonadIO m, IsTextTag o) => o -> m Bool
getTextTagTabsSet obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "tabs-set"

-- | Set the value of the “@tabs-set@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textTag [ #tabsSet 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextTagTabsSet :: (MonadIO m, IsTextTag o) => o -> Bool -> m ()
setTextTagTabsSet obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "tabs-set" val

-- | Construct a `GValueConstruct` with valid value for the “@tabs-set@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextTagTabsSet :: (IsTextTag o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructTextTagTabsSet val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "tabs-set" val

#if defined(ENABLE_OVERLOADING)
data TextTagTabsSetPropertyInfo
instance AttrInfo TextTagTabsSetPropertyInfo where
    type AttrAllowedOps TextTagTabsSetPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TextTagTabsSetPropertyInfo = IsTextTag
    type AttrSetTypeConstraint TextTagTabsSetPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint TextTagTabsSetPropertyInfo = (~) Bool
    type AttrTransferType TextTagTabsSetPropertyInfo = Bool
    type AttrGetType TextTagTabsSetPropertyInfo = Bool
    type AttrLabel TextTagTabsSetPropertyInfo = "tabs-set"
    type AttrOrigin TextTagTabsSetPropertyInfo = TextTag
    attrGet = getTextTagTabsSet
    attrSet = setTextTagTabsSet
    attrTransfer _ v = do
        return v
    attrConstruct = constructTextTagTabsSet
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextTag.tabsSet"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextTag.html#g:attr:tabsSet"
        })
#endif

-- VVV Prop "underline"
   -- Type: TInterface (Name {namespace = "Pango", name = "Underline"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@underline@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textTag #underline
-- @
getTextTagUnderline :: (MonadIO m, IsTextTag o) => o -> m Pango.Enums.Underline
getTextTagUnderline obj = MIO.liftIO $ B.Properties.getObjectPropertyEnum obj "underline"

-- | Set the value of the “@underline@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textTag [ #underline 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextTagUnderline :: (MonadIO m, IsTextTag o) => o -> Pango.Enums.Underline -> m ()
setTextTagUnderline obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyEnum obj "underline" val

-- | Construct a `GValueConstruct` with valid value for the “@underline@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextTagUnderline :: (IsTextTag o, MIO.MonadIO m) => Pango.Enums.Underline -> m (GValueConstruct o)
constructTextTagUnderline val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyEnum "underline" val

#if defined(ENABLE_OVERLOADING)
data TextTagUnderlinePropertyInfo
instance AttrInfo TextTagUnderlinePropertyInfo where
    type AttrAllowedOps TextTagUnderlinePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TextTagUnderlinePropertyInfo = IsTextTag
    type AttrSetTypeConstraint TextTagUnderlinePropertyInfo = (~) Pango.Enums.Underline
    type AttrTransferTypeConstraint TextTagUnderlinePropertyInfo = (~) Pango.Enums.Underline
    type AttrTransferType TextTagUnderlinePropertyInfo = Pango.Enums.Underline
    type AttrGetType TextTagUnderlinePropertyInfo = Pango.Enums.Underline
    type AttrLabel TextTagUnderlinePropertyInfo = "underline"
    type AttrOrigin TextTagUnderlinePropertyInfo = TextTag
    attrGet = getTextTagUnderline
    attrSet = setTextTagUnderline
    attrTransfer _ v = do
        return v
    attrConstruct = constructTextTagUnderline
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextTag.underline"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextTag.html#g:attr:underline"
        })
#endif

-- VVV Prop "underline-rgba"
   -- Type: TInterface (Name {namespace = "Gdk", name = "RGBA"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@underline-rgba@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textTag #underlineRgba
-- @
getTextTagUnderlineRgba :: (MonadIO m, IsTextTag o) => o -> m (Maybe Gdk.RGBA.RGBA)
getTextTagUnderlineRgba obj = MIO.liftIO $ B.Properties.getObjectPropertyBoxed obj "underline-rgba" Gdk.RGBA.RGBA

-- | Set the value of the “@underline-rgba@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textTag [ #underlineRgba 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextTagUnderlineRgba :: (MonadIO m, IsTextTag o) => o -> Gdk.RGBA.RGBA -> m ()
setTextTagUnderlineRgba obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBoxed obj "underline-rgba" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@underline-rgba@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextTagUnderlineRgba :: (IsTextTag o, MIO.MonadIO m) => Gdk.RGBA.RGBA -> m (GValueConstruct o)
constructTextTagUnderlineRgba val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBoxed "underline-rgba" (P.Just val)

-- | Set the value of the “@underline-rgba@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #underlineRgba
-- @
clearTextTagUnderlineRgba :: (MonadIO m, IsTextTag o) => o -> m ()
clearTextTagUnderlineRgba obj = liftIO $ B.Properties.setObjectPropertyBoxed obj "underline-rgba" (Nothing :: Maybe Gdk.RGBA.RGBA)

#if defined(ENABLE_OVERLOADING)
data TextTagUnderlineRgbaPropertyInfo
instance AttrInfo TextTagUnderlineRgbaPropertyInfo where
    type AttrAllowedOps TextTagUnderlineRgbaPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint TextTagUnderlineRgbaPropertyInfo = IsTextTag
    type AttrSetTypeConstraint TextTagUnderlineRgbaPropertyInfo = (~) Gdk.RGBA.RGBA
    type AttrTransferTypeConstraint TextTagUnderlineRgbaPropertyInfo = (~) Gdk.RGBA.RGBA
    type AttrTransferType TextTagUnderlineRgbaPropertyInfo = Gdk.RGBA.RGBA
    type AttrGetType TextTagUnderlineRgbaPropertyInfo = (Maybe Gdk.RGBA.RGBA)
    type AttrLabel TextTagUnderlineRgbaPropertyInfo = "underline-rgba"
    type AttrOrigin TextTagUnderlineRgbaPropertyInfo = TextTag
    attrGet = getTextTagUnderlineRgba
    attrSet = setTextTagUnderlineRgba
    attrTransfer _ v = do
        return v
    attrConstruct = constructTextTagUnderlineRgba
    attrClear = clearTextTagUnderlineRgba
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextTag.underlineRgba"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextTag.html#g:attr:underlineRgba"
        })
#endif

-- VVV Prop "underline-rgba-set"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@underline-rgba-set@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textTag #underlineRgbaSet
-- @
getTextTagUnderlineRgbaSet :: (MonadIO m, IsTextTag o) => o -> m Bool
getTextTagUnderlineRgbaSet obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "underline-rgba-set"

-- | Set the value of the “@underline-rgba-set@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textTag [ #underlineRgbaSet 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextTagUnderlineRgbaSet :: (MonadIO m, IsTextTag o) => o -> Bool -> m ()
setTextTagUnderlineRgbaSet obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "underline-rgba-set" val

-- | Construct a `GValueConstruct` with valid value for the “@underline-rgba-set@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextTagUnderlineRgbaSet :: (IsTextTag o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructTextTagUnderlineRgbaSet val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "underline-rgba-set" val

#if defined(ENABLE_OVERLOADING)
data TextTagUnderlineRgbaSetPropertyInfo
instance AttrInfo TextTagUnderlineRgbaSetPropertyInfo where
    type AttrAllowedOps TextTagUnderlineRgbaSetPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TextTagUnderlineRgbaSetPropertyInfo = IsTextTag
    type AttrSetTypeConstraint TextTagUnderlineRgbaSetPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint TextTagUnderlineRgbaSetPropertyInfo = (~) Bool
    type AttrTransferType TextTagUnderlineRgbaSetPropertyInfo = Bool
    type AttrGetType TextTagUnderlineRgbaSetPropertyInfo = Bool
    type AttrLabel TextTagUnderlineRgbaSetPropertyInfo = "underline-rgba-set"
    type AttrOrigin TextTagUnderlineRgbaSetPropertyInfo = TextTag
    attrGet = getTextTagUnderlineRgbaSet
    attrSet = setTextTagUnderlineRgbaSet
    attrTransfer _ v = do
        return v
    attrConstruct = constructTextTagUnderlineRgbaSet
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextTag.underlineRgbaSet"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextTag.html#g:attr:underlineRgbaSet"
        })
#endif

-- VVV Prop "underline-set"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@underline-set@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textTag #underlineSet
-- @
getTextTagUnderlineSet :: (MonadIO m, IsTextTag o) => o -> m Bool
getTextTagUnderlineSet obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "underline-set"

-- | Set the value of the “@underline-set@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textTag [ #underlineSet 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextTagUnderlineSet :: (MonadIO m, IsTextTag o) => o -> Bool -> m ()
setTextTagUnderlineSet obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "underline-set" val

-- | Construct a `GValueConstruct` with valid value for the “@underline-set@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextTagUnderlineSet :: (IsTextTag o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructTextTagUnderlineSet val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "underline-set" val

#if defined(ENABLE_OVERLOADING)
data TextTagUnderlineSetPropertyInfo
instance AttrInfo TextTagUnderlineSetPropertyInfo where
    type AttrAllowedOps TextTagUnderlineSetPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TextTagUnderlineSetPropertyInfo = IsTextTag
    type AttrSetTypeConstraint TextTagUnderlineSetPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint TextTagUnderlineSetPropertyInfo = (~) Bool
    type AttrTransferType TextTagUnderlineSetPropertyInfo = Bool
    type AttrGetType TextTagUnderlineSetPropertyInfo = Bool
    type AttrLabel TextTagUnderlineSetPropertyInfo = "underline-set"
    type AttrOrigin TextTagUnderlineSetPropertyInfo = TextTag
    attrGet = getTextTagUnderlineSet
    attrSet = setTextTagUnderlineSet
    attrTransfer _ v = do
        return v
    attrConstruct = constructTextTagUnderlineSet
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextTag.underlineSet"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextTag.html#g:attr:underlineSet"
        })
#endif

-- VVV Prop "variant"
   -- Type: TInterface (Name {namespace = "Pango", name = "Variant"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@variant@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textTag #variant
-- @
getTextTagVariant :: (MonadIO m, IsTextTag o) => o -> m Pango.Enums.Variant
getTextTagVariant obj = MIO.liftIO $ B.Properties.getObjectPropertyEnum obj "variant"

-- | Set the value of the “@variant@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textTag [ #variant 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextTagVariant :: (MonadIO m, IsTextTag o) => o -> Pango.Enums.Variant -> m ()
setTextTagVariant obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyEnum obj "variant" val

-- | Construct a `GValueConstruct` with valid value for the “@variant@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextTagVariant :: (IsTextTag o, MIO.MonadIO m) => Pango.Enums.Variant -> m (GValueConstruct o)
constructTextTagVariant val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyEnum "variant" val

#if defined(ENABLE_OVERLOADING)
data TextTagVariantPropertyInfo
instance AttrInfo TextTagVariantPropertyInfo where
    type AttrAllowedOps TextTagVariantPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TextTagVariantPropertyInfo = IsTextTag
    type AttrSetTypeConstraint TextTagVariantPropertyInfo = (~) Pango.Enums.Variant
    type AttrTransferTypeConstraint TextTagVariantPropertyInfo = (~) Pango.Enums.Variant
    type AttrTransferType TextTagVariantPropertyInfo = Pango.Enums.Variant
    type AttrGetType TextTagVariantPropertyInfo = Pango.Enums.Variant
    type AttrLabel TextTagVariantPropertyInfo = "variant"
    type AttrOrigin TextTagVariantPropertyInfo = TextTag
    attrGet = getTextTagVariant
    attrSet = setTextTagVariant
    attrTransfer _ v = do
        return v
    attrConstruct = constructTextTagVariant
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextTag.variant"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextTag.html#g:attr:variant"
        })
#endif

-- VVV Prop "variant-set"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@variant-set@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textTag #variantSet
-- @
getTextTagVariantSet :: (MonadIO m, IsTextTag o) => o -> m Bool
getTextTagVariantSet obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "variant-set"

-- | Set the value of the “@variant-set@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textTag [ #variantSet 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextTagVariantSet :: (MonadIO m, IsTextTag o) => o -> Bool -> m ()
setTextTagVariantSet obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "variant-set" val

-- | Construct a `GValueConstruct` with valid value for the “@variant-set@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextTagVariantSet :: (IsTextTag o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructTextTagVariantSet val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "variant-set" val

#if defined(ENABLE_OVERLOADING)
data TextTagVariantSetPropertyInfo
instance AttrInfo TextTagVariantSetPropertyInfo where
    type AttrAllowedOps TextTagVariantSetPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TextTagVariantSetPropertyInfo = IsTextTag
    type AttrSetTypeConstraint TextTagVariantSetPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint TextTagVariantSetPropertyInfo = (~) Bool
    type AttrTransferType TextTagVariantSetPropertyInfo = Bool
    type AttrGetType TextTagVariantSetPropertyInfo = Bool
    type AttrLabel TextTagVariantSetPropertyInfo = "variant-set"
    type AttrOrigin TextTagVariantSetPropertyInfo = TextTag
    attrGet = getTextTagVariantSet
    attrSet = setTextTagVariantSet
    attrTransfer _ v = do
        return v
    attrConstruct = constructTextTagVariantSet
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextTag.variantSet"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextTag.html#g:attr:variantSet"
        })
#endif

-- VVV Prop "weight"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@weight@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textTag #weight
-- @
getTextTagWeight :: (MonadIO m, IsTextTag o) => o -> m Int32
getTextTagWeight obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "weight"

-- | Set the value of the “@weight@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textTag [ #weight 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextTagWeight :: (MonadIO m, IsTextTag o) => o -> Int32 -> m ()
setTextTagWeight obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyInt32 obj "weight" val

-- | Construct a `GValueConstruct` with valid value for the “@weight@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextTagWeight :: (IsTextTag o, MIO.MonadIO m) => Int32 -> m (GValueConstruct o)
constructTextTagWeight val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyInt32 "weight" val

#if defined(ENABLE_OVERLOADING)
data TextTagWeightPropertyInfo
instance AttrInfo TextTagWeightPropertyInfo where
    type AttrAllowedOps TextTagWeightPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TextTagWeightPropertyInfo = IsTextTag
    type AttrSetTypeConstraint TextTagWeightPropertyInfo = (~) Int32
    type AttrTransferTypeConstraint TextTagWeightPropertyInfo = (~) Int32
    type AttrTransferType TextTagWeightPropertyInfo = Int32
    type AttrGetType TextTagWeightPropertyInfo = Int32
    type AttrLabel TextTagWeightPropertyInfo = "weight"
    type AttrOrigin TextTagWeightPropertyInfo = TextTag
    attrGet = getTextTagWeight
    attrSet = setTextTagWeight
    attrTransfer _ v = do
        return v
    attrConstruct = constructTextTagWeight
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextTag.weight"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextTag.html#g:attr:weight"
        })
#endif

-- VVV Prop "weight-set"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@weight-set@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textTag #weightSet
-- @
getTextTagWeightSet :: (MonadIO m, IsTextTag o) => o -> m Bool
getTextTagWeightSet obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "weight-set"

-- | Set the value of the “@weight-set@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textTag [ #weightSet 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextTagWeightSet :: (MonadIO m, IsTextTag o) => o -> Bool -> m ()
setTextTagWeightSet obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "weight-set" val

-- | Construct a `GValueConstruct` with valid value for the “@weight-set@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextTagWeightSet :: (IsTextTag o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructTextTagWeightSet val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "weight-set" val

#if defined(ENABLE_OVERLOADING)
data TextTagWeightSetPropertyInfo
instance AttrInfo TextTagWeightSetPropertyInfo where
    type AttrAllowedOps TextTagWeightSetPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TextTagWeightSetPropertyInfo = IsTextTag
    type AttrSetTypeConstraint TextTagWeightSetPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint TextTagWeightSetPropertyInfo = (~) Bool
    type AttrTransferType TextTagWeightSetPropertyInfo = Bool
    type AttrGetType TextTagWeightSetPropertyInfo = Bool
    type AttrLabel TextTagWeightSetPropertyInfo = "weight-set"
    type AttrOrigin TextTagWeightSetPropertyInfo = TextTag
    attrGet = getTextTagWeightSet
    attrSet = setTextTagWeightSet
    attrTransfer _ v = do
        return v
    attrConstruct = constructTextTagWeightSet
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextTag.weightSet"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextTag.html#g:attr:weightSet"
        })
#endif

-- VVV Prop "wrap-mode"
   -- Type: TInterface (Name {namespace = "Gtk", name = "WrapMode"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@wrap-mode@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textTag #wrapMode
-- @
getTextTagWrapMode :: (MonadIO m, IsTextTag o) => o -> m Gtk.Enums.WrapMode
getTextTagWrapMode obj = MIO.liftIO $ B.Properties.getObjectPropertyEnum obj "wrap-mode"

-- | Set the value of the “@wrap-mode@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textTag [ #wrapMode 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextTagWrapMode :: (MonadIO m, IsTextTag o) => o -> Gtk.Enums.WrapMode -> m ()
setTextTagWrapMode obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyEnum obj "wrap-mode" val

-- | Construct a `GValueConstruct` with valid value for the “@wrap-mode@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextTagWrapMode :: (IsTextTag o, MIO.MonadIO m) => Gtk.Enums.WrapMode -> m (GValueConstruct o)
constructTextTagWrapMode val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyEnum "wrap-mode" val

#if defined(ENABLE_OVERLOADING)
data TextTagWrapModePropertyInfo
instance AttrInfo TextTagWrapModePropertyInfo where
    type AttrAllowedOps TextTagWrapModePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TextTagWrapModePropertyInfo = IsTextTag
    type AttrSetTypeConstraint TextTagWrapModePropertyInfo = (~) Gtk.Enums.WrapMode
    type AttrTransferTypeConstraint TextTagWrapModePropertyInfo = (~) Gtk.Enums.WrapMode
    type AttrTransferType TextTagWrapModePropertyInfo = Gtk.Enums.WrapMode
    type AttrGetType TextTagWrapModePropertyInfo = Gtk.Enums.WrapMode
    type AttrLabel TextTagWrapModePropertyInfo = "wrap-mode"
    type AttrOrigin TextTagWrapModePropertyInfo = TextTag
    attrGet = getTextTagWrapMode
    attrSet = setTextTagWrapMode
    attrTransfer _ v = do
        return v
    attrConstruct = constructTextTagWrapMode
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextTag.wrapMode"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextTag.html#g:attr:wrapMode"
        })
#endif

-- VVV Prop "wrap-mode-set"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@wrap-mode-set@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textTag #wrapModeSet
-- @
getTextTagWrapModeSet :: (MonadIO m, IsTextTag o) => o -> m Bool
getTextTagWrapModeSet obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "wrap-mode-set"

-- | Set the value of the “@wrap-mode-set@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textTag [ #wrapModeSet 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextTagWrapModeSet :: (MonadIO m, IsTextTag o) => o -> Bool -> m ()
setTextTagWrapModeSet obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "wrap-mode-set" val

-- | Construct a `GValueConstruct` with valid value for the “@wrap-mode-set@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextTagWrapModeSet :: (IsTextTag o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructTextTagWrapModeSet val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "wrap-mode-set" val

#if defined(ENABLE_OVERLOADING)
data TextTagWrapModeSetPropertyInfo
instance AttrInfo TextTagWrapModeSetPropertyInfo where
    type AttrAllowedOps TextTagWrapModeSetPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TextTagWrapModeSetPropertyInfo = IsTextTag
    type AttrSetTypeConstraint TextTagWrapModeSetPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint TextTagWrapModeSetPropertyInfo = (~) Bool
    type AttrTransferType TextTagWrapModeSetPropertyInfo = Bool
    type AttrGetType TextTagWrapModeSetPropertyInfo = Bool
    type AttrLabel TextTagWrapModeSetPropertyInfo = "wrap-mode-set"
    type AttrOrigin TextTagWrapModeSetPropertyInfo = TextTag
    attrGet = getTextTagWrapModeSet
    attrSet = setTextTagWrapModeSet
    attrTransfer _ v = do
        return v
    attrConstruct = constructTextTagWrapModeSet
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextTag.wrapModeSet"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextTag.html#g:attr:wrapModeSet"
        })
#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList TextTag
type instance O.AttributeList TextTag = TextTagAttributeList
type TextTagAttributeList = ('[ '("accumulativeMargin", TextTagAccumulativeMarginPropertyInfo), '("background", TextTagBackgroundPropertyInfo), '("backgroundFullHeight", TextTagBackgroundFullHeightPropertyInfo), '("backgroundFullHeightSet", TextTagBackgroundFullHeightSetPropertyInfo), '("backgroundGdk", TextTagBackgroundGdkPropertyInfo), '("backgroundRgba", TextTagBackgroundRgbaPropertyInfo), '("backgroundSet", TextTagBackgroundSetPropertyInfo), '("direction", TextTagDirectionPropertyInfo), '("editable", TextTagEditablePropertyInfo), '("editableSet", TextTagEditableSetPropertyInfo), '("fallback", TextTagFallbackPropertyInfo), '("fallbackSet", TextTagFallbackSetPropertyInfo), '("family", TextTagFamilyPropertyInfo), '("familySet", TextTagFamilySetPropertyInfo), '("font", TextTagFontPropertyInfo), '("fontDesc", TextTagFontDescPropertyInfo), '("fontFeatures", TextTagFontFeaturesPropertyInfo), '("fontFeaturesSet", TextTagFontFeaturesSetPropertyInfo), '("foreground", TextTagForegroundPropertyInfo), '("foregroundGdk", TextTagForegroundGdkPropertyInfo), '("foregroundRgba", TextTagForegroundRgbaPropertyInfo), '("foregroundSet", TextTagForegroundSetPropertyInfo), '("indent", TextTagIndentPropertyInfo), '("indentSet", TextTagIndentSetPropertyInfo), '("invisible", TextTagInvisiblePropertyInfo), '("invisibleSet", TextTagInvisibleSetPropertyInfo), '("justification", TextTagJustificationPropertyInfo), '("justificationSet", TextTagJustificationSetPropertyInfo), '("language", TextTagLanguagePropertyInfo), '("languageSet", TextTagLanguageSetPropertyInfo), '("leftMargin", TextTagLeftMarginPropertyInfo), '("leftMarginSet", TextTagLeftMarginSetPropertyInfo), '("letterSpacing", TextTagLetterSpacingPropertyInfo), '("letterSpacingSet", TextTagLetterSpacingSetPropertyInfo), '("name", TextTagNamePropertyInfo), '("paragraphBackground", TextTagParagraphBackgroundPropertyInfo), '("paragraphBackgroundGdk", TextTagParagraphBackgroundGdkPropertyInfo), '("paragraphBackgroundRgba", TextTagParagraphBackgroundRgbaPropertyInfo), '("paragraphBackgroundSet", TextTagParagraphBackgroundSetPropertyInfo), '("pixelsAboveLines", TextTagPixelsAboveLinesPropertyInfo), '("pixelsAboveLinesSet", TextTagPixelsAboveLinesSetPropertyInfo), '("pixelsBelowLines", TextTagPixelsBelowLinesPropertyInfo), '("pixelsBelowLinesSet", TextTagPixelsBelowLinesSetPropertyInfo), '("pixelsInsideWrap", TextTagPixelsInsideWrapPropertyInfo), '("pixelsInsideWrapSet", TextTagPixelsInsideWrapSetPropertyInfo), '("rightMargin", TextTagRightMarginPropertyInfo), '("rightMarginSet", TextTagRightMarginSetPropertyInfo), '("rise", TextTagRisePropertyInfo), '("riseSet", TextTagRiseSetPropertyInfo), '("scale", TextTagScalePropertyInfo), '("scaleSet", TextTagScaleSetPropertyInfo), '("size", TextTagSizePropertyInfo), '("sizePoints", TextTagSizePointsPropertyInfo), '("sizeSet", TextTagSizeSetPropertyInfo), '("stretch", TextTagStretchPropertyInfo), '("stretchSet", TextTagStretchSetPropertyInfo), '("strikethrough", TextTagStrikethroughPropertyInfo), '("strikethroughRgba", TextTagStrikethroughRgbaPropertyInfo), '("strikethroughRgbaSet", TextTagStrikethroughRgbaSetPropertyInfo), '("strikethroughSet", TextTagStrikethroughSetPropertyInfo), '("style", TextTagStylePropertyInfo), '("styleSet", TextTagStyleSetPropertyInfo), '("tabs", TextTagTabsPropertyInfo), '("tabsSet", TextTagTabsSetPropertyInfo), '("underline", TextTagUnderlinePropertyInfo), '("underlineRgba", TextTagUnderlineRgbaPropertyInfo), '("underlineRgbaSet", TextTagUnderlineRgbaSetPropertyInfo), '("underlineSet", TextTagUnderlineSetPropertyInfo), '("variant", TextTagVariantPropertyInfo), '("variantSet", TextTagVariantSetPropertyInfo), '("weight", TextTagWeightPropertyInfo), '("weightSet", TextTagWeightSetPropertyInfo), '("wrapMode", TextTagWrapModePropertyInfo), '("wrapModeSet", TextTagWrapModeSetPropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
textTagAccumulativeMargin :: AttrLabelProxy "accumulativeMargin"
textTagAccumulativeMargin = AttrLabelProxy

textTagBackground :: AttrLabelProxy "background"
textTagBackground = AttrLabelProxy

textTagBackgroundFullHeight :: AttrLabelProxy "backgroundFullHeight"
textTagBackgroundFullHeight = AttrLabelProxy

textTagBackgroundFullHeightSet :: AttrLabelProxy "backgroundFullHeightSet"
textTagBackgroundFullHeightSet = AttrLabelProxy

textTagBackgroundGdk :: AttrLabelProxy "backgroundGdk"
textTagBackgroundGdk = AttrLabelProxy

textTagBackgroundRgba :: AttrLabelProxy "backgroundRgba"
textTagBackgroundRgba = AttrLabelProxy

textTagBackgroundSet :: AttrLabelProxy "backgroundSet"
textTagBackgroundSet = AttrLabelProxy

textTagDirection :: AttrLabelProxy "direction"
textTagDirection = AttrLabelProxy

textTagEditable :: AttrLabelProxy "editable"
textTagEditable = AttrLabelProxy

textTagEditableSet :: AttrLabelProxy "editableSet"
textTagEditableSet = AttrLabelProxy

textTagFallback :: AttrLabelProxy "fallback"
textTagFallback = AttrLabelProxy

textTagFallbackSet :: AttrLabelProxy "fallbackSet"
textTagFallbackSet = AttrLabelProxy

textTagFamily :: AttrLabelProxy "family"
textTagFamily = AttrLabelProxy

textTagFamilySet :: AttrLabelProxy "familySet"
textTagFamilySet = AttrLabelProxy

textTagFont :: AttrLabelProxy "font"
textTagFont = AttrLabelProxy

textTagFontDesc :: AttrLabelProxy "fontDesc"
textTagFontDesc = AttrLabelProxy

textTagFontFeatures :: AttrLabelProxy "fontFeatures"
textTagFontFeatures = AttrLabelProxy

textTagFontFeaturesSet :: AttrLabelProxy "fontFeaturesSet"
textTagFontFeaturesSet = AttrLabelProxy

textTagForeground :: AttrLabelProxy "foreground"
textTagForeground = AttrLabelProxy

textTagForegroundGdk :: AttrLabelProxy "foregroundGdk"
textTagForegroundGdk = AttrLabelProxy

textTagForegroundRgba :: AttrLabelProxy "foregroundRgba"
textTagForegroundRgba = AttrLabelProxy

textTagForegroundSet :: AttrLabelProxy "foregroundSet"
textTagForegroundSet = AttrLabelProxy

textTagIndent :: AttrLabelProxy "indent"
textTagIndent = AttrLabelProxy

textTagIndentSet :: AttrLabelProxy "indentSet"
textTagIndentSet = AttrLabelProxy

textTagInvisible :: AttrLabelProxy "invisible"
textTagInvisible = AttrLabelProxy

textTagInvisibleSet :: AttrLabelProxy "invisibleSet"
textTagInvisibleSet = AttrLabelProxy

textTagJustification :: AttrLabelProxy "justification"
textTagJustification = AttrLabelProxy

textTagJustificationSet :: AttrLabelProxy "justificationSet"
textTagJustificationSet = AttrLabelProxy

textTagLanguage :: AttrLabelProxy "language"
textTagLanguage = AttrLabelProxy

textTagLanguageSet :: AttrLabelProxy "languageSet"
textTagLanguageSet = AttrLabelProxy

textTagLeftMargin :: AttrLabelProxy "leftMargin"
textTagLeftMargin = AttrLabelProxy

textTagLeftMarginSet :: AttrLabelProxy "leftMarginSet"
textTagLeftMarginSet = AttrLabelProxy

textTagLetterSpacing :: AttrLabelProxy "letterSpacing"
textTagLetterSpacing = AttrLabelProxy

textTagLetterSpacingSet :: AttrLabelProxy "letterSpacingSet"
textTagLetterSpacingSet = AttrLabelProxy

textTagName :: AttrLabelProxy "name"
textTagName = AttrLabelProxy

textTagParagraphBackground :: AttrLabelProxy "paragraphBackground"
textTagParagraphBackground = AttrLabelProxy

textTagParagraphBackgroundGdk :: AttrLabelProxy "paragraphBackgroundGdk"
textTagParagraphBackgroundGdk = AttrLabelProxy

textTagParagraphBackgroundRgba :: AttrLabelProxy "paragraphBackgroundRgba"
textTagParagraphBackgroundRgba = AttrLabelProxy

textTagParagraphBackgroundSet :: AttrLabelProxy "paragraphBackgroundSet"
textTagParagraphBackgroundSet = AttrLabelProxy

textTagPixelsAboveLines :: AttrLabelProxy "pixelsAboveLines"
textTagPixelsAboveLines = AttrLabelProxy

textTagPixelsAboveLinesSet :: AttrLabelProxy "pixelsAboveLinesSet"
textTagPixelsAboveLinesSet = AttrLabelProxy

textTagPixelsBelowLines :: AttrLabelProxy "pixelsBelowLines"
textTagPixelsBelowLines = AttrLabelProxy

textTagPixelsBelowLinesSet :: AttrLabelProxy "pixelsBelowLinesSet"
textTagPixelsBelowLinesSet = AttrLabelProxy

textTagPixelsInsideWrap :: AttrLabelProxy "pixelsInsideWrap"
textTagPixelsInsideWrap = AttrLabelProxy

textTagPixelsInsideWrapSet :: AttrLabelProxy "pixelsInsideWrapSet"
textTagPixelsInsideWrapSet = AttrLabelProxy

textTagRightMargin :: AttrLabelProxy "rightMargin"
textTagRightMargin = AttrLabelProxy

textTagRightMarginSet :: AttrLabelProxy "rightMarginSet"
textTagRightMarginSet = AttrLabelProxy

textTagRise :: AttrLabelProxy "rise"
textTagRise = AttrLabelProxy

textTagRiseSet :: AttrLabelProxy "riseSet"
textTagRiseSet = AttrLabelProxy

textTagScale :: AttrLabelProxy "scale"
textTagScale = AttrLabelProxy

textTagScaleSet :: AttrLabelProxy "scaleSet"
textTagScaleSet = AttrLabelProxy

textTagSize :: AttrLabelProxy "size"
textTagSize = AttrLabelProxy

textTagSizePoints :: AttrLabelProxy "sizePoints"
textTagSizePoints = AttrLabelProxy

textTagSizeSet :: AttrLabelProxy "sizeSet"
textTagSizeSet = AttrLabelProxy

textTagStretch :: AttrLabelProxy "stretch"
textTagStretch = AttrLabelProxy

textTagStretchSet :: AttrLabelProxy "stretchSet"
textTagStretchSet = AttrLabelProxy

textTagStrikethrough :: AttrLabelProxy "strikethrough"
textTagStrikethrough = AttrLabelProxy

textTagStrikethroughRgba :: AttrLabelProxy "strikethroughRgba"
textTagStrikethroughRgba = AttrLabelProxy

textTagStrikethroughRgbaSet :: AttrLabelProxy "strikethroughRgbaSet"
textTagStrikethroughRgbaSet = AttrLabelProxy

textTagStrikethroughSet :: AttrLabelProxy "strikethroughSet"
textTagStrikethroughSet = AttrLabelProxy

textTagStyle :: AttrLabelProxy "style"
textTagStyle = AttrLabelProxy

textTagStyleSet :: AttrLabelProxy "styleSet"
textTagStyleSet = AttrLabelProxy

textTagTabs :: AttrLabelProxy "tabs"
textTagTabs = AttrLabelProxy

textTagTabsSet :: AttrLabelProxy "tabsSet"
textTagTabsSet = AttrLabelProxy

textTagUnderline :: AttrLabelProxy "underline"
textTagUnderline = AttrLabelProxy

textTagUnderlineRgba :: AttrLabelProxy "underlineRgba"
textTagUnderlineRgba = AttrLabelProxy

textTagUnderlineRgbaSet :: AttrLabelProxy "underlineRgbaSet"
textTagUnderlineRgbaSet = AttrLabelProxy

textTagUnderlineSet :: AttrLabelProxy "underlineSet"
textTagUnderlineSet = AttrLabelProxy

textTagVariant :: AttrLabelProxy "variant"
textTagVariant = AttrLabelProxy

textTagVariantSet :: AttrLabelProxy "variantSet"
textTagVariantSet = AttrLabelProxy

textTagWeight :: AttrLabelProxy "weight"
textTagWeight = AttrLabelProxy

textTagWeightSet :: AttrLabelProxy "weightSet"
textTagWeightSet = AttrLabelProxy

textTagWrapMode :: AttrLabelProxy "wrapMode"
textTagWrapMode = AttrLabelProxy

textTagWrapModeSet :: AttrLabelProxy "wrapModeSet"
textTagWrapModeSet = AttrLabelProxy

#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList TextTag = TextTagSignalList
type TextTagSignalList = ('[ '("event", TextTagEventSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo)] :: [(Symbol, *)])

#endif

-- method TextTag::new
-- method type : Constructor
-- Args: [ Arg
--           { argCName = "name"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "tag name, or %NULL" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "TextTag" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_text_tag_new" gtk_text_tag_new :: 
    CString ->                              -- name : TBasicType TUTF8
    IO (Ptr TextTag)

-- | Creates a t'GI.Gtk.Objects.TextTag.TextTag'. Configure the tag using object arguments,
-- i.e. using @/g_object_set()/@.
textTagNew ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    Maybe (T.Text)
    -- ^ /@name@/: tag name, or 'P.Nothing'
    -> m TextTag
    -- ^ __Returns:__ a new t'GI.Gtk.Objects.TextTag.TextTag'
textTagNew name = liftIO $ do
    maybeName <- case name of
        Nothing -> return nullPtr
        Just jName -> do
            jName' <- textToCString jName
            return jName'
    result <- gtk_text_tag_new maybeName
    checkUnexpectedReturnNULL "textTagNew" result
    result' <- (wrapObject TextTag) result
    freeMem maybeName
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method TextTag::changed
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tag"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextTag" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextTag." , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "size_changed"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "whether the change affects the #GtkTextView layout."
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

foreign import ccall "gtk_text_tag_changed" gtk_text_tag_changed :: 
    Ptr TextTag ->                          -- tag : TInterface (Name {namespace = "Gtk", name = "TextTag"})
    CInt ->                                 -- size_changed : TBasicType TBoolean
    IO ()

-- | Emits the [TextTagTable::tagChanged]("GI.Gtk.Objects.TextTagTable#g:signal:tagChanged") signal on the t'GI.Gtk.Objects.TextTagTable.TextTagTable' where
-- the tag is included.
-- 
-- The signal is already emitted when setting a t'GI.Gtk.Objects.TextTag.TextTag' property. This
-- function is useful for a t'GI.Gtk.Objects.TextTag.TextTag' subclass.
-- 
-- /Since: 3.20/
textTagChanged ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextTag a) =>
    a
    -- ^ /@tag@/: a t'GI.Gtk.Objects.TextTag.TextTag'.
    -> Bool
    -- ^ /@sizeChanged@/: whether the change affects the t'GI.Gtk.Objects.TextView.TextView' layout.
    -> m ()
textTagChanged tag sizeChanged = liftIO $ do
    tag' <- unsafeManagedPtrCastPtr tag
    let sizeChanged' = (fromIntegral . fromEnum) sizeChanged
    gtk_text_tag_changed tag' sizeChanged'
    touchManagedPtr tag
    return ()

#if defined(ENABLE_OVERLOADING)
data TextTagChangedMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsTextTag a) => O.OverloadedMethod TextTagChangedMethodInfo a signature where
    overloadedMethod = textTagChanged

instance O.OverloadedMethodInfo TextTagChangedMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextTag.textTagChanged",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextTag.html#v:textTagChanged"
        })


#endif

-- method TextTag::event
-- method type : OrdinaryMethod
-- Args: [ Arg
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
--           { argCName = "event_object"
--           , argType =
--               TInterface Name { namespace = "GObject" , name = "Object" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "object that received the event, such as a widget"
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
--           , argType = TInterface Name { namespace = "Gdk" , name = "Event" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the event" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "location where the event was received"
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

foreign import ccall "gtk_text_tag_event" gtk_text_tag_event :: 
    Ptr TextTag ->                          -- tag : TInterface (Name {namespace = "Gtk", name = "TextTag"})
    Ptr GObject.Object.Object ->            -- event_object : TInterface (Name {namespace = "GObject", name = "Object"})
    Ptr Gdk.Event.Event ->                  -- event : TInterface (Name {namespace = "Gdk", name = "Event"})
    Ptr Gtk.TextIter.TextIter ->            -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    IO CInt

-- | Emits the “event” signal on the t'GI.Gtk.Objects.TextTag.TextTag'.
textTagEvent ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextTag a, GObject.Object.IsObject b) =>
    a
    -- ^ /@tag@/: a t'GI.Gtk.Objects.TextTag.TextTag'
    -> b
    -- ^ /@eventObject@/: object that received the event, such as a widget
    -> Gdk.Event.Event
    -- ^ /@event@/: the event
    -> Gtk.TextIter.TextIter
    -- ^ /@iter@/: location where the event was received
    -> m Bool
    -- ^ __Returns:__ result of signal emission (whether the event was handled)
textTagEvent tag eventObject event iter = liftIO $ do
    tag' <- unsafeManagedPtrCastPtr tag
    eventObject' <- unsafeManagedPtrCastPtr eventObject
    event' <- unsafeManagedPtrGetPtr event
    iter' <- unsafeManagedPtrGetPtr iter
    result <- gtk_text_tag_event tag' eventObject' event' iter'
    let result' = (/= 0) result
    touchManagedPtr tag
    touchManagedPtr eventObject
    touchManagedPtr event
    touchManagedPtr iter
    return result'

#if defined(ENABLE_OVERLOADING)
data TextTagEventMethodInfo
instance (signature ~ (b -> Gdk.Event.Event -> Gtk.TextIter.TextIter -> m Bool), MonadIO m, IsTextTag a, GObject.Object.IsObject b) => O.OverloadedMethod TextTagEventMethodInfo a signature where
    overloadedMethod = textTagEvent

instance O.OverloadedMethodInfo TextTagEventMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextTag.textTagEvent",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextTag.html#v:textTagEvent"
        })


#endif

-- method TextTag::get_priority
-- method type : OrdinaryMethod
-- Args: [ Arg
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
-- returnType: Just (TBasicType TInt)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_text_tag_get_priority" gtk_text_tag_get_priority :: 
    Ptr TextTag ->                          -- tag : TInterface (Name {namespace = "Gtk", name = "TextTag"})
    IO Int32

-- | Get the tag priority.
textTagGetPriority ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextTag a) =>
    a
    -- ^ /@tag@/: a t'GI.Gtk.Objects.TextTag.TextTag'
    -> m Int32
    -- ^ __Returns:__ The tag’s priority.
textTagGetPriority tag = liftIO $ do
    tag' <- unsafeManagedPtrCastPtr tag
    result <- gtk_text_tag_get_priority tag'
    touchManagedPtr tag
    return result

#if defined(ENABLE_OVERLOADING)
data TextTagGetPriorityMethodInfo
instance (signature ~ (m Int32), MonadIO m, IsTextTag a) => O.OverloadedMethod TextTagGetPriorityMethodInfo a signature where
    overloadedMethod = textTagGetPriority

instance O.OverloadedMethodInfo TextTagGetPriorityMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextTag.textTagGetPriority",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextTag.html#v:textTagGetPriority"
        })


#endif

-- method TextTag::set_priority
-- method type : OrdinaryMethod
-- Args: [ Arg
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
--           { argCName = "priority"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the new priority" , sinceVersion = Nothing }
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

foreign import ccall "gtk_text_tag_set_priority" gtk_text_tag_set_priority :: 
    Ptr TextTag ->                          -- tag : TInterface (Name {namespace = "Gtk", name = "TextTag"})
    Int32 ->                                -- priority : TBasicType TInt
    IO ()

-- | Sets the priority of a t'GI.Gtk.Objects.TextTag.TextTag'. Valid priorities
-- start at 0 and go to one less than 'GI.Gtk.Objects.TextTagTable.textTagTableGetSize'.
-- Each tag in a table has a unique priority; setting the priority
-- of one tag shifts the priorities of all the other tags in the
-- table to maintain a unique priority for each tag. Higher priority
-- tags “win” if two tags both set the same text attribute. When adding
-- a tag to a tag table, it will be assigned the highest priority in
-- the table by default; so normally the precedence of a set of tags
-- is the order in which they were added to the table, or created with
-- @/gtk_text_buffer_create_tag()/@, which adds the tag to the buffer’s table
-- automatically.
textTagSetPriority ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextTag a) =>
    a
    -- ^ /@tag@/: a t'GI.Gtk.Objects.TextTag.TextTag'
    -> Int32
    -- ^ /@priority@/: the new priority
    -> m ()
textTagSetPriority tag priority = liftIO $ do
    tag' <- unsafeManagedPtrCastPtr tag
    gtk_text_tag_set_priority tag' priority
    touchManagedPtr tag
    return ()

#if defined(ENABLE_OVERLOADING)
data TextTagSetPriorityMethodInfo
instance (signature ~ (Int32 -> m ()), MonadIO m, IsTextTag a) => O.OverloadedMethod TextTagSetPriorityMethodInfo a signature where
    overloadedMethod = textTagSetPriority

instance O.OverloadedMethodInfo TextTagSetPriorityMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextTag.textTagSetPriority",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextTag.html#v:textTagSetPriority"
        })


#endif


