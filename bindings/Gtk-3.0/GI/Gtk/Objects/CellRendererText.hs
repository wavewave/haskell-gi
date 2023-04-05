{-# LANGUAGE ImplicitParams, RankNTypes, TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- A t'GI.Gtk.Objects.CellRendererText.CellRendererText' renders a given text in its cell, using the font, color and
-- style information provided by its properties. The text will be ellipsized if it is
-- too long and the [CellRendererText:ellipsize]("GI.Gtk.Objects.CellRendererText#g:attr:ellipsize") property allows it.
-- 
-- If the [CellRenderer:mode]("GI.Gtk.Objects.CellRenderer#g:attr:mode") is 'GI.Gtk.Enums.CellRendererModeEditable',
-- the t'GI.Gtk.Objects.CellRendererText.CellRendererText' allows to edit its text using an entry.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.CellRendererText
    ( 

-- * Exported types
    CellRendererText(..)                    ,
    IsCellRendererText                      ,
    toCellRendererText                      ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [activate]("GI.Gtk.Objects.CellRenderer#g:method:activate"), [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [isActivatable]("GI.Gtk.Objects.CellRenderer#g:method:isActivatable"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [render]("GI.Gtk.Objects.CellRenderer#g:method:render"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [startEditing]("GI.Gtk.Objects.CellRenderer#g:method:startEditing"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [stopEditing]("GI.Gtk.Objects.CellRenderer#g:method:stopEditing"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getAlignedArea]("GI.Gtk.Objects.CellRenderer#g:method:getAlignedArea"), [getAlignment]("GI.Gtk.Objects.CellRenderer#g:method:getAlignment"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getFixedSize]("GI.Gtk.Objects.CellRenderer#g:method:getFixedSize"), [getPadding]("GI.Gtk.Objects.CellRenderer#g:method:getPadding"), [getPreferredHeight]("GI.Gtk.Objects.CellRenderer#g:method:getPreferredHeight"), [getPreferredHeightForWidth]("GI.Gtk.Objects.CellRenderer#g:method:getPreferredHeightForWidth"), [getPreferredSize]("GI.Gtk.Objects.CellRenderer#g:method:getPreferredSize"), [getPreferredWidth]("GI.Gtk.Objects.CellRenderer#g:method:getPreferredWidth"), [getPreferredWidthForHeight]("GI.Gtk.Objects.CellRenderer#g:method:getPreferredWidthForHeight"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getRequestMode]("GI.Gtk.Objects.CellRenderer#g:method:getRequestMode"), [getSensitive]("GI.Gtk.Objects.CellRenderer#g:method:getSensitive"), [getSize]("GI.Gtk.Objects.CellRenderer#g:method:getSize"), [getState]("GI.Gtk.Objects.CellRenderer#g:method:getState"), [getVisible]("GI.Gtk.Objects.CellRenderer#g:method:getVisible").
-- 
-- ==== Setters
-- [setAlignment]("GI.Gtk.Objects.CellRenderer#g:method:setAlignment"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setFixedHeightFromFont]("GI.Gtk.Objects.CellRendererText#g:method:setFixedHeightFromFont"), [setFixedSize]("GI.Gtk.Objects.CellRenderer#g:method:setFixedSize"), [setPadding]("GI.Gtk.Objects.CellRenderer#g:method:setPadding"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setSensitive]("GI.Gtk.Objects.CellRenderer#g:method:setSensitive"), [setVisible]("GI.Gtk.Objects.CellRenderer#g:method:setVisible").

#if defined(ENABLE_OVERLOADING)
    ResolveCellRendererTextMethod           ,
#endif

-- ** new #method:new#

    cellRendererTextNew                     ,


-- ** setFixedHeightFromFont #method:setFixedHeightFromFont#

#if defined(ENABLE_OVERLOADING)
    CellRendererTextSetFixedHeightFromFontMethodInfo,
#endif
    cellRendererTextSetFixedHeightFromFont  ,




 -- * Properties


-- ** alignSet #attr:alignSet#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    CellRendererTextAlignSetPropertyInfo    ,
#endif
#if defined(ENABLE_OVERLOADING)
    cellRendererTextAlignSet                ,
#endif
    constructCellRendererTextAlignSet       ,
    getCellRendererTextAlignSet             ,
    setCellRendererTextAlignSet             ,


-- ** alignment #attr:alignment#
-- | Specifies how to align the lines of text with respect to each other.
-- 
-- Note that this property describes how to align the lines of text in
-- case there are several of them. The \"xalign\" property of t'GI.Gtk.Objects.CellRenderer.CellRenderer',
-- on the other hand, sets the horizontal alignment of the whole text.
-- 
-- /Since: 2.10/

#if defined(ENABLE_OVERLOADING)
    CellRendererTextAlignmentPropertyInfo   ,
#endif
#if defined(ENABLE_OVERLOADING)
    cellRendererTextAlignment               ,
#endif
    constructCellRendererTextAlignment      ,
    getCellRendererTextAlignment            ,
    setCellRendererTextAlignment            ,


-- ** attributes #attr:attributes#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    CellRendererTextAttributesPropertyInfo  ,
#endif
#if defined(ENABLE_OVERLOADING)
    cellRendererTextAttributes              ,
#endif
    clearCellRendererTextAttributes         ,
    constructCellRendererTextAttributes     ,
    getCellRendererTextAttributes           ,
    setCellRendererTextAttributes           ,


-- ** background #attr:background#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    CellRendererTextBackgroundPropertyInfo  ,
#endif
#if defined(ENABLE_OVERLOADING)
    cellRendererTextBackground              ,
#endif
    clearCellRendererTextBackground         ,
    constructCellRendererTextBackground     ,
    setCellRendererTextBackground           ,


-- ** backgroundGdk #attr:backgroundGdk#
-- | Background color as a t'GI.Gdk.Structs.Color.Color'

#if defined(ENABLE_OVERLOADING)
    CellRendererTextBackgroundGdkPropertyInfo,
#endif
#if defined(ENABLE_OVERLOADING)
    cellRendererTextBackgroundGdk           ,
#endif
    clearCellRendererTextBackgroundGdk      ,
    constructCellRendererTextBackgroundGdk  ,
    getCellRendererTextBackgroundGdk        ,
    setCellRendererTextBackgroundGdk        ,


-- ** backgroundRgba #attr:backgroundRgba#
-- | Background color as a t'GI.Gdk.Structs.RGBA.RGBA'
-- 
-- /Since: 3.0/

#if defined(ENABLE_OVERLOADING)
    CellRendererTextBackgroundRgbaPropertyInfo,
#endif
#if defined(ENABLE_OVERLOADING)
    cellRendererTextBackgroundRgba          ,
#endif
    clearCellRendererTextBackgroundRgba     ,
    constructCellRendererTextBackgroundRgba ,
    getCellRendererTextBackgroundRgba       ,
    setCellRendererTextBackgroundRgba       ,


-- ** backgroundSet #attr:backgroundSet#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    CellRendererTextBackgroundSetPropertyInfo,
#endif
#if defined(ENABLE_OVERLOADING)
    cellRendererTextBackgroundSet           ,
#endif
    constructCellRendererTextBackgroundSet  ,
    getCellRendererTextBackgroundSet        ,
    setCellRendererTextBackgroundSet        ,


-- ** editable #attr:editable#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    CellRendererTextEditablePropertyInfo    ,
#endif
#if defined(ENABLE_OVERLOADING)
    cellRendererTextEditable                ,
#endif
    constructCellRendererTextEditable       ,
    getCellRendererTextEditable             ,
    setCellRendererTextEditable             ,


-- ** editableSet #attr:editableSet#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    CellRendererTextEditableSetPropertyInfo ,
#endif
#if defined(ENABLE_OVERLOADING)
    cellRendererTextEditableSet             ,
#endif
    constructCellRendererTextEditableSet    ,
    getCellRendererTextEditableSet          ,
    setCellRendererTextEditableSet          ,


-- ** ellipsize #attr:ellipsize#
-- | Specifies the preferred place to ellipsize the string, if the cell renderer
-- does not have enough room to display the entire string. Setting it to
-- 'GI.Pango.Enums.EllipsizeModeNone' turns off ellipsizing. See the wrap-width property
-- for another way of making the text fit into a given width.
-- 
-- /Since: 2.6/

#if defined(ENABLE_OVERLOADING)
    CellRendererTextEllipsizePropertyInfo   ,
#endif
#if defined(ENABLE_OVERLOADING)
    cellRendererTextEllipsize               ,
#endif
    constructCellRendererTextEllipsize      ,
    getCellRendererTextEllipsize            ,
    setCellRendererTextEllipsize            ,


-- ** ellipsizeSet #attr:ellipsizeSet#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    CellRendererTextEllipsizeSetPropertyInfo,
#endif
#if defined(ENABLE_OVERLOADING)
    cellRendererTextEllipsizeSet            ,
#endif
    constructCellRendererTextEllipsizeSet   ,
    getCellRendererTextEllipsizeSet         ,
    setCellRendererTextEllipsizeSet         ,


-- ** family #attr:family#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    CellRendererTextFamilyPropertyInfo      ,
#endif
#if defined(ENABLE_OVERLOADING)
    cellRendererTextFamily                  ,
#endif
    clearCellRendererTextFamily             ,
    constructCellRendererTextFamily         ,
    getCellRendererTextFamily               ,
    setCellRendererTextFamily               ,


-- ** familySet #attr:familySet#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    CellRendererTextFamilySetPropertyInfo   ,
#endif
#if defined(ENABLE_OVERLOADING)
    cellRendererTextFamilySet               ,
#endif
    constructCellRendererTextFamilySet      ,
    getCellRendererTextFamilySet            ,
    setCellRendererTextFamilySet            ,


-- ** font #attr:font#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    CellRendererTextFontPropertyInfo        ,
#endif
#if defined(ENABLE_OVERLOADING)
    cellRendererTextFont                    ,
#endif
    clearCellRendererTextFont               ,
    constructCellRendererTextFont           ,
    getCellRendererTextFont                 ,
    setCellRendererTextFont                 ,


-- ** fontDesc #attr:fontDesc#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    CellRendererTextFontDescPropertyInfo    ,
#endif
#if defined(ENABLE_OVERLOADING)
    cellRendererTextFontDesc                ,
#endif
    clearCellRendererTextFontDesc           ,
    constructCellRendererTextFontDesc       ,
    getCellRendererTextFontDesc             ,
    setCellRendererTextFontDesc             ,


-- ** foreground #attr:foreground#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    CellRendererTextForegroundPropertyInfo  ,
#endif
#if defined(ENABLE_OVERLOADING)
    cellRendererTextForeground              ,
#endif
    clearCellRendererTextForeground         ,
    constructCellRendererTextForeground     ,
    setCellRendererTextForeground           ,


-- ** foregroundGdk #attr:foregroundGdk#
-- | Foreground color as a t'GI.Gdk.Structs.Color.Color'

#if defined(ENABLE_OVERLOADING)
    CellRendererTextForegroundGdkPropertyInfo,
#endif
#if defined(ENABLE_OVERLOADING)
    cellRendererTextForegroundGdk           ,
#endif
    clearCellRendererTextForegroundGdk      ,
    constructCellRendererTextForegroundGdk  ,
    getCellRendererTextForegroundGdk        ,
    setCellRendererTextForegroundGdk        ,


-- ** foregroundRgba #attr:foregroundRgba#
-- | Foreground color as a t'GI.Gdk.Structs.RGBA.RGBA'
-- 
-- /Since: 3.0/

#if defined(ENABLE_OVERLOADING)
    CellRendererTextForegroundRgbaPropertyInfo,
#endif
#if defined(ENABLE_OVERLOADING)
    cellRendererTextForegroundRgba          ,
#endif
    clearCellRendererTextForegroundRgba     ,
    constructCellRendererTextForegroundRgba ,
    getCellRendererTextForegroundRgba       ,
    setCellRendererTextForegroundRgba       ,


-- ** foregroundSet #attr:foregroundSet#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    CellRendererTextForegroundSetPropertyInfo,
#endif
#if defined(ENABLE_OVERLOADING)
    cellRendererTextForegroundSet           ,
#endif
    constructCellRendererTextForegroundSet  ,
    getCellRendererTextForegroundSet        ,
    setCellRendererTextForegroundSet        ,


-- ** language #attr:language#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    CellRendererTextLanguagePropertyInfo    ,
#endif
#if defined(ENABLE_OVERLOADING)
    cellRendererTextLanguage                ,
#endif
    clearCellRendererTextLanguage           ,
    constructCellRendererTextLanguage       ,
    getCellRendererTextLanguage             ,
    setCellRendererTextLanguage             ,


-- ** languageSet #attr:languageSet#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    CellRendererTextLanguageSetPropertyInfo ,
#endif
#if defined(ENABLE_OVERLOADING)
    cellRendererTextLanguageSet             ,
#endif
    constructCellRendererTextLanguageSet    ,
    getCellRendererTextLanguageSet          ,
    setCellRendererTextLanguageSet          ,


-- ** markup #attr:markup#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    CellRendererTextMarkupPropertyInfo      ,
#endif
#if defined(ENABLE_OVERLOADING)
    cellRendererTextMarkup                  ,
#endif
    clearCellRendererTextMarkup             ,
    constructCellRendererTextMarkup         ,
    setCellRendererTextMarkup               ,


-- ** maxWidthChars #attr:maxWidthChars#
-- | The desired maximum width of the cell, in characters. If this property
-- is set to -1, the width will be calculated automatically.
-- 
-- For cell renderers that ellipsize or wrap text; this property
-- controls the maximum reported width of the cell. The
-- cell should not receive any greater allocation unless it is
-- set to expand in its t'GI.Gtk.Interfaces.CellLayout.CellLayout' and all of the cell\'s siblings
-- have received their natural width.
-- 
-- /Since: 3.0/

#if defined(ENABLE_OVERLOADING)
    CellRendererTextMaxWidthCharsPropertyInfo,
#endif
#if defined(ENABLE_OVERLOADING)
    cellRendererTextMaxWidthChars           ,
#endif
    constructCellRendererTextMaxWidthChars  ,
    getCellRendererTextMaxWidthChars        ,
    setCellRendererTextMaxWidthChars        ,


-- ** placeholderText #attr:placeholderText#
-- | The text that will be displayed in the t'GI.Gtk.Objects.CellRenderer.CellRenderer' if
-- [CellRendererText:editable]("GI.Gtk.Objects.CellRendererText#g:attr:editable") is 'P.True' and the cell is empty.
-- 
-- Since 3.6

#if defined(ENABLE_OVERLOADING)
    CellRendererTextPlaceholderTextPropertyInfo,
#endif
#if defined(ENABLE_OVERLOADING)
    cellRendererTextPlaceholderText         ,
#endif
    clearCellRendererTextPlaceholderText    ,
    constructCellRendererTextPlaceholderText,
    getCellRendererTextPlaceholderText      ,
    setCellRendererTextPlaceholderText      ,


-- ** rise #attr:rise#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    CellRendererTextRisePropertyInfo        ,
#endif
#if defined(ENABLE_OVERLOADING)
    cellRendererTextRise                    ,
#endif
    constructCellRendererTextRise           ,
    getCellRendererTextRise                 ,
    setCellRendererTextRise                 ,


-- ** riseSet #attr:riseSet#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    CellRendererTextRiseSetPropertyInfo     ,
#endif
#if defined(ENABLE_OVERLOADING)
    cellRendererTextRiseSet                 ,
#endif
    constructCellRendererTextRiseSet        ,
    getCellRendererTextRiseSet              ,
    setCellRendererTextRiseSet              ,


-- ** scale #attr:scale#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    CellRendererTextScalePropertyInfo       ,
#endif
#if defined(ENABLE_OVERLOADING)
    cellRendererTextScale                   ,
#endif
    constructCellRendererTextScale          ,
    getCellRendererTextScale                ,
    setCellRendererTextScale                ,


-- ** scaleSet #attr:scaleSet#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    CellRendererTextScaleSetPropertyInfo    ,
#endif
#if defined(ENABLE_OVERLOADING)
    cellRendererTextScaleSet                ,
#endif
    constructCellRendererTextScaleSet       ,
    getCellRendererTextScaleSet             ,
    setCellRendererTextScaleSet             ,


-- ** singleParagraphMode #attr:singleParagraphMode#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    CellRendererTextSingleParagraphModePropertyInfo,
#endif
#if defined(ENABLE_OVERLOADING)
    cellRendererTextSingleParagraphMode     ,
#endif
    constructCellRendererTextSingleParagraphMode,
    getCellRendererTextSingleParagraphMode  ,
    setCellRendererTextSingleParagraphMode  ,


-- ** size #attr:size#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    CellRendererTextSizePropertyInfo        ,
#endif
#if defined(ENABLE_OVERLOADING)
    cellRendererTextSize                    ,
#endif
    constructCellRendererTextSize           ,
    getCellRendererTextSize                 ,
    setCellRendererTextSize                 ,


-- ** sizePoints #attr:sizePoints#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    CellRendererTextSizePointsPropertyInfo  ,
#endif
#if defined(ENABLE_OVERLOADING)
    cellRendererTextSizePoints              ,
#endif
    constructCellRendererTextSizePoints     ,
    getCellRendererTextSizePoints           ,
    setCellRendererTextSizePoints           ,


-- ** sizeSet #attr:sizeSet#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    CellRendererTextSizeSetPropertyInfo     ,
#endif
#if defined(ENABLE_OVERLOADING)
    cellRendererTextSizeSet                 ,
#endif
    constructCellRendererTextSizeSet        ,
    getCellRendererTextSizeSet              ,
    setCellRendererTextSizeSet              ,


-- ** stretch #attr:stretch#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    CellRendererTextStretchPropertyInfo     ,
#endif
#if defined(ENABLE_OVERLOADING)
    cellRendererTextStretch                 ,
#endif
    constructCellRendererTextStretch        ,
    getCellRendererTextStretch              ,
    setCellRendererTextStretch              ,


-- ** stretchSet #attr:stretchSet#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    CellRendererTextStretchSetPropertyInfo  ,
#endif
#if defined(ENABLE_OVERLOADING)
    cellRendererTextStretchSet              ,
#endif
    constructCellRendererTextStretchSet     ,
    getCellRendererTextStretchSet           ,
    setCellRendererTextStretchSet           ,


-- ** strikethrough #attr:strikethrough#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    CellRendererTextStrikethroughPropertyInfo,
#endif
#if defined(ENABLE_OVERLOADING)
    cellRendererTextStrikethrough           ,
#endif
    constructCellRendererTextStrikethrough  ,
    getCellRendererTextStrikethrough        ,
    setCellRendererTextStrikethrough        ,


-- ** strikethroughSet #attr:strikethroughSet#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    CellRendererTextStrikethroughSetPropertyInfo,
#endif
#if defined(ENABLE_OVERLOADING)
    cellRendererTextStrikethroughSet        ,
#endif
    constructCellRendererTextStrikethroughSet,
    getCellRendererTextStrikethroughSet     ,
    setCellRendererTextStrikethroughSet     ,


-- ** style #attr:style#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    CellRendererTextStylePropertyInfo       ,
#endif
#if defined(ENABLE_OVERLOADING)
    cellRendererTextStyle                   ,
#endif
    constructCellRendererTextStyle          ,
    getCellRendererTextStyle                ,
    setCellRendererTextStyle                ,


-- ** styleSet #attr:styleSet#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    CellRendererTextStyleSetPropertyInfo    ,
#endif
#if defined(ENABLE_OVERLOADING)
    cellRendererTextStyleSet                ,
#endif
    constructCellRendererTextStyleSet       ,
    getCellRendererTextStyleSet             ,
    setCellRendererTextStyleSet             ,


-- ** text #attr:text#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    CellRendererTextTextPropertyInfo        ,
#endif
#if defined(ENABLE_OVERLOADING)
    cellRendererTextText                    ,
#endif
    clearCellRendererTextText               ,
    constructCellRendererTextText           ,
    getCellRendererTextText                 ,
    setCellRendererTextText                 ,


-- ** underline #attr:underline#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    CellRendererTextUnderlinePropertyInfo   ,
#endif
#if defined(ENABLE_OVERLOADING)
    cellRendererTextUnderline               ,
#endif
    constructCellRendererTextUnderline      ,
    getCellRendererTextUnderline            ,
    setCellRendererTextUnderline            ,


-- ** underlineSet #attr:underlineSet#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    CellRendererTextUnderlineSetPropertyInfo,
#endif
#if defined(ENABLE_OVERLOADING)
    cellRendererTextUnderlineSet            ,
#endif
    constructCellRendererTextUnderlineSet   ,
    getCellRendererTextUnderlineSet         ,
    setCellRendererTextUnderlineSet         ,


-- ** variant #attr:variant#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    CellRendererTextVariantPropertyInfo     ,
#endif
#if defined(ENABLE_OVERLOADING)
    cellRendererTextVariant                 ,
#endif
    constructCellRendererTextVariant        ,
    getCellRendererTextVariant              ,
    setCellRendererTextVariant              ,


-- ** variantSet #attr:variantSet#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    CellRendererTextVariantSetPropertyInfo  ,
#endif
#if defined(ENABLE_OVERLOADING)
    cellRendererTextVariantSet              ,
#endif
    constructCellRendererTextVariantSet     ,
    getCellRendererTextVariantSet           ,
    setCellRendererTextVariantSet           ,


-- ** weight #attr:weight#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    CellRendererTextWeightPropertyInfo      ,
#endif
#if defined(ENABLE_OVERLOADING)
    cellRendererTextWeight                  ,
#endif
    constructCellRendererTextWeight         ,
    getCellRendererTextWeight               ,
    setCellRendererTextWeight               ,


-- ** weightSet #attr:weightSet#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    CellRendererTextWeightSetPropertyInfo   ,
#endif
#if defined(ENABLE_OVERLOADING)
    cellRendererTextWeightSet               ,
#endif
    constructCellRendererTextWeightSet      ,
    getCellRendererTextWeightSet            ,
    setCellRendererTextWeightSet            ,


-- ** widthChars #attr:widthChars#
-- | The desired width of the cell, in characters. If this property is set to
-- -1, the width will be calculated automatically, otherwise the cell will
-- request either 3 characters or the property value, whichever is greater.
-- 
-- /Since: 2.6/

#if defined(ENABLE_OVERLOADING)
    CellRendererTextWidthCharsPropertyInfo  ,
#endif
#if defined(ENABLE_OVERLOADING)
    cellRendererTextWidthChars              ,
#endif
    constructCellRendererTextWidthChars     ,
    getCellRendererTextWidthChars           ,
    setCellRendererTextWidthChars           ,


-- ** wrapMode #attr:wrapMode#
-- | Specifies how to break the string into multiple lines, if the cell
-- renderer does not have enough room to display the entire string.
-- This property has no effect unless the wrap-width property is set.
-- 
-- /Since: 2.8/

#if defined(ENABLE_OVERLOADING)
    CellRendererTextWrapModePropertyInfo    ,
#endif
#if defined(ENABLE_OVERLOADING)
    cellRendererTextWrapMode                ,
#endif
    constructCellRendererTextWrapMode       ,
    getCellRendererTextWrapMode             ,
    setCellRendererTextWrapMode             ,


-- ** wrapWidth #attr:wrapWidth#
-- | Specifies the minimum width at which the text is wrapped. The wrap-mode property can
-- be used to influence at what character positions the line breaks can be placed.
-- Setting wrap-width to -1 turns wrapping off.
-- 
-- /Since: 2.8/

#if defined(ENABLE_OVERLOADING)
    CellRendererTextWrapWidthPropertyInfo   ,
#endif
#if defined(ENABLE_OVERLOADING)
    cellRendererTextWrapWidth               ,
#endif
    constructCellRendererTextWrapWidth      ,
    getCellRendererTextWrapWidth            ,
    setCellRendererTextWrapWidth            ,




 -- * Signals


-- ** edited #signal:edited#

    CellRendererTextEditedCallback          ,
#if defined(ENABLE_OVERLOADING)
    CellRendererTextEditedSignalInfo        ,
#endif
    afterCellRendererTextEdited             ,
    onCellRendererTextEdited                ,




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
import {-# SOURCE #-} qualified GI.Gtk.Objects.CellRenderer as Gtk.CellRenderer
import qualified GI.Pango.Enums as Pango.Enums
import qualified GI.Pango.Structs.AttrList as Pango.AttrList
import qualified GI.Pango.Structs.FontDescription as Pango.FontDescription

-- | Memory-managed wrapper type.
newtype CellRendererText = CellRendererText (SP.ManagedPtr CellRendererText)
    deriving (Eq)

instance SP.ManagedPtrNewtype CellRendererText where
    toManagedPtr (CellRendererText p) = p

foreign import ccall "gtk_cell_renderer_text_get_type"
    c_gtk_cell_renderer_text_get_type :: IO B.Types.GType

instance B.Types.TypedObject CellRendererText where
    glibType = c_gtk_cell_renderer_text_get_type

instance B.Types.GObject CellRendererText

-- | Type class for types which can be safely cast to `CellRendererText`, for instance with `toCellRendererText`.
class (SP.GObject o, O.IsDescendantOf CellRendererText o) => IsCellRendererText o
instance (SP.GObject o, O.IsDescendantOf CellRendererText o) => IsCellRendererText o

instance O.HasParentTypes CellRendererText
type instance O.ParentTypes CellRendererText = '[Gtk.CellRenderer.CellRenderer, GObject.Object.Object]

-- | Cast to `CellRendererText`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toCellRendererText :: (MIO.MonadIO m, IsCellRendererText o) => o -> m CellRendererText
toCellRendererText = MIO.liftIO . B.ManagedPtr.unsafeCastTo CellRendererText

-- | Convert 'CellRendererText' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe CellRendererText) where
    gvalueGType_ = c_gtk_cell_renderer_text_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr CellRendererText)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr CellRendererText)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject CellRendererText ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveCellRendererTextMethod (t :: Symbol) (o :: *) :: * where
    ResolveCellRendererTextMethod "activate" o = Gtk.CellRenderer.CellRendererActivateMethodInfo
    ResolveCellRendererTextMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveCellRendererTextMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveCellRendererTextMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveCellRendererTextMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveCellRendererTextMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveCellRendererTextMethod "isActivatable" o = Gtk.CellRenderer.CellRendererIsActivatableMethodInfo
    ResolveCellRendererTextMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveCellRendererTextMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveCellRendererTextMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveCellRendererTextMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveCellRendererTextMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveCellRendererTextMethod "render" o = Gtk.CellRenderer.CellRendererRenderMethodInfo
    ResolveCellRendererTextMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveCellRendererTextMethod "startEditing" o = Gtk.CellRenderer.CellRendererStartEditingMethodInfo
    ResolveCellRendererTextMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveCellRendererTextMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveCellRendererTextMethod "stopEditing" o = Gtk.CellRenderer.CellRendererStopEditingMethodInfo
    ResolveCellRendererTextMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveCellRendererTextMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveCellRendererTextMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveCellRendererTextMethod "getAlignedArea" o = Gtk.CellRenderer.CellRendererGetAlignedAreaMethodInfo
    ResolveCellRendererTextMethod "getAlignment" o = Gtk.CellRenderer.CellRendererGetAlignmentMethodInfo
    ResolveCellRendererTextMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveCellRendererTextMethod "getFixedSize" o = Gtk.CellRenderer.CellRendererGetFixedSizeMethodInfo
    ResolveCellRendererTextMethod "getPadding" o = Gtk.CellRenderer.CellRendererGetPaddingMethodInfo
    ResolveCellRendererTextMethod "getPreferredHeight" o = Gtk.CellRenderer.CellRendererGetPreferredHeightMethodInfo
    ResolveCellRendererTextMethod "getPreferredHeightForWidth" o = Gtk.CellRenderer.CellRendererGetPreferredHeightForWidthMethodInfo
    ResolveCellRendererTextMethod "getPreferredSize" o = Gtk.CellRenderer.CellRendererGetPreferredSizeMethodInfo
    ResolveCellRendererTextMethod "getPreferredWidth" o = Gtk.CellRenderer.CellRendererGetPreferredWidthMethodInfo
    ResolveCellRendererTextMethod "getPreferredWidthForHeight" o = Gtk.CellRenderer.CellRendererGetPreferredWidthForHeightMethodInfo
    ResolveCellRendererTextMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveCellRendererTextMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveCellRendererTextMethod "getRequestMode" o = Gtk.CellRenderer.CellRendererGetRequestModeMethodInfo
    ResolveCellRendererTextMethod "getSensitive" o = Gtk.CellRenderer.CellRendererGetSensitiveMethodInfo
    ResolveCellRendererTextMethod "getSize" o = Gtk.CellRenderer.CellRendererGetSizeMethodInfo
    ResolveCellRendererTextMethod "getState" o = Gtk.CellRenderer.CellRendererGetStateMethodInfo
    ResolveCellRendererTextMethod "getVisible" o = Gtk.CellRenderer.CellRendererGetVisibleMethodInfo
    ResolveCellRendererTextMethod "setAlignment" o = Gtk.CellRenderer.CellRendererSetAlignmentMethodInfo
    ResolveCellRendererTextMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveCellRendererTextMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveCellRendererTextMethod "setFixedHeightFromFont" o = CellRendererTextSetFixedHeightFromFontMethodInfo
    ResolveCellRendererTextMethod "setFixedSize" o = Gtk.CellRenderer.CellRendererSetFixedSizeMethodInfo
    ResolveCellRendererTextMethod "setPadding" o = Gtk.CellRenderer.CellRendererSetPaddingMethodInfo
    ResolveCellRendererTextMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveCellRendererTextMethod "setSensitive" o = Gtk.CellRenderer.CellRendererSetSensitiveMethodInfo
    ResolveCellRendererTextMethod "setVisible" o = Gtk.CellRenderer.CellRendererSetVisibleMethodInfo
    ResolveCellRendererTextMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveCellRendererTextMethod t CellRendererText, O.OverloadedMethod info CellRendererText p) => OL.IsLabel t (CellRendererText -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveCellRendererTextMethod t CellRendererText, O.OverloadedMethod info CellRendererText p, R.HasField t CellRendererText p) => R.HasField t CellRendererText p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveCellRendererTextMethod t CellRendererText, O.OverloadedMethodInfo info CellRendererText) => OL.IsLabel t (O.MethodProxy info CellRendererText) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- signal CellRendererText::edited
-- | This signal is emitted after /@renderer@/ has been edited.
-- 
-- It is the responsibility of the application to update the model
-- and store /@newText@/ at the position indicated by /@path@/.
type CellRendererTextEditedCallback =
    T.Text
    -- ^ /@path@/: the path identifying the edited cell
    -> T.Text
    -- ^ /@newText@/: the new text
    -> IO ()

type C_CellRendererTextEditedCallback =
    Ptr CellRendererText ->                 -- object
    CString ->
    CString ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_CellRendererTextEditedCallback`.
foreign import ccall "wrapper"
    mk_CellRendererTextEditedCallback :: C_CellRendererTextEditedCallback -> IO (FunPtr C_CellRendererTextEditedCallback)

wrap_CellRendererTextEditedCallback :: 
    GObject a => (a -> CellRendererTextEditedCallback) ->
    C_CellRendererTextEditedCallback
wrap_CellRendererTextEditedCallback gi'cb gi'selfPtr path newText _ = do
    path' <- cstringToText path
    newText' <- cstringToText newText
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  path' newText'


-- | Connect a signal handler for the [edited](#signal:edited) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' cellRendererText #edited callback
-- @
-- 
-- 
onCellRendererTextEdited :: (IsCellRendererText a, MonadIO m) => a -> ((?self :: a) => CellRendererTextEditedCallback) -> m SignalHandlerId
onCellRendererTextEdited obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_CellRendererTextEditedCallback wrapped
    wrapped'' <- mk_CellRendererTextEditedCallback wrapped'
    connectSignalFunPtr obj "edited" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [edited](#signal:edited) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' cellRendererText #edited callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterCellRendererTextEdited :: (IsCellRendererText a, MonadIO m) => a -> ((?self :: a) => CellRendererTextEditedCallback) -> m SignalHandlerId
afterCellRendererTextEdited obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_CellRendererTextEditedCallback wrapped
    wrapped'' <- mk_CellRendererTextEditedCallback wrapped'
    connectSignalFunPtr obj "edited" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data CellRendererTextEditedSignalInfo
instance SignalInfo CellRendererTextEditedSignalInfo where
    type HaskellCallbackType CellRendererTextEditedSignalInfo = CellRendererTextEditedCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_CellRendererTextEditedCallback cb
        cb'' <- mk_CellRendererTextEditedCallback cb'
        connectSignalFunPtr obj "edited" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRendererText::edited"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRendererText.html#g:signal:edited"})

#endif

-- VVV Prop "align-set"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@align-set@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' cellRendererText #alignSet
-- @
getCellRendererTextAlignSet :: (MonadIO m, IsCellRendererText o) => o -> m Bool
getCellRendererTextAlignSet obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "align-set"

-- | Set the value of the “@align-set@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' cellRendererText [ #alignSet 'Data.GI.Base.Attributes.:=' value ]
-- @
setCellRendererTextAlignSet :: (MonadIO m, IsCellRendererText o) => o -> Bool -> m ()
setCellRendererTextAlignSet obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "align-set" val

-- | Construct a `GValueConstruct` with valid value for the “@align-set@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructCellRendererTextAlignSet :: (IsCellRendererText o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructCellRendererTextAlignSet val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "align-set" val

#if defined(ENABLE_OVERLOADING)
data CellRendererTextAlignSetPropertyInfo
instance AttrInfo CellRendererTextAlignSetPropertyInfo where
    type AttrAllowedOps CellRendererTextAlignSetPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint CellRendererTextAlignSetPropertyInfo = IsCellRendererText
    type AttrSetTypeConstraint CellRendererTextAlignSetPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint CellRendererTextAlignSetPropertyInfo = (~) Bool
    type AttrTransferType CellRendererTextAlignSetPropertyInfo = Bool
    type AttrGetType CellRendererTextAlignSetPropertyInfo = Bool
    type AttrLabel CellRendererTextAlignSetPropertyInfo = "align-set"
    type AttrOrigin CellRendererTextAlignSetPropertyInfo = CellRendererText
    attrGet = getCellRendererTextAlignSet
    attrSet = setCellRendererTextAlignSet
    attrTransfer _ v = do
        return v
    attrConstruct = constructCellRendererTextAlignSet
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRendererText.alignSet"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRendererText.html#g:attr:alignSet"
        })
#endif

-- VVV Prop "alignment"
   -- Type: TInterface (Name {namespace = "Pango", name = "Alignment"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@alignment@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' cellRendererText #alignment
-- @
getCellRendererTextAlignment :: (MonadIO m, IsCellRendererText o) => o -> m Pango.Enums.Alignment
getCellRendererTextAlignment obj = MIO.liftIO $ B.Properties.getObjectPropertyEnum obj "alignment"

-- | Set the value of the “@alignment@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' cellRendererText [ #alignment 'Data.GI.Base.Attributes.:=' value ]
-- @
setCellRendererTextAlignment :: (MonadIO m, IsCellRendererText o) => o -> Pango.Enums.Alignment -> m ()
setCellRendererTextAlignment obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyEnum obj "alignment" val

-- | Construct a `GValueConstruct` with valid value for the “@alignment@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructCellRendererTextAlignment :: (IsCellRendererText o, MIO.MonadIO m) => Pango.Enums.Alignment -> m (GValueConstruct o)
constructCellRendererTextAlignment val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyEnum "alignment" val

#if defined(ENABLE_OVERLOADING)
data CellRendererTextAlignmentPropertyInfo
instance AttrInfo CellRendererTextAlignmentPropertyInfo where
    type AttrAllowedOps CellRendererTextAlignmentPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint CellRendererTextAlignmentPropertyInfo = IsCellRendererText
    type AttrSetTypeConstraint CellRendererTextAlignmentPropertyInfo = (~) Pango.Enums.Alignment
    type AttrTransferTypeConstraint CellRendererTextAlignmentPropertyInfo = (~) Pango.Enums.Alignment
    type AttrTransferType CellRendererTextAlignmentPropertyInfo = Pango.Enums.Alignment
    type AttrGetType CellRendererTextAlignmentPropertyInfo = Pango.Enums.Alignment
    type AttrLabel CellRendererTextAlignmentPropertyInfo = "alignment"
    type AttrOrigin CellRendererTextAlignmentPropertyInfo = CellRendererText
    attrGet = getCellRendererTextAlignment
    attrSet = setCellRendererTextAlignment
    attrTransfer _ v = do
        return v
    attrConstruct = constructCellRendererTextAlignment
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRendererText.alignment"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRendererText.html#g:attr:alignment"
        })
#endif

-- VVV Prop "attributes"
   -- Type: TInterface (Name {namespace = "Pango", name = "AttrList"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@attributes@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' cellRendererText #attributes
-- @
getCellRendererTextAttributes :: (MonadIO m, IsCellRendererText o) => o -> m (Maybe Pango.AttrList.AttrList)
getCellRendererTextAttributes obj = MIO.liftIO $ B.Properties.getObjectPropertyBoxed obj "attributes" Pango.AttrList.AttrList

-- | Set the value of the “@attributes@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' cellRendererText [ #attributes 'Data.GI.Base.Attributes.:=' value ]
-- @
setCellRendererTextAttributes :: (MonadIO m, IsCellRendererText o) => o -> Pango.AttrList.AttrList -> m ()
setCellRendererTextAttributes obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBoxed obj "attributes" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@attributes@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructCellRendererTextAttributes :: (IsCellRendererText o, MIO.MonadIO m) => Pango.AttrList.AttrList -> m (GValueConstruct o)
constructCellRendererTextAttributes val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBoxed "attributes" (P.Just val)

-- | Set the value of the “@attributes@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #attributes
-- @
clearCellRendererTextAttributes :: (MonadIO m, IsCellRendererText o) => o -> m ()
clearCellRendererTextAttributes obj = liftIO $ B.Properties.setObjectPropertyBoxed obj "attributes" (Nothing :: Maybe Pango.AttrList.AttrList)

#if defined(ENABLE_OVERLOADING)
data CellRendererTextAttributesPropertyInfo
instance AttrInfo CellRendererTextAttributesPropertyInfo where
    type AttrAllowedOps CellRendererTextAttributesPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint CellRendererTextAttributesPropertyInfo = IsCellRendererText
    type AttrSetTypeConstraint CellRendererTextAttributesPropertyInfo = (~) Pango.AttrList.AttrList
    type AttrTransferTypeConstraint CellRendererTextAttributesPropertyInfo = (~) Pango.AttrList.AttrList
    type AttrTransferType CellRendererTextAttributesPropertyInfo = Pango.AttrList.AttrList
    type AttrGetType CellRendererTextAttributesPropertyInfo = (Maybe Pango.AttrList.AttrList)
    type AttrLabel CellRendererTextAttributesPropertyInfo = "attributes"
    type AttrOrigin CellRendererTextAttributesPropertyInfo = CellRendererText
    attrGet = getCellRendererTextAttributes
    attrSet = setCellRendererTextAttributes
    attrTransfer _ v = do
        return v
    attrConstruct = constructCellRendererTextAttributes
    attrClear = clearCellRendererTextAttributes
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRendererText.attributes"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRendererText.html#g:attr:attributes"
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
-- 'Data.GI.Base.Attributes.set' cellRendererText [ #background 'Data.GI.Base.Attributes.:=' value ]
-- @
setCellRendererTextBackground :: (MonadIO m, IsCellRendererText o) => o -> T.Text -> m ()
setCellRendererTextBackground obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "background" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@background@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructCellRendererTextBackground :: (IsCellRendererText o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructCellRendererTextBackground val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "background" (P.Just val)

-- | Set the value of the “@background@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #background
-- @
clearCellRendererTextBackground :: (MonadIO m, IsCellRendererText o) => o -> m ()
clearCellRendererTextBackground obj = liftIO $ B.Properties.setObjectPropertyString obj "background" (Nothing :: Maybe T.Text)

#if defined(ENABLE_OVERLOADING)
data CellRendererTextBackgroundPropertyInfo
instance AttrInfo CellRendererTextBackgroundPropertyInfo where
    type AttrAllowedOps CellRendererTextBackgroundPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrClear]
    type AttrBaseTypeConstraint CellRendererTextBackgroundPropertyInfo = IsCellRendererText
    type AttrSetTypeConstraint CellRendererTextBackgroundPropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint CellRendererTextBackgroundPropertyInfo = (~) T.Text
    type AttrTransferType CellRendererTextBackgroundPropertyInfo = T.Text
    type AttrGetType CellRendererTextBackgroundPropertyInfo = ()
    type AttrLabel CellRendererTextBackgroundPropertyInfo = "background"
    type AttrOrigin CellRendererTextBackgroundPropertyInfo = CellRendererText
    attrGet = undefined
    attrSet = setCellRendererTextBackground
    attrTransfer _ v = do
        return v
    attrConstruct = constructCellRendererTextBackground
    attrClear = clearCellRendererTextBackground
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRendererText.background"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRendererText.html#g:attr:background"
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
-- 'Data.GI.Base.Attributes.get' cellRendererText #backgroundGdk
-- @
getCellRendererTextBackgroundGdk :: (MonadIO m, IsCellRendererText o) => o -> m (Maybe Gdk.Color.Color)
getCellRendererTextBackgroundGdk obj = MIO.liftIO $ B.Properties.getObjectPropertyBoxed obj "background-gdk" Gdk.Color.Color

-- | Set the value of the “@background-gdk@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' cellRendererText [ #backgroundGdk 'Data.GI.Base.Attributes.:=' value ]
-- @
setCellRendererTextBackgroundGdk :: (MonadIO m, IsCellRendererText o) => o -> Gdk.Color.Color -> m ()
setCellRendererTextBackgroundGdk obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBoxed obj "background-gdk" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@background-gdk@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructCellRendererTextBackgroundGdk :: (IsCellRendererText o, MIO.MonadIO m) => Gdk.Color.Color -> m (GValueConstruct o)
constructCellRendererTextBackgroundGdk val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBoxed "background-gdk" (P.Just val)

-- | Set the value of the “@background-gdk@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #backgroundGdk
-- @
clearCellRendererTextBackgroundGdk :: (MonadIO m, IsCellRendererText o) => o -> m ()
clearCellRendererTextBackgroundGdk obj = liftIO $ B.Properties.setObjectPropertyBoxed obj "background-gdk" (Nothing :: Maybe Gdk.Color.Color)

#if defined(ENABLE_OVERLOADING)
data CellRendererTextBackgroundGdkPropertyInfo
instance AttrInfo CellRendererTextBackgroundGdkPropertyInfo where
    type AttrAllowedOps CellRendererTextBackgroundGdkPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint CellRendererTextBackgroundGdkPropertyInfo = IsCellRendererText
    type AttrSetTypeConstraint CellRendererTextBackgroundGdkPropertyInfo = (~) Gdk.Color.Color
    type AttrTransferTypeConstraint CellRendererTextBackgroundGdkPropertyInfo = (~) Gdk.Color.Color
    type AttrTransferType CellRendererTextBackgroundGdkPropertyInfo = Gdk.Color.Color
    type AttrGetType CellRendererTextBackgroundGdkPropertyInfo = (Maybe Gdk.Color.Color)
    type AttrLabel CellRendererTextBackgroundGdkPropertyInfo = "background-gdk"
    type AttrOrigin CellRendererTextBackgroundGdkPropertyInfo = CellRendererText
    attrGet = getCellRendererTextBackgroundGdk
    attrSet = setCellRendererTextBackgroundGdk
    attrTransfer _ v = do
        return v
    attrConstruct = constructCellRendererTextBackgroundGdk
    attrClear = clearCellRendererTextBackgroundGdk
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRendererText.backgroundGdk"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRendererText.html#g:attr:backgroundGdk"
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
-- 'Data.GI.Base.Attributes.get' cellRendererText #backgroundRgba
-- @
getCellRendererTextBackgroundRgba :: (MonadIO m, IsCellRendererText o) => o -> m (Maybe Gdk.RGBA.RGBA)
getCellRendererTextBackgroundRgba obj = MIO.liftIO $ B.Properties.getObjectPropertyBoxed obj "background-rgba" Gdk.RGBA.RGBA

-- | Set the value of the “@background-rgba@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' cellRendererText [ #backgroundRgba 'Data.GI.Base.Attributes.:=' value ]
-- @
setCellRendererTextBackgroundRgba :: (MonadIO m, IsCellRendererText o) => o -> Gdk.RGBA.RGBA -> m ()
setCellRendererTextBackgroundRgba obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBoxed obj "background-rgba" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@background-rgba@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructCellRendererTextBackgroundRgba :: (IsCellRendererText o, MIO.MonadIO m) => Gdk.RGBA.RGBA -> m (GValueConstruct o)
constructCellRendererTextBackgroundRgba val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBoxed "background-rgba" (P.Just val)

-- | Set the value of the “@background-rgba@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #backgroundRgba
-- @
clearCellRendererTextBackgroundRgba :: (MonadIO m, IsCellRendererText o) => o -> m ()
clearCellRendererTextBackgroundRgba obj = liftIO $ B.Properties.setObjectPropertyBoxed obj "background-rgba" (Nothing :: Maybe Gdk.RGBA.RGBA)

#if defined(ENABLE_OVERLOADING)
data CellRendererTextBackgroundRgbaPropertyInfo
instance AttrInfo CellRendererTextBackgroundRgbaPropertyInfo where
    type AttrAllowedOps CellRendererTextBackgroundRgbaPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint CellRendererTextBackgroundRgbaPropertyInfo = IsCellRendererText
    type AttrSetTypeConstraint CellRendererTextBackgroundRgbaPropertyInfo = (~) Gdk.RGBA.RGBA
    type AttrTransferTypeConstraint CellRendererTextBackgroundRgbaPropertyInfo = (~) Gdk.RGBA.RGBA
    type AttrTransferType CellRendererTextBackgroundRgbaPropertyInfo = Gdk.RGBA.RGBA
    type AttrGetType CellRendererTextBackgroundRgbaPropertyInfo = (Maybe Gdk.RGBA.RGBA)
    type AttrLabel CellRendererTextBackgroundRgbaPropertyInfo = "background-rgba"
    type AttrOrigin CellRendererTextBackgroundRgbaPropertyInfo = CellRendererText
    attrGet = getCellRendererTextBackgroundRgba
    attrSet = setCellRendererTextBackgroundRgba
    attrTransfer _ v = do
        return v
    attrConstruct = constructCellRendererTextBackgroundRgba
    attrClear = clearCellRendererTextBackgroundRgba
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRendererText.backgroundRgba"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRendererText.html#g:attr:backgroundRgba"
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
-- 'Data.GI.Base.Attributes.get' cellRendererText #backgroundSet
-- @
getCellRendererTextBackgroundSet :: (MonadIO m, IsCellRendererText o) => o -> m Bool
getCellRendererTextBackgroundSet obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "background-set"

-- | Set the value of the “@background-set@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' cellRendererText [ #backgroundSet 'Data.GI.Base.Attributes.:=' value ]
-- @
setCellRendererTextBackgroundSet :: (MonadIO m, IsCellRendererText o) => o -> Bool -> m ()
setCellRendererTextBackgroundSet obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "background-set" val

-- | Construct a `GValueConstruct` with valid value for the “@background-set@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructCellRendererTextBackgroundSet :: (IsCellRendererText o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructCellRendererTextBackgroundSet val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "background-set" val

#if defined(ENABLE_OVERLOADING)
data CellRendererTextBackgroundSetPropertyInfo
instance AttrInfo CellRendererTextBackgroundSetPropertyInfo where
    type AttrAllowedOps CellRendererTextBackgroundSetPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint CellRendererTextBackgroundSetPropertyInfo = IsCellRendererText
    type AttrSetTypeConstraint CellRendererTextBackgroundSetPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint CellRendererTextBackgroundSetPropertyInfo = (~) Bool
    type AttrTransferType CellRendererTextBackgroundSetPropertyInfo = Bool
    type AttrGetType CellRendererTextBackgroundSetPropertyInfo = Bool
    type AttrLabel CellRendererTextBackgroundSetPropertyInfo = "background-set"
    type AttrOrigin CellRendererTextBackgroundSetPropertyInfo = CellRendererText
    attrGet = getCellRendererTextBackgroundSet
    attrSet = setCellRendererTextBackgroundSet
    attrTransfer _ v = do
        return v
    attrConstruct = constructCellRendererTextBackgroundSet
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRendererText.backgroundSet"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRendererText.html#g:attr:backgroundSet"
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
-- 'Data.GI.Base.Attributes.get' cellRendererText #editable
-- @
getCellRendererTextEditable :: (MonadIO m, IsCellRendererText o) => o -> m Bool
getCellRendererTextEditable obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "editable"

-- | Set the value of the “@editable@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' cellRendererText [ #editable 'Data.GI.Base.Attributes.:=' value ]
-- @
setCellRendererTextEditable :: (MonadIO m, IsCellRendererText o) => o -> Bool -> m ()
setCellRendererTextEditable obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "editable" val

-- | Construct a `GValueConstruct` with valid value for the “@editable@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructCellRendererTextEditable :: (IsCellRendererText o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructCellRendererTextEditable val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "editable" val

#if defined(ENABLE_OVERLOADING)
data CellRendererTextEditablePropertyInfo
instance AttrInfo CellRendererTextEditablePropertyInfo where
    type AttrAllowedOps CellRendererTextEditablePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint CellRendererTextEditablePropertyInfo = IsCellRendererText
    type AttrSetTypeConstraint CellRendererTextEditablePropertyInfo = (~) Bool
    type AttrTransferTypeConstraint CellRendererTextEditablePropertyInfo = (~) Bool
    type AttrTransferType CellRendererTextEditablePropertyInfo = Bool
    type AttrGetType CellRendererTextEditablePropertyInfo = Bool
    type AttrLabel CellRendererTextEditablePropertyInfo = "editable"
    type AttrOrigin CellRendererTextEditablePropertyInfo = CellRendererText
    attrGet = getCellRendererTextEditable
    attrSet = setCellRendererTextEditable
    attrTransfer _ v = do
        return v
    attrConstruct = constructCellRendererTextEditable
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRendererText.editable"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRendererText.html#g:attr:editable"
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
-- 'Data.GI.Base.Attributes.get' cellRendererText #editableSet
-- @
getCellRendererTextEditableSet :: (MonadIO m, IsCellRendererText o) => o -> m Bool
getCellRendererTextEditableSet obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "editable-set"

-- | Set the value of the “@editable-set@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' cellRendererText [ #editableSet 'Data.GI.Base.Attributes.:=' value ]
-- @
setCellRendererTextEditableSet :: (MonadIO m, IsCellRendererText o) => o -> Bool -> m ()
setCellRendererTextEditableSet obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "editable-set" val

-- | Construct a `GValueConstruct` with valid value for the “@editable-set@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructCellRendererTextEditableSet :: (IsCellRendererText o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructCellRendererTextEditableSet val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "editable-set" val

#if defined(ENABLE_OVERLOADING)
data CellRendererTextEditableSetPropertyInfo
instance AttrInfo CellRendererTextEditableSetPropertyInfo where
    type AttrAllowedOps CellRendererTextEditableSetPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint CellRendererTextEditableSetPropertyInfo = IsCellRendererText
    type AttrSetTypeConstraint CellRendererTextEditableSetPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint CellRendererTextEditableSetPropertyInfo = (~) Bool
    type AttrTransferType CellRendererTextEditableSetPropertyInfo = Bool
    type AttrGetType CellRendererTextEditableSetPropertyInfo = Bool
    type AttrLabel CellRendererTextEditableSetPropertyInfo = "editable-set"
    type AttrOrigin CellRendererTextEditableSetPropertyInfo = CellRendererText
    attrGet = getCellRendererTextEditableSet
    attrSet = setCellRendererTextEditableSet
    attrTransfer _ v = do
        return v
    attrConstruct = constructCellRendererTextEditableSet
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRendererText.editableSet"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRendererText.html#g:attr:editableSet"
        })
#endif

-- VVV Prop "ellipsize"
   -- Type: TInterface (Name {namespace = "Pango", name = "EllipsizeMode"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@ellipsize@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' cellRendererText #ellipsize
-- @
getCellRendererTextEllipsize :: (MonadIO m, IsCellRendererText o) => o -> m Pango.Enums.EllipsizeMode
getCellRendererTextEllipsize obj = MIO.liftIO $ B.Properties.getObjectPropertyEnum obj "ellipsize"

-- | Set the value of the “@ellipsize@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' cellRendererText [ #ellipsize 'Data.GI.Base.Attributes.:=' value ]
-- @
setCellRendererTextEllipsize :: (MonadIO m, IsCellRendererText o) => o -> Pango.Enums.EllipsizeMode -> m ()
setCellRendererTextEllipsize obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyEnum obj "ellipsize" val

-- | Construct a `GValueConstruct` with valid value for the “@ellipsize@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructCellRendererTextEllipsize :: (IsCellRendererText o, MIO.MonadIO m) => Pango.Enums.EllipsizeMode -> m (GValueConstruct o)
constructCellRendererTextEllipsize val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyEnum "ellipsize" val

#if defined(ENABLE_OVERLOADING)
data CellRendererTextEllipsizePropertyInfo
instance AttrInfo CellRendererTextEllipsizePropertyInfo where
    type AttrAllowedOps CellRendererTextEllipsizePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint CellRendererTextEllipsizePropertyInfo = IsCellRendererText
    type AttrSetTypeConstraint CellRendererTextEllipsizePropertyInfo = (~) Pango.Enums.EllipsizeMode
    type AttrTransferTypeConstraint CellRendererTextEllipsizePropertyInfo = (~) Pango.Enums.EllipsizeMode
    type AttrTransferType CellRendererTextEllipsizePropertyInfo = Pango.Enums.EllipsizeMode
    type AttrGetType CellRendererTextEllipsizePropertyInfo = Pango.Enums.EllipsizeMode
    type AttrLabel CellRendererTextEllipsizePropertyInfo = "ellipsize"
    type AttrOrigin CellRendererTextEllipsizePropertyInfo = CellRendererText
    attrGet = getCellRendererTextEllipsize
    attrSet = setCellRendererTextEllipsize
    attrTransfer _ v = do
        return v
    attrConstruct = constructCellRendererTextEllipsize
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRendererText.ellipsize"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRendererText.html#g:attr:ellipsize"
        })
#endif

-- VVV Prop "ellipsize-set"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@ellipsize-set@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' cellRendererText #ellipsizeSet
-- @
getCellRendererTextEllipsizeSet :: (MonadIO m, IsCellRendererText o) => o -> m Bool
getCellRendererTextEllipsizeSet obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "ellipsize-set"

-- | Set the value of the “@ellipsize-set@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' cellRendererText [ #ellipsizeSet 'Data.GI.Base.Attributes.:=' value ]
-- @
setCellRendererTextEllipsizeSet :: (MonadIO m, IsCellRendererText o) => o -> Bool -> m ()
setCellRendererTextEllipsizeSet obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "ellipsize-set" val

-- | Construct a `GValueConstruct` with valid value for the “@ellipsize-set@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructCellRendererTextEllipsizeSet :: (IsCellRendererText o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructCellRendererTextEllipsizeSet val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "ellipsize-set" val

#if defined(ENABLE_OVERLOADING)
data CellRendererTextEllipsizeSetPropertyInfo
instance AttrInfo CellRendererTextEllipsizeSetPropertyInfo where
    type AttrAllowedOps CellRendererTextEllipsizeSetPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint CellRendererTextEllipsizeSetPropertyInfo = IsCellRendererText
    type AttrSetTypeConstraint CellRendererTextEllipsizeSetPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint CellRendererTextEllipsizeSetPropertyInfo = (~) Bool
    type AttrTransferType CellRendererTextEllipsizeSetPropertyInfo = Bool
    type AttrGetType CellRendererTextEllipsizeSetPropertyInfo = Bool
    type AttrLabel CellRendererTextEllipsizeSetPropertyInfo = "ellipsize-set"
    type AttrOrigin CellRendererTextEllipsizeSetPropertyInfo = CellRendererText
    attrGet = getCellRendererTextEllipsizeSet
    attrSet = setCellRendererTextEllipsizeSet
    attrTransfer _ v = do
        return v
    attrConstruct = constructCellRendererTextEllipsizeSet
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRendererText.ellipsizeSet"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRendererText.html#g:attr:ellipsizeSet"
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
-- 'Data.GI.Base.Attributes.get' cellRendererText #family
-- @
getCellRendererTextFamily :: (MonadIO m, IsCellRendererText o) => o -> m (Maybe T.Text)
getCellRendererTextFamily obj = MIO.liftIO $ B.Properties.getObjectPropertyString obj "family"

-- | Set the value of the “@family@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' cellRendererText [ #family 'Data.GI.Base.Attributes.:=' value ]
-- @
setCellRendererTextFamily :: (MonadIO m, IsCellRendererText o) => o -> T.Text -> m ()
setCellRendererTextFamily obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "family" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@family@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructCellRendererTextFamily :: (IsCellRendererText o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructCellRendererTextFamily val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "family" (P.Just val)

-- | Set the value of the “@family@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #family
-- @
clearCellRendererTextFamily :: (MonadIO m, IsCellRendererText o) => o -> m ()
clearCellRendererTextFamily obj = liftIO $ B.Properties.setObjectPropertyString obj "family" (Nothing :: Maybe T.Text)

#if defined(ENABLE_OVERLOADING)
data CellRendererTextFamilyPropertyInfo
instance AttrInfo CellRendererTextFamilyPropertyInfo where
    type AttrAllowedOps CellRendererTextFamilyPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint CellRendererTextFamilyPropertyInfo = IsCellRendererText
    type AttrSetTypeConstraint CellRendererTextFamilyPropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint CellRendererTextFamilyPropertyInfo = (~) T.Text
    type AttrTransferType CellRendererTextFamilyPropertyInfo = T.Text
    type AttrGetType CellRendererTextFamilyPropertyInfo = (Maybe T.Text)
    type AttrLabel CellRendererTextFamilyPropertyInfo = "family"
    type AttrOrigin CellRendererTextFamilyPropertyInfo = CellRendererText
    attrGet = getCellRendererTextFamily
    attrSet = setCellRendererTextFamily
    attrTransfer _ v = do
        return v
    attrConstruct = constructCellRendererTextFamily
    attrClear = clearCellRendererTextFamily
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRendererText.family"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRendererText.html#g:attr:family"
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
-- 'Data.GI.Base.Attributes.get' cellRendererText #familySet
-- @
getCellRendererTextFamilySet :: (MonadIO m, IsCellRendererText o) => o -> m Bool
getCellRendererTextFamilySet obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "family-set"

-- | Set the value of the “@family-set@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' cellRendererText [ #familySet 'Data.GI.Base.Attributes.:=' value ]
-- @
setCellRendererTextFamilySet :: (MonadIO m, IsCellRendererText o) => o -> Bool -> m ()
setCellRendererTextFamilySet obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "family-set" val

-- | Construct a `GValueConstruct` with valid value for the “@family-set@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructCellRendererTextFamilySet :: (IsCellRendererText o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructCellRendererTextFamilySet val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "family-set" val

#if defined(ENABLE_OVERLOADING)
data CellRendererTextFamilySetPropertyInfo
instance AttrInfo CellRendererTextFamilySetPropertyInfo where
    type AttrAllowedOps CellRendererTextFamilySetPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint CellRendererTextFamilySetPropertyInfo = IsCellRendererText
    type AttrSetTypeConstraint CellRendererTextFamilySetPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint CellRendererTextFamilySetPropertyInfo = (~) Bool
    type AttrTransferType CellRendererTextFamilySetPropertyInfo = Bool
    type AttrGetType CellRendererTextFamilySetPropertyInfo = Bool
    type AttrLabel CellRendererTextFamilySetPropertyInfo = "family-set"
    type AttrOrigin CellRendererTextFamilySetPropertyInfo = CellRendererText
    attrGet = getCellRendererTextFamilySet
    attrSet = setCellRendererTextFamilySet
    attrTransfer _ v = do
        return v
    attrConstruct = constructCellRendererTextFamilySet
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRendererText.familySet"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRendererText.html#g:attr:familySet"
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
-- 'Data.GI.Base.Attributes.get' cellRendererText #font
-- @
getCellRendererTextFont :: (MonadIO m, IsCellRendererText o) => o -> m (Maybe T.Text)
getCellRendererTextFont obj = MIO.liftIO $ B.Properties.getObjectPropertyString obj "font"

-- | Set the value of the “@font@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' cellRendererText [ #font 'Data.GI.Base.Attributes.:=' value ]
-- @
setCellRendererTextFont :: (MonadIO m, IsCellRendererText o) => o -> T.Text -> m ()
setCellRendererTextFont obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "font" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@font@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructCellRendererTextFont :: (IsCellRendererText o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructCellRendererTextFont val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "font" (P.Just val)

-- | Set the value of the “@font@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #font
-- @
clearCellRendererTextFont :: (MonadIO m, IsCellRendererText o) => o -> m ()
clearCellRendererTextFont obj = liftIO $ B.Properties.setObjectPropertyString obj "font" (Nothing :: Maybe T.Text)

#if defined(ENABLE_OVERLOADING)
data CellRendererTextFontPropertyInfo
instance AttrInfo CellRendererTextFontPropertyInfo where
    type AttrAllowedOps CellRendererTextFontPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint CellRendererTextFontPropertyInfo = IsCellRendererText
    type AttrSetTypeConstraint CellRendererTextFontPropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint CellRendererTextFontPropertyInfo = (~) T.Text
    type AttrTransferType CellRendererTextFontPropertyInfo = T.Text
    type AttrGetType CellRendererTextFontPropertyInfo = (Maybe T.Text)
    type AttrLabel CellRendererTextFontPropertyInfo = "font"
    type AttrOrigin CellRendererTextFontPropertyInfo = CellRendererText
    attrGet = getCellRendererTextFont
    attrSet = setCellRendererTextFont
    attrTransfer _ v = do
        return v
    attrConstruct = constructCellRendererTextFont
    attrClear = clearCellRendererTextFont
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRendererText.font"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRendererText.html#g:attr:font"
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
-- 'Data.GI.Base.Attributes.get' cellRendererText #fontDesc
-- @
getCellRendererTextFontDesc :: (MonadIO m, IsCellRendererText o) => o -> m (Maybe Pango.FontDescription.FontDescription)
getCellRendererTextFontDesc obj = MIO.liftIO $ B.Properties.getObjectPropertyBoxed obj "font-desc" Pango.FontDescription.FontDescription

-- | Set the value of the “@font-desc@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' cellRendererText [ #fontDesc 'Data.GI.Base.Attributes.:=' value ]
-- @
setCellRendererTextFontDesc :: (MonadIO m, IsCellRendererText o) => o -> Pango.FontDescription.FontDescription -> m ()
setCellRendererTextFontDesc obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBoxed obj "font-desc" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@font-desc@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructCellRendererTextFontDesc :: (IsCellRendererText o, MIO.MonadIO m) => Pango.FontDescription.FontDescription -> m (GValueConstruct o)
constructCellRendererTextFontDesc val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBoxed "font-desc" (P.Just val)

-- | Set the value of the “@font-desc@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #fontDesc
-- @
clearCellRendererTextFontDesc :: (MonadIO m, IsCellRendererText o) => o -> m ()
clearCellRendererTextFontDesc obj = liftIO $ B.Properties.setObjectPropertyBoxed obj "font-desc" (Nothing :: Maybe Pango.FontDescription.FontDescription)

#if defined(ENABLE_OVERLOADING)
data CellRendererTextFontDescPropertyInfo
instance AttrInfo CellRendererTextFontDescPropertyInfo where
    type AttrAllowedOps CellRendererTextFontDescPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint CellRendererTextFontDescPropertyInfo = IsCellRendererText
    type AttrSetTypeConstraint CellRendererTextFontDescPropertyInfo = (~) Pango.FontDescription.FontDescription
    type AttrTransferTypeConstraint CellRendererTextFontDescPropertyInfo = (~) Pango.FontDescription.FontDescription
    type AttrTransferType CellRendererTextFontDescPropertyInfo = Pango.FontDescription.FontDescription
    type AttrGetType CellRendererTextFontDescPropertyInfo = (Maybe Pango.FontDescription.FontDescription)
    type AttrLabel CellRendererTextFontDescPropertyInfo = "font-desc"
    type AttrOrigin CellRendererTextFontDescPropertyInfo = CellRendererText
    attrGet = getCellRendererTextFontDesc
    attrSet = setCellRendererTextFontDesc
    attrTransfer _ v = do
        return v
    attrConstruct = constructCellRendererTextFontDesc
    attrClear = clearCellRendererTextFontDesc
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRendererText.fontDesc"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRendererText.html#g:attr:fontDesc"
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
-- 'Data.GI.Base.Attributes.set' cellRendererText [ #foreground 'Data.GI.Base.Attributes.:=' value ]
-- @
setCellRendererTextForeground :: (MonadIO m, IsCellRendererText o) => o -> T.Text -> m ()
setCellRendererTextForeground obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "foreground" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@foreground@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructCellRendererTextForeground :: (IsCellRendererText o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructCellRendererTextForeground val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "foreground" (P.Just val)

-- | Set the value of the “@foreground@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #foreground
-- @
clearCellRendererTextForeground :: (MonadIO m, IsCellRendererText o) => o -> m ()
clearCellRendererTextForeground obj = liftIO $ B.Properties.setObjectPropertyString obj "foreground" (Nothing :: Maybe T.Text)

#if defined(ENABLE_OVERLOADING)
data CellRendererTextForegroundPropertyInfo
instance AttrInfo CellRendererTextForegroundPropertyInfo where
    type AttrAllowedOps CellRendererTextForegroundPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrClear]
    type AttrBaseTypeConstraint CellRendererTextForegroundPropertyInfo = IsCellRendererText
    type AttrSetTypeConstraint CellRendererTextForegroundPropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint CellRendererTextForegroundPropertyInfo = (~) T.Text
    type AttrTransferType CellRendererTextForegroundPropertyInfo = T.Text
    type AttrGetType CellRendererTextForegroundPropertyInfo = ()
    type AttrLabel CellRendererTextForegroundPropertyInfo = "foreground"
    type AttrOrigin CellRendererTextForegroundPropertyInfo = CellRendererText
    attrGet = undefined
    attrSet = setCellRendererTextForeground
    attrTransfer _ v = do
        return v
    attrConstruct = constructCellRendererTextForeground
    attrClear = clearCellRendererTextForeground
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRendererText.foreground"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRendererText.html#g:attr:foreground"
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
-- 'Data.GI.Base.Attributes.get' cellRendererText #foregroundGdk
-- @
getCellRendererTextForegroundGdk :: (MonadIO m, IsCellRendererText o) => o -> m (Maybe Gdk.Color.Color)
getCellRendererTextForegroundGdk obj = MIO.liftIO $ B.Properties.getObjectPropertyBoxed obj "foreground-gdk" Gdk.Color.Color

-- | Set the value of the “@foreground-gdk@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' cellRendererText [ #foregroundGdk 'Data.GI.Base.Attributes.:=' value ]
-- @
setCellRendererTextForegroundGdk :: (MonadIO m, IsCellRendererText o) => o -> Gdk.Color.Color -> m ()
setCellRendererTextForegroundGdk obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBoxed obj "foreground-gdk" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@foreground-gdk@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructCellRendererTextForegroundGdk :: (IsCellRendererText o, MIO.MonadIO m) => Gdk.Color.Color -> m (GValueConstruct o)
constructCellRendererTextForegroundGdk val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBoxed "foreground-gdk" (P.Just val)

-- | Set the value of the “@foreground-gdk@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #foregroundGdk
-- @
clearCellRendererTextForegroundGdk :: (MonadIO m, IsCellRendererText o) => o -> m ()
clearCellRendererTextForegroundGdk obj = liftIO $ B.Properties.setObjectPropertyBoxed obj "foreground-gdk" (Nothing :: Maybe Gdk.Color.Color)

#if defined(ENABLE_OVERLOADING)
data CellRendererTextForegroundGdkPropertyInfo
instance AttrInfo CellRendererTextForegroundGdkPropertyInfo where
    type AttrAllowedOps CellRendererTextForegroundGdkPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint CellRendererTextForegroundGdkPropertyInfo = IsCellRendererText
    type AttrSetTypeConstraint CellRendererTextForegroundGdkPropertyInfo = (~) Gdk.Color.Color
    type AttrTransferTypeConstraint CellRendererTextForegroundGdkPropertyInfo = (~) Gdk.Color.Color
    type AttrTransferType CellRendererTextForegroundGdkPropertyInfo = Gdk.Color.Color
    type AttrGetType CellRendererTextForegroundGdkPropertyInfo = (Maybe Gdk.Color.Color)
    type AttrLabel CellRendererTextForegroundGdkPropertyInfo = "foreground-gdk"
    type AttrOrigin CellRendererTextForegroundGdkPropertyInfo = CellRendererText
    attrGet = getCellRendererTextForegroundGdk
    attrSet = setCellRendererTextForegroundGdk
    attrTransfer _ v = do
        return v
    attrConstruct = constructCellRendererTextForegroundGdk
    attrClear = clearCellRendererTextForegroundGdk
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRendererText.foregroundGdk"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRendererText.html#g:attr:foregroundGdk"
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
-- 'Data.GI.Base.Attributes.get' cellRendererText #foregroundRgba
-- @
getCellRendererTextForegroundRgba :: (MonadIO m, IsCellRendererText o) => o -> m (Maybe Gdk.RGBA.RGBA)
getCellRendererTextForegroundRgba obj = MIO.liftIO $ B.Properties.getObjectPropertyBoxed obj "foreground-rgba" Gdk.RGBA.RGBA

-- | Set the value of the “@foreground-rgba@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' cellRendererText [ #foregroundRgba 'Data.GI.Base.Attributes.:=' value ]
-- @
setCellRendererTextForegroundRgba :: (MonadIO m, IsCellRendererText o) => o -> Gdk.RGBA.RGBA -> m ()
setCellRendererTextForegroundRgba obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBoxed obj "foreground-rgba" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@foreground-rgba@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructCellRendererTextForegroundRgba :: (IsCellRendererText o, MIO.MonadIO m) => Gdk.RGBA.RGBA -> m (GValueConstruct o)
constructCellRendererTextForegroundRgba val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBoxed "foreground-rgba" (P.Just val)

-- | Set the value of the “@foreground-rgba@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #foregroundRgba
-- @
clearCellRendererTextForegroundRgba :: (MonadIO m, IsCellRendererText o) => o -> m ()
clearCellRendererTextForegroundRgba obj = liftIO $ B.Properties.setObjectPropertyBoxed obj "foreground-rgba" (Nothing :: Maybe Gdk.RGBA.RGBA)

#if defined(ENABLE_OVERLOADING)
data CellRendererTextForegroundRgbaPropertyInfo
instance AttrInfo CellRendererTextForegroundRgbaPropertyInfo where
    type AttrAllowedOps CellRendererTextForegroundRgbaPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint CellRendererTextForegroundRgbaPropertyInfo = IsCellRendererText
    type AttrSetTypeConstraint CellRendererTextForegroundRgbaPropertyInfo = (~) Gdk.RGBA.RGBA
    type AttrTransferTypeConstraint CellRendererTextForegroundRgbaPropertyInfo = (~) Gdk.RGBA.RGBA
    type AttrTransferType CellRendererTextForegroundRgbaPropertyInfo = Gdk.RGBA.RGBA
    type AttrGetType CellRendererTextForegroundRgbaPropertyInfo = (Maybe Gdk.RGBA.RGBA)
    type AttrLabel CellRendererTextForegroundRgbaPropertyInfo = "foreground-rgba"
    type AttrOrigin CellRendererTextForegroundRgbaPropertyInfo = CellRendererText
    attrGet = getCellRendererTextForegroundRgba
    attrSet = setCellRendererTextForegroundRgba
    attrTransfer _ v = do
        return v
    attrConstruct = constructCellRendererTextForegroundRgba
    attrClear = clearCellRendererTextForegroundRgba
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRendererText.foregroundRgba"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRendererText.html#g:attr:foregroundRgba"
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
-- 'Data.GI.Base.Attributes.get' cellRendererText #foregroundSet
-- @
getCellRendererTextForegroundSet :: (MonadIO m, IsCellRendererText o) => o -> m Bool
getCellRendererTextForegroundSet obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "foreground-set"

-- | Set the value of the “@foreground-set@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' cellRendererText [ #foregroundSet 'Data.GI.Base.Attributes.:=' value ]
-- @
setCellRendererTextForegroundSet :: (MonadIO m, IsCellRendererText o) => o -> Bool -> m ()
setCellRendererTextForegroundSet obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "foreground-set" val

-- | Construct a `GValueConstruct` with valid value for the “@foreground-set@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructCellRendererTextForegroundSet :: (IsCellRendererText o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructCellRendererTextForegroundSet val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "foreground-set" val

#if defined(ENABLE_OVERLOADING)
data CellRendererTextForegroundSetPropertyInfo
instance AttrInfo CellRendererTextForegroundSetPropertyInfo where
    type AttrAllowedOps CellRendererTextForegroundSetPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint CellRendererTextForegroundSetPropertyInfo = IsCellRendererText
    type AttrSetTypeConstraint CellRendererTextForegroundSetPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint CellRendererTextForegroundSetPropertyInfo = (~) Bool
    type AttrTransferType CellRendererTextForegroundSetPropertyInfo = Bool
    type AttrGetType CellRendererTextForegroundSetPropertyInfo = Bool
    type AttrLabel CellRendererTextForegroundSetPropertyInfo = "foreground-set"
    type AttrOrigin CellRendererTextForegroundSetPropertyInfo = CellRendererText
    attrGet = getCellRendererTextForegroundSet
    attrSet = setCellRendererTextForegroundSet
    attrTransfer _ v = do
        return v
    attrConstruct = constructCellRendererTextForegroundSet
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRendererText.foregroundSet"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRendererText.html#g:attr:foregroundSet"
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
-- 'Data.GI.Base.Attributes.get' cellRendererText #language
-- @
getCellRendererTextLanguage :: (MonadIO m, IsCellRendererText o) => o -> m (Maybe T.Text)
getCellRendererTextLanguage obj = MIO.liftIO $ B.Properties.getObjectPropertyString obj "language"

-- | Set the value of the “@language@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' cellRendererText [ #language 'Data.GI.Base.Attributes.:=' value ]
-- @
setCellRendererTextLanguage :: (MonadIO m, IsCellRendererText o) => o -> T.Text -> m ()
setCellRendererTextLanguage obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "language" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@language@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructCellRendererTextLanguage :: (IsCellRendererText o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructCellRendererTextLanguage val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "language" (P.Just val)

-- | Set the value of the “@language@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #language
-- @
clearCellRendererTextLanguage :: (MonadIO m, IsCellRendererText o) => o -> m ()
clearCellRendererTextLanguage obj = liftIO $ B.Properties.setObjectPropertyString obj "language" (Nothing :: Maybe T.Text)

#if defined(ENABLE_OVERLOADING)
data CellRendererTextLanguagePropertyInfo
instance AttrInfo CellRendererTextLanguagePropertyInfo where
    type AttrAllowedOps CellRendererTextLanguagePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint CellRendererTextLanguagePropertyInfo = IsCellRendererText
    type AttrSetTypeConstraint CellRendererTextLanguagePropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint CellRendererTextLanguagePropertyInfo = (~) T.Text
    type AttrTransferType CellRendererTextLanguagePropertyInfo = T.Text
    type AttrGetType CellRendererTextLanguagePropertyInfo = (Maybe T.Text)
    type AttrLabel CellRendererTextLanguagePropertyInfo = "language"
    type AttrOrigin CellRendererTextLanguagePropertyInfo = CellRendererText
    attrGet = getCellRendererTextLanguage
    attrSet = setCellRendererTextLanguage
    attrTransfer _ v = do
        return v
    attrConstruct = constructCellRendererTextLanguage
    attrClear = clearCellRendererTextLanguage
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRendererText.language"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRendererText.html#g:attr:language"
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
-- 'Data.GI.Base.Attributes.get' cellRendererText #languageSet
-- @
getCellRendererTextLanguageSet :: (MonadIO m, IsCellRendererText o) => o -> m Bool
getCellRendererTextLanguageSet obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "language-set"

-- | Set the value of the “@language-set@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' cellRendererText [ #languageSet 'Data.GI.Base.Attributes.:=' value ]
-- @
setCellRendererTextLanguageSet :: (MonadIO m, IsCellRendererText o) => o -> Bool -> m ()
setCellRendererTextLanguageSet obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "language-set" val

-- | Construct a `GValueConstruct` with valid value for the “@language-set@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructCellRendererTextLanguageSet :: (IsCellRendererText o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructCellRendererTextLanguageSet val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "language-set" val

#if defined(ENABLE_OVERLOADING)
data CellRendererTextLanguageSetPropertyInfo
instance AttrInfo CellRendererTextLanguageSetPropertyInfo where
    type AttrAllowedOps CellRendererTextLanguageSetPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint CellRendererTextLanguageSetPropertyInfo = IsCellRendererText
    type AttrSetTypeConstraint CellRendererTextLanguageSetPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint CellRendererTextLanguageSetPropertyInfo = (~) Bool
    type AttrTransferType CellRendererTextLanguageSetPropertyInfo = Bool
    type AttrGetType CellRendererTextLanguageSetPropertyInfo = Bool
    type AttrLabel CellRendererTextLanguageSetPropertyInfo = "language-set"
    type AttrOrigin CellRendererTextLanguageSetPropertyInfo = CellRendererText
    attrGet = getCellRendererTextLanguageSet
    attrSet = setCellRendererTextLanguageSet
    attrTransfer _ v = do
        return v
    attrConstruct = constructCellRendererTextLanguageSet
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRendererText.languageSet"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRendererText.html#g:attr:languageSet"
        })
#endif

-- VVV Prop "markup"
   -- Type: TBasicType TUTF8
   -- Flags: [PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Set the value of the “@markup@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' cellRendererText [ #markup 'Data.GI.Base.Attributes.:=' value ]
-- @
setCellRendererTextMarkup :: (MonadIO m, IsCellRendererText o) => o -> T.Text -> m ()
setCellRendererTextMarkup obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "markup" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@markup@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructCellRendererTextMarkup :: (IsCellRendererText o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructCellRendererTextMarkup val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "markup" (P.Just val)

-- | Set the value of the “@markup@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #markup
-- @
clearCellRendererTextMarkup :: (MonadIO m, IsCellRendererText o) => o -> m ()
clearCellRendererTextMarkup obj = liftIO $ B.Properties.setObjectPropertyString obj "markup" (Nothing :: Maybe T.Text)

#if defined(ENABLE_OVERLOADING)
data CellRendererTextMarkupPropertyInfo
instance AttrInfo CellRendererTextMarkupPropertyInfo where
    type AttrAllowedOps CellRendererTextMarkupPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrClear]
    type AttrBaseTypeConstraint CellRendererTextMarkupPropertyInfo = IsCellRendererText
    type AttrSetTypeConstraint CellRendererTextMarkupPropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint CellRendererTextMarkupPropertyInfo = (~) T.Text
    type AttrTransferType CellRendererTextMarkupPropertyInfo = T.Text
    type AttrGetType CellRendererTextMarkupPropertyInfo = ()
    type AttrLabel CellRendererTextMarkupPropertyInfo = "markup"
    type AttrOrigin CellRendererTextMarkupPropertyInfo = CellRendererText
    attrGet = undefined
    attrSet = setCellRendererTextMarkup
    attrTransfer _ v = do
        return v
    attrConstruct = constructCellRendererTextMarkup
    attrClear = clearCellRendererTextMarkup
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRendererText.markup"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRendererText.html#g:attr:markup"
        })
#endif

-- VVV Prop "max-width-chars"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@max-width-chars@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' cellRendererText #maxWidthChars
-- @
getCellRendererTextMaxWidthChars :: (MonadIO m, IsCellRendererText o) => o -> m Int32
getCellRendererTextMaxWidthChars obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "max-width-chars"

-- | Set the value of the “@max-width-chars@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' cellRendererText [ #maxWidthChars 'Data.GI.Base.Attributes.:=' value ]
-- @
setCellRendererTextMaxWidthChars :: (MonadIO m, IsCellRendererText o) => o -> Int32 -> m ()
setCellRendererTextMaxWidthChars obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyInt32 obj "max-width-chars" val

-- | Construct a `GValueConstruct` with valid value for the “@max-width-chars@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructCellRendererTextMaxWidthChars :: (IsCellRendererText o, MIO.MonadIO m) => Int32 -> m (GValueConstruct o)
constructCellRendererTextMaxWidthChars val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyInt32 "max-width-chars" val

#if defined(ENABLE_OVERLOADING)
data CellRendererTextMaxWidthCharsPropertyInfo
instance AttrInfo CellRendererTextMaxWidthCharsPropertyInfo where
    type AttrAllowedOps CellRendererTextMaxWidthCharsPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint CellRendererTextMaxWidthCharsPropertyInfo = IsCellRendererText
    type AttrSetTypeConstraint CellRendererTextMaxWidthCharsPropertyInfo = (~) Int32
    type AttrTransferTypeConstraint CellRendererTextMaxWidthCharsPropertyInfo = (~) Int32
    type AttrTransferType CellRendererTextMaxWidthCharsPropertyInfo = Int32
    type AttrGetType CellRendererTextMaxWidthCharsPropertyInfo = Int32
    type AttrLabel CellRendererTextMaxWidthCharsPropertyInfo = "max-width-chars"
    type AttrOrigin CellRendererTextMaxWidthCharsPropertyInfo = CellRendererText
    attrGet = getCellRendererTextMaxWidthChars
    attrSet = setCellRendererTextMaxWidthChars
    attrTransfer _ v = do
        return v
    attrConstruct = constructCellRendererTextMaxWidthChars
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRendererText.maxWidthChars"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRendererText.html#g:attr:maxWidthChars"
        })
#endif

-- VVV Prop "placeholder-text"
   -- Type: TBasicType TUTF8
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@placeholder-text@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' cellRendererText #placeholderText
-- @
getCellRendererTextPlaceholderText :: (MonadIO m, IsCellRendererText o) => o -> m (Maybe T.Text)
getCellRendererTextPlaceholderText obj = MIO.liftIO $ B.Properties.getObjectPropertyString obj "placeholder-text"

-- | Set the value of the “@placeholder-text@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' cellRendererText [ #placeholderText 'Data.GI.Base.Attributes.:=' value ]
-- @
setCellRendererTextPlaceholderText :: (MonadIO m, IsCellRendererText o) => o -> T.Text -> m ()
setCellRendererTextPlaceholderText obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "placeholder-text" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@placeholder-text@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructCellRendererTextPlaceholderText :: (IsCellRendererText o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructCellRendererTextPlaceholderText val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "placeholder-text" (P.Just val)

-- | Set the value of the “@placeholder-text@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #placeholderText
-- @
clearCellRendererTextPlaceholderText :: (MonadIO m, IsCellRendererText o) => o -> m ()
clearCellRendererTextPlaceholderText obj = liftIO $ B.Properties.setObjectPropertyString obj "placeholder-text" (Nothing :: Maybe T.Text)

#if defined(ENABLE_OVERLOADING)
data CellRendererTextPlaceholderTextPropertyInfo
instance AttrInfo CellRendererTextPlaceholderTextPropertyInfo where
    type AttrAllowedOps CellRendererTextPlaceholderTextPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint CellRendererTextPlaceholderTextPropertyInfo = IsCellRendererText
    type AttrSetTypeConstraint CellRendererTextPlaceholderTextPropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint CellRendererTextPlaceholderTextPropertyInfo = (~) T.Text
    type AttrTransferType CellRendererTextPlaceholderTextPropertyInfo = T.Text
    type AttrGetType CellRendererTextPlaceholderTextPropertyInfo = (Maybe T.Text)
    type AttrLabel CellRendererTextPlaceholderTextPropertyInfo = "placeholder-text"
    type AttrOrigin CellRendererTextPlaceholderTextPropertyInfo = CellRendererText
    attrGet = getCellRendererTextPlaceholderText
    attrSet = setCellRendererTextPlaceholderText
    attrTransfer _ v = do
        return v
    attrConstruct = constructCellRendererTextPlaceholderText
    attrClear = clearCellRendererTextPlaceholderText
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRendererText.placeholderText"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRendererText.html#g:attr:placeholderText"
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
-- 'Data.GI.Base.Attributes.get' cellRendererText #rise
-- @
getCellRendererTextRise :: (MonadIO m, IsCellRendererText o) => o -> m Int32
getCellRendererTextRise obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "rise"

-- | Set the value of the “@rise@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' cellRendererText [ #rise 'Data.GI.Base.Attributes.:=' value ]
-- @
setCellRendererTextRise :: (MonadIO m, IsCellRendererText o) => o -> Int32 -> m ()
setCellRendererTextRise obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyInt32 obj "rise" val

-- | Construct a `GValueConstruct` with valid value for the “@rise@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructCellRendererTextRise :: (IsCellRendererText o, MIO.MonadIO m) => Int32 -> m (GValueConstruct o)
constructCellRendererTextRise val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyInt32 "rise" val

#if defined(ENABLE_OVERLOADING)
data CellRendererTextRisePropertyInfo
instance AttrInfo CellRendererTextRisePropertyInfo where
    type AttrAllowedOps CellRendererTextRisePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint CellRendererTextRisePropertyInfo = IsCellRendererText
    type AttrSetTypeConstraint CellRendererTextRisePropertyInfo = (~) Int32
    type AttrTransferTypeConstraint CellRendererTextRisePropertyInfo = (~) Int32
    type AttrTransferType CellRendererTextRisePropertyInfo = Int32
    type AttrGetType CellRendererTextRisePropertyInfo = Int32
    type AttrLabel CellRendererTextRisePropertyInfo = "rise"
    type AttrOrigin CellRendererTextRisePropertyInfo = CellRendererText
    attrGet = getCellRendererTextRise
    attrSet = setCellRendererTextRise
    attrTransfer _ v = do
        return v
    attrConstruct = constructCellRendererTextRise
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRendererText.rise"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRendererText.html#g:attr:rise"
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
-- 'Data.GI.Base.Attributes.get' cellRendererText #riseSet
-- @
getCellRendererTextRiseSet :: (MonadIO m, IsCellRendererText o) => o -> m Bool
getCellRendererTextRiseSet obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "rise-set"

-- | Set the value of the “@rise-set@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' cellRendererText [ #riseSet 'Data.GI.Base.Attributes.:=' value ]
-- @
setCellRendererTextRiseSet :: (MonadIO m, IsCellRendererText o) => o -> Bool -> m ()
setCellRendererTextRiseSet obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "rise-set" val

-- | Construct a `GValueConstruct` with valid value for the “@rise-set@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructCellRendererTextRiseSet :: (IsCellRendererText o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructCellRendererTextRiseSet val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "rise-set" val

#if defined(ENABLE_OVERLOADING)
data CellRendererTextRiseSetPropertyInfo
instance AttrInfo CellRendererTextRiseSetPropertyInfo where
    type AttrAllowedOps CellRendererTextRiseSetPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint CellRendererTextRiseSetPropertyInfo = IsCellRendererText
    type AttrSetTypeConstraint CellRendererTextRiseSetPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint CellRendererTextRiseSetPropertyInfo = (~) Bool
    type AttrTransferType CellRendererTextRiseSetPropertyInfo = Bool
    type AttrGetType CellRendererTextRiseSetPropertyInfo = Bool
    type AttrLabel CellRendererTextRiseSetPropertyInfo = "rise-set"
    type AttrOrigin CellRendererTextRiseSetPropertyInfo = CellRendererText
    attrGet = getCellRendererTextRiseSet
    attrSet = setCellRendererTextRiseSet
    attrTransfer _ v = do
        return v
    attrConstruct = constructCellRendererTextRiseSet
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRendererText.riseSet"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRendererText.html#g:attr:riseSet"
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
-- 'Data.GI.Base.Attributes.get' cellRendererText #scale
-- @
getCellRendererTextScale :: (MonadIO m, IsCellRendererText o) => o -> m Double
getCellRendererTextScale obj = MIO.liftIO $ B.Properties.getObjectPropertyDouble obj "scale"

-- | Set the value of the “@scale@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' cellRendererText [ #scale 'Data.GI.Base.Attributes.:=' value ]
-- @
setCellRendererTextScale :: (MonadIO m, IsCellRendererText o) => o -> Double -> m ()
setCellRendererTextScale obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyDouble obj "scale" val

-- | Construct a `GValueConstruct` with valid value for the “@scale@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructCellRendererTextScale :: (IsCellRendererText o, MIO.MonadIO m) => Double -> m (GValueConstruct o)
constructCellRendererTextScale val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyDouble "scale" val

#if defined(ENABLE_OVERLOADING)
data CellRendererTextScalePropertyInfo
instance AttrInfo CellRendererTextScalePropertyInfo where
    type AttrAllowedOps CellRendererTextScalePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint CellRendererTextScalePropertyInfo = IsCellRendererText
    type AttrSetTypeConstraint CellRendererTextScalePropertyInfo = (~) Double
    type AttrTransferTypeConstraint CellRendererTextScalePropertyInfo = (~) Double
    type AttrTransferType CellRendererTextScalePropertyInfo = Double
    type AttrGetType CellRendererTextScalePropertyInfo = Double
    type AttrLabel CellRendererTextScalePropertyInfo = "scale"
    type AttrOrigin CellRendererTextScalePropertyInfo = CellRendererText
    attrGet = getCellRendererTextScale
    attrSet = setCellRendererTextScale
    attrTransfer _ v = do
        return v
    attrConstruct = constructCellRendererTextScale
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRendererText.scale"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRendererText.html#g:attr:scale"
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
-- 'Data.GI.Base.Attributes.get' cellRendererText #scaleSet
-- @
getCellRendererTextScaleSet :: (MonadIO m, IsCellRendererText o) => o -> m Bool
getCellRendererTextScaleSet obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "scale-set"

-- | Set the value of the “@scale-set@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' cellRendererText [ #scaleSet 'Data.GI.Base.Attributes.:=' value ]
-- @
setCellRendererTextScaleSet :: (MonadIO m, IsCellRendererText o) => o -> Bool -> m ()
setCellRendererTextScaleSet obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "scale-set" val

-- | Construct a `GValueConstruct` with valid value for the “@scale-set@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructCellRendererTextScaleSet :: (IsCellRendererText o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructCellRendererTextScaleSet val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "scale-set" val

#if defined(ENABLE_OVERLOADING)
data CellRendererTextScaleSetPropertyInfo
instance AttrInfo CellRendererTextScaleSetPropertyInfo where
    type AttrAllowedOps CellRendererTextScaleSetPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint CellRendererTextScaleSetPropertyInfo = IsCellRendererText
    type AttrSetTypeConstraint CellRendererTextScaleSetPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint CellRendererTextScaleSetPropertyInfo = (~) Bool
    type AttrTransferType CellRendererTextScaleSetPropertyInfo = Bool
    type AttrGetType CellRendererTextScaleSetPropertyInfo = Bool
    type AttrLabel CellRendererTextScaleSetPropertyInfo = "scale-set"
    type AttrOrigin CellRendererTextScaleSetPropertyInfo = CellRendererText
    attrGet = getCellRendererTextScaleSet
    attrSet = setCellRendererTextScaleSet
    attrTransfer _ v = do
        return v
    attrConstruct = constructCellRendererTextScaleSet
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRendererText.scaleSet"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRendererText.html#g:attr:scaleSet"
        })
#endif

-- VVV Prop "single-paragraph-mode"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@single-paragraph-mode@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' cellRendererText #singleParagraphMode
-- @
getCellRendererTextSingleParagraphMode :: (MonadIO m, IsCellRendererText o) => o -> m Bool
getCellRendererTextSingleParagraphMode obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "single-paragraph-mode"

-- | Set the value of the “@single-paragraph-mode@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' cellRendererText [ #singleParagraphMode 'Data.GI.Base.Attributes.:=' value ]
-- @
setCellRendererTextSingleParagraphMode :: (MonadIO m, IsCellRendererText o) => o -> Bool -> m ()
setCellRendererTextSingleParagraphMode obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "single-paragraph-mode" val

-- | Construct a `GValueConstruct` with valid value for the “@single-paragraph-mode@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructCellRendererTextSingleParagraphMode :: (IsCellRendererText o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructCellRendererTextSingleParagraphMode val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "single-paragraph-mode" val

#if defined(ENABLE_OVERLOADING)
data CellRendererTextSingleParagraphModePropertyInfo
instance AttrInfo CellRendererTextSingleParagraphModePropertyInfo where
    type AttrAllowedOps CellRendererTextSingleParagraphModePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint CellRendererTextSingleParagraphModePropertyInfo = IsCellRendererText
    type AttrSetTypeConstraint CellRendererTextSingleParagraphModePropertyInfo = (~) Bool
    type AttrTransferTypeConstraint CellRendererTextSingleParagraphModePropertyInfo = (~) Bool
    type AttrTransferType CellRendererTextSingleParagraphModePropertyInfo = Bool
    type AttrGetType CellRendererTextSingleParagraphModePropertyInfo = Bool
    type AttrLabel CellRendererTextSingleParagraphModePropertyInfo = "single-paragraph-mode"
    type AttrOrigin CellRendererTextSingleParagraphModePropertyInfo = CellRendererText
    attrGet = getCellRendererTextSingleParagraphMode
    attrSet = setCellRendererTextSingleParagraphMode
    attrTransfer _ v = do
        return v
    attrConstruct = constructCellRendererTextSingleParagraphMode
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRendererText.singleParagraphMode"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRendererText.html#g:attr:singleParagraphMode"
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
-- 'Data.GI.Base.Attributes.get' cellRendererText #size
-- @
getCellRendererTextSize :: (MonadIO m, IsCellRendererText o) => o -> m Int32
getCellRendererTextSize obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "size"

-- | Set the value of the “@size@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' cellRendererText [ #size 'Data.GI.Base.Attributes.:=' value ]
-- @
setCellRendererTextSize :: (MonadIO m, IsCellRendererText o) => o -> Int32 -> m ()
setCellRendererTextSize obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyInt32 obj "size" val

-- | Construct a `GValueConstruct` with valid value for the “@size@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructCellRendererTextSize :: (IsCellRendererText o, MIO.MonadIO m) => Int32 -> m (GValueConstruct o)
constructCellRendererTextSize val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyInt32 "size" val

#if defined(ENABLE_OVERLOADING)
data CellRendererTextSizePropertyInfo
instance AttrInfo CellRendererTextSizePropertyInfo where
    type AttrAllowedOps CellRendererTextSizePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint CellRendererTextSizePropertyInfo = IsCellRendererText
    type AttrSetTypeConstraint CellRendererTextSizePropertyInfo = (~) Int32
    type AttrTransferTypeConstraint CellRendererTextSizePropertyInfo = (~) Int32
    type AttrTransferType CellRendererTextSizePropertyInfo = Int32
    type AttrGetType CellRendererTextSizePropertyInfo = Int32
    type AttrLabel CellRendererTextSizePropertyInfo = "size"
    type AttrOrigin CellRendererTextSizePropertyInfo = CellRendererText
    attrGet = getCellRendererTextSize
    attrSet = setCellRendererTextSize
    attrTransfer _ v = do
        return v
    attrConstruct = constructCellRendererTextSize
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRendererText.size"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRendererText.html#g:attr:size"
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
-- 'Data.GI.Base.Attributes.get' cellRendererText #sizePoints
-- @
getCellRendererTextSizePoints :: (MonadIO m, IsCellRendererText o) => o -> m Double
getCellRendererTextSizePoints obj = MIO.liftIO $ B.Properties.getObjectPropertyDouble obj "size-points"

-- | Set the value of the “@size-points@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' cellRendererText [ #sizePoints 'Data.GI.Base.Attributes.:=' value ]
-- @
setCellRendererTextSizePoints :: (MonadIO m, IsCellRendererText o) => o -> Double -> m ()
setCellRendererTextSizePoints obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyDouble obj "size-points" val

-- | Construct a `GValueConstruct` with valid value for the “@size-points@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructCellRendererTextSizePoints :: (IsCellRendererText o, MIO.MonadIO m) => Double -> m (GValueConstruct o)
constructCellRendererTextSizePoints val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyDouble "size-points" val

#if defined(ENABLE_OVERLOADING)
data CellRendererTextSizePointsPropertyInfo
instance AttrInfo CellRendererTextSizePointsPropertyInfo where
    type AttrAllowedOps CellRendererTextSizePointsPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint CellRendererTextSizePointsPropertyInfo = IsCellRendererText
    type AttrSetTypeConstraint CellRendererTextSizePointsPropertyInfo = (~) Double
    type AttrTransferTypeConstraint CellRendererTextSizePointsPropertyInfo = (~) Double
    type AttrTransferType CellRendererTextSizePointsPropertyInfo = Double
    type AttrGetType CellRendererTextSizePointsPropertyInfo = Double
    type AttrLabel CellRendererTextSizePointsPropertyInfo = "size-points"
    type AttrOrigin CellRendererTextSizePointsPropertyInfo = CellRendererText
    attrGet = getCellRendererTextSizePoints
    attrSet = setCellRendererTextSizePoints
    attrTransfer _ v = do
        return v
    attrConstruct = constructCellRendererTextSizePoints
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRendererText.sizePoints"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRendererText.html#g:attr:sizePoints"
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
-- 'Data.GI.Base.Attributes.get' cellRendererText #sizeSet
-- @
getCellRendererTextSizeSet :: (MonadIO m, IsCellRendererText o) => o -> m Bool
getCellRendererTextSizeSet obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "size-set"

-- | Set the value of the “@size-set@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' cellRendererText [ #sizeSet 'Data.GI.Base.Attributes.:=' value ]
-- @
setCellRendererTextSizeSet :: (MonadIO m, IsCellRendererText o) => o -> Bool -> m ()
setCellRendererTextSizeSet obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "size-set" val

-- | Construct a `GValueConstruct` with valid value for the “@size-set@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructCellRendererTextSizeSet :: (IsCellRendererText o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructCellRendererTextSizeSet val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "size-set" val

#if defined(ENABLE_OVERLOADING)
data CellRendererTextSizeSetPropertyInfo
instance AttrInfo CellRendererTextSizeSetPropertyInfo where
    type AttrAllowedOps CellRendererTextSizeSetPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint CellRendererTextSizeSetPropertyInfo = IsCellRendererText
    type AttrSetTypeConstraint CellRendererTextSizeSetPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint CellRendererTextSizeSetPropertyInfo = (~) Bool
    type AttrTransferType CellRendererTextSizeSetPropertyInfo = Bool
    type AttrGetType CellRendererTextSizeSetPropertyInfo = Bool
    type AttrLabel CellRendererTextSizeSetPropertyInfo = "size-set"
    type AttrOrigin CellRendererTextSizeSetPropertyInfo = CellRendererText
    attrGet = getCellRendererTextSizeSet
    attrSet = setCellRendererTextSizeSet
    attrTransfer _ v = do
        return v
    attrConstruct = constructCellRendererTextSizeSet
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRendererText.sizeSet"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRendererText.html#g:attr:sizeSet"
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
-- 'Data.GI.Base.Attributes.get' cellRendererText #stretch
-- @
getCellRendererTextStretch :: (MonadIO m, IsCellRendererText o) => o -> m Pango.Enums.Stretch
getCellRendererTextStretch obj = MIO.liftIO $ B.Properties.getObjectPropertyEnum obj "stretch"

-- | Set the value of the “@stretch@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' cellRendererText [ #stretch 'Data.GI.Base.Attributes.:=' value ]
-- @
setCellRendererTextStretch :: (MonadIO m, IsCellRendererText o) => o -> Pango.Enums.Stretch -> m ()
setCellRendererTextStretch obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyEnum obj "stretch" val

-- | Construct a `GValueConstruct` with valid value for the “@stretch@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructCellRendererTextStretch :: (IsCellRendererText o, MIO.MonadIO m) => Pango.Enums.Stretch -> m (GValueConstruct o)
constructCellRendererTextStretch val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyEnum "stretch" val

#if defined(ENABLE_OVERLOADING)
data CellRendererTextStretchPropertyInfo
instance AttrInfo CellRendererTextStretchPropertyInfo where
    type AttrAllowedOps CellRendererTextStretchPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint CellRendererTextStretchPropertyInfo = IsCellRendererText
    type AttrSetTypeConstraint CellRendererTextStretchPropertyInfo = (~) Pango.Enums.Stretch
    type AttrTransferTypeConstraint CellRendererTextStretchPropertyInfo = (~) Pango.Enums.Stretch
    type AttrTransferType CellRendererTextStretchPropertyInfo = Pango.Enums.Stretch
    type AttrGetType CellRendererTextStretchPropertyInfo = Pango.Enums.Stretch
    type AttrLabel CellRendererTextStretchPropertyInfo = "stretch"
    type AttrOrigin CellRendererTextStretchPropertyInfo = CellRendererText
    attrGet = getCellRendererTextStretch
    attrSet = setCellRendererTextStretch
    attrTransfer _ v = do
        return v
    attrConstruct = constructCellRendererTextStretch
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRendererText.stretch"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRendererText.html#g:attr:stretch"
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
-- 'Data.GI.Base.Attributes.get' cellRendererText #stretchSet
-- @
getCellRendererTextStretchSet :: (MonadIO m, IsCellRendererText o) => o -> m Bool
getCellRendererTextStretchSet obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "stretch-set"

-- | Set the value of the “@stretch-set@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' cellRendererText [ #stretchSet 'Data.GI.Base.Attributes.:=' value ]
-- @
setCellRendererTextStretchSet :: (MonadIO m, IsCellRendererText o) => o -> Bool -> m ()
setCellRendererTextStretchSet obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "stretch-set" val

-- | Construct a `GValueConstruct` with valid value for the “@stretch-set@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructCellRendererTextStretchSet :: (IsCellRendererText o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructCellRendererTextStretchSet val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "stretch-set" val

#if defined(ENABLE_OVERLOADING)
data CellRendererTextStretchSetPropertyInfo
instance AttrInfo CellRendererTextStretchSetPropertyInfo where
    type AttrAllowedOps CellRendererTextStretchSetPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint CellRendererTextStretchSetPropertyInfo = IsCellRendererText
    type AttrSetTypeConstraint CellRendererTextStretchSetPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint CellRendererTextStretchSetPropertyInfo = (~) Bool
    type AttrTransferType CellRendererTextStretchSetPropertyInfo = Bool
    type AttrGetType CellRendererTextStretchSetPropertyInfo = Bool
    type AttrLabel CellRendererTextStretchSetPropertyInfo = "stretch-set"
    type AttrOrigin CellRendererTextStretchSetPropertyInfo = CellRendererText
    attrGet = getCellRendererTextStretchSet
    attrSet = setCellRendererTextStretchSet
    attrTransfer _ v = do
        return v
    attrConstruct = constructCellRendererTextStretchSet
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRendererText.stretchSet"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRendererText.html#g:attr:stretchSet"
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
-- 'Data.GI.Base.Attributes.get' cellRendererText #strikethrough
-- @
getCellRendererTextStrikethrough :: (MonadIO m, IsCellRendererText o) => o -> m Bool
getCellRendererTextStrikethrough obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "strikethrough"

-- | Set the value of the “@strikethrough@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' cellRendererText [ #strikethrough 'Data.GI.Base.Attributes.:=' value ]
-- @
setCellRendererTextStrikethrough :: (MonadIO m, IsCellRendererText o) => o -> Bool -> m ()
setCellRendererTextStrikethrough obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "strikethrough" val

-- | Construct a `GValueConstruct` with valid value for the “@strikethrough@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructCellRendererTextStrikethrough :: (IsCellRendererText o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructCellRendererTextStrikethrough val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "strikethrough" val

#if defined(ENABLE_OVERLOADING)
data CellRendererTextStrikethroughPropertyInfo
instance AttrInfo CellRendererTextStrikethroughPropertyInfo where
    type AttrAllowedOps CellRendererTextStrikethroughPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint CellRendererTextStrikethroughPropertyInfo = IsCellRendererText
    type AttrSetTypeConstraint CellRendererTextStrikethroughPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint CellRendererTextStrikethroughPropertyInfo = (~) Bool
    type AttrTransferType CellRendererTextStrikethroughPropertyInfo = Bool
    type AttrGetType CellRendererTextStrikethroughPropertyInfo = Bool
    type AttrLabel CellRendererTextStrikethroughPropertyInfo = "strikethrough"
    type AttrOrigin CellRendererTextStrikethroughPropertyInfo = CellRendererText
    attrGet = getCellRendererTextStrikethrough
    attrSet = setCellRendererTextStrikethrough
    attrTransfer _ v = do
        return v
    attrConstruct = constructCellRendererTextStrikethrough
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRendererText.strikethrough"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRendererText.html#g:attr:strikethrough"
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
-- 'Data.GI.Base.Attributes.get' cellRendererText #strikethroughSet
-- @
getCellRendererTextStrikethroughSet :: (MonadIO m, IsCellRendererText o) => o -> m Bool
getCellRendererTextStrikethroughSet obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "strikethrough-set"

-- | Set the value of the “@strikethrough-set@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' cellRendererText [ #strikethroughSet 'Data.GI.Base.Attributes.:=' value ]
-- @
setCellRendererTextStrikethroughSet :: (MonadIO m, IsCellRendererText o) => o -> Bool -> m ()
setCellRendererTextStrikethroughSet obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "strikethrough-set" val

-- | Construct a `GValueConstruct` with valid value for the “@strikethrough-set@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructCellRendererTextStrikethroughSet :: (IsCellRendererText o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructCellRendererTextStrikethroughSet val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "strikethrough-set" val

#if defined(ENABLE_OVERLOADING)
data CellRendererTextStrikethroughSetPropertyInfo
instance AttrInfo CellRendererTextStrikethroughSetPropertyInfo where
    type AttrAllowedOps CellRendererTextStrikethroughSetPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint CellRendererTextStrikethroughSetPropertyInfo = IsCellRendererText
    type AttrSetTypeConstraint CellRendererTextStrikethroughSetPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint CellRendererTextStrikethroughSetPropertyInfo = (~) Bool
    type AttrTransferType CellRendererTextStrikethroughSetPropertyInfo = Bool
    type AttrGetType CellRendererTextStrikethroughSetPropertyInfo = Bool
    type AttrLabel CellRendererTextStrikethroughSetPropertyInfo = "strikethrough-set"
    type AttrOrigin CellRendererTextStrikethroughSetPropertyInfo = CellRendererText
    attrGet = getCellRendererTextStrikethroughSet
    attrSet = setCellRendererTextStrikethroughSet
    attrTransfer _ v = do
        return v
    attrConstruct = constructCellRendererTextStrikethroughSet
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRendererText.strikethroughSet"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRendererText.html#g:attr:strikethroughSet"
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
-- 'Data.GI.Base.Attributes.get' cellRendererText #style
-- @
getCellRendererTextStyle :: (MonadIO m, IsCellRendererText o) => o -> m Pango.Enums.Style
getCellRendererTextStyle obj = MIO.liftIO $ B.Properties.getObjectPropertyEnum obj "style"

-- | Set the value of the “@style@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' cellRendererText [ #style 'Data.GI.Base.Attributes.:=' value ]
-- @
setCellRendererTextStyle :: (MonadIO m, IsCellRendererText o) => o -> Pango.Enums.Style -> m ()
setCellRendererTextStyle obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyEnum obj "style" val

-- | Construct a `GValueConstruct` with valid value for the “@style@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructCellRendererTextStyle :: (IsCellRendererText o, MIO.MonadIO m) => Pango.Enums.Style -> m (GValueConstruct o)
constructCellRendererTextStyle val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyEnum "style" val

#if defined(ENABLE_OVERLOADING)
data CellRendererTextStylePropertyInfo
instance AttrInfo CellRendererTextStylePropertyInfo where
    type AttrAllowedOps CellRendererTextStylePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint CellRendererTextStylePropertyInfo = IsCellRendererText
    type AttrSetTypeConstraint CellRendererTextStylePropertyInfo = (~) Pango.Enums.Style
    type AttrTransferTypeConstraint CellRendererTextStylePropertyInfo = (~) Pango.Enums.Style
    type AttrTransferType CellRendererTextStylePropertyInfo = Pango.Enums.Style
    type AttrGetType CellRendererTextStylePropertyInfo = Pango.Enums.Style
    type AttrLabel CellRendererTextStylePropertyInfo = "style"
    type AttrOrigin CellRendererTextStylePropertyInfo = CellRendererText
    attrGet = getCellRendererTextStyle
    attrSet = setCellRendererTextStyle
    attrTransfer _ v = do
        return v
    attrConstruct = constructCellRendererTextStyle
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRendererText.style"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRendererText.html#g:attr:style"
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
-- 'Data.GI.Base.Attributes.get' cellRendererText #styleSet
-- @
getCellRendererTextStyleSet :: (MonadIO m, IsCellRendererText o) => o -> m Bool
getCellRendererTextStyleSet obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "style-set"

-- | Set the value of the “@style-set@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' cellRendererText [ #styleSet 'Data.GI.Base.Attributes.:=' value ]
-- @
setCellRendererTextStyleSet :: (MonadIO m, IsCellRendererText o) => o -> Bool -> m ()
setCellRendererTextStyleSet obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "style-set" val

-- | Construct a `GValueConstruct` with valid value for the “@style-set@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructCellRendererTextStyleSet :: (IsCellRendererText o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructCellRendererTextStyleSet val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "style-set" val

#if defined(ENABLE_OVERLOADING)
data CellRendererTextStyleSetPropertyInfo
instance AttrInfo CellRendererTextStyleSetPropertyInfo where
    type AttrAllowedOps CellRendererTextStyleSetPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint CellRendererTextStyleSetPropertyInfo = IsCellRendererText
    type AttrSetTypeConstraint CellRendererTextStyleSetPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint CellRendererTextStyleSetPropertyInfo = (~) Bool
    type AttrTransferType CellRendererTextStyleSetPropertyInfo = Bool
    type AttrGetType CellRendererTextStyleSetPropertyInfo = Bool
    type AttrLabel CellRendererTextStyleSetPropertyInfo = "style-set"
    type AttrOrigin CellRendererTextStyleSetPropertyInfo = CellRendererText
    attrGet = getCellRendererTextStyleSet
    attrSet = setCellRendererTextStyleSet
    attrTransfer _ v = do
        return v
    attrConstruct = constructCellRendererTextStyleSet
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRendererText.styleSet"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRendererText.html#g:attr:styleSet"
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
-- 'Data.GI.Base.Attributes.get' cellRendererText #text
-- @
getCellRendererTextText :: (MonadIO m, IsCellRendererText o) => o -> m (Maybe T.Text)
getCellRendererTextText obj = MIO.liftIO $ B.Properties.getObjectPropertyString obj "text"

-- | Set the value of the “@text@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' cellRendererText [ #text 'Data.GI.Base.Attributes.:=' value ]
-- @
setCellRendererTextText :: (MonadIO m, IsCellRendererText o) => o -> T.Text -> m ()
setCellRendererTextText obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "text" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@text@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructCellRendererTextText :: (IsCellRendererText o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructCellRendererTextText val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "text" (P.Just val)

-- | Set the value of the “@text@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #text
-- @
clearCellRendererTextText :: (MonadIO m, IsCellRendererText o) => o -> m ()
clearCellRendererTextText obj = liftIO $ B.Properties.setObjectPropertyString obj "text" (Nothing :: Maybe T.Text)

#if defined(ENABLE_OVERLOADING)
data CellRendererTextTextPropertyInfo
instance AttrInfo CellRendererTextTextPropertyInfo where
    type AttrAllowedOps CellRendererTextTextPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint CellRendererTextTextPropertyInfo = IsCellRendererText
    type AttrSetTypeConstraint CellRendererTextTextPropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint CellRendererTextTextPropertyInfo = (~) T.Text
    type AttrTransferType CellRendererTextTextPropertyInfo = T.Text
    type AttrGetType CellRendererTextTextPropertyInfo = (Maybe T.Text)
    type AttrLabel CellRendererTextTextPropertyInfo = "text"
    type AttrOrigin CellRendererTextTextPropertyInfo = CellRendererText
    attrGet = getCellRendererTextText
    attrSet = setCellRendererTextText
    attrTransfer _ v = do
        return v
    attrConstruct = constructCellRendererTextText
    attrClear = clearCellRendererTextText
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRendererText.text"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRendererText.html#g:attr:text"
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
-- 'Data.GI.Base.Attributes.get' cellRendererText #underline
-- @
getCellRendererTextUnderline :: (MonadIO m, IsCellRendererText o) => o -> m Pango.Enums.Underline
getCellRendererTextUnderline obj = MIO.liftIO $ B.Properties.getObjectPropertyEnum obj "underline"

-- | Set the value of the “@underline@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' cellRendererText [ #underline 'Data.GI.Base.Attributes.:=' value ]
-- @
setCellRendererTextUnderline :: (MonadIO m, IsCellRendererText o) => o -> Pango.Enums.Underline -> m ()
setCellRendererTextUnderline obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyEnum obj "underline" val

-- | Construct a `GValueConstruct` with valid value for the “@underline@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructCellRendererTextUnderline :: (IsCellRendererText o, MIO.MonadIO m) => Pango.Enums.Underline -> m (GValueConstruct o)
constructCellRendererTextUnderline val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyEnum "underline" val

#if defined(ENABLE_OVERLOADING)
data CellRendererTextUnderlinePropertyInfo
instance AttrInfo CellRendererTextUnderlinePropertyInfo where
    type AttrAllowedOps CellRendererTextUnderlinePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint CellRendererTextUnderlinePropertyInfo = IsCellRendererText
    type AttrSetTypeConstraint CellRendererTextUnderlinePropertyInfo = (~) Pango.Enums.Underline
    type AttrTransferTypeConstraint CellRendererTextUnderlinePropertyInfo = (~) Pango.Enums.Underline
    type AttrTransferType CellRendererTextUnderlinePropertyInfo = Pango.Enums.Underline
    type AttrGetType CellRendererTextUnderlinePropertyInfo = Pango.Enums.Underline
    type AttrLabel CellRendererTextUnderlinePropertyInfo = "underline"
    type AttrOrigin CellRendererTextUnderlinePropertyInfo = CellRendererText
    attrGet = getCellRendererTextUnderline
    attrSet = setCellRendererTextUnderline
    attrTransfer _ v = do
        return v
    attrConstruct = constructCellRendererTextUnderline
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRendererText.underline"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRendererText.html#g:attr:underline"
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
-- 'Data.GI.Base.Attributes.get' cellRendererText #underlineSet
-- @
getCellRendererTextUnderlineSet :: (MonadIO m, IsCellRendererText o) => o -> m Bool
getCellRendererTextUnderlineSet obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "underline-set"

-- | Set the value of the “@underline-set@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' cellRendererText [ #underlineSet 'Data.GI.Base.Attributes.:=' value ]
-- @
setCellRendererTextUnderlineSet :: (MonadIO m, IsCellRendererText o) => o -> Bool -> m ()
setCellRendererTextUnderlineSet obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "underline-set" val

-- | Construct a `GValueConstruct` with valid value for the “@underline-set@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructCellRendererTextUnderlineSet :: (IsCellRendererText o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructCellRendererTextUnderlineSet val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "underline-set" val

#if defined(ENABLE_OVERLOADING)
data CellRendererTextUnderlineSetPropertyInfo
instance AttrInfo CellRendererTextUnderlineSetPropertyInfo where
    type AttrAllowedOps CellRendererTextUnderlineSetPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint CellRendererTextUnderlineSetPropertyInfo = IsCellRendererText
    type AttrSetTypeConstraint CellRendererTextUnderlineSetPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint CellRendererTextUnderlineSetPropertyInfo = (~) Bool
    type AttrTransferType CellRendererTextUnderlineSetPropertyInfo = Bool
    type AttrGetType CellRendererTextUnderlineSetPropertyInfo = Bool
    type AttrLabel CellRendererTextUnderlineSetPropertyInfo = "underline-set"
    type AttrOrigin CellRendererTextUnderlineSetPropertyInfo = CellRendererText
    attrGet = getCellRendererTextUnderlineSet
    attrSet = setCellRendererTextUnderlineSet
    attrTransfer _ v = do
        return v
    attrConstruct = constructCellRendererTextUnderlineSet
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRendererText.underlineSet"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRendererText.html#g:attr:underlineSet"
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
-- 'Data.GI.Base.Attributes.get' cellRendererText #variant
-- @
getCellRendererTextVariant :: (MonadIO m, IsCellRendererText o) => o -> m Pango.Enums.Variant
getCellRendererTextVariant obj = MIO.liftIO $ B.Properties.getObjectPropertyEnum obj "variant"

-- | Set the value of the “@variant@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' cellRendererText [ #variant 'Data.GI.Base.Attributes.:=' value ]
-- @
setCellRendererTextVariant :: (MonadIO m, IsCellRendererText o) => o -> Pango.Enums.Variant -> m ()
setCellRendererTextVariant obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyEnum obj "variant" val

-- | Construct a `GValueConstruct` with valid value for the “@variant@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructCellRendererTextVariant :: (IsCellRendererText o, MIO.MonadIO m) => Pango.Enums.Variant -> m (GValueConstruct o)
constructCellRendererTextVariant val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyEnum "variant" val

#if defined(ENABLE_OVERLOADING)
data CellRendererTextVariantPropertyInfo
instance AttrInfo CellRendererTextVariantPropertyInfo where
    type AttrAllowedOps CellRendererTextVariantPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint CellRendererTextVariantPropertyInfo = IsCellRendererText
    type AttrSetTypeConstraint CellRendererTextVariantPropertyInfo = (~) Pango.Enums.Variant
    type AttrTransferTypeConstraint CellRendererTextVariantPropertyInfo = (~) Pango.Enums.Variant
    type AttrTransferType CellRendererTextVariantPropertyInfo = Pango.Enums.Variant
    type AttrGetType CellRendererTextVariantPropertyInfo = Pango.Enums.Variant
    type AttrLabel CellRendererTextVariantPropertyInfo = "variant"
    type AttrOrigin CellRendererTextVariantPropertyInfo = CellRendererText
    attrGet = getCellRendererTextVariant
    attrSet = setCellRendererTextVariant
    attrTransfer _ v = do
        return v
    attrConstruct = constructCellRendererTextVariant
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRendererText.variant"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRendererText.html#g:attr:variant"
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
-- 'Data.GI.Base.Attributes.get' cellRendererText #variantSet
-- @
getCellRendererTextVariantSet :: (MonadIO m, IsCellRendererText o) => o -> m Bool
getCellRendererTextVariantSet obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "variant-set"

-- | Set the value of the “@variant-set@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' cellRendererText [ #variantSet 'Data.GI.Base.Attributes.:=' value ]
-- @
setCellRendererTextVariantSet :: (MonadIO m, IsCellRendererText o) => o -> Bool -> m ()
setCellRendererTextVariantSet obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "variant-set" val

-- | Construct a `GValueConstruct` with valid value for the “@variant-set@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructCellRendererTextVariantSet :: (IsCellRendererText o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructCellRendererTextVariantSet val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "variant-set" val

#if defined(ENABLE_OVERLOADING)
data CellRendererTextVariantSetPropertyInfo
instance AttrInfo CellRendererTextVariantSetPropertyInfo where
    type AttrAllowedOps CellRendererTextVariantSetPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint CellRendererTextVariantSetPropertyInfo = IsCellRendererText
    type AttrSetTypeConstraint CellRendererTextVariantSetPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint CellRendererTextVariantSetPropertyInfo = (~) Bool
    type AttrTransferType CellRendererTextVariantSetPropertyInfo = Bool
    type AttrGetType CellRendererTextVariantSetPropertyInfo = Bool
    type AttrLabel CellRendererTextVariantSetPropertyInfo = "variant-set"
    type AttrOrigin CellRendererTextVariantSetPropertyInfo = CellRendererText
    attrGet = getCellRendererTextVariantSet
    attrSet = setCellRendererTextVariantSet
    attrTransfer _ v = do
        return v
    attrConstruct = constructCellRendererTextVariantSet
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRendererText.variantSet"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRendererText.html#g:attr:variantSet"
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
-- 'Data.GI.Base.Attributes.get' cellRendererText #weight
-- @
getCellRendererTextWeight :: (MonadIO m, IsCellRendererText o) => o -> m Int32
getCellRendererTextWeight obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "weight"

-- | Set the value of the “@weight@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' cellRendererText [ #weight 'Data.GI.Base.Attributes.:=' value ]
-- @
setCellRendererTextWeight :: (MonadIO m, IsCellRendererText o) => o -> Int32 -> m ()
setCellRendererTextWeight obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyInt32 obj "weight" val

-- | Construct a `GValueConstruct` with valid value for the “@weight@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructCellRendererTextWeight :: (IsCellRendererText o, MIO.MonadIO m) => Int32 -> m (GValueConstruct o)
constructCellRendererTextWeight val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyInt32 "weight" val

#if defined(ENABLE_OVERLOADING)
data CellRendererTextWeightPropertyInfo
instance AttrInfo CellRendererTextWeightPropertyInfo where
    type AttrAllowedOps CellRendererTextWeightPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint CellRendererTextWeightPropertyInfo = IsCellRendererText
    type AttrSetTypeConstraint CellRendererTextWeightPropertyInfo = (~) Int32
    type AttrTransferTypeConstraint CellRendererTextWeightPropertyInfo = (~) Int32
    type AttrTransferType CellRendererTextWeightPropertyInfo = Int32
    type AttrGetType CellRendererTextWeightPropertyInfo = Int32
    type AttrLabel CellRendererTextWeightPropertyInfo = "weight"
    type AttrOrigin CellRendererTextWeightPropertyInfo = CellRendererText
    attrGet = getCellRendererTextWeight
    attrSet = setCellRendererTextWeight
    attrTransfer _ v = do
        return v
    attrConstruct = constructCellRendererTextWeight
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRendererText.weight"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRendererText.html#g:attr:weight"
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
-- 'Data.GI.Base.Attributes.get' cellRendererText #weightSet
-- @
getCellRendererTextWeightSet :: (MonadIO m, IsCellRendererText o) => o -> m Bool
getCellRendererTextWeightSet obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "weight-set"

-- | Set the value of the “@weight-set@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' cellRendererText [ #weightSet 'Data.GI.Base.Attributes.:=' value ]
-- @
setCellRendererTextWeightSet :: (MonadIO m, IsCellRendererText o) => o -> Bool -> m ()
setCellRendererTextWeightSet obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "weight-set" val

-- | Construct a `GValueConstruct` with valid value for the “@weight-set@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructCellRendererTextWeightSet :: (IsCellRendererText o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructCellRendererTextWeightSet val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "weight-set" val

#if defined(ENABLE_OVERLOADING)
data CellRendererTextWeightSetPropertyInfo
instance AttrInfo CellRendererTextWeightSetPropertyInfo where
    type AttrAllowedOps CellRendererTextWeightSetPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint CellRendererTextWeightSetPropertyInfo = IsCellRendererText
    type AttrSetTypeConstraint CellRendererTextWeightSetPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint CellRendererTextWeightSetPropertyInfo = (~) Bool
    type AttrTransferType CellRendererTextWeightSetPropertyInfo = Bool
    type AttrGetType CellRendererTextWeightSetPropertyInfo = Bool
    type AttrLabel CellRendererTextWeightSetPropertyInfo = "weight-set"
    type AttrOrigin CellRendererTextWeightSetPropertyInfo = CellRendererText
    attrGet = getCellRendererTextWeightSet
    attrSet = setCellRendererTextWeightSet
    attrTransfer _ v = do
        return v
    attrConstruct = constructCellRendererTextWeightSet
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRendererText.weightSet"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRendererText.html#g:attr:weightSet"
        })
#endif

-- VVV Prop "width-chars"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@width-chars@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' cellRendererText #widthChars
-- @
getCellRendererTextWidthChars :: (MonadIO m, IsCellRendererText o) => o -> m Int32
getCellRendererTextWidthChars obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "width-chars"

-- | Set the value of the “@width-chars@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' cellRendererText [ #widthChars 'Data.GI.Base.Attributes.:=' value ]
-- @
setCellRendererTextWidthChars :: (MonadIO m, IsCellRendererText o) => o -> Int32 -> m ()
setCellRendererTextWidthChars obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyInt32 obj "width-chars" val

-- | Construct a `GValueConstruct` with valid value for the “@width-chars@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructCellRendererTextWidthChars :: (IsCellRendererText o, MIO.MonadIO m) => Int32 -> m (GValueConstruct o)
constructCellRendererTextWidthChars val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyInt32 "width-chars" val

#if defined(ENABLE_OVERLOADING)
data CellRendererTextWidthCharsPropertyInfo
instance AttrInfo CellRendererTextWidthCharsPropertyInfo where
    type AttrAllowedOps CellRendererTextWidthCharsPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint CellRendererTextWidthCharsPropertyInfo = IsCellRendererText
    type AttrSetTypeConstraint CellRendererTextWidthCharsPropertyInfo = (~) Int32
    type AttrTransferTypeConstraint CellRendererTextWidthCharsPropertyInfo = (~) Int32
    type AttrTransferType CellRendererTextWidthCharsPropertyInfo = Int32
    type AttrGetType CellRendererTextWidthCharsPropertyInfo = Int32
    type AttrLabel CellRendererTextWidthCharsPropertyInfo = "width-chars"
    type AttrOrigin CellRendererTextWidthCharsPropertyInfo = CellRendererText
    attrGet = getCellRendererTextWidthChars
    attrSet = setCellRendererTextWidthChars
    attrTransfer _ v = do
        return v
    attrConstruct = constructCellRendererTextWidthChars
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRendererText.widthChars"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRendererText.html#g:attr:widthChars"
        })
#endif

-- VVV Prop "wrap-mode"
   -- Type: TInterface (Name {namespace = "Pango", name = "WrapMode"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@wrap-mode@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' cellRendererText #wrapMode
-- @
getCellRendererTextWrapMode :: (MonadIO m, IsCellRendererText o) => o -> m Pango.Enums.WrapMode
getCellRendererTextWrapMode obj = MIO.liftIO $ B.Properties.getObjectPropertyEnum obj "wrap-mode"

-- | Set the value of the “@wrap-mode@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' cellRendererText [ #wrapMode 'Data.GI.Base.Attributes.:=' value ]
-- @
setCellRendererTextWrapMode :: (MonadIO m, IsCellRendererText o) => o -> Pango.Enums.WrapMode -> m ()
setCellRendererTextWrapMode obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyEnum obj "wrap-mode" val

-- | Construct a `GValueConstruct` with valid value for the “@wrap-mode@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructCellRendererTextWrapMode :: (IsCellRendererText o, MIO.MonadIO m) => Pango.Enums.WrapMode -> m (GValueConstruct o)
constructCellRendererTextWrapMode val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyEnum "wrap-mode" val

#if defined(ENABLE_OVERLOADING)
data CellRendererTextWrapModePropertyInfo
instance AttrInfo CellRendererTextWrapModePropertyInfo where
    type AttrAllowedOps CellRendererTextWrapModePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint CellRendererTextWrapModePropertyInfo = IsCellRendererText
    type AttrSetTypeConstraint CellRendererTextWrapModePropertyInfo = (~) Pango.Enums.WrapMode
    type AttrTransferTypeConstraint CellRendererTextWrapModePropertyInfo = (~) Pango.Enums.WrapMode
    type AttrTransferType CellRendererTextWrapModePropertyInfo = Pango.Enums.WrapMode
    type AttrGetType CellRendererTextWrapModePropertyInfo = Pango.Enums.WrapMode
    type AttrLabel CellRendererTextWrapModePropertyInfo = "wrap-mode"
    type AttrOrigin CellRendererTextWrapModePropertyInfo = CellRendererText
    attrGet = getCellRendererTextWrapMode
    attrSet = setCellRendererTextWrapMode
    attrTransfer _ v = do
        return v
    attrConstruct = constructCellRendererTextWrapMode
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRendererText.wrapMode"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRendererText.html#g:attr:wrapMode"
        })
#endif

-- VVV Prop "wrap-width"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@wrap-width@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' cellRendererText #wrapWidth
-- @
getCellRendererTextWrapWidth :: (MonadIO m, IsCellRendererText o) => o -> m Int32
getCellRendererTextWrapWidth obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "wrap-width"

-- | Set the value of the “@wrap-width@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' cellRendererText [ #wrapWidth 'Data.GI.Base.Attributes.:=' value ]
-- @
setCellRendererTextWrapWidth :: (MonadIO m, IsCellRendererText o) => o -> Int32 -> m ()
setCellRendererTextWrapWidth obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyInt32 obj "wrap-width" val

-- | Construct a `GValueConstruct` with valid value for the “@wrap-width@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructCellRendererTextWrapWidth :: (IsCellRendererText o, MIO.MonadIO m) => Int32 -> m (GValueConstruct o)
constructCellRendererTextWrapWidth val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyInt32 "wrap-width" val

#if defined(ENABLE_OVERLOADING)
data CellRendererTextWrapWidthPropertyInfo
instance AttrInfo CellRendererTextWrapWidthPropertyInfo where
    type AttrAllowedOps CellRendererTextWrapWidthPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint CellRendererTextWrapWidthPropertyInfo = IsCellRendererText
    type AttrSetTypeConstraint CellRendererTextWrapWidthPropertyInfo = (~) Int32
    type AttrTransferTypeConstraint CellRendererTextWrapWidthPropertyInfo = (~) Int32
    type AttrTransferType CellRendererTextWrapWidthPropertyInfo = Int32
    type AttrGetType CellRendererTextWrapWidthPropertyInfo = Int32
    type AttrLabel CellRendererTextWrapWidthPropertyInfo = "wrap-width"
    type AttrOrigin CellRendererTextWrapWidthPropertyInfo = CellRendererText
    attrGet = getCellRendererTextWrapWidth
    attrSet = setCellRendererTextWrapWidth
    attrTransfer _ v = do
        return v
    attrConstruct = constructCellRendererTextWrapWidth
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRendererText.wrapWidth"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRendererText.html#g:attr:wrapWidth"
        })
#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList CellRendererText
type instance O.AttributeList CellRendererText = CellRendererTextAttributeList
type CellRendererTextAttributeList = ('[ '("alignSet", CellRendererTextAlignSetPropertyInfo), '("alignment", CellRendererTextAlignmentPropertyInfo), '("attributes", CellRendererTextAttributesPropertyInfo), '("background", CellRendererTextBackgroundPropertyInfo), '("backgroundGdk", CellRendererTextBackgroundGdkPropertyInfo), '("backgroundRgba", CellRendererTextBackgroundRgbaPropertyInfo), '("backgroundSet", CellRendererTextBackgroundSetPropertyInfo), '("cellBackground", Gtk.CellRenderer.CellRendererCellBackgroundPropertyInfo), '("cellBackgroundGdk", Gtk.CellRenderer.CellRendererCellBackgroundGdkPropertyInfo), '("cellBackgroundRgba", Gtk.CellRenderer.CellRendererCellBackgroundRgbaPropertyInfo), '("cellBackgroundSet", Gtk.CellRenderer.CellRendererCellBackgroundSetPropertyInfo), '("editable", CellRendererTextEditablePropertyInfo), '("editableSet", CellRendererTextEditableSetPropertyInfo), '("editing", Gtk.CellRenderer.CellRendererEditingPropertyInfo), '("ellipsize", CellRendererTextEllipsizePropertyInfo), '("ellipsizeSet", CellRendererTextEllipsizeSetPropertyInfo), '("family", CellRendererTextFamilyPropertyInfo), '("familySet", CellRendererTextFamilySetPropertyInfo), '("font", CellRendererTextFontPropertyInfo), '("fontDesc", CellRendererTextFontDescPropertyInfo), '("foreground", CellRendererTextForegroundPropertyInfo), '("foregroundGdk", CellRendererTextForegroundGdkPropertyInfo), '("foregroundRgba", CellRendererTextForegroundRgbaPropertyInfo), '("foregroundSet", CellRendererTextForegroundSetPropertyInfo), '("height", Gtk.CellRenderer.CellRendererHeightPropertyInfo), '("isExpanded", Gtk.CellRenderer.CellRendererIsExpandedPropertyInfo), '("isExpander", Gtk.CellRenderer.CellRendererIsExpanderPropertyInfo), '("language", CellRendererTextLanguagePropertyInfo), '("languageSet", CellRendererTextLanguageSetPropertyInfo), '("markup", CellRendererTextMarkupPropertyInfo), '("maxWidthChars", CellRendererTextMaxWidthCharsPropertyInfo), '("mode", Gtk.CellRenderer.CellRendererModePropertyInfo), '("placeholderText", CellRendererTextPlaceholderTextPropertyInfo), '("rise", CellRendererTextRisePropertyInfo), '("riseSet", CellRendererTextRiseSetPropertyInfo), '("scale", CellRendererTextScalePropertyInfo), '("scaleSet", CellRendererTextScaleSetPropertyInfo), '("sensitive", Gtk.CellRenderer.CellRendererSensitivePropertyInfo), '("singleParagraphMode", CellRendererTextSingleParagraphModePropertyInfo), '("size", CellRendererTextSizePropertyInfo), '("sizePoints", CellRendererTextSizePointsPropertyInfo), '("sizeSet", CellRendererTextSizeSetPropertyInfo), '("stretch", CellRendererTextStretchPropertyInfo), '("stretchSet", CellRendererTextStretchSetPropertyInfo), '("strikethrough", CellRendererTextStrikethroughPropertyInfo), '("strikethroughSet", CellRendererTextStrikethroughSetPropertyInfo), '("style", CellRendererTextStylePropertyInfo), '("styleSet", CellRendererTextStyleSetPropertyInfo), '("text", CellRendererTextTextPropertyInfo), '("underline", CellRendererTextUnderlinePropertyInfo), '("underlineSet", CellRendererTextUnderlineSetPropertyInfo), '("variant", CellRendererTextVariantPropertyInfo), '("variantSet", CellRendererTextVariantSetPropertyInfo), '("visible", Gtk.CellRenderer.CellRendererVisiblePropertyInfo), '("weight", CellRendererTextWeightPropertyInfo), '("weightSet", CellRendererTextWeightSetPropertyInfo), '("width", Gtk.CellRenderer.CellRendererWidthPropertyInfo), '("widthChars", CellRendererTextWidthCharsPropertyInfo), '("wrapMode", CellRendererTextWrapModePropertyInfo), '("wrapWidth", CellRendererTextWrapWidthPropertyInfo), '("xalign", Gtk.CellRenderer.CellRendererXalignPropertyInfo), '("xpad", Gtk.CellRenderer.CellRendererXpadPropertyInfo), '("yalign", Gtk.CellRenderer.CellRendererYalignPropertyInfo), '("ypad", Gtk.CellRenderer.CellRendererYpadPropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
cellRendererTextAlignSet :: AttrLabelProxy "alignSet"
cellRendererTextAlignSet = AttrLabelProxy

cellRendererTextAlignment :: AttrLabelProxy "alignment"
cellRendererTextAlignment = AttrLabelProxy

cellRendererTextAttributes :: AttrLabelProxy "attributes"
cellRendererTextAttributes = AttrLabelProxy

cellRendererTextBackground :: AttrLabelProxy "background"
cellRendererTextBackground = AttrLabelProxy

cellRendererTextBackgroundGdk :: AttrLabelProxy "backgroundGdk"
cellRendererTextBackgroundGdk = AttrLabelProxy

cellRendererTextBackgroundRgba :: AttrLabelProxy "backgroundRgba"
cellRendererTextBackgroundRgba = AttrLabelProxy

cellRendererTextBackgroundSet :: AttrLabelProxy "backgroundSet"
cellRendererTextBackgroundSet = AttrLabelProxy

cellRendererTextEditable :: AttrLabelProxy "editable"
cellRendererTextEditable = AttrLabelProxy

cellRendererTextEditableSet :: AttrLabelProxy "editableSet"
cellRendererTextEditableSet = AttrLabelProxy

cellRendererTextEllipsize :: AttrLabelProxy "ellipsize"
cellRendererTextEllipsize = AttrLabelProxy

cellRendererTextEllipsizeSet :: AttrLabelProxy "ellipsizeSet"
cellRendererTextEllipsizeSet = AttrLabelProxy

cellRendererTextFamily :: AttrLabelProxy "family"
cellRendererTextFamily = AttrLabelProxy

cellRendererTextFamilySet :: AttrLabelProxy "familySet"
cellRendererTextFamilySet = AttrLabelProxy

cellRendererTextFont :: AttrLabelProxy "font"
cellRendererTextFont = AttrLabelProxy

cellRendererTextFontDesc :: AttrLabelProxy "fontDesc"
cellRendererTextFontDesc = AttrLabelProxy

cellRendererTextForeground :: AttrLabelProxy "foreground"
cellRendererTextForeground = AttrLabelProxy

cellRendererTextForegroundGdk :: AttrLabelProxy "foregroundGdk"
cellRendererTextForegroundGdk = AttrLabelProxy

cellRendererTextForegroundRgba :: AttrLabelProxy "foregroundRgba"
cellRendererTextForegroundRgba = AttrLabelProxy

cellRendererTextForegroundSet :: AttrLabelProxy "foregroundSet"
cellRendererTextForegroundSet = AttrLabelProxy

cellRendererTextLanguage :: AttrLabelProxy "language"
cellRendererTextLanguage = AttrLabelProxy

cellRendererTextLanguageSet :: AttrLabelProxy "languageSet"
cellRendererTextLanguageSet = AttrLabelProxy

cellRendererTextMarkup :: AttrLabelProxy "markup"
cellRendererTextMarkup = AttrLabelProxy

cellRendererTextMaxWidthChars :: AttrLabelProxy "maxWidthChars"
cellRendererTextMaxWidthChars = AttrLabelProxy

cellRendererTextPlaceholderText :: AttrLabelProxy "placeholderText"
cellRendererTextPlaceholderText = AttrLabelProxy

cellRendererTextRise :: AttrLabelProxy "rise"
cellRendererTextRise = AttrLabelProxy

cellRendererTextRiseSet :: AttrLabelProxy "riseSet"
cellRendererTextRiseSet = AttrLabelProxy

cellRendererTextScale :: AttrLabelProxy "scale"
cellRendererTextScale = AttrLabelProxy

cellRendererTextScaleSet :: AttrLabelProxy "scaleSet"
cellRendererTextScaleSet = AttrLabelProxy

cellRendererTextSingleParagraphMode :: AttrLabelProxy "singleParagraphMode"
cellRendererTextSingleParagraphMode = AttrLabelProxy

cellRendererTextSize :: AttrLabelProxy "size"
cellRendererTextSize = AttrLabelProxy

cellRendererTextSizePoints :: AttrLabelProxy "sizePoints"
cellRendererTextSizePoints = AttrLabelProxy

cellRendererTextSizeSet :: AttrLabelProxy "sizeSet"
cellRendererTextSizeSet = AttrLabelProxy

cellRendererTextStretch :: AttrLabelProxy "stretch"
cellRendererTextStretch = AttrLabelProxy

cellRendererTextStretchSet :: AttrLabelProxy "stretchSet"
cellRendererTextStretchSet = AttrLabelProxy

cellRendererTextStrikethrough :: AttrLabelProxy "strikethrough"
cellRendererTextStrikethrough = AttrLabelProxy

cellRendererTextStrikethroughSet :: AttrLabelProxy "strikethroughSet"
cellRendererTextStrikethroughSet = AttrLabelProxy

cellRendererTextStyle :: AttrLabelProxy "style"
cellRendererTextStyle = AttrLabelProxy

cellRendererTextStyleSet :: AttrLabelProxy "styleSet"
cellRendererTextStyleSet = AttrLabelProxy

cellRendererTextText :: AttrLabelProxy "text"
cellRendererTextText = AttrLabelProxy

cellRendererTextUnderline :: AttrLabelProxy "underline"
cellRendererTextUnderline = AttrLabelProxy

cellRendererTextUnderlineSet :: AttrLabelProxy "underlineSet"
cellRendererTextUnderlineSet = AttrLabelProxy

cellRendererTextVariant :: AttrLabelProxy "variant"
cellRendererTextVariant = AttrLabelProxy

cellRendererTextVariantSet :: AttrLabelProxy "variantSet"
cellRendererTextVariantSet = AttrLabelProxy

cellRendererTextWeight :: AttrLabelProxy "weight"
cellRendererTextWeight = AttrLabelProxy

cellRendererTextWeightSet :: AttrLabelProxy "weightSet"
cellRendererTextWeightSet = AttrLabelProxy

cellRendererTextWidthChars :: AttrLabelProxy "widthChars"
cellRendererTextWidthChars = AttrLabelProxy

cellRendererTextWrapMode :: AttrLabelProxy "wrapMode"
cellRendererTextWrapMode = AttrLabelProxy

cellRendererTextWrapWidth :: AttrLabelProxy "wrapWidth"
cellRendererTextWrapWidth = AttrLabelProxy

#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList CellRendererText = CellRendererTextSignalList
type CellRendererTextSignalList = ('[ '("edited", CellRendererTextEditedSignalInfo), '("editingCanceled", Gtk.CellRenderer.CellRendererEditingCanceledSignalInfo), '("editingStarted", Gtk.CellRenderer.CellRendererEditingStartedSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo)] :: [(Symbol, *)])

#endif

-- method CellRendererText::new
-- method type : Constructor
-- Args: []
-- Lengths: []
-- returnType: Just
--               (TInterface Name { namespace = "Gtk" , name = "CellRendererText" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_cell_renderer_text_new" gtk_cell_renderer_text_new :: 
    IO (Ptr CellRendererText)

-- | Creates a new t'GI.Gtk.Objects.CellRendererText.CellRendererText'. Adjust how text is drawn using
-- object properties. Object properties can be
-- set globally (with @/g_object_set()/@). Also, with t'GI.Gtk.Objects.TreeViewColumn.TreeViewColumn',
-- you can bind a property to a value in a t'GI.Gtk.Interfaces.TreeModel.TreeModel'. For example,
-- you can bind the “text” property on the cell renderer to a string
-- value in the model, thus rendering a different string in each row
-- of the t'GI.Gtk.Objects.TreeView.TreeView'
cellRendererTextNew ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m CellRendererText
    -- ^ __Returns:__ the new cell renderer
cellRendererTextNew  = liftIO $ do
    result <- gtk_cell_renderer_text_new
    checkUnexpectedReturnNULL "cellRendererTextNew" result
    result' <- (newObject CellRendererText) result
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method CellRendererText::set_fixed_height_from_font
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "renderer"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellRendererText" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkCellRendererText"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "number_of_rows"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "Number of rows of text each cell renderer is allocated, or -1"
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

foreign import ccall "gtk_cell_renderer_text_set_fixed_height_from_font" gtk_cell_renderer_text_set_fixed_height_from_font :: 
    Ptr CellRendererText ->                 -- renderer : TInterface (Name {namespace = "Gtk", name = "CellRendererText"})
    Int32 ->                                -- number_of_rows : TBasicType TInt
    IO ()

-- | Sets the height of a renderer to explicitly be determined by the “font” and
-- “y_pad” property set on it.  Further changes in these properties do not
-- affect the height, so they must be accompanied by a subsequent call to this
-- function.  Using this function is unflexible, and should really only be used
-- if calculating the size of a cell is too slow (ie, a massive number of cells
-- displayed).  If /@numberOfRows@/ is -1, then the fixed height is unset, and
-- the height is determined by the properties again.
cellRendererTextSetFixedHeightFromFont ::
    (B.CallStack.HasCallStack, MonadIO m, IsCellRendererText a) =>
    a
    -- ^ /@renderer@/: A t'GI.Gtk.Objects.CellRendererText.CellRendererText'
    -> Int32
    -- ^ /@numberOfRows@/: Number of rows of text each cell renderer is allocated, or -1
    -> m ()
cellRendererTextSetFixedHeightFromFont renderer numberOfRows = liftIO $ do
    renderer' <- unsafeManagedPtrCastPtr renderer
    gtk_cell_renderer_text_set_fixed_height_from_font renderer' numberOfRows
    touchManagedPtr renderer
    return ()

#if defined(ENABLE_OVERLOADING)
data CellRendererTextSetFixedHeightFromFontMethodInfo
instance (signature ~ (Int32 -> m ()), MonadIO m, IsCellRendererText a) => O.OverloadedMethod CellRendererTextSetFixedHeightFromFontMethodInfo a signature where
    overloadedMethod = cellRendererTextSetFixedHeightFromFont

instance O.OverloadedMethodInfo CellRendererTextSetFixedHeightFromFontMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRendererText.cellRendererTextSetFixedHeightFromFont",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRendererText.html#v:cellRendererTextSetFixedHeightFromFont"
        })


#endif


