{-# LANGUAGE ImplicitParams, RankNTypes, TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- The t'GI.Gtk.Objects.CellArea.CellArea' is an abstract class for t'GI.Gtk.Interfaces.CellLayout.CellLayout' widgets
-- (also referred to as \"layouting widgets\") to interface with an
-- arbitrary number of @/GtkCellRenderers/@ and interact with the user
-- for a given t'GI.Gtk.Interfaces.TreeModel.TreeModel' row.
-- 
-- The cell area handles events, focus navigation, drawing and
-- size requests and allocations for a given row of data.
-- 
-- Usually users dont have to interact with the t'GI.Gtk.Objects.CellArea.CellArea' directly
-- unless they are implementing a cell-layouting widget themselves.
-- 
-- = Requesting area sizes
-- 
-- As outlined in
-- [GtkWidget’s geometry management section][geometry-management],
-- GTK+ uses a height-for-width
-- geometry management system to compute the sizes of widgets and user
-- interfaces. t'GI.Gtk.Objects.CellArea.CellArea' uses the same semantics to calculate the
-- size of an area for an arbitrary number of t'GI.Gtk.Interfaces.TreeModel.TreeModel' rows.
-- 
-- When requesting the size of a cell area one needs to calculate
-- the size for a handful of rows, and this will be done differently by
-- different layouting widgets. For instance a t'GI.Gtk.Objects.TreeViewColumn.TreeViewColumn'
-- always lines up the areas from top to bottom while a t'GI.Gtk.Objects.IconView.IconView'
-- on the other hand might enforce that all areas received the same
-- width and wrap the areas around, requesting height for more cell
-- areas when allocated less width.
-- 
-- It’s also important for areas to maintain some cell
-- alignments with areas rendered for adjacent rows (cells can
-- appear “columnized” inside an area even when the size of
-- cells are different in each row). For this reason the t'GI.Gtk.Objects.CellArea.CellArea'
-- uses a t'GI.Gtk.Objects.CellAreaContext.CellAreaContext' object to store the alignments
-- and sizes along the way (as well as the overall largest minimum
-- and natural size for all the rows which have been calculated
-- with the said context).
-- 
-- The t'GI.Gtk.Objects.CellAreaContext.CellAreaContext' is an opaque object specific to the
-- t'GI.Gtk.Objects.CellArea.CellArea' which created it (see 'GI.Gtk.Objects.CellArea.cellAreaCreateContext').
-- The owning cell-layouting widget can create as many contexts as
-- it wishes to calculate sizes of rows which should receive the
-- same size in at least one orientation (horizontally or vertically),
-- However, it’s important that the same t'GI.Gtk.Objects.CellAreaContext.CellAreaContext' which
-- was used to request the sizes for a given t'GI.Gtk.Interfaces.TreeModel.TreeModel' row be
-- used when rendering or processing events for that row.
-- 
-- In order to request the width of all the rows at the root level
-- of a t'GI.Gtk.Interfaces.TreeModel.TreeModel' one would do the following:
-- 
-- 
-- === /C code/
-- >
-- >GtkTreeIter iter;
-- >gint        minimum_width;
-- >gint        natural_width;
-- >
-- >valid = gtk_tree_model_get_iter_first (model, &iter);
-- >while (valid)
-- >  {
-- >    gtk_cell_area_apply_attributes (area, model, &iter, FALSE, FALSE);
-- >    gtk_cell_area_get_preferred_width (area, context, widget, NULL, NULL);
-- >
-- >    valid = gtk_tree_model_iter_next (model, &iter);
-- >  }
-- >gtk_cell_area_context_get_preferred_width (context, &minimum_width, &natural_width);
-- 
-- 
-- Note that in this example it’s not important to observe the
-- returned minimum and natural width of the area for each row
-- unless the cell-layouting object is actually interested in the
-- widths of individual rows. The overall width is however stored
-- in the accompanying t'GI.Gtk.Objects.CellAreaContext.CellAreaContext' object and can be consulted
-- at any time.
-- 
-- This can be useful since t'GI.Gtk.Interfaces.CellLayout.CellLayout' widgets usually have to
-- support requesting and rendering rows in treemodels with an
-- exceedingly large amount of rows. The t'GI.Gtk.Interfaces.CellLayout.CellLayout' widget in
-- that case would calculate the required width of the rows in an
-- idle or timeout source (see @/g_timeout_add()/@) and when the widget
-- is requested its actual width in t'GI.Gtk.Structs.WidgetClass.WidgetClass'.@/get_preferred_width/@()
-- it can simply consult the width accumulated so far in the
-- t'GI.Gtk.Objects.CellAreaContext.CellAreaContext' object.
-- 
-- A simple example where rows are rendered from top to bottom and
-- take up the full width of the layouting widget would look like:
-- 
-- 
-- === /C code/
-- >
-- >static void
-- >foo_get_preferred_width (GtkWidget       *widget,
-- >                         gint            *minimum_size,
-- >                         gint            *natural_size)
-- >{
-- >  Foo        *foo  = FOO (widget);
-- >  FooPrivate *priv = foo->priv;
-- >
-- >  foo_ensure_at_least_one_handfull_of_rows_have_been_requested (foo);
-- >
-- >  gtk_cell_area_context_get_preferred_width (priv->context, minimum_size, natural_size);
-- >}
-- 
-- 
-- In the above example the Foo widget has to make sure that some
-- row sizes have been calculated (the amount of rows that Foo judged
-- was appropriate to request space for in a single timeout iteration)
-- before simply returning the amount of space required by the area via
-- the t'GI.Gtk.Objects.CellAreaContext.CellAreaContext'.
-- 
-- Requesting the height for width (or width for height) of an area is
-- a similar task except in this case the t'GI.Gtk.Objects.CellAreaContext.CellAreaContext' does not
-- store the data (actually, it does not know how much space the layouting
-- widget plans to allocate it for every row. It’s up to the layouting
-- widget to render each row of data with the appropriate height and
-- width which was requested by the t'GI.Gtk.Objects.CellArea.CellArea').
-- 
-- In order to request the height for width of all the rows at the
-- root level of a t'GI.Gtk.Interfaces.TreeModel.TreeModel' one would do the following:
-- 
-- 
-- === /C code/
-- >
-- >GtkTreeIter iter;
-- >gint        minimum_height;
-- >gint        natural_height;
-- >gint        full_minimum_height = 0;
-- >gint        full_natural_height = 0;
-- >
-- >valid = gtk_tree_model_get_iter_first (model, &iter);
-- >while (valid)
-- >  {
-- >    gtk_cell_area_apply_attributes (area, model, &iter, FALSE, FALSE);
-- >    gtk_cell_area_get_preferred_height_for_width (area, context, widget,
-- >                                                  width, &minimum_height, &natural_height);
-- >
-- >    if (width_is_for_allocation)
-- >       cache_row_height (&iter, minimum_height, natural_height);
-- >
-- >    full_minimum_height += minimum_height;
-- >    full_natural_height += natural_height;
-- >
-- >    valid = gtk_tree_model_iter_next (model, &iter);
-- >  }
-- 
-- 
-- Note that in the above example we would need to cache the heights
-- returned for each row so that we would know what sizes to render the
-- areas for each row. However we would only want to really cache the
-- heights if the request is intended for the layouting widgets real
-- allocation.
-- 
-- In some cases the layouting widget is requested the height for an
-- arbitrary for_width, this is a special case for layouting widgets
-- who need to request size for tens of thousands  of rows. For this
-- case it’s only important that the layouting widget calculate
-- one reasonably sized chunk of rows and return that height
-- synchronously. The reasoning here is that any layouting widget is
-- at least capable of synchronously calculating enough height to fill
-- the screen height (or scrolled window height) in response to a single
-- call to t'GI.Gtk.Structs.WidgetClass.WidgetClass'.@/get_preferred_height_for_width/@(). Returning
-- a perfect height for width that is larger than the screen area is
-- inconsequential since after the layouting receives an allocation
-- from a scrolled window it simply continues to drive the scrollbar
-- values while more and more height is required for the row heights
-- that are calculated in the background.
-- 
-- = Rendering Areas
-- 
-- Once area sizes have been aquired at least for the rows in the
-- visible area of the layouting widget they can be rendered at
-- t'GI.Gtk.Structs.WidgetClass.WidgetClass'.@/draw/@() time.
-- 
-- A crude example of how to render all the rows at the root level
-- runs as follows:
-- 
-- 
-- === /C code/
-- >
-- >GtkAllocation allocation;
-- >GdkRectangle  cell_area = { 0, };
-- >GtkTreeIter   iter;
-- >gint          minimum_width;
-- >gint          natural_width;
-- >
-- >gtk_widget_get_allocation (widget, &allocation);
-- >cell_area.width = allocation.width;
-- >
-- >valid = gtk_tree_model_get_iter_first (model, &iter);
-- >while (valid)
-- >  {
-- >    cell_area.height = get_cached_height_for_row (&iter);
-- >
-- >    gtk_cell_area_apply_attributes (area, model, &iter, FALSE, FALSE);
-- >    gtk_cell_area_render (area, context, widget, cr,
-- >                          &cell_area, &cell_area, state_flags, FALSE);
-- >
-- >    cell_area.y += cell_area.height;
-- >
-- >    valid = gtk_tree_model_iter_next (model, &iter);
-- >  }
-- 
-- 
-- Note that the cached height in this example really depends on how
-- the layouting widget works. The layouting widget might decide to
-- give every row its minimum or natural height or, if the model content
-- is expected to fit inside the layouting widget without scrolling, it
-- would make sense to calculate the allocation for each row at
-- [Widget::sizeAllocate]("GI.Gtk.Objects.Widget#g:signal:sizeAllocate") time using 'GI.Gtk.Functions.distributeNaturalAllocation'.
-- 
-- = Handling Events and Driving Keyboard Focus
-- 
-- Passing events to the area is as simple as handling events on any
-- normal widget and then passing them to the 'GI.Gtk.Objects.CellArea.cellAreaEvent'
-- API as they come in. Usually t'GI.Gtk.Objects.CellArea.CellArea' is only interested in
-- button events, however some customized derived areas can be implemented
-- who are interested in handling other events. Handling an event can
-- trigger the [CellArea::focusChanged]("GI.Gtk.Objects.CellArea#g:signal:focusChanged") signal to fire; as well as
-- [CellArea::addEditable]("GI.Gtk.Objects.CellArea#g:signal:addEditable") in the case that an editable cell was
-- clicked and needs to start editing. You can call
-- 'GI.Gtk.Objects.CellArea.cellAreaStopEditing' at any time to cancel any cell editing
-- that is currently in progress.
-- 
-- The t'GI.Gtk.Objects.CellArea.CellArea' drives keyboard focus from cell to cell in a way
-- similar to t'GI.Gtk.Objects.Widget.Widget'. For layouting widgets that support giving
-- focus to cells it’s important to remember to pass 'GI.Gtk.Flags.CellRendererStateFocused'
-- to the area functions for the row that has focus and to tell the
-- area to paint the focus at render time.
-- 
-- Layouting widgets that accept focus on cells should implement the
-- t'GI.Gtk.Structs.WidgetClass.WidgetClass'.@/focus/@() virtual method. The layouting widget is always
-- responsible for knowing where t'GI.Gtk.Interfaces.TreeModel.TreeModel' rows are rendered inside
-- the widget, so at t'GI.Gtk.Structs.WidgetClass.WidgetClass'.@/focus/@() time the layouting widget
-- should use the t'GI.Gtk.Objects.CellArea.CellArea' methods to navigate focus inside the area
-- and then observe the GtkDirectionType to pass the focus to adjacent
-- rows and areas.
-- 
-- A basic example of how the t'GI.Gtk.Structs.WidgetClass.WidgetClass'.@/focus/@() virtual method
-- should be implemented:
-- 
-- 
-- === /C code/
-- >
-- >static gboolean
-- >foo_focus (GtkWidget       *widget,
-- >           GtkDirectionType direction)
-- >{
-- >  Foo        *foo  = FOO (widget);
-- >  FooPrivate *priv = foo->priv;
-- >  gint        focus_row;
-- >  gboolean    have_focus = FALSE;
-- >
-- >  focus_row = priv->focus_row;
-- >
-- >  if (!gtk_widget_has_focus (widget))
-- >    gtk_widget_grab_focus (widget);
-- >
-- >  valid = gtk_tree_model_iter_nth_child (priv->model, &iter, NULL, priv->focus_row);
-- >  while (valid)
-- >    {
-- >      gtk_cell_area_apply_attributes (priv->area, priv->model, &iter, FALSE, FALSE);
-- >
-- >      if (gtk_cell_area_focus (priv->area, direction))
-- >        {
-- >           priv->focus_row = focus_row;
-- >           have_focus = TRUE;
-- >           break;
-- >        }
-- >      else
-- >        {
-- >          if (direction == GTK_DIR_RIGHT ||
-- >              direction == GTK_DIR_LEFT)
-- >            break;
-- >          else if (direction == GTK_DIR_UP ||
-- >                   direction == GTK_DIR_TAB_BACKWARD)
-- >           {
-- >              if (focus_row == 0)
-- >                break;
-- >              else
-- >               {
-- >                  focus_row--;
-- >                  valid = gtk_tree_model_iter_nth_child (priv->model, &iter, NULL, focus_row);
-- >               }
-- >            }
-- >          else
-- >            {
-- >              if (focus_row == last_row)
-- >                break;
-- >              else
-- >                {
-- >                  focus_row++;
-- >                  valid = gtk_tree_model_iter_next (priv->model, &iter);
-- >                }
-- >            }
-- >        }
-- >    }
-- >    return have_focus;
-- >}
-- 
-- 
-- Note that the layouting widget is responsible for matching the
-- GtkDirectionType values to the way it lays out its cells.
-- 
-- = Cell Properties
-- 
-- The t'GI.Gtk.Objects.CellArea.CellArea' introduces cell properties for @/GtkCellRenderers/@
-- in very much the same way that t'GI.Gtk.Objects.Container.Container' introduces
-- [child properties][child-properties]
-- for @/GtkWidgets/@. This provides some general interfaces for defining
-- the relationship cell areas have with their cells. For instance in a
-- t'GI.Gtk.Objects.CellAreaBox.CellAreaBox' a cell might “expand” and receive extra space when
-- the area is allocated more than its full natural request, or a cell
-- might be configured to “align” with adjacent rows which were requested
-- and rendered with the same t'GI.Gtk.Objects.CellAreaContext.CellAreaContext'.
-- 
-- Use 'GI.Gtk.Structs.CellAreaClass.cellAreaClassInstallCellProperty' to install cell
-- properties for a cell area class and 'GI.Gtk.Structs.CellAreaClass.cellAreaClassFindCellProperty'
-- or 'GI.Gtk.Structs.CellAreaClass.cellAreaClassListCellProperties' to get information about
-- existing cell properties.
-- 
-- To set the value of a cell property, use 'GI.Gtk.Objects.CellArea.cellAreaCellSetProperty',
-- @/gtk_cell_area_cell_set()/@ or @/gtk_cell_area_cell_set_valist()/@. To obtain
-- the value of a cell property, use 'GI.Gtk.Objects.CellArea.cellAreaCellGetProperty',
-- @/gtk_cell_area_cell_get()/@ or @/gtk_cell_area_cell_get_valist()/@.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.CellArea
    ( 

-- * Exported types
    CellArea(..)                            ,
    IsCellArea                              ,
    toCellArea                              ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [activate]("GI.Gtk.Objects.CellArea#g:method:activate"), [activateCell]("GI.Gtk.Objects.CellArea#g:method:activateCell"), [add]("GI.Gtk.Objects.CellArea#g:method:add"), [addAttribute]("GI.Gtk.Interfaces.CellLayout#g:method:addAttribute"), [addChild]("GI.Gtk.Interfaces.Buildable#g:method:addChild"), [addFocusSibling]("GI.Gtk.Objects.CellArea#g:method:addFocusSibling"), [applyAttributes]("GI.Gtk.Objects.CellArea#g:method:applyAttributes"), [attributeConnect]("GI.Gtk.Objects.CellArea#g:method:attributeConnect"), [attributeDisconnect]("GI.Gtk.Objects.CellArea#g:method:attributeDisconnect"), [attributeGetColumn]("GI.Gtk.Objects.CellArea#g:method:attributeGetColumn"), [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [cellGetProperty]("GI.Gtk.Objects.CellArea#g:method:cellGetProperty"), [cellSetProperty]("GI.Gtk.Objects.CellArea#g:method:cellSetProperty"), [clear]("GI.Gtk.Interfaces.CellLayout#g:method:clear"), [clearAttributes]("GI.Gtk.Interfaces.CellLayout#g:method:clearAttributes"), [constructChild]("GI.Gtk.Interfaces.Buildable#g:method:constructChild"), [copyContext]("GI.Gtk.Objects.CellArea#g:method:copyContext"), [createContext]("GI.Gtk.Objects.CellArea#g:method:createContext"), [customFinished]("GI.Gtk.Interfaces.Buildable#g:method:customFinished"), [customTagEnd]("GI.Gtk.Interfaces.Buildable#g:method:customTagEnd"), [customTagStart]("GI.Gtk.Interfaces.Buildable#g:method:customTagStart"), [event]("GI.Gtk.Objects.CellArea#g:method:event"), [focus]("GI.Gtk.Objects.CellArea#g:method:focus"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [foreach]("GI.Gtk.Objects.CellArea#g:method:foreach"), [foreachAlloc]("GI.Gtk.Objects.CellArea#g:method:foreachAlloc"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [hasRenderer]("GI.Gtk.Objects.CellArea#g:method:hasRenderer"), [innerCellArea]("GI.Gtk.Objects.CellArea#g:method:innerCellArea"), [isActivatable]("GI.Gtk.Objects.CellArea#g:method:isActivatable"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [isFocusSibling]("GI.Gtk.Objects.CellArea#g:method:isFocusSibling"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [packEnd]("GI.Gtk.Interfaces.CellLayout#g:method:packEnd"), [packStart]("GI.Gtk.Interfaces.CellLayout#g:method:packStart"), [parserFinished]("GI.Gtk.Interfaces.Buildable#g:method:parserFinished"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [remove]("GI.Gtk.Objects.CellArea#g:method:remove"), [removeFocusSibling]("GI.Gtk.Objects.CellArea#g:method:removeFocusSibling"), [render]("GI.Gtk.Objects.CellArea#g:method:render"), [reorder]("GI.Gtk.Interfaces.CellLayout#g:method:reorder"), [requestRenderer]("GI.Gtk.Objects.CellArea#g:method:requestRenderer"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [stopEditing]("GI.Gtk.Objects.CellArea#g:method:stopEditing"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getArea]("GI.Gtk.Interfaces.CellLayout#g:method:getArea"), [getCellAllocation]("GI.Gtk.Objects.CellArea#g:method:getCellAllocation"), [getCellAtPosition]("GI.Gtk.Objects.CellArea#g:method:getCellAtPosition"), [getCells]("GI.Gtk.Interfaces.CellLayout#g:method:getCells"), [getCurrentPathString]("GI.Gtk.Objects.CellArea#g:method:getCurrentPathString"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getEditWidget]("GI.Gtk.Objects.CellArea#g:method:getEditWidget"), [getEditedCell]("GI.Gtk.Objects.CellArea#g:method:getEditedCell"), [getFocusCell]("GI.Gtk.Objects.CellArea#g:method:getFocusCell"), [getFocusFromSibling]("GI.Gtk.Objects.CellArea#g:method:getFocusFromSibling"), [getFocusSiblings]("GI.Gtk.Objects.CellArea#g:method:getFocusSiblings"), [getInternalChild]("GI.Gtk.Interfaces.Buildable#g:method:getInternalChild"), [getName]("GI.Gtk.Interfaces.Buildable#g:method:getName"), [getPreferredHeight]("GI.Gtk.Objects.CellArea#g:method:getPreferredHeight"), [getPreferredHeightForWidth]("GI.Gtk.Objects.CellArea#g:method:getPreferredHeightForWidth"), [getPreferredWidth]("GI.Gtk.Objects.CellArea#g:method:getPreferredWidth"), [getPreferredWidthForHeight]("GI.Gtk.Objects.CellArea#g:method:getPreferredWidthForHeight"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getRequestMode]("GI.Gtk.Objects.CellArea#g:method:getRequestMode").
-- 
-- ==== Setters
-- [setBuildableProperty]("GI.Gtk.Interfaces.Buildable#g:method:setBuildableProperty"), [setCellDataFunc]("GI.Gtk.Interfaces.CellLayout#g:method:setCellDataFunc"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setFocusCell]("GI.Gtk.Objects.CellArea#g:method:setFocusCell"), [setName]("GI.Gtk.Interfaces.Buildable#g:method:setName"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty").

#if defined(ENABLE_OVERLOADING)
    ResolveCellAreaMethod                   ,
#endif

-- ** activate #method:activate#

#if defined(ENABLE_OVERLOADING)
    CellAreaActivateMethodInfo              ,
#endif
    cellAreaActivate                        ,


-- ** activateCell #method:activateCell#

#if defined(ENABLE_OVERLOADING)
    CellAreaActivateCellMethodInfo          ,
#endif
    cellAreaActivateCell                    ,


-- ** add #method:add#

#if defined(ENABLE_OVERLOADING)
    CellAreaAddMethodInfo                   ,
#endif
    cellAreaAdd                             ,


-- ** addFocusSibling #method:addFocusSibling#

#if defined(ENABLE_OVERLOADING)
    CellAreaAddFocusSiblingMethodInfo       ,
#endif
    cellAreaAddFocusSibling                 ,


-- ** applyAttributes #method:applyAttributes#

#if defined(ENABLE_OVERLOADING)
    CellAreaApplyAttributesMethodInfo       ,
#endif
    cellAreaApplyAttributes                 ,


-- ** attributeConnect #method:attributeConnect#

#if defined(ENABLE_OVERLOADING)
    CellAreaAttributeConnectMethodInfo      ,
#endif
    cellAreaAttributeConnect                ,


-- ** attributeDisconnect #method:attributeDisconnect#

#if defined(ENABLE_OVERLOADING)
    CellAreaAttributeDisconnectMethodInfo   ,
#endif
    cellAreaAttributeDisconnect             ,


-- ** attributeGetColumn #method:attributeGetColumn#

#if defined(ENABLE_OVERLOADING)
    CellAreaAttributeGetColumnMethodInfo    ,
#endif
    cellAreaAttributeGetColumn              ,


-- ** cellGetProperty #method:cellGetProperty#

#if defined(ENABLE_OVERLOADING)
    CellAreaCellGetPropertyMethodInfo       ,
#endif
    cellAreaCellGetProperty                 ,


-- ** cellSetProperty #method:cellSetProperty#

#if defined(ENABLE_OVERLOADING)
    CellAreaCellSetPropertyMethodInfo       ,
#endif
    cellAreaCellSetProperty                 ,


-- ** copyContext #method:copyContext#

#if defined(ENABLE_OVERLOADING)
    CellAreaCopyContextMethodInfo           ,
#endif
    cellAreaCopyContext                     ,


-- ** createContext #method:createContext#

#if defined(ENABLE_OVERLOADING)
    CellAreaCreateContextMethodInfo         ,
#endif
    cellAreaCreateContext                   ,


-- ** event #method:event#

#if defined(ENABLE_OVERLOADING)
    CellAreaEventMethodInfo                 ,
#endif
    cellAreaEvent                           ,


-- ** focus #method:focus#

#if defined(ENABLE_OVERLOADING)
    CellAreaFocusMethodInfo                 ,
#endif
    cellAreaFocus                           ,


-- ** foreach #method:foreach#

#if defined(ENABLE_OVERLOADING)
    CellAreaForeachMethodInfo               ,
#endif
    cellAreaForeach                         ,


-- ** foreachAlloc #method:foreachAlloc#

#if defined(ENABLE_OVERLOADING)
    CellAreaForeachAllocMethodInfo          ,
#endif
    cellAreaForeachAlloc                    ,


-- ** getCellAllocation #method:getCellAllocation#

#if defined(ENABLE_OVERLOADING)
    CellAreaGetCellAllocationMethodInfo     ,
#endif
    cellAreaGetCellAllocation               ,


-- ** getCellAtPosition #method:getCellAtPosition#

#if defined(ENABLE_OVERLOADING)
    CellAreaGetCellAtPositionMethodInfo     ,
#endif
    cellAreaGetCellAtPosition               ,


-- ** getCurrentPathString #method:getCurrentPathString#

#if defined(ENABLE_OVERLOADING)
    CellAreaGetCurrentPathStringMethodInfo  ,
#endif
    cellAreaGetCurrentPathString            ,


-- ** getEditWidget #method:getEditWidget#

#if defined(ENABLE_OVERLOADING)
    CellAreaGetEditWidgetMethodInfo         ,
#endif
    cellAreaGetEditWidget                   ,


-- ** getEditedCell #method:getEditedCell#

#if defined(ENABLE_OVERLOADING)
    CellAreaGetEditedCellMethodInfo         ,
#endif
    cellAreaGetEditedCell                   ,


-- ** getFocusCell #method:getFocusCell#

#if defined(ENABLE_OVERLOADING)
    CellAreaGetFocusCellMethodInfo          ,
#endif
    cellAreaGetFocusCell                    ,


-- ** getFocusFromSibling #method:getFocusFromSibling#

#if defined(ENABLE_OVERLOADING)
    CellAreaGetFocusFromSiblingMethodInfo   ,
#endif
    cellAreaGetFocusFromSibling             ,


-- ** getFocusSiblings #method:getFocusSiblings#

#if defined(ENABLE_OVERLOADING)
    CellAreaGetFocusSiblingsMethodInfo      ,
#endif
    cellAreaGetFocusSiblings                ,


-- ** getPreferredHeight #method:getPreferredHeight#

#if defined(ENABLE_OVERLOADING)
    CellAreaGetPreferredHeightMethodInfo    ,
#endif
    cellAreaGetPreferredHeight              ,


-- ** getPreferredHeightForWidth #method:getPreferredHeightForWidth#

#if defined(ENABLE_OVERLOADING)
    CellAreaGetPreferredHeightForWidthMethodInfo,
#endif
    cellAreaGetPreferredHeightForWidth      ,


-- ** getPreferredWidth #method:getPreferredWidth#

#if defined(ENABLE_OVERLOADING)
    CellAreaGetPreferredWidthMethodInfo     ,
#endif
    cellAreaGetPreferredWidth               ,


-- ** getPreferredWidthForHeight #method:getPreferredWidthForHeight#

#if defined(ENABLE_OVERLOADING)
    CellAreaGetPreferredWidthForHeightMethodInfo,
#endif
    cellAreaGetPreferredWidthForHeight      ,


-- ** getRequestMode #method:getRequestMode#

#if defined(ENABLE_OVERLOADING)
    CellAreaGetRequestModeMethodInfo        ,
#endif
    cellAreaGetRequestMode                  ,


-- ** hasRenderer #method:hasRenderer#

#if defined(ENABLE_OVERLOADING)
    CellAreaHasRendererMethodInfo           ,
#endif
    cellAreaHasRenderer                     ,


-- ** innerCellArea #method:innerCellArea#

#if defined(ENABLE_OVERLOADING)
    CellAreaInnerCellAreaMethodInfo         ,
#endif
    cellAreaInnerCellArea                   ,


-- ** isActivatable #method:isActivatable#

#if defined(ENABLE_OVERLOADING)
    CellAreaIsActivatableMethodInfo         ,
#endif
    cellAreaIsActivatable                   ,


-- ** isFocusSibling #method:isFocusSibling#

#if defined(ENABLE_OVERLOADING)
    CellAreaIsFocusSiblingMethodInfo        ,
#endif
    cellAreaIsFocusSibling                  ,


-- ** remove #method:remove#

#if defined(ENABLE_OVERLOADING)
    CellAreaRemoveMethodInfo                ,
#endif
    cellAreaRemove                          ,


-- ** removeFocusSibling #method:removeFocusSibling#

#if defined(ENABLE_OVERLOADING)
    CellAreaRemoveFocusSiblingMethodInfo    ,
#endif
    cellAreaRemoveFocusSibling              ,


-- ** render #method:render#

#if defined(ENABLE_OVERLOADING)
    CellAreaRenderMethodInfo                ,
#endif
    cellAreaRender                          ,


-- ** requestRenderer #method:requestRenderer#

#if defined(ENABLE_OVERLOADING)
    CellAreaRequestRendererMethodInfo       ,
#endif
    cellAreaRequestRenderer                 ,


-- ** setFocusCell #method:setFocusCell#

#if defined(ENABLE_OVERLOADING)
    CellAreaSetFocusCellMethodInfo          ,
#endif
    cellAreaSetFocusCell                    ,


-- ** stopEditing #method:stopEditing#

#if defined(ENABLE_OVERLOADING)
    CellAreaStopEditingMethodInfo           ,
#endif
    cellAreaStopEditing                     ,




 -- * Properties


-- ** editWidget #attr:editWidget#
-- | The widget currently editing the edited cell
-- 
-- This property is read-only and only changes as
-- a result of a call 'GI.Gtk.Objects.CellArea.cellAreaActivateCell'.
-- 
-- /Since: 3.0/

#if defined(ENABLE_OVERLOADING)
    CellAreaEditWidgetPropertyInfo          ,
#endif
#if defined(ENABLE_OVERLOADING)
    cellAreaEditWidget                      ,
#endif
    getCellAreaEditWidget                   ,


-- ** editedCell #attr:editedCell#
-- | The cell in the area that is currently edited
-- 
-- This property is read-only and only changes as
-- a result of a call 'GI.Gtk.Objects.CellArea.cellAreaActivateCell'.
-- 
-- /Since: 3.0/

#if defined(ENABLE_OVERLOADING)
    CellAreaEditedCellPropertyInfo          ,
#endif
#if defined(ENABLE_OVERLOADING)
    cellAreaEditedCell                      ,
#endif
    getCellAreaEditedCell                   ,


-- ** focusCell #attr:focusCell#
-- | The cell in the area that currently has focus
-- 
-- /Since: 3.0/

#if defined(ENABLE_OVERLOADING)
    CellAreaFocusCellPropertyInfo           ,
#endif
#if defined(ENABLE_OVERLOADING)
    cellAreaFocusCell                       ,
#endif
    constructCellAreaFocusCell              ,
    getCellAreaFocusCell                    ,
    setCellAreaFocusCell                    ,




 -- * Signals


-- ** addEditable #signal:addEditable#

    CellAreaAddEditableCallback             ,
#if defined(ENABLE_OVERLOADING)
    CellAreaAddEditableSignalInfo           ,
#endif
    afterCellAreaAddEditable                ,
    onCellAreaAddEditable                   ,


-- ** applyAttributes #signal:applyAttributes#

    CellAreaApplyAttributesCallback         ,
#if defined(ENABLE_OVERLOADING)
    CellAreaApplyAttributesSignalInfo       ,
#endif
    afterCellAreaApplyAttributes            ,
    onCellAreaApplyAttributes               ,


-- ** focusChanged #signal:focusChanged#

    CellAreaFocusChangedCallback            ,
#if defined(ENABLE_OVERLOADING)
    CellAreaFocusChangedSignalInfo          ,
#endif
    afterCellAreaFocusChanged               ,
    onCellAreaFocusChanged                  ,


-- ** removeEditable #signal:removeEditable#

    CellAreaRemoveEditableCallback          ,
#if defined(ENABLE_OVERLOADING)
    CellAreaRemoveEditableSignalInfo        ,
#endif
    afterCellAreaRemoveEditable             ,
    onCellAreaRemoveEditable                ,




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

import qualified GI.Cairo.Structs.Context as Cairo.Context
import qualified GI.GObject.Objects.Object as GObject.Object
import qualified GI.Gdk.Structs.Rectangle as Gdk.Rectangle
import qualified GI.Gdk.Unions.Event as Gdk.Event
import qualified GI.Gtk.Callbacks as Gtk.Callbacks
import {-# SOURCE #-} qualified GI.Gtk.Enums as Gtk.Enums
import {-# SOURCE #-} qualified GI.Gtk.Flags as Gtk.Flags
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.Buildable as Gtk.Buildable
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.CellEditable as Gtk.CellEditable
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.CellLayout as Gtk.CellLayout
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.TreeModel as Gtk.TreeModel
import {-# SOURCE #-} qualified GI.Gtk.Objects.CellAreaContext as Gtk.CellAreaContext
import {-# SOURCE #-} qualified GI.Gtk.Objects.CellRenderer as Gtk.CellRenderer
import {-# SOURCE #-} qualified GI.Gtk.Objects.Widget as Gtk.Widget
import {-# SOURCE #-} qualified GI.Gtk.Structs.TreeIter as Gtk.TreeIter

-- | Memory-managed wrapper type.
newtype CellArea = CellArea (SP.ManagedPtr CellArea)
    deriving (Eq)

instance SP.ManagedPtrNewtype CellArea where
    toManagedPtr (CellArea p) = p

foreign import ccall "gtk_cell_area_get_type"
    c_gtk_cell_area_get_type :: IO B.Types.GType

instance B.Types.TypedObject CellArea where
    glibType = c_gtk_cell_area_get_type

instance B.Types.GObject CellArea

-- | Type class for types which can be safely cast to `CellArea`, for instance with `toCellArea`.
class (SP.GObject o, O.IsDescendantOf CellArea o) => IsCellArea o
instance (SP.GObject o, O.IsDescendantOf CellArea o) => IsCellArea o

instance O.HasParentTypes CellArea
type instance O.ParentTypes CellArea = '[GObject.Object.Object, Gtk.Buildable.Buildable, Gtk.CellLayout.CellLayout]

-- | Cast to `CellArea`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toCellArea :: (MIO.MonadIO m, IsCellArea o) => o -> m CellArea
toCellArea = MIO.liftIO . B.ManagedPtr.unsafeCastTo CellArea

-- | Convert 'CellArea' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe CellArea) where
    gvalueGType_ = c_gtk_cell_area_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr CellArea)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr CellArea)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject CellArea ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveCellAreaMethod (t :: Symbol) (o :: *) :: * where
    ResolveCellAreaMethod "activate" o = CellAreaActivateMethodInfo
    ResolveCellAreaMethod "activateCell" o = CellAreaActivateCellMethodInfo
    ResolveCellAreaMethod "add" o = CellAreaAddMethodInfo
    ResolveCellAreaMethod "addAttribute" o = Gtk.CellLayout.CellLayoutAddAttributeMethodInfo
    ResolveCellAreaMethod "addChild" o = Gtk.Buildable.BuildableAddChildMethodInfo
    ResolveCellAreaMethod "addFocusSibling" o = CellAreaAddFocusSiblingMethodInfo
    ResolveCellAreaMethod "applyAttributes" o = CellAreaApplyAttributesMethodInfo
    ResolveCellAreaMethod "attributeConnect" o = CellAreaAttributeConnectMethodInfo
    ResolveCellAreaMethod "attributeDisconnect" o = CellAreaAttributeDisconnectMethodInfo
    ResolveCellAreaMethod "attributeGetColumn" o = CellAreaAttributeGetColumnMethodInfo
    ResolveCellAreaMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveCellAreaMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveCellAreaMethod "cellGetProperty" o = CellAreaCellGetPropertyMethodInfo
    ResolveCellAreaMethod "cellSetProperty" o = CellAreaCellSetPropertyMethodInfo
    ResolveCellAreaMethod "clear" o = Gtk.CellLayout.CellLayoutClearMethodInfo
    ResolveCellAreaMethod "clearAttributes" o = Gtk.CellLayout.CellLayoutClearAttributesMethodInfo
    ResolveCellAreaMethod "constructChild" o = Gtk.Buildable.BuildableConstructChildMethodInfo
    ResolveCellAreaMethod "copyContext" o = CellAreaCopyContextMethodInfo
    ResolveCellAreaMethod "createContext" o = CellAreaCreateContextMethodInfo
    ResolveCellAreaMethod "customFinished" o = Gtk.Buildable.BuildableCustomFinishedMethodInfo
    ResolveCellAreaMethod "customTagEnd" o = Gtk.Buildable.BuildableCustomTagEndMethodInfo
    ResolveCellAreaMethod "customTagStart" o = Gtk.Buildable.BuildableCustomTagStartMethodInfo
    ResolveCellAreaMethod "event" o = CellAreaEventMethodInfo
    ResolveCellAreaMethod "focus" o = CellAreaFocusMethodInfo
    ResolveCellAreaMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveCellAreaMethod "foreach" o = CellAreaForeachMethodInfo
    ResolveCellAreaMethod "foreachAlloc" o = CellAreaForeachAllocMethodInfo
    ResolveCellAreaMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveCellAreaMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveCellAreaMethod "hasRenderer" o = CellAreaHasRendererMethodInfo
    ResolveCellAreaMethod "innerCellArea" o = CellAreaInnerCellAreaMethodInfo
    ResolveCellAreaMethod "isActivatable" o = CellAreaIsActivatableMethodInfo
    ResolveCellAreaMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveCellAreaMethod "isFocusSibling" o = CellAreaIsFocusSiblingMethodInfo
    ResolveCellAreaMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveCellAreaMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveCellAreaMethod "packEnd" o = Gtk.CellLayout.CellLayoutPackEndMethodInfo
    ResolveCellAreaMethod "packStart" o = Gtk.CellLayout.CellLayoutPackStartMethodInfo
    ResolveCellAreaMethod "parserFinished" o = Gtk.Buildable.BuildableParserFinishedMethodInfo
    ResolveCellAreaMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveCellAreaMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveCellAreaMethod "remove" o = CellAreaRemoveMethodInfo
    ResolveCellAreaMethod "removeFocusSibling" o = CellAreaRemoveFocusSiblingMethodInfo
    ResolveCellAreaMethod "render" o = CellAreaRenderMethodInfo
    ResolveCellAreaMethod "reorder" o = Gtk.CellLayout.CellLayoutReorderMethodInfo
    ResolveCellAreaMethod "requestRenderer" o = CellAreaRequestRendererMethodInfo
    ResolveCellAreaMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveCellAreaMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveCellAreaMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveCellAreaMethod "stopEditing" o = CellAreaStopEditingMethodInfo
    ResolveCellAreaMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveCellAreaMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveCellAreaMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveCellAreaMethod "getArea" o = Gtk.CellLayout.CellLayoutGetAreaMethodInfo
    ResolveCellAreaMethod "getCellAllocation" o = CellAreaGetCellAllocationMethodInfo
    ResolveCellAreaMethod "getCellAtPosition" o = CellAreaGetCellAtPositionMethodInfo
    ResolveCellAreaMethod "getCells" o = Gtk.CellLayout.CellLayoutGetCellsMethodInfo
    ResolveCellAreaMethod "getCurrentPathString" o = CellAreaGetCurrentPathStringMethodInfo
    ResolveCellAreaMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveCellAreaMethod "getEditWidget" o = CellAreaGetEditWidgetMethodInfo
    ResolveCellAreaMethod "getEditedCell" o = CellAreaGetEditedCellMethodInfo
    ResolveCellAreaMethod "getFocusCell" o = CellAreaGetFocusCellMethodInfo
    ResolveCellAreaMethod "getFocusFromSibling" o = CellAreaGetFocusFromSiblingMethodInfo
    ResolveCellAreaMethod "getFocusSiblings" o = CellAreaGetFocusSiblingsMethodInfo
    ResolveCellAreaMethod "getInternalChild" o = Gtk.Buildable.BuildableGetInternalChildMethodInfo
    ResolveCellAreaMethod "getName" o = Gtk.Buildable.BuildableGetNameMethodInfo
    ResolveCellAreaMethod "getPreferredHeight" o = CellAreaGetPreferredHeightMethodInfo
    ResolveCellAreaMethod "getPreferredHeightForWidth" o = CellAreaGetPreferredHeightForWidthMethodInfo
    ResolveCellAreaMethod "getPreferredWidth" o = CellAreaGetPreferredWidthMethodInfo
    ResolveCellAreaMethod "getPreferredWidthForHeight" o = CellAreaGetPreferredWidthForHeightMethodInfo
    ResolveCellAreaMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveCellAreaMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveCellAreaMethod "getRequestMode" o = CellAreaGetRequestModeMethodInfo
    ResolveCellAreaMethod "setBuildableProperty" o = Gtk.Buildable.BuildableSetBuildablePropertyMethodInfo
    ResolveCellAreaMethod "setCellDataFunc" o = Gtk.CellLayout.CellLayoutSetCellDataFuncMethodInfo
    ResolveCellAreaMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveCellAreaMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveCellAreaMethod "setFocusCell" o = CellAreaSetFocusCellMethodInfo
    ResolveCellAreaMethod "setName" o = Gtk.Buildable.BuildableSetNameMethodInfo
    ResolveCellAreaMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveCellAreaMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveCellAreaMethod t CellArea, O.OverloadedMethod info CellArea p) => OL.IsLabel t (CellArea -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveCellAreaMethod t CellArea, O.OverloadedMethod info CellArea p, R.HasField t CellArea p) => R.HasField t CellArea p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveCellAreaMethod t CellArea, O.OverloadedMethodInfo info CellArea) => OL.IsLabel t (O.MethodProxy info CellArea) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- signal CellArea::add-editable
-- | Indicates that editing has started on /@renderer@/ and that /@editable@/
-- should be added to the owning cell-layouting widget at /@cellArea@/.
-- 
-- /Since: 3.0/
type CellAreaAddEditableCallback =
    Gtk.CellRenderer.CellRenderer
    -- ^ /@renderer@/: the t'GI.Gtk.Objects.CellRenderer.CellRenderer' that started the edited
    -> Gtk.CellEditable.CellEditable
    -- ^ /@editable@/: the t'GI.Gtk.Interfaces.CellEditable.CellEditable' widget to add
    -> Gdk.Rectangle.Rectangle
    -- ^ /@cellArea@/: the t'GI.Gtk.Objects.Widget.Widget' relative t'GI.Gdk.Structs.Rectangle.Rectangle' coordinates
    --             where /@editable@/ should be added
    -> T.Text
    -- ^ /@path@/: the t'GI.Gtk.Structs.TreePath.TreePath' string this edit was initiated for
    -> IO ()

type C_CellAreaAddEditableCallback =
    Ptr CellArea ->                         -- object
    Ptr Gtk.CellRenderer.CellRenderer ->
    Ptr Gtk.CellEditable.CellEditable ->
    Ptr Gdk.Rectangle.Rectangle ->
    CString ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_CellAreaAddEditableCallback`.
foreign import ccall "wrapper"
    mk_CellAreaAddEditableCallback :: C_CellAreaAddEditableCallback -> IO (FunPtr C_CellAreaAddEditableCallback)

wrap_CellAreaAddEditableCallback :: 
    GObject a => (a -> CellAreaAddEditableCallback) ->
    C_CellAreaAddEditableCallback
wrap_CellAreaAddEditableCallback gi'cb gi'selfPtr renderer editable cellArea path _ = do
    renderer' <- (newObject Gtk.CellRenderer.CellRenderer) renderer
    editable' <- (newObject Gtk.CellEditable.CellEditable) editable
    B.ManagedPtr.withTransient  cellArea $ \cellArea' -> do
        path' <- cstringToText path
        B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  renderer' editable' cellArea' path'


-- | Connect a signal handler for the [addEditable](#signal:addEditable) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' cellArea #addEditable callback
-- @
-- 
-- 
onCellAreaAddEditable :: (IsCellArea a, MonadIO m) => a -> ((?self :: a) => CellAreaAddEditableCallback) -> m SignalHandlerId
onCellAreaAddEditable obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_CellAreaAddEditableCallback wrapped
    wrapped'' <- mk_CellAreaAddEditableCallback wrapped'
    connectSignalFunPtr obj "add-editable" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [addEditable](#signal:addEditable) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' cellArea #addEditable callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterCellAreaAddEditable :: (IsCellArea a, MonadIO m) => a -> ((?self :: a) => CellAreaAddEditableCallback) -> m SignalHandlerId
afterCellAreaAddEditable obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_CellAreaAddEditableCallback wrapped
    wrapped'' <- mk_CellAreaAddEditableCallback wrapped'
    connectSignalFunPtr obj "add-editable" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data CellAreaAddEditableSignalInfo
instance SignalInfo CellAreaAddEditableSignalInfo where
    type HaskellCallbackType CellAreaAddEditableSignalInfo = CellAreaAddEditableCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_CellAreaAddEditableCallback cb
        cb'' <- mk_CellAreaAddEditableCallback cb'
        connectSignalFunPtr obj "add-editable" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellArea::add-editable"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellArea.html#g:signal:addEditable"})

#endif

-- signal CellArea::apply-attributes
-- | This signal is emitted whenever applying attributes to /@area@/ from /@model@/
-- 
-- /Since: 3.0/
type CellAreaApplyAttributesCallback =
    Gtk.TreeModel.TreeModel
    -- ^ /@model@/: the t'GI.Gtk.Interfaces.TreeModel.TreeModel' to apply the attributes from
    -> Gtk.TreeIter.TreeIter
    -- ^ /@iter@/: the t'GI.Gtk.Structs.TreeIter.TreeIter' indicating which row to apply the attributes of
    -> Bool
    -- ^ /@isExpander@/: whether the view shows children for this row
    -> Bool
    -- ^ /@isExpanded@/: whether the view is currently showing the children of this row
    -> IO ()

type C_CellAreaApplyAttributesCallback =
    Ptr CellArea ->                         -- object
    Ptr Gtk.TreeModel.TreeModel ->
    Ptr Gtk.TreeIter.TreeIter ->
    CInt ->
    CInt ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_CellAreaApplyAttributesCallback`.
foreign import ccall "wrapper"
    mk_CellAreaApplyAttributesCallback :: C_CellAreaApplyAttributesCallback -> IO (FunPtr C_CellAreaApplyAttributesCallback)

wrap_CellAreaApplyAttributesCallback :: 
    GObject a => (a -> CellAreaApplyAttributesCallback) ->
    C_CellAreaApplyAttributesCallback
wrap_CellAreaApplyAttributesCallback gi'cb gi'selfPtr model iter isExpander isExpanded _ = do
    model' <- (newObject Gtk.TreeModel.TreeModel) model
    B.ManagedPtr.withTransient  iter $ \iter' -> do
        let isExpander' = (/= 0) isExpander
        let isExpanded' = (/= 0) isExpanded
        B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  model' iter' isExpander' isExpanded'


-- | Connect a signal handler for the [applyAttributes](#signal:applyAttributes) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' cellArea #applyAttributes callback
-- @
-- 
-- 
onCellAreaApplyAttributes :: (IsCellArea a, MonadIO m) => a -> ((?self :: a) => CellAreaApplyAttributesCallback) -> m SignalHandlerId
onCellAreaApplyAttributes obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_CellAreaApplyAttributesCallback wrapped
    wrapped'' <- mk_CellAreaApplyAttributesCallback wrapped'
    connectSignalFunPtr obj "apply-attributes" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [applyAttributes](#signal:applyAttributes) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' cellArea #applyAttributes callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterCellAreaApplyAttributes :: (IsCellArea a, MonadIO m) => a -> ((?self :: a) => CellAreaApplyAttributesCallback) -> m SignalHandlerId
afterCellAreaApplyAttributes obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_CellAreaApplyAttributesCallback wrapped
    wrapped'' <- mk_CellAreaApplyAttributesCallback wrapped'
    connectSignalFunPtr obj "apply-attributes" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data CellAreaApplyAttributesSignalInfo
instance SignalInfo CellAreaApplyAttributesSignalInfo where
    type HaskellCallbackType CellAreaApplyAttributesSignalInfo = CellAreaApplyAttributesCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_CellAreaApplyAttributesCallback cb
        cb'' <- mk_CellAreaApplyAttributesCallback cb'
        connectSignalFunPtr obj "apply-attributes" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellArea::apply-attributes"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellArea.html#g:signal:applyAttributes"})

#endif

-- signal CellArea::focus-changed
-- | Indicates that focus changed on this /@area@/. This signal
-- is emitted either as a result of focus handling or event
-- handling.
-- 
-- It\'s possible that the signal is emitted even if the
-- currently focused renderer did not change, this is
-- because focus may change to the same renderer in the
-- same cell area for a different row of data.
-- 
-- /Since: 3.0/
type CellAreaFocusChangedCallback =
    Gtk.CellRenderer.CellRenderer
    -- ^ /@renderer@/: the t'GI.Gtk.Objects.CellRenderer.CellRenderer' that has focus
    -> T.Text
    -- ^ /@path@/: the current t'GI.Gtk.Structs.TreePath.TreePath' string set for /@area@/
    -> IO ()

type C_CellAreaFocusChangedCallback =
    Ptr CellArea ->                         -- object
    Ptr Gtk.CellRenderer.CellRenderer ->
    CString ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_CellAreaFocusChangedCallback`.
foreign import ccall "wrapper"
    mk_CellAreaFocusChangedCallback :: C_CellAreaFocusChangedCallback -> IO (FunPtr C_CellAreaFocusChangedCallback)

wrap_CellAreaFocusChangedCallback :: 
    GObject a => (a -> CellAreaFocusChangedCallback) ->
    C_CellAreaFocusChangedCallback
wrap_CellAreaFocusChangedCallback gi'cb gi'selfPtr renderer path _ = do
    renderer' <- (newObject Gtk.CellRenderer.CellRenderer) renderer
    path' <- cstringToText path
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  renderer' path'


-- | Connect a signal handler for the [focusChanged](#signal:focusChanged) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' cellArea #focusChanged callback
-- @
-- 
-- 
onCellAreaFocusChanged :: (IsCellArea a, MonadIO m) => a -> ((?self :: a) => CellAreaFocusChangedCallback) -> m SignalHandlerId
onCellAreaFocusChanged obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_CellAreaFocusChangedCallback wrapped
    wrapped'' <- mk_CellAreaFocusChangedCallback wrapped'
    connectSignalFunPtr obj "focus-changed" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [focusChanged](#signal:focusChanged) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' cellArea #focusChanged callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterCellAreaFocusChanged :: (IsCellArea a, MonadIO m) => a -> ((?self :: a) => CellAreaFocusChangedCallback) -> m SignalHandlerId
afterCellAreaFocusChanged obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_CellAreaFocusChangedCallback wrapped
    wrapped'' <- mk_CellAreaFocusChangedCallback wrapped'
    connectSignalFunPtr obj "focus-changed" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data CellAreaFocusChangedSignalInfo
instance SignalInfo CellAreaFocusChangedSignalInfo where
    type HaskellCallbackType CellAreaFocusChangedSignalInfo = CellAreaFocusChangedCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_CellAreaFocusChangedCallback cb
        cb'' <- mk_CellAreaFocusChangedCallback cb'
        connectSignalFunPtr obj "focus-changed" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellArea::focus-changed"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellArea.html#g:signal:focusChanged"})

#endif

-- signal CellArea::remove-editable
-- | Indicates that editing finished on /@renderer@/ and that /@editable@/
-- should be removed from the owning cell-layouting widget.
-- 
-- /Since: 3.0/
type CellAreaRemoveEditableCallback =
    Gtk.CellRenderer.CellRenderer
    -- ^ /@renderer@/: the t'GI.Gtk.Objects.CellRenderer.CellRenderer' that finished editeding
    -> Gtk.CellEditable.CellEditable
    -- ^ /@editable@/: the t'GI.Gtk.Interfaces.CellEditable.CellEditable' widget to remove
    -> IO ()

type C_CellAreaRemoveEditableCallback =
    Ptr CellArea ->                         -- object
    Ptr Gtk.CellRenderer.CellRenderer ->
    Ptr Gtk.CellEditable.CellEditable ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_CellAreaRemoveEditableCallback`.
foreign import ccall "wrapper"
    mk_CellAreaRemoveEditableCallback :: C_CellAreaRemoveEditableCallback -> IO (FunPtr C_CellAreaRemoveEditableCallback)

wrap_CellAreaRemoveEditableCallback :: 
    GObject a => (a -> CellAreaRemoveEditableCallback) ->
    C_CellAreaRemoveEditableCallback
wrap_CellAreaRemoveEditableCallback gi'cb gi'selfPtr renderer editable _ = do
    renderer' <- (newObject Gtk.CellRenderer.CellRenderer) renderer
    editable' <- (newObject Gtk.CellEditable.CellEditable) editable
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  renderer' editable'


-- | Connect a signal handler for the [removeEditable](#signal:removeEditable) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' cellArea #removeEditable callback
-- @
-- 
-- 
onCellAreaRemoveEditable :: (IsCellArea a, MonadIO m) => a -> ((?self :: a) => CellAreaRemoveEditableCallback) -> m SignalHandlerId
onCellAreaRemoveEditable obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_CellAreaRemoveEditableCallback wrapped
    wrapped'' <- mk_CellAreaRemoveEditableCallback wrapped'
    connectSignalFunPtr obj "remove-editable" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [removeEditable](#signal:removeEditable) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' cellArea #removeEditable callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterCellAreaRemoveEditable :: (IsCellArea a, MonadIO m) => a -> ((?self :: a) => CellAreaRemoveEditableCallback) -> m SignalHandlerId
afterCellAreaRemoveEditable obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_CellAreaRemoveEditableCallback wrapped
    wrapped'' <- mk_CellAreaRemoveEditableCallback wrapped'
    connectSignalFunPtr obj "remove-editable" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data CellAreaRemoveEditableSignalInfo
instance SignalInfo CellAreaRemoveEditableSignalInfo where
    type HaskellCallbackType CellAreaRemoveEditableSignalInfo = CellAreaRemoveEditableCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_CellAreaRemoveEditableCallback cb
        cb'' <- mk_CellAreaRemoveEditableCallback cb'
        connectSignalFunPtr obj "remove-editable" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellArea::remove-editable"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellArea.html#g:signal:removeEditable"})

#endif

-- VVV Prop "edit-widget"
   -- Type: TInterface (Name {namespace = "Gtk", name = "CellEditable"})
   -- Flags: [PropertyReadable]
   -- Nullable: (Just False,Nothing)

-- | Get the value of the “@edit-widget@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' cellArea #editWidget
-- @
getCellAreaEditWidget :: (MonadIO m, IsCellArea o) => o -> m Gtk.CellEditable.CellEditable
getCellAreaEditWidget obj = MIO.liftIO $ checkUnexpectedNothing "getCellAreaEditWidget" $ B.Properties.getObjectPropertyObject obj "edit-widget" Gtk.CellEditable.CellEditable

#if defined(ENABLE_OVERLOADING)
data CellAreaEditWidgetPropertyInfo
instance AttrInfo CellAreaEditWidgetPropertyInfo where
    type AttrAllowedOps CellAreaEditWidgetPropertyInfo = '[ 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint CellAreaEditWidgetPropertyInfo = IsCellArea
    type AttrSetTypeConstraint CellAreaEditWidgetPropertyInfo = (~) ()
    type AttrTransferTypeConstraint CellAreaEditWidgetPropertyInfo = (~) ()
    type AttrTransferType CellAreaEditWidgetPropertyInfo = ()
    type AttrGetType CellAreaEditWidgetPropertyInfo = Gtk.CellEditable.CellEditable
    type AttrLabel CellAreaEditWidgetPropertyInfo = "edit-widget"
    type AttrOrigin CellAreaEditWidgetPropertyInfo = CellArea
    attrGet = getCellAreaEditWidget
    attrSet = undefined
    attrTransfer _ = undefined
    attrConstruct = undefined
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellArea.editWidget"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellArea.html#g:attr:editWidget"
        })
#endif

-- VVV Prop "edited-cell"
   -- Type: TInterface (Name {namespace = "Gtk", name = "CellRenderer"})
   -- Flags: [PropertyReadable]
   -- Nullable: (Just False,Nothing)

-- | Get the value of the “@edited-cell@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' cellArea #editedCell
-- @
getCellAreaEditedCell :: (MonadIO m, IsCellArea o) => o -> m Gtk.CellRenderer.CellRenderer
getCellAreaEditedCell obj = MIO.liftIO $ checkUnexpectedNothing "getCellAreaEditedCell" $ B.Properties.getObjectPropertyObject obj "edited-cell" Gtk.CellRenderer.CellRenderer

#if defined(ENABLE_OVERLOADING)
data CellAreaEditedCellPropertyInfo
instance AttrInfo CellAreaEditedCellPropertyInfo where
    type AttrAllowedOps CellAreaEditedCellPropertyInfo = '[ 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint CellAreaEditedCellPropertyInfo = IsCellArea
    type AttrSetTypeConstraint CellAreaEditedCellPropertyInfo = (~) ()
    type AttrTransferTypeConstraint CellAreaEditedCellPropertyInfo = (~) ()
    type AttrTransferType CellAreaEditedCellPropertyInfo = ()
    type AttrGetType CellAreaEditedCellPropertyInfo = Gtk.CellRenderer.CellRenderer
    type AttrLabel CellAreaEditedCellPropertyInfo = "edited-cell"
    type AttrOrigin CellAreaEditedCellPropertyInfo = CellArea
    attrGet = getCellAreaEditedCell
    attrSet = undefined
    attrTransfer _ = undefined
    attrConstruct = undefined
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellArea.editedCell"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellArea.html#g:attr:editedCell"
        })
#endif

-- VVV Prop "focus-cell"
   -- Type: TInterface (Name {namespace = "Gtk", name = "CellRenderer"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@focus-cell@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' cellArea #focusCell
-- @
getCellAreaFocusCell :: (MonadIO m, IsCellArea o) => o -> m Gtk.CellRenderer.CellRenderer
getCellAreaFocusCell obj = MIO.liftIO $ checkUnexpectedNothing "getCellAreaFocusCell" $ B.Properties.getObjectPropertyObject obj "focus-cell" Gtk.CellRenderer.CellRenderer

-- | Set the value of the “@focus-cell@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' cellArea [ #focusCell 'Data.GI.Base.Attributes.:=' value ]
-- @
setCellAreaFocusCell :: (MonadIO m, IsCellArea o, Gtk.CellRenderer.IsCellRenderer a) => o -> a -> m ()
setCellAreaFocusCell obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyObject obj "focus-cell" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@focus-cell@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructCellAreaFocusCell :: (IsCellArea o, MIO.MonadIO m, Gtk.CellRenderer.IsCellRenderer a) => a -> m (GValueConstruct o)
constructCellAreaFocusCell val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyObject "focus-cell" (P.Just val)

#if defined(ENABLE_OVERLOADING)
data CellAreaFocusCellPropertyInfo
instance AttrInfo CellAreaFocusCellPropertyInfo where
    type AttrAllowedOps CellAreaFocusCellPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint CellAreaFocusCellPropertyInfo = IsCellArea
    type AttrSetTypeConstraint CellAreaFocusCellPropertyInfo = Gtk.CellRenderer.IsCellRenderer
    type AttrTransferTypeConstraint CellAreaFocusCellPropertyInfo = Gtk.CellRenderer.IsCellRenderer
    type AttrTransferType CellAreaFocusCellPropertyInfo = Gtk.CellRenderer.CellRenderer
    type AttrGetType CellAreaFocusCellPropertyInfo = Gtk.CellRenderer.CellRenderer
    type AttrLabel CellAreaFocusCellPropertyInfo = "focus-cell"
    type AttrOrigin CellAreaFocusCellPropertyInfo = CellArea
    attrGet = getCellAreaFocusCell
    attrSet = setCellAreaFocusCell
    attrTransfer _ v = do
        unsafeCastTo Gtk.CellRenderer.CellRenderer v
    attrConstruct = constructCellAreaFocusCell
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellArea.focusCell"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellArea.html#g:attr:focusCell"
        })
#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList CellArea
type instance O.AttributeList CellArea = CellAreaAttributeList
type CellAreaAttributeList = ('[ '("editWidget", CellAreaEditWidgetPropertyInfo), '("editedCell", CellAreaEditedCellPropertyInfo), '("focusCell", CellAreaFocusCellPropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
cellAreaEditWidget :: AttrLabelProxy "editWidget"
cellAreaEditWidget = AttrLabelProxy

cellAreaEditedCell :: AttrLabelProxy "editedCell"
cellAreaEditedCell = AttrLabelProxy

cellAreaFocusCell :: AttrLabelProxy "focusCell"
cellAreaFocusCell = AttrLabelProxy

#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList CellArea = CellAreaSignalList
type CellAreaSignalList = ('[ '("addEditable", CellAreaAddEditableSignalInfo), '("applyAttributes", CellAreaApplyAttributesSignalInfo), '("focusChanged", CellAreaFocusChangedSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo), '("removeEditable", CellAreaRemoveEditableSignalInfo)] :: [(Symbol, *)])

#endif

-- method CellArea::activate
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "area"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellArea" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCellArea" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellAreaContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "the #GtkCellAreaContext in context with the current row data"
--                 , sinceVersion = Nothing
--                 }
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
--                 { rawDocText = Just "the #GtkWidget that @area is rendering on"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "cell_area"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "Rectangle" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the size and location of @area relative to @widget\8217s allocation"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "flags"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellRendererState" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the #GtkCellRendererState flags for @area for this row of data."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "edit_only"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "if %TRUE then only cell renderers that are %GTK_CELL_RENDERER_MODE_EDITABLE\n            will be activated."
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

foreign import ccall "gtk_cell_area_activate" gtk_cell_area_activate :: 
    Ptr CellArea ->                         -- area : TInterface (Name {namespace = "Gtk", name = "CellArea"})
    Ptr Gtk.CellAreaContext.CellAreaContext -> -- context : TInterface (Name {namespace = "Gtk", name = "CellAreaContext"})
    Ptr Gtk.Widget.Widget ->                -- widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    Ptr Gdk.Rectangle.Rectangle ->          -- cell_area : TInterface (Name {namespace = "Gdk", name = "Rectangle"})
    CUInt ->                                -- flags : TInterface (Name {namespace = "Gtk", name = "CellRendererState"})
    CInt ->                                 -- edit_only : TBasicType TBoolean
    IO CInt

-- | Activates /@area@/, usually by activating the currently focused
-- cell, however some subclasses which embed widgets in the area
-- can also activate a widget if it currently has the focus.
-- 
-- /Since: 3.0/
cellAreaActivate ::
    (B.CallStack.HasCallStack, MonadIO m, IsCellArea a, Gtk.CellAreaContext.IsCellAreaContext b, Gtk.Widget.IsWidget c) =>
    a
    -- ^ /@area@/: a t'GI.Gtk.Objects.CellArea.CellArea'
    -> b
    -- ^ /@context@/: the t'GI.Gtk.Objects.CellAreaContext.CellAreaContext' in context with the current row data
    -> c
    -- ^ /@widget@/: the t'GI.Gtk.Objects.Widget.Widget' that /@area@/ is rendering on
    -> Gdk.Rectangle.Rectangle
    -- ^ /@cellArea@/: the size and location of /@area@/ relative to /@widget@/’s allocation
    -> [Gtk.Flags.CellRendererState]
    -- ^ /@flags@/: the t'GI.Gtk.Flags.CellRendererState' flags for /@area@/ for this row of data.
    -> Bool
    -- ^ /@editOnly@/: if 'P.True' then only cell renderers that are 'GI.Gtk.Enums.CellRendererModeEditable'
    --             will be activated.
    -> m Bool
    -- ^ __Returns:__ Whether /@area@/ was successfully activated.
cellAreaActivate area context widget cellArea flags editOnly = liftIO $ do
    area' <- unsafeManagedPtrCastPtr area
    context' <- unsafeManagedPtrCastPtr context
    widget' <- unsafeManagedPtrCastPtr widget
    cellArea' <- unsafeManagedPtrGetPtr cellArea
    let flags' = gflagsToWord flags
    let editOnly' = (fromIntegral . fromEnum) editOnly
    result <- gtk_cell_area_activate area' context' widget' cellArea' flags' editOnly'
    let result' = (/= 0) result
    touchManagedPtr area
    touchManagedPtr context
    touchManagedPtr widget
    touchManagedPtr cellArea
    return result'

#if defined(ENABLE_OVERLOADING)
data CellAreaActivateMethodInfo
instance (signature ~ (b -> c -> Gdk.Rectangle.Rectangle -> [Gtk.Flags.CellRendererState] -> Bool -> m Bool), MonadIO m, IsCellArea a, Gtk.CellAreaContext.IsCellAreaContext b, Gtk.Widget.IsWidget c) => O.OverloadedMethod CellAreaActivateMethodInfo a signature where
    overloadedMethod = cellAreaActivate

instance O.OverloadedMethodInfo CellAreaActivateMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellArea.cellAreaActivate",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellArea.html#v:cellAreaActivate"
        })


#endif

-- method CellArea::activate_cell
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "area"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellArea" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCellArea" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "the #GtkWidget that @area is rendering onto"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "renderer"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellRenderer" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the #GtkCellRenderer in @area to activate"
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
--                 { rawDocText =
--                     Just "the #GdkEvent for which cell activation should occur"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "cell_area"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "Rectangle" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the #GdkRectangle in @widget relative coordinates\n            of @renderer for the current row."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "flags"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellRendererState" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the #GtkCellRendererState for @renderer"
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

foreign import ccall "gtk_cell_area_activate_cell" gtk_cell_area_activate_cell :: 
    Ptr CellArea ->                         -- area : TInterface (Name {namespace = "Gtk", name = "CellArea"})
    Ptr Gtk.Widget.Widget ->                -- widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    Ptr Gtk.CellRenderer.CellRenderer ->    -- renderer : TInterface (Name {namespace = "Gtk", name = "CellRenderer"})
    Ptr Gdk.Event.Event ->                  -- event : TInterface (Name {namespace = "Gdk", name = "Event"})
    Ptr Gdk.Rectangle.Rectangle ->          -- cell_area : TInterface (Name {namespace = "Gdk", name = "Rectangle"})
    CUInt ->                                -- flags : TInterface (Name {namespace = "Gtk", name = "CellRendererState"})
    IO CInt

-- | This is used by t'GI.Gtk.Objects.CellArea.CellArea' subclasses when handling events
-- to activate cells, the base t'GI.Gtk.Objects.CellArea.CellArea' class activates cells
-- for keyboard events for free in its own GtkCellArea->@/activate()/@
-- implementation.
-- 
-- /Since: 3.0/
cellAreaActivateCell ::
    (B.CallStack.HasCallStack, MonadIO m, IsCellArea a, Gtk.Widget.IsWidget b, Gtk.CellRenderer.IsCellRenderer c) =>
    a
    -- ^ /@area@/: a t'GI.Gtk.Objects.CellArea.CellArea'
    -> b
    -- ^ /@widget@/: the t'GI.Gtk.Objects.Widget.Widget' that /@area@/ is rendering onto
    -> c
    -- ^ /@renderer@/: the t'GI.Gtk.Objects.CellRenderer.CellRenderer' in /@area@/ to activate
    -> Gdk.Event.Event
    -- ^ /@event@/: the t'GI.Gdk.Unions.Event.Event' for which cell activation should occur
    -> Gdk.Rectangle.Rectangle
    -- ^ /@cellArea@/: the t'GI.Gdk.Structs.Rectangle.Rectangle' in /@widget@/ relative coordinates
    --             of /@renderer@/ for the current row.
    -> [Gtk.Flags.CellRendererState]
    -- ^ /@flags@/: the t'GI.Gtk.Flags.CellRendererState' for /@renderer@/
    -> m Bool
    -- ^ __Returns:__ whether cell activation was successful
cellAreaActivateCell area widget renderer event cellArea flags = liftIO $ do
    area' <- unsafeManagedPtrCastPtr area
    widget' <- unsafeManagedPtrCastPtr widget
    renderer' <- unsafeManagedPtrCastPtr renderer
    event' <- unsafeManagedPtrGetPtr event
    cellArea' <- unsafeManagedPtrGetPtr cellArea
    let flags' = gflagsToWord flags
    result <- gtk_cell_area_activate_cell area' widget' renderer' event' cellArea' flags'
    let result' = (/= 0) result
    touchManagedPtr area
    touchManagedPtr widget
    touchManagedPtr renderer
    touchManagedPtr event
    touchManagedPtr cellArea
    return result'

#if defined(ENABLE_OVERLOADING)
data CellAreaActivateCellMethodInfo
instance (signature ~ (b -> c -> Gdk.Event.Event -> Gdk.Rectangle.Rectangle -> [Gtk.Flags.CellRendererState] -> m Bool), MonadIO m, IsCellArea a, Gtk.Widget.IsWidget b, Gtk.CellRenderer.IsCellRenderer c) => O.OverloadedMethod CellAreaActivateCellMethodInfo a signature where
    overloadedMethod = cellAreaActivateCell

instance O.OverloadedMethodInfo CellAreaActivateCellMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellArea.cellAreaActivateCell",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellArea.html#v:cellAreaActivateCell"
        })


#endif

-- method CellArea::add
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "area"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellArea" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCellArea" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "renderer"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellRenderer" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the #GtkCellRenderer to add to @area"
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

foreign import ccall "gtk_cell_area_add" gtk_cell_area_add :: 
    Ptr CellArea ->                         -- area : TInterface (Name {namespace = "Gtk", name = "CellArea"})
    Ptr Gtk.CellRenderer.CellRenderer ->    -- renderer : TInterface (Name {namespace = "Gtk", name = "CellRenderer"})
    IO ()

-- | Adds /@renderer@/ to /@area@/ with the default child cell properties.
-- 
-- /Since: 3.0/
cellAreaAdd ::
    (B.CallStack.HasCallStack, MonadIO m, IsCellArea a, Gtk.CellRenderer.IsCellRenderer b) =>
    a
    -- ^ /@area@/: a t'GI.Gtk.Objects.CellArea.CellArea'
    -> b
    -- ^ /@renderer@/: the t'GI.Gtk.Objects.CellRenderer.CellRenderer' to add to /@area@/
    -> m ()
cellAreaAdd area renderer = liftIO $ do
    area' <- unsafeManagedPtrCastPtr area
    renderer' <- unsafeManagedPtrCastPtr renderer
    gtk_cell_area_add area' renderer'
    touchManagedPtr area
    touchManagedPtr renderer
    return ()

#if defined(ENABLE_OVERLOADING)
data CellAreaAddMethodInfo
instance (signature ~ (b -> m ()), MonadIO m, IsCellArea a, Gtk.CellRenderer.IsCellRenderer b) => O.OverloadedMethod CellAreaAddMethodInfo a signature where
    overloadedMethod = cellAreaAdd

instance O.OverloadedMethodInfo CellAreaAddMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellArea.cellAreaAdd",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellArea.html#v:cellAreaAdd"
        })


#endif

-- method CellArea::add_focus_sibling
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "area"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellArea" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCellArea" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "renderer"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellRenderer" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the #GtkCellRenderer expected to have focus"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "sibling"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellRenderer" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "the #GtkCellRenderer to add to @renderer\8217s focus area"
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

foreign import ccall "gtk_cell_area_add_focus_sibling" gtk_cell_area_add_focus_sibling :: 
    Ptr CellArea ->                         -- area : TInterface (Name {namespace = "Gtk", name = "CellArea"})
    Ptr Gtk.CellRenderer.CellRenderer ->    -- renderer : TInterface (Name {namespace = "Gtk", name = "CellRenderer"})
    Ptr Gtk.CellRenderer.CellRenderer ->    -- sibling : TInterface (Name {namespace = "Gtk", name = "CellRenderer"})
    IO ()

-- | Adds /@sibling@/ to /@renderer@/’s focusable area, focus will be drawn
-- around /@renderer@/ and all of its siblings if /@renderer@/ can
-- focus for a given row.
-- 
-- Events handled by focus siblings can also activate the given
-- focusable /@renderer@/.
-- 
-- /Since: 3.0/
cellAreaAddFocusSibling ::
    (B.CallStack.HasCallStack, MonadIO m, IsCellArea a, Gtk.CellRenderer.IsCellRenderer b, Gtk.CellRenderer.IsCellRenderer c) =>
    a
    -- ^ /@area@/: a t'GI.Gtk.Objects.CellArea.CellArea'
    -> b
    -- ^ /@renderer@/: the t'GI.Gtk.Objects.CellRenderer.CellRenderer' expected to have focus
    -> c
    -- ^ /@sibling@/: the t'GI.Gtk.Objects.CellRenderer.CellRenderer' to add to /@renderer@/’s focus area
    -> m ()
cellAreaAddFocusSibling area renderer sibling = liftIO $ do
    area' <- unsafeManagedPtrCastPtr area
    renderer' <- unsafeManagedPtrCastPtr renderer
    sibling' <- unsafeManagedPtrCastPtr sibling
    gtk_cell_area_add_focus_sibling area' renderer' sibling'
    touchManagedPtr area
    touchManagedPtr renderer
    touchManagedPtr sibling
    return ()

#if defined(ENABLE_OVERLOADING)
data CellAreaAddFocusSiblingMethodInfo
instance (signature ~ (b -> c -> m ()), MonadIO m, IsCellArea a, Gtk.CellRenderer.IsCellRenderer b, Gtk.CellRenderer.IsCellRenderer c) => O.OverloadedMethod CellAreaAddFocusSiblingMethodInfo a signature where
    overloadedMethod = cellAreaAddFocusSibling

instance O.OverloadedMethodInfo CellAreaAddFocusSiblingMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellArea.cellAreaAddFocusSibling",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellArea.html#v:cellAreaAddFocusSibling"
        })


#endif

-- method CellArea::apply_attributes
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "area"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellArea" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCellArea" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "tree_model"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeModel" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the #GtkTreeModel to pull values from"
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
--               TInterface Name { namespace = "Gtk" , name = "TreeIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "the #GtkTreeIter in @tree_model to apply values for"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "is_expander"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "whether @iter has children"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "is_expanded"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "whether @iter is expanded in the view and\n              children are visible"
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

foreign import ccall "gtk_cell_area_apply_attributes" gtk_cell_area_apply_attributes :: 
    Ptr CellArea ->                         -- area : TInterface (Name {namespace = "Gtk", name = "CellArea"})
    Ptr Gtk.TreeModel.TreeModel ->          -- tree_model : TInterface (Name {namespace = "Gtk", name = "TreeModel"})
    Ptr Gtk.TreeIter.TreeIter ->            -- iter : TInterface (Name {namespace = "Gtk", name = "TreeIter"})
    CInt ->                                 -- is_expander : TBasicType TBoolean
    CInt ->                                 -- is_expanded : TBasicType TBoolean
    IO ()

-- | Applies any connected attributes to the renderers in
-- /@area@/ by pulling the values from /@treeModel@/.
-- 
-- /Since: 3.0/
cellAreaApplyAttributes ::
    (B.CallStack.HasCallStack, MonadIO m, IsCellArea a, Gtk.TreeModel.IsTreeModel b) =>
    a
    -- ^ /@area@/: a t'GI.Gtk.Objects.CellArea.CellArea'
    -> b
    -- ^ /@treeModel@/: the t'GI.Gtk.Interfaces.TreeModel.TreeModel' to pull values from
    -> Gtk.TreeIter.TreeIter
    -- ^ /@iter@/: the t'GI.Gtk.Structs.TreeIter.TreeIter' in /@treeModel@/ to apply values for
    -> Bool
    -- ^ /@isExpander@/: whether /@iter@/ has children
    -> Bool
    -- ^ /@isExpanded@/: whether /@iter@/ is expanded in the view and
    --               children are visible
    -> m ()
cellAreaApplyAttributes area treeModel iter isExpander isExpanded = liftIO $ do
    area' <- unsafeManagedPtrCastPtr area
    treeModel' <- unsafeManagedPtrCastPtr treeModel
    iter' <- unsafeManagedPtrGetPtr iter
    let isExpander' = (fromIntegral . fromEnum) isExpander
    let isExpanded' = (fromIntegral . fromEnum) isExpanded
    gtk_cell_area_apply_attributes area' treeModel' iter' isExpander' isExpanded'
    touchManagedPtr area
    touchManagedPtr treeModel
    touchManagedPtr iter
    return ()

#if defined(ENABLE_OVERLOADING)
data CellAreaApplyAttributesMethodInfo
instance (signature ~ (b -> Gtk.TreeIter.TreeIter -> Bool -> Bool -> m ()), MonadIO m, IsCellArea a, Gtk.TreeModel.IsTreeModel b) => O.OverloadedMethod CellAreaApplyAttributesMethodInfo a signature where
    overloadedMethod = cellAreaApplyAttributes

instance O.OverloadedMethodInfo CellAreaApplyAttributesMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellArea.cellAreaApplyAttributes",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellArea.html#v:cellAreaApplyAttributes"
        })


#endif

-- method CellArea::attribute_connect
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "area"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellArea" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCellArea" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "renderer"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellRenderer" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "the #GtkCellRenderer to connect an attribute for"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "attribute"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the attribute name" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "column"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "the #GtkTreeModel column to fetch attribute values from"
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

foreign import ccall "gtk_cell_area_attribute_connect" gtk_cell_area_attribute_connect :: 
    Ptr CellArea ->                         -- area : TInterface (Name {namespace = "Gtk", name = "CellArea"})
    Ptr Gtk.CellRenderer.CellRenderer ->    -- renderer : TInterface (Name {namespace = "Gtk", name = "CellRenderer"})
    CString ->                              -- attribute : TBasicType TUTF8
    Int32 ->                                -- column : TBasicType TInt
    IO ()

-- | Connects an /@attribute@/ to apply values from /@column@/ for the
-- t'GI.Gtk.Interfaces.TreeModel.TreeModel' in use.
-- 
-- /Since: 3.0/
cellAreaAttributeConnect ::
    (B.CallStack.HasCallStack, MonadIO m, IsCellArea a, Gtk.CellRenderer.IsCellRenderer b) =>
    a
    -- ^ /@area@/: a t'GI.Gtk.Objects.CellArea.CellArea'
    -> b
    -- ^ /@renderer@/: the t'GI.Gtk.Objects.CellRenderer.CellRenderer' to connect an attribute for
    -> T.Text
    -- ^ /@attribute@/: the attribute name
    -> Int32
    -- ^ /@column@/: the t'GI.Gtk.Interfaces.TreeModel.TreeModel' column to fetch attribute values from
    -> m ()
cellAreaAttributeConnect area renderer attribute column = liftIO $ do
    area' <- unsafeManagedPtrCastPtr area
    renderer' <- unsafeManagedPtrCastPtr renderer
    attribute' <- textToCString attribute
    gtk_cell_area_attribute_connect area' renderer' attribute' column
    touchManagedPtr area
    touchManagedPtr renderer
    freeMem attribute'
    return ()

#if defined(ENABLE_OVERLOADING)
data CellAreaAttributeConnectMethodInfo
instance (signature ~ (b -> T.Text -> Int32 -> m ()), MonadIO m, IsCellArea a, Gtk.CellRenderer.IsCellRenderer b) => O.OverloadedMethod CellAreaAttributeConnectMethodInfo a signature where
    overloadedMethod = cellAreaAttributeConnect

instance O.OverloadedMethodInfo CellAreaAttributeConnectMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellArea.cellAreaAttributeConnect",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellArea.html#v:cellAreaAttributeConnect"
        })


#endif

-- method CellArea::attribute_disconnect
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "area"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellArea" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCellArea" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "renderer"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellRenderer" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "the #GtkCellRenderer to disconnect an attribute for"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "attribute"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the attribute name" , sinceVersion = Nothing }
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

foreign import ccall "gtk_cell_area_attribute_disconnect" gtk_cell_area_attribute_disconnect :: 
    Ptr CellArea ->                         -- area : TInterface (Name {namespace = "Gtk", name = "CellArea"})
    Ptr Gtk.CellRenderer.CellRenderer ->    -- renderer : TInterface (Name {namespace = "Gtk", name = "CellRenderer"})
    CString ->                              -- attribute : TBasicType TUTF8
    IO ()

-- | Disconnects /@attribute@/ for the /@renderer@/ in /@area@/ so that
-- attribute will no longer be updated with values from the
-- model.
-- 
-- /Since: 3.0/
cellAreaAttributeDisconnect ::
    (B.CallStack.HasCallStack, MonadIO m, IsCellArea a, Gtk.CellRenderer.IsCellRenderer b) =>
    a
    -- ^ /@area@/: a t'GI.Gtk.Objects.CellArea.CellArea'
    -> b
    -- ^ /@renderer@/: the t'GI.Gtk.Objects.CellRenderer.CellRenderer' to disconnect an attribute for
    -> T.Text
    -- ^ /@attribute@/: the attribute name
    -> m ()
cellAreaAttributeDisconnect area renderer attribute = liftIO $ do
    area' <- unsafeManagedPtrCastPtr area
    renderer' <- unsafeManagedPtrCastPtr renderer
    attribute' <- textToCString attribute
    gtk_cell_area_attribute_disconnect area' renderer' attribute'
    touchManagedPtr area
    touchManagedPtr renderer
    freeMem attribute'
    return ()

#if defined(ENABLE_OVERLOADING)
data CellAreaAttributeDisconnectMethodInfo
instance (signature ~ (b -> T.Text -> m ()), MonadIO m, IsCellArea a, Gtk.CellRenderer.IsCellRenderer b) => O.OverloadedMethod CellAreaAttributeDisconnectMethodInfo a signature where
    overloadedMethod = cellAreaAttributeDisconnect

instance O.OverloadedMethodInfo CellAreaAttributeDisconnectMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellArea.cellAreaAttributeDisconnect",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellArea.html#v:cellAreaAttributeDisconnect"
        })


#endif

-- method CellArea::attribute_get_column
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "area"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellArea" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCellArea" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "renderer"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellRenderer" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCellRenderer" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "attribute"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "an attribute on the renderer"
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

foreign import ccall "gtk_cell_area_attribute_get_column" gtk_cell_area_attribute_get_column :: 
    Ptr CellArea ->                         -- area : TInterface (Name {namespace = "Gtk", name = "CellArea"})
    Ptr Gtk.CellRenderer.CellRenderer ->    -- renderer : TInterface (Name {namespace = "Gtk", name = "CellRenderer"})
    CString ->                              -- attribute : TBasicType TUTF8
    IO Int32

-- | Returns the model column that an attribute has been mapped to,
-- or -1 if the attribute is not mapped.
-- 
-- /Since: 3.14/
cellAreaAttributeGetColumn ::
    (B.CallStack.HasCallStack, MonadIO m, IsCellArea a, Gtk.CellRenderer.IsCellRenderer b) =>
    a
    -- ^ /@area@/: a t'GI.Gtk.Objects.CellArea.CellArea'
    -> b
    -- ^ /@renderer@/: a t'GI.Gtk.Objects.CellRenderer.CellRenderer'
    -> T.Text
    -- ^ /@attribute@/: an attribute on the renderer
    -> m Int32
    -- ^ __Returns:__ the model column, or -1
cellAreaAttributeGetColumn area renderer attribute = liftIO $ do
    area' <- unsafeManagedPtrCastPtr area
    renderer' <- unsafeManagedPtrCastPtr renderer
    attribute' <- textToCString attribute
    result <- gtk_cell_area_attribute_get_column area' renderer' attribute'
    touchManagedPtr area
    touchManagedPtr renderer
    freeMem attribute'
    return result

#if defined(ENABLE_OVERLOADING)
data CellAreaAttributeGetColumnMethodInfo
instance (signature ~ (b -> T.Text -> m Int32), MonadIO m, IsCellArea a, Gtk.CellRenderer.IsCellRenderer b) => O.OverloadedMethod CellAreaAttributeGetColumnMethodInfo a signature where
    overloadedMethod = cellAreaAttributeGetColumn

instance O.OverloadedMethodInfo CellAreaAttributeGetColumnMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellArea.cellAreaAttributeGetColumn",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellArea.html#v:cellAreaAttributeGetColumn"
        })


#endif

-- method CellArea::cell_get_property
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "area"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellArea" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCellArea" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "renderer"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellRenderer" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCellRenderer inside @area"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "property_name"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the name of the property to get"
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
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a location to return the value"
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

foreign import ccall "gtk_cell_area_cell_get_property" gtk_cell_area_cell_get_property :: 
    Ptr CellArea ->                         -- area : TInterface (Name {namespace = "Gtk", name = "CellArea"})
    Ptr Gtk.CellRenderer.CellRenderer ->    -- renderer : TInterface (Name {namespace = "Gtk", name = "CellRenderer"})
    CString ->                              -- property_name : TBasicType TUTF8
    Ptr GValue ->                           -- value : TGValue
    IO ()

-- | Gets the value of a cell property for /@renderer@/ in /@area@/.
-- 
-- /Since: 3.0/
cellAreaCellGetProperty ::
    (B.CallStack.HasCallStack, MonadIO m, IsCellArea a, Gtk.CellRenderer.IsCellRenderer b) =>
    a
    -- ^ /@area@/: a t'GI.Gtk.Objects.CellArea.CellArea'
    -> b
    -- ^ /@renderer@/: a t'GI.Gtk.Objects.CellRenderer.CellRenderer' inside /@area@/
    -> T.Text
    -- ^ /@propertyName@/: the name of the property to get
    -> GValue
    -- ^ /@value@/: a location to return the value
    -> m ()
cellAreaCellGetProperty area renderer propertyName value = liftIO $ do
    area' <- unsafeManagedPtrCastPtr area
    renderer' <- unsafeManagedPtrCastPtr renderer
    propertyName' <- textToCString propertyName
    value' <- unsafeManagedPtrGetPtr value
    gtk_cell_area_cell_get_property area' renderer' propertyName' value'
    touchManagedPtr area
    touchManagedPtr renderer
    touchManagedPtr value
    freeMem propertyName'
    return ()

#if defined(ENABLE_OVERLOADING)
data CellAreaCellGetPropertyMethodInfo
instance (signature ~ (b -> T.Text -> GValue -> m ()), MonadIO m, IsCellArea a, Gtk.CellRenderer.IsCellRenderer b) => O.OverloadedMethod CellAreaCellGetPropertyMethodInfo a signature where
    overloadedMethod = cellAreaCellGetProperty

instance O.OverloadedMethodInfo CellAreaCellGetPropertyMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellArea.cellAreaCellGetProperty",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellArea.html#v:cellAreaCellGetProperty"
        })


#endif

-- method CellArea::cell_set_property
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "area"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellArea" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCellArea" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "renderer"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellRenderer" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCellRenderer inside @area"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "property_name"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the name of the cell property to set"
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
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the value to set the cell property to"
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

foreign import ccall "gtk_cell_area_cell_set_property" gtk_cell_area_cell_set_property :: 
    Ptr CellArea ->                         -- area : TInterface (Name {namespace = "Gtk", name = "CellArea"})
    Ptr Gtk.CellRenderer.CellRenderer ->    -- renderer : TInterface (Name {namespace = "Gtk", name = "CellRenderer"})
    CString ->                              -- property_name : TBasicType TUTF8
    Ptr GValue ->                           -- value : TGValue
    IO ()

-- | Sets a cell property for /@renderer@/ in /@area@/.
-- 
-- /Since: 3.0/
cellAreaCellSetProperty ::
    (B.CallStack.HasCallStack, MonadIO m, IsCellArea a, Gtk.CellRenderer.IsCellRenderer b) =>
    a
    -- ^ /@area@/: a t'GI.Gtk.Objects.CellArea.CellArea'
    -> b
    -- ^ /@renderer@/: a t'GI.Gtk.Objects.CellRenderer.CellRenderer' inside /@area@/
    -> T.Text
    -- ^ /@propertyName@/: the name of the cell property to set
    -> GValue
    -- ^ /@value@/: the value to set the cell property to
    -> m ()
cellAreaCellSetProperty area renderer propertyName value = liftIO $ do
    area' <- unsafeManagedPtrCastPtr area
    renderer' <- unsafeManagedPtrCastPtr renderer
    propertyName' <- textToCString propertyName
    value' <- unsafeManagedPtrGetPtr value
    gtk_cell_area_cell_set_property area' renderer' propertyName' value'
    touchManagedPtr area
    touchManagedPtr renderer
    touchManagedPtr value
    freeMem propertyName'
    return ()

#if defined(ENABLE_OVERLOADING)
data CellAreaCellSetPropertyMethodInfo
instance (signature ~ (b -> T.Text -> GValue -> m ()), MonadIO m, IsCellArea a, Gtk.CellRenderer.IsCellRenderer b) => O.OverloadedMethod CellAreaCellSetPropertyMethodInfo a signature where
    overloadedMethod = cellAreaCellSetProperty

instance O.OverloadedMethodInfo CellAreaCellSetPropertyMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellArea.cellAreaCellSetProperty",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellArea.html#v:cellAreaCellSetProperty"
        })


#endif

-- method CellArea::copy_context
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "area"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellArea" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCellArea" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellAreaContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the #GtkCellAreaContext to copy"
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
--               (TInterface Name { namespace = "Gtk" , name = "CellAreaContext" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_cell_area_copy_context" gtk_cell_area_copy_context :: 
    Ptr CellArea ->                         -- area : TInterface (Name {namespace = "Gtk", name = "CellArea"})
    Ptr Gtk.CellAreaContext.CellAreaContext -> -- context : TInterface (Name {namespace = "Gtk", name = "CellAreaContext"})
    IO (Ptr Gtk.CellAreaContext.CellAreaContext)

-- | This is sometimes needed for cases where rows need to share
-- alignments in one orientation but may be separately grouped
-- in the opposing orientation.
-- 
-- For instance, t'GI.Gtk.Objects.IconView.IconView' creates all icons (rows) to have
-- the same width and the cells theirin to have the same
-- horizontal alignments. However each row of icons may have
-- a separate collective height. t'GI.Gtk.Objects.IconView.IconView' uses this to
-- request the heights of each row based on a context which
-- was already used to request all the row widths that are
-- to be displayed.
-- 
-- /Since: 3.0/
cellAreaCopyContext ::
    (B.CallStack.HasCallStack, MonadIO m, IsCellArea a, Gtk.CellAreaContext.IsCellAreaContext b) =>
    a
    -- ^ /@area@/: a t'GI.Gtk.Objects.CellArea.CellArea'
    -> b
    -- ^ /@context@/: the t'GI.Gtk.Objects.CellAreaContext.CellAreaContext' to copy
    -> m Gtk.CellAreaContext.CellAreaContext
    -- ^ __Returns:__ a newly created t'GI.Gtk.Objects.CellAreaContext.CellAreaContext' copy of /@context@/.
cellAreaCopyContext area context = liftIO $ do
    area' <- unsafeManagedPtrCastPtr area
    context' <- unsafeManagedPtrCastPtr context
    result <- gtk_cell_area_copy_context area' context'
    checkUnexpectedReturnNULL "cellAreaCopyContext" result
    result' <- (wrapObject Gtk.CellAreaContext.CellAreaContext) result
    touchManagedPtr area
    touchManagedPtr context
    return result'

#if defined(ENABLE_OVERLOADING)
data CellAreaCopyContextMethodInfo
instance (signature ~ (b -> m Gtk.CellAreaContext.CellAreaContext), MonadIO m, IsCellArea a, Gtk.CellAreaContext.IsCellAreaContext b) => O.OverloadedMethod CellAreaCopyContextMethodInfo a signature where
    overloadedMethod = cellAreaCopyContext

instance O.OverloadedMethodInfo CellAreaCopyContextMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellArea.cellAreaCopyContext",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellArea.html#v:cellAreaCopyContext"
        })


#endif

-- method CellArea::create_context
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "area"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellArea" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCellArea" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just
--               (TInterface Name { namespace = "Gtk" , name = "CellAreaContext" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_cell_area_create_context" gtk_cell_area_create_context :: 
    Ptr CellArea ->                         -- area : TInterface (Name {namespace = "Gtk", name = "CellArea"})
    IO (Ptr Gtk.CellAreaContext.CellAreaContext)

-- | Creates a t'GI.Gtk.Objects.CellAreaContext.CellAreaContext' to be used with /@area@/ for
-- all purposes. t'GI.Gtk.Objects.CellAreaContext.CellAreaContext' stores geometry information
-- for rows for which it was operated on, it is important to use
-- the same context for the same row of data at all times (i.e.
-- one should render and handle events with the same t'GI.Gtk.Objects.CellAreaContext.CellAreaContext'
-- which was used to request the size of those rows of data).
-- 
-- /Since: 3.0/
cellAreaCreateContext ::
    (B.CallStack.HasCallStack, MonadIO m, IsCellArea a) =>
    a
    -- ^ /@area@/: a t'GI.Gtk.Objects.CellArea.CellArea'
    -> m Gtk.CellAreaContext.CellAreaContext
    -- ^ __Returns:__ a newly created t'GI.Gtk.Objects.CellAreaContext.CellAreaContext' which can be used with /@area@/.
cellAreaCreateContext area = liftIO $ do
    area' <- unsafeManagedPtrCastPtr area
    result <- gtk_cell_area_create_context area'
    checkUnexpectedReturnNULL "cellAreaCreateContext" result
    result' <- (wrapObject Gtk.CellAreaContext.CellAreaContext) result
    touchManagedPtr area
    return result'

#if defined(ENABLE_OVERLOADING)
data CellAreaCreateContextMethodInfo
instance (signature ~ (m Gtk.CellAreaContext.CellAreaContext), MonadIO m, IsCellArea a) => O.OverloadedMethod CellAreaCreateContextMethodInfo a signature where
    overloadedMethod = cellAreaCreateContext

instance O.OverloadedMethodInfo CellAreaCreateContextMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellArea.cellAreaCreateContext",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellArea.html#v:cellAreaCreateContext"
        })


#endif

-- method CellArea::event
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "area"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellArea" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCellArea" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellAreaContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the #GtkCellAreaContext for this row of data."
--                 , sinceVersion = Nothing
--                 }
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
--                 { rawDocText = Just "the #GtkWidget that @area is rendering to"
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
--                 { rawDocText = Just "the #GdkEvent to handle"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "cell_area"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "Rectangle" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the @widget relative coordinates for @area"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "flags"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellRendererState" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "the #GtkCellRendererState for @area in this row."
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

foreign import ccall "gtk_cell_area_event" gtk_cell_area_event :: 
    Ptr CellArea ->                         -- area : TInterface (Name {namespace = "Gtk", name = "CellArea"})
    Ptr Gtk.CellAreaContext.CellAreaContext -> -- context : TInterface (Name {namespace = "Gtk", name = "CellAreaContext"})
    Ptr Gtk.Widget.Widget ->                -- widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    Ptr Gdk.Event.Event ->                  -- event : TInterface (Name {namespace = "Gdk", name = "Event"})
    Ptr Gdk.Rectangle.Rectangle ->          -- cell_area : TInterface (Name {namespace = "Gdk", name = "Rectangle"})
    CUInt ->                                -- flags : TInterface (Name {namespace = "Gtk", name = "CellRendererState"})
    IO Int32

-- | Delegates event handling to a t'GI.Gtk.Objects.CellArea.CellArea'.
-- 
-- /Since: 3.0/
cellAreaEvent ::
    (B.CallStack.HasCallStack, MonadIO m, IsCellArea a, Gtk.CellAreaContext.IsCellAreaContext b, Gtk.Widget.IsWidget c) =>
    a
    -- ^ /@area@/: a t'GI.Gtk.Objects.CellArea.CellArea'
    -> b
    -- ^ /@context@/: the t'GI.Gtk.Objects.CellAreaContext.CellAreaContext' for this row of data.
    -> c
    -- ^ /@widget@/: the t'GI.Gtk.Objects.Widget.Widget' that /@area@/ is rendering to
    -> Gdk.Event.Event
    -- ^ /@event@/: the t'GI.Gdk.Unions.Event.Event' to handle
    -> Gdk.Rectangle.Rectangle
    -- ^ /@cellArea@/: the /@widget@/ relative coordinates for /@area@/
    -> [Gtk.Flags.CellRendererState]
    -- ^ /@flags@/: the t'GI.Gtk.Flags.CellRendererState' for /@area@/ in this row.
    -> m Int32
    -- ^ __Returns:__ 'P.True' if the event was handled by /@area@/.
cellAreaEvent area context widget event cellArea flags = liftIO $ do
    area' <- unsafeManagedPtrCastPtr area
    context' <- unsafeManagedPtrCastPtr context
    widget' <- unsafeManagedPtrCastPtr widget
    event' <- unsafeManagedPtrGetPtr event
    cellArea' <- unsafeManagedPtrGetPtr cellArea
    let flags' = gflagsToWord flags
    result <- gtk_cell_area_event area' context' widget' event' cellArea' flags'
    touchManagedPtr area
    touchManagedPtr context
    touchManagedPtr widget
    touchManagedPtr event
    touchManagedPtr cellArea
    return result

#if defined(ENABLE_OVERLOADING)
data CellAreaEventMethodInfo
instance (signature ~ (b -> c -> Gdk.Event.Event -> Gdk.Rectangle.Rectangle -> [Gtk.Flags.CellRendererState] -> m Int32), MonadIO m, IsCellArea a, Gtk.CellAreaContext.IsCellAreaContext b, Gtk.Widget.IsWidget c) => O.OverloadedMethod CellAreaEventMethodInfo a signature where
    overloadedMethod = cellAreaEvent

instance O.OverloadedMethodInfo CellAreaEventMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellArea.cellAreaEvent",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellArea.html#v:cellAreaEvent"
        })


#endif

-- method CellArea::focus
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "area"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellArea" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCellArea" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "direction"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "DirectionType" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the #GtkDirectionType"
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

foreign import ccall "gtk_cell_area_focus" gtk_cell_area_focus :: 
    Ptr CellArea ->                         -- area : TInterface (Name {namespace = "Gtk", name = "CellArea"})
    CUInt ->                                -- direction : TInterface (Name {namespace = "Gtk", name = "DirectionType"})
    IO CInt

-- | This should be called by the /@area@/’s owning layout widget
-- when focus is to be passed to /@area@/, or moved within /@area@/
-- for a given /@direction@/ and row data.
-- 
-- Implementing t'GI.Gtk.Objects.CellArea.CellArea' classes should implement this
-- method to receive and navigate focus in its own way particular
-- to how it lays out cells.
-- 
-- /Since: 3.0/
cellAreaFocus ::
    (B.CallStack.HasCallStack, MonadIO m, IsCellArea a) =>
    a
    -- ^ /@area@/: a t'GI.Gtk.Objects.CellArea.CellArea'
    -> Gtk.Enums.DirectionType
    -- ^ /@direction@/: the t'GI.Gtk.Enums.DirectionType'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if focus remains inside /@area@/ as a result of this call.
cellAreaFocus area direction = liftIO $ do
    area' <- unsafeManagedPtrCastPtr area
    let direction' = (fromIntegral . fromEnum) direction
    result <- gtk_cell_area_focus area' direction'
    let result' = (/= 0) result
    touchManagedPtr area
    return result'

#if defined(ENABLE_OVERLOADING)
data CellAreaFocusMethodInfo
instance (signature ~ (Gtk.Enums.DirectionType -> m Bool), MonadIO m, IsCellArea a) => O.OverloadedMethod CellAreaFocusMethodInfo a signature where
    overloadedMethod = cellAreaFocus

instance O.OverloadedMethodInfo CellAreaFocusMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellArea.cellAreaFocus",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellArea.html#v:cellAreaFocus"
        })


#endif

-- method CellArea::foreach
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "area"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellArea" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCellArea" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "callback"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellCallback" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the #GtkCellCallback to call"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeCall
--           , argClosure = 2
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "callback_data"
--           , argType = TBasicType TPtr
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "user provided data pointer"
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

foreign import ccall "gtk_cell_area_foreach" gtk_cell_area_foreach :: 
    Ptr CellArea ->                         -- area : TInterface (Name {namespace = "Gtk", name = "CellArea"})
    FunPtr Gtk.Callbacks.C_CellCallback ->  -- callback : TInterface (Name {namespace = "Gtk", name = "CellCallback"})
    Ptr () ->                               -- callback_data : TBasicType TPtr
    IO ()

-- | Calls /@callback@/ for every t'GI.Gtk.Objects.CellRenderer.CellRenderer' in /@area@/.
-- 
-- /Since: 3.0/
cellAreaForeach ::
    (B.CallStack.HasCallStack, MonadIO m, IsCellArea a) =>
    a
    -- ^ /@area@/: a t'GI.Gtk.Objects.CellArea.CellArea'
    -> Gtk.Callbacks.CellCallback
    -- ^ /@callback@/: the t'GI.Gtk.Callbacks.CellCallback' to call
    -> m ()
cellAreaForeach area callback = liftIO $ do
    area' <- unsafeManagedPtrCastPtr area
    callback' <- Gtk.Callbacks.mk_CellCallback (Gtk.Callbacks.wrap_CellCallback Nothing (Gtk.Callbacks.drop_closures_CellCallback callback))
    let callbackData = nullPtr
    gtk_cell_area_foreach area' callback' callbackData
    safeFreeFunPtr $ castFunPtrToPtr callback'
    touchManagedPtr area
    return ()

#if defined(ENABLE_OVERLOADING)
data CellAreaForeachMethodInfo
instance (signature ~ (Gtk.Callbacks.CellCallback -> m ()), MonadIO m, IsCellArea a) => O.OverloadedMethod CellAreaForeachMethodInfo a signature where
    overloadedMethod = cellAreaForeach

instance O.OverloadedMethodInfo CellAreaForeachMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellArea.cellAreaForeach",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellArea.html#v:cellAreaForeach"
        })


#endif

-- method CellArea::foreach_alloc
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "area"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellArea" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCellArea" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellAreaContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the #GtkCellAreaContext for this row of data."
--                 , sinceVersion = Nothing
--                 }
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
--                 { rawDocText = Just "the #GtkWidget that @area is rendering to"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "cell_area"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "Rectangle" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "the @widget relative coordinates and size for @area"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "background_area"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "Rectangle" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "the @widget relative coordinates of the background area"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "callback"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellAllocCallback" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the #GtkCellAllocCallback to call"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeCall
--           , argClosure = 6
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "callback_data"
--           , argType = TBasicType TPtr
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "user provided data pointer"
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

foreign import ccall "gtk_cell_area_foreach_alloc" gtk_cell_area_foreach_alloc :: 
    Ptr CellArea ->                         -- area : TInterface (Name {namespace = "Gtk", name = "CellArea"})
    Ptr Gtk.CellAreaContext.CellAreaContext -> -- context : TInterface (Name {namespace = "Gtk", name = "CellAreaContext"})
    Ptr Gtk.Widget.Widget ->                -- widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    Ptr Gdk.Rectangle.Rectangle ->          -- cell_area : TInterface (Name {namespace = "Gdk", name = "Rectangle"})
    Ptr Gdk.Rectangle.Rectangle ->          -- background_area : TInterface (Name {namespace = "Gdk", name = "Rectangle"})
    FunPtr Gtk.Callbacks.C_CellAllocCallback -> -- callback : TInterface (Name {namespace = "Gtk", name = "CellAllocCallback"})
    Ptr () ->                               -- callback_data : TBasicType TPtr
    IO ()

-- | Calls /@callback@/ for every t'GI.Gtk.Objects.CellRenderer.CellRenderer' in /@area@/ with the
-- allocated rectangle inside /@cellArea@/.
-- 
-- /Since: 3.0/
cellAreaForeachAlloc ::
    (B.CallStack.HasCallStack, MonadIO m, IsCellArea a, Gtk.CellAreaContext.IsCellAreaContext b, Gtk.Widget.IsWidget c) =>
    a
    -- ^ /@area@/: a t'GI.Gtk.Objects.CellArea.CellArea'
    -> b
    -- ^ /@context@/: the t'GI.Gtk.Objects.CellAreaContext.CellAreaContext' for this row of data.
    -> c
    -- ^ /@widget@/: the t'GI.Gtk.Objects.Widget.Widget' that /@area@/ is rendering to
    -> Gdk.Rectangle.Rectangle
    -- ^ /@cellArea@/: the /@widget@/ relative coordinates and size for /@area@/
    -> Gdk.Rectangle.Rectangle
    -- ^ /@backgroundArea@/: the /@widget@/ relative coordinates of the background area
    -> Gtk.Callbacks.CellAllocCallback
    -- ^ /@callback@/: the t'GI.Gtk.Callbacks.CellAllocCallback' to call
    -> m ()
cellAreaForeachAlloc area context widget cellArea backgroundArea callback = liftIO $ do
    area' <- unsafeManagedPtrCastPtr area
    context' <- unsafeManagedPtrCastPtr context
    widget' <- unsafeManagedPtrCastPtr widget
    cellArea' <- unsafeManagedPtrGetPtr cellArea
    backgroundArea' <- unsafeManagedPtrGetPtr backgroundArea
    callback' <- Gtk.Callbacks.mk_CellAllocCallback (Gtk.Callbacks.wrap_CellAllocCallback Nothing (Gtk.Callbacks.drop_closures_CellAllocCallback callback))
    let callbackData = nullPtr
    gtk_cell_area_foreach_alloc area' context' widget' cellArea' backgroundArea' callback' callbackData
    safeFreeFunPtr $ castFunPtrToPtr callback'
    touchManagedPtr area
    touchManagedPtr context
    touchManagedPtr widget
    touchManagedPtr cellArea
    touchManagedPtr backgroundArea
    return ()

#if defined(ENABLE_OVERLOADING)
data CellAreaForeachAllocMethodInfo
instance (signature ~ (b -> c -> Gdk.Rectangle.Rectangle -> Gdk.Rectangle.Rectangle -> Gtk.Callbacks.CellAllocCallback -> m ()), MonadIO m, IsCellArea a, Gtk.CellAreaContext.IsCellAreaContext b, Gtk.Widget.IsWidget c) => O.OverloadedMethod CellAreaForeachAllocMethodInfo a signature where
    overloadedMethod = cellAreaForeachAlloc

instance O.OverloadedMethodInfo CellAreaForeachAllocMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellArea.cellAreaForeachAlloc",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellArea.html#v:cellAreaForeachAlloc"
        })


#endif

-- method CellArea::get_cell_allocation
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "area"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellArea" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCellArea" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellAreaContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "the #GtkCellAreaContext used to hold sizes for @area."
--                 , sinceVersion = Nothing
--                 }
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
--                 { rawDocText = Just "the #GtkWidget that @area is rendering on"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "renderer"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellRenderer" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "the #GtkCellRenderer to get the allocation for"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "cell_area"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "Rectangle" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the whole allocated area for @area in @widget\n            for this row"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "allocation"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "Rectangle" }
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "where to store the allocation for @renderer"
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

foreign import ccall "gtk_cell_area_get_cell_allocation" gtk_cell_area_get_cell_allocation :: 
    Ptr CellArea ->                         -- area : TInterface (Name {namespace = "Gtk", name = "CellArea"})
    Ptr Gtk.CellAreaContext.CellAreaContext -> -- context : TInterface (Name {namespace = "Gtk", name = "CellAreaContext"})
    Ptr Gtk.Widget.Widget ->                -- widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    Ptr Gtk.CellRenderer.CellRenderer ->    -- renderer : TInterface (Name {namespace = "Gtk", name = "CellRenderer"})
    Ptr Gdk.Rectangle.Rectangle ->          -- cell_area : TInterface (Name {namespace = "Gdk", name = "Rectangle"})
    Ptr Gdk.Rectangle.Rectangle ->          -- allocation : TInterface (Name {namespace = "Gdk", name = "Rectangle"})
    IO ()

-- | Derives the allocation of /@renderer@/ inside /@area@/ if /@area@/
-- were to be renderered in /@cellArea@/.
-- 
-- /Since: 3.0/
cellAreaGetCellAllocation ::
    (B.CallStack.HasCallStack, MonadIO m, IsCellArea a, Gtk.CellAreaContext.IsCellAreaContext b, Gtk.Widget.IsWidget c, Gtk.CellRenderer.IsCellRenderer d) =>
    a
    -- ^ /@area@/: a t'GI.Gtk.Objects.CellArea.CellArea'
    -> b
    -- ^ /@context@/: the t'GI.Gtk.Objects.CellAreaContext.CellAreaContext' used to hold sizes for /@area@/.
    -> c
    -- ^ /@widget@/: the t'GI.Gtk.Objects.Widget.Widget' that /@area@/ is rendering on
    -> d
    -- ^ /@renderer@/: the t'GI.Gtk.Objects.CellRenderer.CellRenderer' to get the allocation for
    -> Gdk.Rectangle.Rectangle
    -- ^ /@cellArea@/: the whole allocated area for /@area@/ in /@widget@/
    --             for this row
    -> m (Gdk.Rectangle.Rectangle)
cellAreaGetCellAllocation area context widget renderer cellArea = liftIO $ do
    area' <- unsafeManagedPtrCastPtr area
    context' <- unsafeManagedPtrCastPtr context
    widget' <- unsafeManagedPtrCastPtr widget
    renderer' <- unsafeManagedPtrCastPtr renderer
    cellArea' <- unsafeManagedPtrGetPtr cellArea
    allocation <- SP.callocBoxedBytes 16 :: IO (Ptr Gdk.Rectangle.Rectangle)
    gtk_cell_area_get_cell_allocation area' context' widget' renderer' cellArea' allocation
    allocation' <- (wrapBoxed Gdk.Rectangle.Rectangle) allocation
    touchManagedPtr area
    touchManagedPtr context
    touchManagedPtr widget
    touchManagedPtr renderer
    touchManagedPtr cellArea
    return allocation'

#if defined(ENABLE_OVERLOADING)
data CellAreaGetCellAllocationMethodInfo
instance (signature ~ (b -> c -> d -> Gdk.Rectangle.Rectangle -> m (Gdk.Rectangle.Rectangle)), MonadIO m, IsCellArea a, Gtk.CellAreaContext.IsCellAreaContext b, Gtk.Widget.IsWidget c, Gtk.CellRenderer.IsCellRenderer d) => O.OverloadedMethod CellAreaGetCellAllocationMethodInfo a signature where
    overloadedMethod = cellAreaGetCellAllocation

instance O.OverloadedMethodInfo CellAreaGetCellAllocationMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellArea.cellAreaGetCellAllocation",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellArea.html#v:cellAreaGetCellAllocation"
        })


#endif

-- method CellArea::get_cell_at_position
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "area"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellArea" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCellArea" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellAreaContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "the #GtkCellAreaContext used to hold sizes for @area."
--                 , sinceVersion = Nothing
--                 }
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
--                 { rawDocText = Just "the #GtkWidget that @area is rendering on"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "cell_area"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "Rectangle" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the whole allocated area for @area in @widget\n            for this row"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "x"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the x position" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "y"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the y position" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "alloc_area"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "Rectangle" }
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "where to store the inner allocated area of the\n                                 returned cell renderer, or %NULL."
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
-- returnType: Just
--               (TInterface Name { namespace = "Gtk" , name = "CellRenderer" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_cell_area_get_cell_at_position" gtk_cell_area_get_cell_at_position :: 
    Ptr CellArea ->                         -- area : TInterface (Name {namespace = "Gtk", name = "CellArea"})
    Ptr Gtk.CellAreaContext.CellAreaContext -> -- context : TInterface (Name {namespace = "Gtk", name = "CellAreaContext"})
    Ptr Gtk.Widget.Widget ->                -- widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    Ptr Gdk.Rectangle.Rectangle ->          -- cell_area : TInterface (Name {namespace = "Gdk", name = "Rectangle"})
    Int32 ->                                -- x : TBasicType TInt
    Int32 ->                                -- y : TBasicType TInt
    Ptr Gdk.Rectangle.Rectangle ->          -- alloc_area : TInterface (Name {namespace = "Gdk", name = "Rectangle"})
    IO (Ptr Gtk.CellRenderer.CellRenderer)

-- | Gets the t'GI.Gtk.Objects.CellRenderer.CellRenderer' at /@x@/ and /@y@/ coordinates inside /@area@/ and optionally
-- returns the full cell allocation for it inside /@cellArea@/.
-- 
-- /Since: 3.0/
cellAreaGetCellAtPosition ::
    (B.CallStack.HasCallStack, MonadIO m, IsCellArea a, Gtk.CellAreaContext.IsCellAreaContext b, Gtk.Widget.IsWidget c) =>
    a
    -- ^ /@area@/: a t'GI.Gtk.Objects.CellArea.CellArea'
    -> b
    -- ^ /@context@/: the t'GI.Gtk.Objects.CellAreaContext.CellAreaContext' used to hold sizes for /@area@/.
    -> c
    -- ^ /@widget@/: the t'GI.Gtk.Objects.Widget.Widget' that /@area@/ is rendering on
    -> Gdk.Rectangle.Rectangle
    -- ^ /@cellArea@/: the whole allocated area for /@area@/ in /@widget@/
    --             for this row
    -> Int32
    -- ^ /@x@/: the x position
    -> Int32
    -- ^ /@y@/: the y position
    -> m ((Gtk.CellRenderer.CellRenderer, Gdk.Rectangle.Rectangle))
    -- ^ __Returns:__ the t'GI.Gtk.Objects.CellRenderer.CellRenderer' at /@x@/ and /@y@/.
cellAreaGetCellAtPosition area context widget cellArea x y = liftIO $ do
    area' <- unsafeManagedPtrCastPtr area
    context' <- unsafeManagedPtrCastPtr context
    widget' <- unsafeManagedPtrCastPtr widget
    cellArea' <- unsafeManagedPtrGetPtr cellArea
    allocArea <- SP.callocBoxedBytes 16 :: IO (Ptr Gdk.Rectangle.Rectangle)
    result <- gtk_cell_area_get_cell_at_position area' context' widget' cellArea' x y allocArea
    checkUnexpectedReturnNULL "cellAreaGetCellAtPosition" result
    result' <- (newObject Gtk.CellRenderer.CellRenderer) result
    allocArea' <- (wrapBoxed Gdk.Rectangle.Rectangle) allocArea
    touchManagedPtr area
    touchManagedPtr context
    touchManagedPtr widget
    touchManagedPtr cellArea
    return (result', allocArea')

#if defined(ENABLE_OVERLOADING)
data CellAreaGetCellAtPositionMethodInfo
instance (signature ~ (b -> c -> Gdk.Rectangle.Rectangle -> Int32 -> Int32 -> m ((Gtk.CellRenderer.CellRenderer, Gdk.Rectangle.Rectangle))), MonadIO m, IsCellArea a, Gtk.CellAreaContext.IsCellAreaContext b, Gtk.Widget.IsWidget c) => O.OverloadedMethod CellAreaGetCellAtPositionMethodInfo a signature where
    overloadedMethod = cellAreaGetCellAtPosition

instance O.OverloadedMethodInfo CellAreaGetCellAtPositionMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellArea.cellAreaGetCellAtPosition",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellArea.html#v:cellAreaGetCellAtPosition"
        })


#endif

-- method CellArea::get_current_path_string
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "area"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellArea" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCellArea" , sinceVersion = Nothing }
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

foreign import ccall "gtk_cell_area_get_current_path_string" gtk_cell_area_get_current_path_string :: 
    Ptr CellArea ->                         -- area : TInterface (Name {namespace = "Gtk", name = "CellArea"})
    IO CString

-- | Gets the current t'GI.Gtk.Structs.TreePath.TreePath' string for the currently
-- applied t'GI.Gtk.Structs.TreeIter.TreeIter', this is implicitly updated when
-- 'GI.Gtk.Objects.CellArea.cellAreaApplyAttributes' is called and can be
-- used to interact with renderers from t'GI.Gtk.Objects.CellArea.CellArea'
-- subclasses.
-- 
-- /Since: 3.0/
cellAreaGetCurrentPathString ::
    (B.CallStack.HasCallStack, MonadIO m, IsCellArea a) =>
    a
    -- ^ /@area@/: a t'GI.Gtk.Objects.CellArea.CellArea'
    -> m T.Text
    -- ^ __Returns:__ The current t'GI.Gtk.Structs.TreePath.TreePath' string for the current
    -- attributes applied to /@area@/. This string belongs to the area and
    -- should not be freed.
cellAreaGetCurrentPathString area = liftIO $ do
    area' <- unsafeManagedPtrCastPtr area
    result <- gtk_cell_area_get_current_path_string area'
    checkUnexpectedReturnNULL "cellAreaGetCurrentPathString" result
    result' <- cstringToText result
    touchManagedPtr area
    return result'

#if defined(ENABLE_OVERLOADING)
data CellAreaGetCurrentPathStringMethodInfo
instance (signature ~ (m T.Text), MonadIO m, IsCellArea a) => O.OverloadedMethod CellAreaGetCurrentPathStringMethodInfo a signature where
    overloadedMethod = cellAreaGetCurrentPathString

instance O.OverloadedMethodInfo CellAreaGetCurrentPathStringMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellArea.cellAreaGetCurrentPathString",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellArea.html#v:cellAreaGetCurrentPathString"
        })


#endif

-- method CellArea::get_edit_widget
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "area"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellArea" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCellArea" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just
--               (TInterface Name { namespace = "Gtk" , name = "CellEditable" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_cell_area_get_edit_widget" gtk_cell_area_get_edit_widget :: 
    Ptr CellArea ->                         -- area : TInterface (Name {namespace = "Gtk", name = "CellArea"})
    IO (Ptr Gtk.CellEditable.CellEditable)

-- | Gets the t'GI.Gtk.Interfaces.CellEditable.CellEditable' widget currently used
-- to edit the currently edited cell.
-- 
-- /Since: 3.0/
cellAreaGetEditWidget ::
    (B.CallStack.HasCallStack, MonadIO m, IsCellArea a) =>
    a
    -- ^ /@area@/: a t'GI.Gtk.Objects.CellArea.CellArea'
    -> m Gtk.CellEditable.CellEditable
    -- ^ __Returns:__ The currently active t'GI.Gtk.Interfaces.CellEditable.CellEditable' widget
cellAreaGetEditWidget area = liftIO $ do
    area' <- unsafeManagedPtrCastPtr area
    result <- gtk_cell_area_get_edit_widget area'
    checkUnexpectedReturnNULL "cellAreaGetEditWidget" result
    result' <- (newObject Gtk.CellEditable.CellEditable) result
    touchManagedPtr area
    return result'

#if defined(ENABLE_OVERLOADING)
data CellAreaGetEditWidgetMethodInfo
instance (signature ~ (m Gtk.CellEditable.CellEditable), MonadIO m, IsCellArea a) => O.OverloadedMethod CellAreaGetEditWidgetMethodInfo a signature where
    overloadedMethod = cellAreaGetEditWidget

instance O.OverloadedMethodInfo CellAreaGetEditWidgetMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellArea.cellAreaGetEditWidget",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellArea.html#v:cellAreaGetEditWidget"
        })


#endif

-- method CellArea::get_edited_cell
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "area"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellArea" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCellArea" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just
--               (TInterface Name { namespace = "Gtk" , name = "CellRenderer" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_cell_area_get_edited_cell" gtk_cell_area_get_edited_cell :: 
    Ptr CellArea ->                         -- area : TInterface (Name {namespace = "Gtk", name = "CellArea"})
    IO (Ptr Gtk.CellRenderer.CellRenderer)

-- | Gets the t'GI.Gtk.Objects.CellRenderer.CellRenderer' in /@area@/ that is currently
-- being edited.
-- 
-- /Since: 3.0/
cellAreaGetEditedCell ::
    (B.CallStack.HasCallStack, MonadIO m, IsCellArea a) =>
    a
    -- ^ /@area@/: a t'GI.Gtk.Objects.CellArea.CellArea'
    -> m Gtk.CellRenderer.CellRenderer
    -- ^ __Returns:__ The currently edited t'GI.Gtk.Objects.CellRenderer.CellRenderer'
cellAreaGetEditedCell area = liftIO $ do
    area' <- unsafeManagedPtrCastPtr area
    result <- gtk_cell_area_get_edited_cell area'
    checkUnexpectedReturnNULL "cellAreaGetEditedCell" result
    result' <- (newObject Gtk.CellRenderer.CellRenderer) result
    touchManagedPtr area
    return result'

#if defined(ENABLE_OVERLOADING)
data CellAreaGetEditedCellMethodInfo
instance (signature ~ (m Gtk.CellRenderer.CellRenderer), MonadIO m, IsCellArea a) => O.OverloadedMethod CellAreaGetEditedCellMethodInfo a signature where
    overloadedMethod = cellAreaGetEditedCell

instance O.OverloadedMethodInfo CellAreaGetEditedCellMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellArea.cellAreaGetEditedCell",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellArea.html#v:cellAreaGetEditedCell"
        })


#endif

-- method CellArea::get_focus_cell
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "area"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellArea" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCellArea" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just
--               (TInterface Name { namespace = "Gtk" , name = "CellRenderer" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_cell_area_get_focus_cell" gtk_cell_area_get_focus_cell :: 
    Ptr CellArea ->                         -- area : TInterface (Name {namespace = "Gtk", name = "CellArea"})
    IO (Ptr Gtk.CellRenderer.CellRenderer)

-- | Retrieves the currently focused cell for /@area@/
-- 
-- /Since: 3.0/
cellAreaGetFocusCell ::
    (B.CallStack.HasCallStack, MonadIO m, IsCellArea a) =>
    a
    -- ^ /@area@/: a t'GI.Gtk.Objects.CellArea.CellArea'
    -> m Gtk.CellRenderer.CellRenderer
    -- ^ __Returns:__ the currently focused cell in /@area@/.
cellAreaGetFocusCell area = liftIO $ do
    area' <- unsafeManagedPtrCastPtr area
    result <- gtk_cell_area_get_focus_cell area'
    checkUnexpectedReturnNULL "cellAreaGetFocusCell" result
    result' <- (newObject Gtk.CellRenderer.CellRenderer) result
    touchManagedPtr area
    return result'

#if defined(ENABLE_OVERLOADING)
data CellAreaGetFocusCellMethodInfo
instance (signature ~ (m Gtk.CellRenderer.CellRenderer), MonadIO m, IsCellArea a) => O.OverloadedMethod CellAreaGetFocusCellMethodInfo a signature where
    overloadedMethod = cellAreaGetFocusCell

instance O.OverloadedMethodInfo CellAreaGetFocusCellMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellArea.cellAreaGetFocusCell",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellArea.html#v:cellAreaGetFocusCell"
        })


#endif

-- method CellArea::get_focus_from_sibling
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "area"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellArea" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCellArea" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "renderer"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellRenderer" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the #GtkCellRenderer"
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
--               (TInterface Name { namespace = "Gtk" , name = "CellRenderer" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_cell_area_get_focus_from_sibling" gtk_cell_area_get_focus_from_sibling :: 
    Ptr CellArea ->                         -- area : TInterface (Name {namespace = "Gtk", name = "CellArea"})
    Ptr Gtk.CellRenderer.CellRenderer ->    -- renderer : TInterface (Name {namespace = "Gtk", name = "CellRenderer"})
    IO (Ptr Gtk.CellRenderer.CellRenderer)

-- | Gets the t'GI.Gtk.Objects.CellRenderer.CellRenderer' which is expected to be focusable
-- for which /@renderer@/ is, or may be a sibling.
-- 
-- This is handy for t'GI.Gtk.Objects.CellArea.CellArea' subclasses when handling events,
-- after determining the renderer at the event location it can
-- then chose to activate the focus cell for which the event
-- cell may have been a sibling.
-- 
-- /Since: 3.0/
cellAreaGetFocusFromSibling ::
    (B.CallStack.HasCallStack, MonadIO m, IsCellArea a, Gtk.CellRenderer.IsCellRenderer b) =>
    a
    -- ^ /@area@/: a t'GI.Gtk.Objects.CellArea.CellArea'
    -> b
    -- ^ /@renderer@/: the t'GI.Gtk.Objects.CellRenderer.CellRenderer'
    -> m (Maybe Gtk.CellRenderer.CellRenderer)
    -- ^ __Returns:__ the t'GI.Gtk.Objects.CellRenderer.CellRenderer' for which /@renderer@/
    --    is a sibling, or 'P.Nothing'.
cellAreaGetFocusFromSibling area renderer = liftIO $ do
    area' <- unsafeManagedPtrCastPtr area
    renderer' <- unsafeManagedPtrCastPtr renderer
    result <- gtk_cell_area_get_focus_from_sibling area' renderer'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (newObject Gtk.CellRenderer.CellRenderer) result'
        return result''
    touchManagedPtr area
    touchManagedPtr renderer
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data CellAreaGetFocusFromSiblingMethodInfo
instance (signature ~ (b -> m (Maybe Gtk.CellRenderer.CellRenderer)), MonadIO m, IsCellArea a, Gtk.CellRenderer.IsCellRenderer b) => O.OverloadedMethod CellAreaGetFocusFromSiblingMethodInfo a signature where
    overloadedMethod = cellAreaGetFocusFromSibling

instance O.OverloadedMethodInfo CellAreaGetFocusFromSiblingMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellArea.cellAreaGetFocusFromSibling",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellArea.html#v:cellAreaGetFocusFromSibling"
        })


#endif

-- method CellArea::get_focus_siblings
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "area"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellArea" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCellArea" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "renderer"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellRenderer" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the #GtkCellRenderer expected to have focus"
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
--                  (TInterface Name { namespace = "Gtk" , name = "CellRenderer" }))
-- throws : False
-- Skip return : False

foreign import ccall "gtk_cell_area_get_focus_siblings" gtk_cell_area_get_focus_siblings :: 
    Ptr CellArea ->                         -- area : TInterface (Name {namespace = "Gtk", name = "CellArea"})
    Ptr Gtk.CellRenderer.CellRenderer ->    -- renderer : TInterface (Name {namespace = "Gtk", name = "CellRenderer"})
    IO (Ptr (GList (Ptr Gtk.CellRenderer.CellRenderer)))

-- | Gets the focus sibling cell renderers for /@renderer@/.
-- 
-- /Since: 3.0/
cellAreaGetFocusSiblings ::
    (B.CallStack.HasCallStack, MonadIO m, IsCellArea a, Gtk.CellRenderer.IsCellRenderer b) =>
    a
    -- ^ /@area@/: a t'GI.Gtk.Objects.CellArea.CellArea'
    -> b
    -- ^ /@renderer@/: the t'GI.Gtk.Objects.CellRenderer.CellRenderer' expected to have focus
    -> m [Gtk.CellRenderer.CellRenderer]
    -- ^ __Returns:__ A t'GI.GLib.Structs.List.List' of @/GtkCellRenderers/@.
    --       The returned list is internal and should not be freed.
cellAreaGetFocusSiblings area renderer = liftIO $ do
    area' <- unsafeManagedPtrCastPtr area
    renderer' <- unsafeManagedPtrCastPtr renderer
    result <- gtk_cell_area_get_focus_siblings area' renderer'
    result' <- unpackGList result
    result'' <- mapM (newObject Gtk.CellRenderer.CellRenderer) result'
    touchManagedPtr area
    touchManagedPtr renderer
    return result''

#if defined(ENABLE_OVERLOADING)
data CellAreaGetFocusSiblingsMethodInfo
instance (signature ~ (b -> m [Gtk.CellRenderer.CellRenderer]), MonadIO m, IsCellArea a, Gtk.CellRenderer.IsCellRenderer b) => O.OverloadedMethod CellAreaGetFocusSiblingsMethodInfo a signature where
    overloadedMethod = cellAreaGetFocusSiblings

instance O.OverloadedMethodInfo CellAreaGetFocusSiblingsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellArea.cellAreaGetFocusSiblings",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellArea.html#v:cellAreaGetFocusSiblings"
        })


#endif

-- method CellArea::get_preferred_height
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "area"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellArea" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCellArea" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellAreaContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "the #GtkCellAreaContext to perform this request with"
--                 , sinceVersion = Nothing
--                 }
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
--                 { rawDocText = Just "the #GtkWidget where @area will be rendering"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "minimum_height"
--           , argType = TBasicType TInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "location to store the minimum height, or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       , Arg
--           { argCName = "natural_height"
--           , argType = TBasicType TInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "location to store the natural height, or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       ]
-- Lengths: []
-- returnType: Nothing
-- throws : False
-- Skip return : False

foreign import ccall "gtk_cell_area_get_preferred_height" gtk_cell_area_get_preferred_height :: 
    Ptr CellArea ->                         -- area : TInterface (Name {namespace = "Gtk", name = "CellArea"})
    Ptr Gtk.CellAreaContext.CellAreaContext -> -- context : TInterface (Name {namespace = "Gtk", name = "CellAreaContext"})
    Ptr Gtk.Widget.Widget ->                -- widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    Ptr Int32 ->                            -- minimum_height : TBasicType TInt
    Ptr Int32 ->                            -- natural_height : TBasicType TInt
    IO ()

-- | Retrieves a cell area’s initial minimum and natural height.
-- 
-- /@area@/ will store some geometrical information in /@context@/ along the way;
-- when requesting sizes over an arbitrary number of rows, it’s not important
-- to check the /@minimumHeight@/ and /@naturalHeight@/ of this call but rather to
-- consult 'GI.Gtk.Objects.CellAreaContext.cellAreaContextGetPreferredHeight' after a series of
-- requests.
-- 
-- /Since: 3.0/
cellAreaGetPreferredHeight ::
    (B.CallStack.HasCallStack, MonadIO m, IsCellArea a, Gtk.CellAreaContext.IsCellAreaContext b, Gtk.Widget.IsWidget c) =>
    a
    -- ^ /@area@/: a t'GI.Gtk.Objects.CellArea.CellArea'
    -> b
    -- ^ /@context@/: the t'GI.Gtk.Objects.CellAreaContext.CellAreaContext' to perform this request with
    -> c
    -- ^ /@widget@/: the t'GI.Gtk.Objects.Widget.Widget' where /@area@/ will be rendering
    -> m ((Int32, Int32))
cellAreaGetPreferredHeight area context widget = liftIO $ do
    area' <- unsafeManagedPtrCastPtr area
    context' <- unsafeManagedPtrCastPtr context
    widget' <- unsafeManagedPtrCastPtr widget
    minimumHeight <- allocMem :: IO (Ptr Int32)
    naturalHeight <- allocMem :: IO (Ptr Int32)
    gtk_cell_area_get_preferred_height area' context' widget' minimumHeight naturalHeight
    minimumHeight' <- peek minimumHeight
    naturalHeight' <- peek naturalHeight
    touchManagedPtr area
    touchManagedPtr context
    touchManagedPtr widget
    freeMem minimumHeight
    freeMem naturalHeight
    return (minimumHeight', naturalHeight')

#if defined(ENABLE_OVERLOADING)
data CellAreaGetPreferredHeightMethodInfo
instance (signature ~ (b -> c -> m ((Int32, Int32))), MonadIO m, IsCellArea a, Gtk.CellAreaContext.IsCellAreaContext b, Gtk.Widget.IsWidget c) => O.OverloadedMethod CellAreaGetPreferredHeightMethodInfo a signature where
    overloadedMethod = cellAreaGetPreferredHeight

instance O.OverloadedMethodInfo CellAreaGetPreferredHeightMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellArea.cellAreaGetPreferredHeight",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellArea.html#v:cellAreaGetPreferredHeight"
        })


#endif

-- method CellArea::get_preferred_height_for_width
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "area"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellArea" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCellArea" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellAreaContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the #GtkCellAreaContext which has already been requested for widths."
--                 , sinceVersion = Nothing
--                 }
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
--                 { rawDocText = Just "the #GtkWidget where @area will be rendering"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "width"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "the width for which to check the height of this area"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "minimum_height"
--           , argType = TBasicType TInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "location to store the minimum height, or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       , Arg
--           { argCName = "natural_height"
--           , argType = TBasicType TInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "location to store the natural height, or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       ]
-- Lengths: []
-- returnType: Nothing
-- throws : False
-- Skip return : False

foreign import ccall "gtk_cell_area_get_preferred_height_for_width" gtk_cell_area_get_preferred_height_for_width :: 
    Ptr CellArea ->                         -- area : TInterface (Name {namespace = "Gtk", name = "CellArea"})
    Ptr Gtk.CellAreaContext.CellAreaContext -> -- context : TInterface (Name {namespace = "Gtk", name = "CellAreaContext"})
    Ptr Gtk.Widget.Widget ->                -- widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    Int32 ->                                -- width : TBasicType TInt
    Ptr Int32 ->                            -- minimum_height : TBasicType TInt
    Ptr Int32 ->                            -- natural_height : TBasicType TInt
    IO ()

-- | Retrieves a cell area’s minimum and natural height if it would be given
-- the specified /@width@/.
-- 
-- /@area@/ stores some geometrical information in /@context@/ along the way
-- while calling 'GI.Gtk.Objects.CellArea.cellAreaGetPreferredWidth'. It’s important to
-- perform a series of 'GI.Gtk.Objects.CellArea.cellAreaGetPreferredWidth' requests with
-- /@context@/ first and then call 'GI.Gtk.Objects.CellArea.cellAreaGetPreferredHeightForWidth'
-- on each cell area individually to get the height for width of each
-- fully requested row.
-- 
-- If at some point, the width of a single row changes, it should be
-- requested with 'GI.Gtk.Objects.CellArea.cellAreaGetPreferredWidth' again and then
-- the full width of the requested rows checked again with
-- 'GI.Gtk.Objects.CellAreaContext.cellAreaContextGetPreferredWidth'.
-- 
-- /Since: 3.0/
cellAreaGetPreferredHeightForWidth ::
    (B.CallStack.HasCallStack, MonadIO m, IsCellArea a, Gtk.CellAreaContext.IsCellAreaContext b, Gtk.Widget.IsWidget c) =>
    a
    -- ^ /@area@/: a t'GI.Gtk.Objects.CellArea.CellArea'
    -> b
    -- ^ /@context@/: the t'GI.Gtk.Objects.CellAreaContext.CellAreaContext' which has already been requested for widths.
    -> c
    -- ^ /@widget@/: the t'GI.Gtk.Objects.Widget.Widget' where /@area@/ will be rendering
    -> Int32
    -- ^ /@width@/: the width for which to check the height of this area
    -> m ((Int32, Int32))
cellAreaGetPreferredHeightForWidth area context widget width = liftIO $ do
    area' <- unsafeManagedPtrCastPtr area
    context' <- unsafeManagedPtrCastPtr context
    widget' <- unsafeManagedPtrCastPtr widget
    minimumHeight <- allocMem :: IO (Ptr Int32)
    naturalHeight <- allocMem :: IO (Ptr Int32)
    gtk_cell_area_get_preferred_height_for_width area' context' widget' width minimumHeight naturalHeight
    minimumHeight' <- peek minimumHeight
    naturalHeight' <- peek naturalHeight
    touchManagedPtr area
    touchManagedPtr context
    touchManagedPtr widget
    freeMem minimumHeight
    freeMem naturalHeight
    return (minimumHeight', naturalHeight')

#if defined(ENABLE_OVERLOADING)
data CellAreaGetPreferredHeightForWidthMethodInfo
instance (signature ~ (b -> c -> Int32 -> m ((Int32, Int32))), MonadIO m, IsCellArea a, Gtk.CellAreaContext.IsCellAreaContext b, Gtk.Widget.IsWidget c) => O.OverloadedMethod CellAreaGetPreferredHeightForWidthMethodInfo a signature where
    overloadedMethod = cellAreaGetPreferredHeightForWidth

instance O.OverloadedMethodInfo CellAreaGetPreferredHeightForWidthMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellArea.cellAreaGetPreferredHeightForWidth",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellArea.html#v:cellAreaGetPreferredHeightForWidth"
        })


#endif

-- method CellArea::get_preferred_width
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "area"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellArea" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCellArea" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellAreaContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "the #GtkCellAreaContext to perform this request with"
--                 , sinceVersion = Nothing
--                 }
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
--                 { rawDocText = Just "the #GtkWidget where @area will be rendering"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "minimum_width"
--           , argType = TBasicType TInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "location to store the minimum width, or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       , Arg
--           { argCName = "natural_width"
--           , argType = TBasicType TInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "location to store the natural width, or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       ]
-- Lengths: []
-- returnType: Nothing
-- throws : False
-- Skip return : False

foreign import ccall "gtk_cell_area_get_preferred_width" gtk_cell_area_get_preferred_width :: 
    Ptr CellArea ->                         -- area : TInterface (Name {namespace = "Gtk", name = "CellArea"})
    Ptr Gtk.CellAreaContext.CellAreaContext -> -- context : TInterface (Name {namespace = "Gtk", name = "CellAreaContext"})
    Ptr Gtk.Widget.Widget ->                -- widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    Ptr Int32 ->                            -- minimum_width : TBasicType TInt
    Ptr Int32 ->                            -- natural_width : TBasicType TInt
    IO ()

-- | Retrieves a cell area’s initial minimum and natural width.
-- 
-- /@area@/ will store some geometrical information in /@context@/ along the way;
-- when requesting sizes over an arbitrary number of rows, it’s not important
-- to check the /@minimumWidth@/ and /@naturalWidth@/ of this call but rather to
-- consult 'GI.Gtk.Objects.CellAreaContext.cellAreaContextGetPreferredWidth' after a series of
-- requests.
-- 
-- /Since: 3.0/
cellAreaGetPreferredWidth ::
    (B.CallStack.HasCallStack, MonadIO m, IsCellArea a, Gtk.CellAreaContext.IsCellAreaContext b, Gtk.Widget.IsWidget c) =>
    a
    -- ^ /@area@/: a t'GI.Gtk.Objects.CellArea.CellArea'
    -> b
    -- ^ /@context@/: the t'GI.Gtk.Objects.CellAreaContext.CellAreaContext' to perform this request with
    -> c
    -- ^ /@widget@/: the t'GI.Gtk.Objects.Widget.Widget' where /@area@/ will be rendering
    -> m ((Int32, Int32))
cellAreaGetPreferredWidth area context widget = liftIO $ do
    area' <- unsafeManagedPtrCastPtr area
    context' <- unsafeManagedPtrCastPtr context
    widget' <- unsafeManagedPtrCastPtr widget
    minimumWidth <- allocMem :: IO (Ptr Int32)
    naturalWidth <- allocMem :: IO (Ptr Int32)
    gtk_cell_area_get_preferred_width area' context' widget' minimumWidth naturalWidth
    minimumWidth' <- peek minimumWidth
    naturalWidth' <- peek naturalWidth
    touchManagedPtr area
    touchManagedPtr context
    touchManagedPtr widget
    freeMem minimumWidth
    freeMem naturalWidth
    return (minimumWidth', naturalWidth')

#if defined(ENABLE_OVERLOADING)
data CellAreaGetPreferredWidthMethodInfo
instance (signature ~ (b -> c -> m ((Int32, Int32))), MonadIO m, IsCellArea a, Gtk.CellAreaContext.IsCellAreaContext b, Gtk.Widget.IsWidget c) => O.OverloadedMethod CellAreaGetPreferredWidthMethodInfo a signature where
    overloadedMethod = cellAreaGetPreferredWidth

instance O.OverloadedMethodInfo CellAreaGetPreferredWidthMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellArea.cellAreaGetPreferredWidth",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellArea.html#v:cellAreaGetPreferredWidth"
        })


#endif

-- method CellArea::get_preferred_width_for_height
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "area"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellArea" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCellArea" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellAreaContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the #GtkCellAreaContext which has already been requested for widths."
--                 , sinceVersion = Nothing
--                 }
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
--                 { rawDocText = Just "the #GtkWidget where @area will be rendering"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "height"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "the height for which to check the width of this area"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "minimum_width"
--           , argType = TBasicType TInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "location to store the minimum width, or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       , Arg
--           { argCName = "natural_width"
--           , argType = TBasicType TInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "location to store the natural width, or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       ]
-- Lengths: []
-- returnType: Nothing
-- throws : False
-- Skip return : False

foreign import ccall "gtk_cell_area_get_preferred_width_for_height" gtk_cell_area_get_preferred_width_for_height :: 
    Ptr CellArea ->                         -- area : TInterface (Name {namespace = "Gtk", name = "CellArea"})
    Ptr Gtk.CellAreaContext.CellAreaContext -> -- context : TInterface (Name {namespace = "Gtk", name = "CellAreaContext"})
    Ptr Gtk.Widget.Widget ->                -- widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    Int32 ->                                -- height : TBasicType TInt
    Ptr Int32 ->                            -- minimum_width : TBasicType TInt
    Ptr Int32 ->                            -- natural_width : TBasicType TInt
    IO ()

-- | Retrieves a cell area’s minimum and natural width if it would be given
-- the specified /@height@/.
-- 
-- /@area@/ stores some geometrical information in /@context@/ along the way
-- while calling 'GI.Gtk.Objects.CellArea.cellAreaGetPreferredHeight'. It’s important to
-- perform a series of 'GI.Gtk.Objects.CellArea.cellAreaGetPreferredHeight' requests with
-- /@context@/ first and then call 'GI.Gtk.Objects.CellArea.cellAreaGetPreferredWidthForHeight'
-- on each cell area individually to get the height for width of each
-- fully requested row.
-- 
-- If at some point, the height of a single row changes, it should be
-- requested with 'GI.Gtk.Objects.CellArea.cellAreaGetPreferredHeight' again and then
-- the full height of the requested rows checked again with
-- 'GI.Gtk.Objects.CellAreaContext.cellAreaContextGetPreferredHeight'.
-- 
-- /Since: 3.0/
cellAreaGetPreferredWidthForHeight ::
    (B.CallStack.HasCallStack, MonadIO m, IsCellArea a, Gtk.CellAreaContext.IsCellAreaContext b, Gtk.Widget.IsWidget c) =>
    a
    -- ^ /@area@/: a t'GI.Gtk.Objects.CellArea.CellArea'
    -> b
    -- ^ /@context@/: the t'GI.Gtk.Objects.CellAreaContext.CellAreaContext' which has already been requested for widths.
    -> c
    -- ^ /@widget@/: the t'GI.Gtk.Objects.Widget.Widget' where /@area@/ will be rendering
    -> Int32
    -- ^ /@height@/: the height for which to check the width of this area
    -> m ((Int32, Int32))
cellAreaGetPreferredWidthForHeight area context widget height = liftIO $ do
    area' <- unsafeManagedPtrCastPtr area
    context' <- unsafeManagedPtrCastPtr context
    widget' <- unsafeManagedPtrCastPtr widget
    minimumWidth <- allocMem :: IO (Ptr Int32)
    naturalWidth <- allocMem :: IO (Ptr Int32)
    gtk_cell_area_get_preferred_width_for_height area' context' widget' height minimumWidth naturalWidth
    minimumWidth' <- peek minimumWidth
    naturalWidth' <- peek naturalWidth
    touchManagedPtr area
    touchManagedPtr context
    touchManagedPtr widget
    freeMem minimumWidth
    freeMem naturalWidth
    return (minimumWidth', naturalWidth')

#if defined(ENABLE_OVERLOADING)
data CellAreaGetPreferredWidthForHeightMethodInfo
instance (signature ~ (b -> c -> Int32 -> m ((Int32, Int32))), MonadIO m, IsCellArea a, Gtk.CellAreaContext.IsCellAreaContext b, Gtk.Widget.IsWidget c) => O.OverloadedMethod CellAreaGetPreferredWidthForHeightMethodInfo a signature where
    overloadedMethod = cellAreaGetPreferredWidthForHeight

instance O.OverloadedMethodInfo CellAreaGetPreferredWidthForHeightMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellArea.cellAreaGetPreferredWidthForHeight",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellArea.html#v:cellAreaGetPreferredWidthForHeight"
        })


#endif

-- method CellArea::get_request_mode
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "area"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellArea" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCellArea" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just
--               (TInterface Name { namespace = "Gtk" , name = "SizeRequestMode" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_cell_area_get_request_mode" gtk_cell_area_get_request_mode :: 
    Ptr CellArea ->                         -- area : TInterface (Name {namespace = "Gtk", name = "CellArea"})
    IO CUInt

-- | Gets whether the area prefers a height-for-width layout
-- or a width-for-height layout.
-- 
-- /Since: 3.0/
cellAreaGetRequestMode ::
    (B.CallStack.HasCallStack, MonadIO m, IsCellArea a) =>
    a
    -- ^ /@area@/: a t'GI.Gtk.Objects.CellArea.CellArea'
    -> m Gtk.Enums.SizeRequestMode
    -- ^ __Returns:__ The t'GI.Gtk.Enums.SizeRequestMode' preferred by /@area@/.
cellAreaGetRequestMode area = liftIO $ do
    area' <- unsafeManagedPtrCastPtr area
    result <- gtk_cell_area_get_request_mode area'
    let result' = (toEnum . fromIntegral) result
    touchManagedPtr area
    return result'

#if defined(ENABLE_OVERLOADING)
data CellAreaGetRequestModeMethodInfo
instance (signature ~ (m Gtk.Enums.SizeRequestMode), MonadIO m, IsCellArea a) => O.OverloadedMethod CellAreaGetRequestModeMethodInfo a signature where
    overloadedMethod = cellAreaGetRequestMode

instance O.OverloadedMethodInfo CellAreaGetRequestModeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellArea.cellAreaGetRequestMode",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellArea.html#v:cellAreaGetRequestMode"
        })


#endif

-- method CellArea::has_renderer
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "area"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellArea" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCellArea" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "renderer"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellRenderer" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the #GtkCellRenderer to check"
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

foreign import ccall "gtk_cell_area_has_renderer" gtk_cell_area_has_renderer :: 
    Ptr CellArea ->                         -- area : TInterface (Name {namespace = "Gtk", name = "CellArea"})
    Ptr Gtk.CellRenderer.CellRenderer ->    -- renderer : TInterface (Name {namespace = "Gtk", name = "CellRenderer"})
    IO CInt

-- | Checks if /@area@/ contains /@renderer@/.
-- 
-- /Since: 3.0/
cellAreaHasRenderer ::
    (B.CallStack.HasCallStack, MonadIO m, IsCellArea a, Gtk.CellRenderer.IsCellRenderer b) =>
    a
    -- ^ /@area@/: a t'GI.Gtk.Objects.CellArea.CellArea'
    -> b
    -- ^ /@renderer@/: the t'GI.Gtk.Objects.CellRenderer.CellRenderer' to check
    -> m Bool
    -- ^ __Returns:__ 'P.True' if /@renderer@/ is in the /@area@/.
cellAreaHasRenderer area renderer = liftIO $ do
    area' <- unsafeManagedPtrCastPtr area
    renderer' <- unsafeManagedPtrCastPtr renderer
    result <- gtk_cell_area_has_renderer area' renderer'
    let result' = (/= 0) result
    touchManagedPtr area
    touchManagedPtr renderer
    return result'

#if defined(ENABLE_OVERLOADING)
data CellAreaHasRendererMethodInfo
instance (signature ~ (b -> m Bool), MonadIO m, IsCellArea a, Gtk.CellRenderer.IsCellRenderer b) => O.OverloadedMethod CellAreaHasRendererMethodInfo a signature where
    overloadedMethod = cellAreaHasRenderer

instance O.OverloadedMethodInfo CellAreaHasRendererMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellArea.cellAreaHasRenderer",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellArea.html#v:cellAreaHasRenderer"
        })


#endif

-- method CellArea::inner_cell_area
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "area"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellArea" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCellArea" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "the #GtkWidget that @area is rendering onto"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "cell_area"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "Rectangle" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the @widget relative coordinates where one of @area\8217s cells\n            is to be placed"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "inner_area"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "Rectangle" }
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the return location for the inner cell area"
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

foreign import ccall "gtk_cell_area_inner_cell_area" gtk_cell_area_inner_cell_area :: 
    Ptr CellArea ->                         -- area : TInterface (Name {namespace = "Gtk", name = "CellArea"})
    Ptr Gtk.Widget.Widget ->                -- widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    Ptr Gdk.Rectangle.Rectangle ->          -- cell_area : TInterface (Name {namespace = "Gdk", name = "Rectangle"})
    Ptr Gdk.Rectangle.Rectangle ->          -- inner_area : TInterface (Name {namespace = "Gdk", name = "Rectangle"})
    IO ()

-- | This is a convenience function for t'GI.Gtk.Objects.CellArea.CellArea' implementations
-- to get the inner area where a given t'GI.Gtk.Objects.CellRenderer.CellRenderer' will be
-- rendered. It removes any padding previously added by 'GI.Gtk.Objects.CellArea.cellAreaRequestRenderer'.
-- 
-- /Since: 3.0/
cellAreaInnerCellArea ::
    (B.CallStack.HasCallStack, MonadIO m, IsCellArea a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@area@/: a t'GI.Gtk.Objects.CellArea.CellArea'
    -> b
    -- ^ /@widget@/: the t'GI.Gtk.Objects.Widget.Widget' that /@area@/ is rendering onto
    -> Gdk.Rectangle.Rectangle
    -- ^ /@cellArea@/: the /@widget@/ relative coordinates where one of /@area@/’s cells
    --             is to be placed
    -> m (Gdk.Rectangle.Rectangle)
cellAreaInnerCellArea area widget cellArea = liftIO $ do
    area' <- unsafeManagedPtrCastPtr area
    widget' <- unsafeManagedPtrCastPtr widget
    cellArea' <- unsafeManagedPtrGetPtr cellArea
    innerArea <- SP.callocBoxedBytes 16 :: IO (Ptr Gdk.Rectangle.Rectangle)
    gtk_cell_area_inner_cell_area area' widget' cellArea' innerArea
    innerArea' <- (wrapBoxed Gdk.Rectangle.Rectangle) innerArea
    touchManagedPtr area
    touchManagedPtr widget
    touchManagedPtr cellArea
    return innerArea'

#if defined(ENABLE_OVERLOADING)
data CellAreaInnerCellAreaMethodInfo
instance (signature ~ (b -> Gdk.Rectangle.Rectangle -> m (Gdk.Rectangle.Rectangle)), MonadIO m, IsCellArea a, Gtk.Widget.IsWidget b) => O.OverloadedMethod CellAreaInnerCellAreaMethodInfo a signature where
    overloadedMethod = cellAreaInnerCellArea

instance O.OverloadedMethodInfo CellAreaInnerCellAreaMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellArea.cellAreaInnerCellArea",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellArea.html#v:cellAreaInnerCellArea"
        })


#endif

-- method CellArea::is_activatable
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "area"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellArea" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCellArea" , sinceVersion = Nothing }
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

foreign import ccall "gtk_cell_area_is_activatable" gtk_cell_area_is_activatable :: 
    Ptr CellArea ->                         -- area : TInterface (Name {namespace = "Gtk", name = "CellArea"})
    IO CInt

-- | Returns whether the area can do anything when activated,
-- after applying new attributes to /@area@/.
-- 
-- /Since: 3.0/
cellAreaIsActivatable ::
    (B.CallStack.HasCallStack, MonadIO m, IsCellArea a) =>
    a
    -- ^ /@area@/: a t'GI.Gtk.Objects.CellArea.CellArea'
    -> m Bool
    -- ^ __Returns:__ whether /@area@/ can do anything when activated.
cellAreaIsActivatable area = liftIO $ do
    area' <- unsafeManagedPtrCastPtr area
    result <- gtk_cell_area_is_activatable area'
    let result' = (/= 0) result
    touchManagedPtr area
    return result'

#if defined(ENABLE_OVERLOADING)
data CellAreaIsActivatableMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsCellArea a) => O.OverloadedMethod CellAreaIsActivatableMethodInfo a signature where
    overloadedMethod = cellAreaIsActivatable

instance O.OverloadedMethodInfo CellAreaIsActivatableMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellArea.cellAreaIsActivatable",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellArea.html#v:cellAreaIsActivatable"
        })


#endif

-- method CellArea::is_focus_sibling
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "area"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellArea" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCellArea" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "renderer"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellRenderer" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the #GtkCellRenderer expected to have focus"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "sibling"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellRenderer" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the #GtkCellRenderer to check against @renderer\8217s sibling list"
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

foreign import ccall "gtk_cell_area_is_focus_sibling" gtk_cell_area_is_focus_sibling :: 
    Ptr CellArea ->                         -- area : TInterface (Name {namespace = "Gtk", name = "CellArea"})
    Ptr Gtk.CellRenderer.CellRenderer ->    -- renderer : TInterface (Name {namespace = "Gtk", name = "CellRenderer"})
    Ptr Gtk.CellRenderer.CellRenderer ->    -- sibling : TInterface (Name {namespace = "Gtk", name = "CellRenderer"})
    IO CInt

-- | Returns whether /@sibling@/ is one of /@renderer@/’s focus siblings
-- (see 'GI.Gtk.Objects.CellArea.cellAreaAddFocusSibling').
-- 
-- /Since: 3.0/
cellAreaIsFocusSibling ::
    (B.CallStack.HasCallStack, MonadIO m, IsCellArea a, Gtk.CellRenderer.IsCellRenderer b, Gtk.CellRenderer.IsCellRenderer c) =>
    a
    -- ^ /@area@/: a t'GI.Gtk.Objects.CellArea.CellArea'
    -> b
    -- ^ /@renderer@/: the t'GI.Gtk.Objects.CellRenderer.CellRenderer' expected to have focus
    -> c
    -- ^ /@sibling@/: the t'GI.Gtk.Objects.CellRenderer.CellRenderer' to check against /@renderer@/’s sibling list
    -> m Bool
    -- ^ __Returns:__ 'P.True' if /@sibling@/ is a focus sibling of /@renderer@/
cellAreaIsFocusSibling area renderer sibling = liftIO $ do
    area' <- unsafeManagedPtrCastPtr area
    renderer' <- unsafeManagedPtrCastPtr renderer
    sibling' <- unsafeManagedPtrCastPtr sibling
    result <- gtk_cell_area_is_focus_sibling area' renderer' sibling'
    let result' = (/= 0) result
    touchManagedPtr area
    touchManagedPtr renderer
    touchManagedPtr sibling
    return result'

#if defined(ENABLE_OVERLOADING)
data CellAreaIsFocusSiblingMethodInfo
instance (signature ~ (b -> c -> m Bool), MonadIO m, IsCellArea a, Gtk.CellRenderer.IsCellRenderer b, Gtk.CellRenderer.IsCellRenderer c) => O.OverloadedMethod CellAreaIsFocusSiblingMethodInfo a signature where
    overloadedMethod = cellAreaIsFocusSibling

instance O.OverloadedMethodInfo CellAreaIsFocusSiblingMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellArea.cellAreaIsFocusSibling",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellArea.html#v:cellAreaIsFocusSibling"
        })


#endif

-- method CellArea::remove
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "area"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellArea" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCellArea" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "renderer"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellRenderer" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the #GtkCellRenderer to remove from @area"
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

foreign import ccall "gtk_cell_area_remove" gtk_cell_area_remove :: 
    Ptr CellArea ->                         -- area : TInterface (Name {namespace = "Gtk", name = "CellArea"})
    Ptr Gtk.CellRenderer.CellRenderer ->    -- renderer : TInterface (Name {namespace = "Gtk", name = "CellRenderer"})
    IO ()

-- | Removes /@renderer@/ from /@area@/.
-- 
-- /Since: 3.0/
cellAreaRemove ::
    (B.CallStack.HasCallStack, MonadIO m, IsCellArea a, Gtk.CellRenderer.IsCellRenderer b) =>
    a
    -- ^ /@area@/: a t'GI.Gtk.Objects.CellArea.CellArea'
    -> b
    -- ^ /@renderer@/: the t'GI.Gtk.Objects.CellRenderer.CellRenderer' to remove from /@area@/
    -> m ()
cellAreaRemove area renderer = liftIO $ do
    area' <- unsafeManagedPtrCastPtr area
    renderer' <- unsafeManagedPtrCastPtr renderer
    gtk_cell_area_remove area' renderer'
    touchManagedPtr area
    touchManagedPtr renderer
    return ()

#if defined(ENABLE_OVERLOADING)
data CellAreaRemoveMethodInfo
instance (signature ~ (b -> m ()), MonadIO m, IsCellArea a, Gtk.CellRenderer.IsCellRenderer b) => O.OverloadedMethod CellAreaRemoveMethodInfo a signature where
    overloadedMethod = cellAreaRemove

instance O.OverloadedMethodInfo CellAreaRemoveMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellArea.cellAreaRemove",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellArea.html#v:cellAreaRemove"
        })


#endif

-- method CellArea::remove_focus_sibling
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "area"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellArea" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCellArea" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "renderer"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellRenderer" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the #GtkCellRenderer expected to have focus"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "sibling"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellRenderer" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the #GtkCellRenderer to remove from @renderer\8217s focus area"
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

foreign import ccall "gtk_cell_area_remove_focus_sibling" gtk_cell_area_remove_focus_sibling :: 
    Ptr CellArea ->                         -- area : TInterface (Name {namespace = "Gtk", name = "CellArea"})
    Ptr Gtk.CellRenderer.CellRenderer ->    -- renderer : TInterface (Name {namespace = "Gtk", name = "CellRenderer"})
    Ptr Gtk.CellRenderer.CellRenderer ->    -- sibling : TInterface (Name {namespace = "Gtk", name = "CellRenderer"})
    IO ()

-- | Removes /@sibling@/ from /@renderer@/’s focus sibling list
-- (see 'GI.Gtk.Objects.CellArea.cellAreaAddFocusSibling').
-- 
-- /Since: 3.0/
cellAreaRemoveFocusSibling ::
    (B.CallStack.HasCallStack, MonadIO m, IsCellArea a, Gtk.CellRenderer.IsCellRenderer b, Gtk.CellRenderer.IsCellRenderer c) =>
    a
    -- ^ /@area@/: a t'GI.Gtk.Objects.CellArea.CellArea'
    -> b
    -- ^ /@renderer@/: the t'GI.Gtk.Objects.CellRenderer.CellRenderer' expected to have focus
    -> c
    -- ^ /@sibling@/: the t'GI.Gtk.Objects.CellRenderer.CellRenderer' to remove from /@renderer@/’s focus area
    -> m ()
cellAreaRemoveFocusSibling area renderer sibling = liftIO $ do
    area' <- unsafeManagedPtrCastPtr area
    renderer' <- unsafeManagedPtrCastPtr renderer
    sibling' <- unsafeManagedPtrCastPtr sibling
    gtk_cell_area_remove_focus_sibling area' renderer' sibling'
    touchManagedPtr area
    touchManagedPtr renderer
    touchManagedPtr sibling
    return ()

#if defined(ENABLE_OVERLOADING)
data CellAreaRemoveFocusSiblingMethodInfo
instance (signature ~ (b -> c -> m ()), MonadIO m, IsCellArea a, Gtk.CellRenderer.IsCellRenderer b, Gtk.CellRenderer.IsCellRenderer c) => O.OverloadedMethod CellAreaRemoveFocusSiblingMethodInfo a signature where
    overloadedMethod = cellAreaRemoveFocusSibling

instance O.OverloadedMethodInfo CellAreaRemoveFocusSiblingMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellArea.cellAreaRemoveFocusSibling",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellArea.html#v:cellAreaRemoveFocusSibling"
        })


#endif

-- method CellArea::render
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "area"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellArea" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCellArea" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellAreaContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the #GtkCellAreaContext for this row of data."
--                 , sinceVersion = Nothing
--                 }
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
--                 { rawDocText = Just "the #GtkWidget that @area is rendering to"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "cr"
--           , argType =
--               TInterface Name { namespace = "cairo" , name = "Context" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the #cairo_t to render with"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "background_area"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "Rectangle" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "the @widget relative coordinates for @area\8217s background"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "cell_area"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "Rectangle" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the @widget relative coordinates for @area"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "flags"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellRendererState" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "the #GtkCellRendererState for @area in this row."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "paint_focus"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "whether @area should paint focus on focused cells for focused rows or not."
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

foreign import ccall "gtk_cell_area_render" gtk_cell_area_render :: 
    Ptr CellArea ->                         -- area : TInterface (Name {namespace = "Gtk", name = "CellArea"})
    Ptr Gtk.CellAreaContext.CellAreaContext -> -- context : TInterface (Name {namespace = "Gtk", name = "CellAreaContext"})
    Ptr Gtk.Widget.Widget ->                -- widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    Ptr Cairo.Context.Context ->            -- cr : TInterface (Name {namespace = "cairo", name = "Context"})
    Ptr Gdk.Rectangle.Rectangle ->          -- background_area : TInterface (Name {namespace = "Gdk", name = "Rectangle"})
    Ptr Gdk.Rectangle.Rectangle ->          -- cell_area : TInterface (Name {namespace = "Gdk", name = "Rectangle"})
    CUInt ->                                -- flags : TInterface (Name {namespace = "Gtk", name = "CellRendererState"})
    CInt ->                                 -- paint_focus : TBasicType TBoolean
    IO ()

-- | Renders /@area@/’s cells according to /@area@/’s layout onto /@widget@/ at
-- the given coordinates.
-- 
-- /Since: 3.0/
cellAreaRender ::
    (B.CallStack.HasCallStack, MonadIO m, IsCellArea a, Gtk.CellAreaContext.IsCellAreaContext b, Gtk.Widget.IsWidget c) =>
    a
    -- ^ /@area@/: a t'GI.Gtk.Objects.CellArea.CellArea'
    -> b
    -- ^ /@context@/: the t'GI.Gtk.Objects.CellAreaContext.CellAreaContext' for this row of data.
    -> c
    -- ^ /@widget@/: the t'GI.Gtk.Objects.Widget.Widget' that /@area@/ is rendering to
    -> Cairo.Context.Context
    -- ^ /@cr@/: the t'GI.Cairo.Structs.Context.Context' to render with
    -> Gdk.Rectangle.Rectangle
    -- ^ /@backgroundArea@/: the /@widget@/ relative coordinates for /@area@/’s background
    -> Gdk.Rectangle.Rectangle
    -- ^ /@cellArea@/: the /@widget@/ relative coordinates for /@area@/
    -> [Gtk.Flags.CellRendererState]
    -- ^ /@flags@/: the t'GI.Gtk.Flags.CellRendererState' for /@area@/ in this row.
    -> Bool
    -- ^ /@paintFocus@/: whether /@area@/ should paint focus on focused cells for focused rows or not.
    -> m ()
cellAreaRender area context widget cr backgroundArea cellArea flags paintFocus = liftIO $ do
    area' <- unsafeManagedPtrCastPtr area
    context' <- unsafeManagedPtrCastPtr context
    widget' <- unsafeManagedPtrCastPtr widget
    cr' <- unsafeManagedPtrGetPtr cr
    backgroundArea' <- unsafeManagedPtrGetPtr backgroundArea
    cellArea' <- unsafeManagedPtrGetPtr cellArea
    let flags' = gflagsToWord flags
    let paintFocus' = (fromIntegral . fromEnum) paintFocus
    gtk_cell_area_render area' context' widget' cr' backgroundArea' cellArea' flags' paintFocus'
    touchManagedPtr area
    touchManagedPtr context
    touchManagedPtr widget
    touchManagedPtr cr
    touchManagedPtr backgroundArea
    touchManagedPtr cellArea
    return ()

#if defined(ENABLE_OVERLOADING)
data CellAreaRenderMethodInfo
instance (signature ~ (b -> c -> Cairo.Context.Context -> Gdk.Rectangle.Rectangle -> Gdk.Rectangle.Rectangle -> [Gtk.Flags.CellRendererState] -> Bool -> m ()), MonadIO m, IsCellArea a, Gtk.CellAreaContext.IsCellAreaContext b, Gtk.Widget.IsWidget c) => O.OverloadedMethod CellAreaRenderMethodInfo a signature where
    overloadedMethod = cellAreaRender

instance O.OverloadedMethodInfo CellAreaRenderMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellArea.cellAreaRender",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellArea.html#v:cellAreaRender"
        })


#endif

-- method CellArea::request_renderer
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "area"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellArea" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCellArea" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "renderer"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellRenderer" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the #GtkCellRenderer to request size for"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "orientation"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Orientation" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the #GtkOrientation in which to request size"
--                 , sinceVersion = Nothing
--                 }
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
--                 { rawDocText = Just "the #GtkWidget that @area is rendering onto"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "for_size"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the allocation contextual size to request for, or -1 if\nthe base request for the orientation is to be returned."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "minimum_size"
--           , argType = TBasicType TInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "location to store the minimum size, or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       , Arg
--           { argCName = "natural_size"
--           , argType = TBasicType TInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "location to store the natural size, or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       ]
-- Lengths: []
-- returnType: Nothing
-- throws : False
-- Skip return : False

foreign import ccall "gtk_cell_area_request_renderer" gtk_cell_area_request_renderer :: 
    Ptr CellArea ->                         -- area : TInterface (Name {namespace = "Gtk", name = "CellArea"})
    Ptr Gtk.CellRenderer.CellRenderer ->    -- renderer : TInterface (Name {namespace = "Gtk", name = "CellRenderer"})
    CUInt ->                                -- orientation : TInterface (Name {namespace = "Gtk", name = "Orientation"})
    Ptr Gtk.Widget.Widget ->                -- widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    Int32 ->                                -- for_size : TBasicType TInt
    Ptr Int32 ->                            -- minimum_size : TBasicType TInt
    Ptr Int32 ->                            -- natural_size : TBasicType TInt
    IO ()

-- | This is a convenience function for t'GI.Gtk.Objects.CellArea.CellArea' implementations
-- to request size for cell renderers. It’s important to use this
-- function to request size and then use 'GI.Gtk.Objects.CellArea.cellAreaInnerCellArea'
-- at render and event time since this function will add padding
-- around the cell for focus painting.
-- 
-- /Since: 3.0/
cellAreaRequestRenderer ::
    (B.CallStack.HasCallStack, MonadIO m, IsCellArea a, Gtk.CellRenderer.IsCellRenderer b, Gtk.Widget.IsWidget c) =>
    a
    -- ^ /@area@/: a t'GI.Gtk.Objects.CellArea.CellArea'
    -> b
    -- ^ /@renderer@/: the t'GI.Gtk.Objects.CellRenderer.CellRenderer' to request size for
    -> Gtk.Enums.Orientation
    -- ^ /@orientation@/: the t'GI.Gtk.Enums.Orientation' in which to request size
    -> c
    -- ^ /@widget@/: the t'GI.Gtk.Objects.Widget.Widget' that /@area@/ is rendering onto
    -> Int32
    -- ^ /@forSize@/: the allocation contextual size to request for, or -1 if
    -- the base request for the orientation is to be returned.
    -> m ((Int32, Int32))
cellAreaRequestRenderer area renderer orientation widget forSize = liftIO $ do
    area' <- unsafeManagedPtrCastPtr area
    renderer' <- unsafeManagedPtrCastPtr renderer
    let orientation' = (fromIntegral . fromEnum) orientation
    widget' <- unsafeManagedPtrCastPtr widget
    minimumSize <- allocMem :: IO (Ptr Int32)
    naturalSize <- allocMem :: IO (Ptr Int32)
    gtk_cell_area_request_renderer area' renderer' orientation' widget' forSize minimumSize naturalSize
    minimumSize' <- peek minimumSize
    naturalSize' <- peek naturalSize
    touchManagedPtr area
    touchManagedPtr renderer
    touchManagedPtr widget
    freeMem minimumSize
    freeMem naturalSize
    return (minimumSize', naturalSize')

#if defined(ENABLE_OVERLOADING)
data CellAreaRequestRendererMethodInfo
instance (signature ~ (b -> Gtk.Enums.Orientation -> c -> Int32 -> m ((Int32, Int32))), MonadIO m, IsCellArea a, Gtk.CellRenderer.IsCellRenderer b, Gtk.Widget.IsWidget c) => O.OverloadedMethod CellAreaRequestRendererMethodInfo a signature where
    overloadedMethod = cellAreaRequestRenderer

instance O.OverloadedMethodInfo CellAreaRequestRendererMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellArea.cellAreaRequestRenderer",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellArea.html#v:cellAreaRequestRenderer"
        })


#endif

-- method CellArea::set_focus_cell
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "area"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellArea" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCellArea" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "renderer"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellRenderer" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the #GtkCellRenderer to give focus to"
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

foreign import ccall "gtk_cell_area_set_focus_cell" gtk_cell_area_set_focus_cell :: 
    Ptr CellArea ->                         -- area : TInterface (Name {namespace = "Gtk", name = "CellArea"})
    Ptr Gtk.CellRenderer.CellRenderer ->    -- renderer : TInterface (Name {namespace = "Gtk", name = "CellRenderer"})
    IO ()

-- | Explicitly sets the currently focused cell to /@renderer@/.
-- 
-- This is generally called by implementations of
-- t'GI.Gtk.Structs.CellAreaClass.CellAreaClass'.@/focus/@() or t'GI.Gtk.Structs.CellAreaClass.CellAreaClass'.@/event/@(),
-- however it can also be used to implement functions such
-- as 'GI.Gtk.Objects.TreeView.treeViewSetCursorOnCell'.
-- 
-- /Since: 3.0/
cellAreaSetFocusCell ::
    (B.CallStack.HasCallStack, MonadIO m, IsCellArea a, Gtk.CellRenderer.IsCellRenderer b) =>
    a
    -- ^ /@area@/: a t'GI.Gtk.Objects.CellArea.CellArea'
    -> b
    -- ^ /@renderer@/: the t'GI.Gtk.Objects.CellRenderer.CellRenderer' to give focus to
    -> m ()
cellAreaSetFocusCell area renderer = liftIO $ do
    area' <- unsafeManagedPtrCastPtr area
    renderer' <- unsafeManagedPtrCastPtr renderer
    gtk_cell_area_set_focus_cell area' renderer'
    touchManagedPtr area
    touchManagedPtr renderer
    return ()

#if defined(ENABLE_OVERLOADING)
data CellAreaSetFocusCellMethodInfo
instance (signature ~ (b -> m ()), MonadIO m, IsCellArea a, Gtk.CellRenderer.IsCellRenderer b) => O.OverloadedMethod CellAreaSetFocusCellMethodInfo a signature where
    overloadedMethod = cellAreaSetFocusCell

instance O.OverloadedMethodInfo CellAreaSetFocusCellMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellArea.cellAreaSetFocusCell",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellArea.html#v:cellAreaSetFocusCell"
        })


#endif

-- method CellArea::stop_editing
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "area"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellArea" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCellArea" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "canceled"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "whether editing was canceled."
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

foreign import ccall "gtk_cell_area_stop_editing" gtk_cell_area_stop_editing :: 
    Ptr CellArea ->                         -- area : TInterface (Name {namespace = "Gtk", name = "CellArea"})
    CInt ->                                 -- canceled : TBasicType TBoolean
    IO ()

-- | Explicitly stops the editing of the currently edited cell.
-- 
-- If /@canceled@/ is 'P.True', the currently edited cell renderer
-- will emit the [editingCanceled](#g:signal:editingCanceled) signal, otherwise the
-- the [editingDone](#g:signal:editingDone) signal will be emitted on the current
-- edit widget.
-- 
-- See 'GI.Gtk.Objects.CellArea.cellAreaGetEditedCell' and 'GI.Gtk.Objects.CellArea.cellAreaGetEditWidget'.
-- 
-- /Since: 3.0/
cellAreaStopEditing ::
    (B.CallStack.HasCallStack, MonadIO m, IsCellArea a) =>
    a
    -- ^ /@area@/: a t'GI.Gtk.Objects.CellArea.CellArea'
    -> Bool
    -- ^ /@canceled@/: whether editing was canceled.
    -> m ()
cellAreaStopEditing area canceled = liftIO $ do
    area' <- unsafeManagedPtrCastPtr area
    let canceled' = (fromIntegral . fromEnum) canceled
    gtk_cell_area_stop_editing area' canceled'
    touchManagedPtr area
    return ()

#if defined(ENABLE_OVERLOADING)
data CellAreaStopEditingMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsCellArea a) => O.OverloadedMethod CellAreaStopEditingMethodInfo a signature where
    overloadedMethod = cellAreaStopEditing

instance O.OverloadedMethodInfo CellAreaStopEditingMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellArea.cellAreaStopEditing",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellArea.html#v:cellAreaStopEditing"
        })


#endif


