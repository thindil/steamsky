# Copyright © 2023-2024 Bartek Jasicki
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met*:
# 1. Redistributions of source code must retain the above copyright
# notice, this list of conditions and the following disclaimer.
# 2. Redistributions in binary form must reproduce the above copyright
# notice, this list of conditions and the following disclaimer in the
# documentation and/or other materials provided with the distribution.
# 3. Neither the name of the copyright holder nor the
# names of its contributors may be used to endorse or promote products
# derived from this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY COPYRIGHT HOLDERS AND CONTRIBUTORS ''AS IS'' AND
# ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
# WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
# DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
# FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
# DAMAGES *(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
# SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
# OR TORT *(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

import std/[hashes, macros]

## Provides code for Nuklear binding

# ---------
# Constants
# ---------
const
  nkWindowBorder*: cint = 1 shl 0
  nkWindowMoveable*: cint = 1 shl 1
  nkWindowScalable*: cint = 1 shl 2
  nkWindowCloseable*: cint = 1 shl 3
  nkWindowMinimizable*: cint = 1 shl 4
  nkWindowNoScrollbar*: cint = 1 shl 5
  nkWindowScaleLeft*: cint = 1 shl 9
  nkWindowTitle*: cint = 1 shl 6

# ------------
# Simple types
# ------------
type
  nk_flags* = cint
  nk_size* = clong
  nk_byte* = uint8
  nk_rune* = cuint

# ------------
# Enumerations
# ------------
type
  nk_style_header_align = enum
    NK_HEADER_LEFT, NK_HEADER_RIGHT
  nk_layout_format* = enum
    NK_DYNAMIC, NK_STATIC
  nk_text_align* = enum
    NK_TEXT_ALIGN_LEFT = 0x01,
    NK_TEXT_ALIGN_CENTERED = 0x02,
    NK_TEXT_ALIGN_RIGHT = 0x04,
    NK_TEXT_ALIGN_TOP = 0x08,
    NK_TEXT_ALIGN_MIDDLE = 0x10,
    NK_TEXT_ALIGN_BOTTOM = 0x20
  nk_text_alignment* = enum
    NK_TEXT_LEFT = NK_TEXT_ALIGN_MIDDLE.int or NK_TEXT_ALIGN_LEFT.int,
    NK_TEXT_CENTERED = NK_TEXT_ALIGN_MIDDLE.int or NK_TEXT_ALIGN_CENTERED.int,
    NK_TEXT_RIGHT = NK_TEXT_ALIGN_MIDDLE.int or NK_TEXT_ALIGN_RIGHT.int
  TreeType* = enum
    node, tab
  ChartType* = enum
    ## The types of charts
    lines, column, chartMax
  nk_bool* = enum
    nkFalse, nkTrue
  nk_modify* = enum
    NK_FIXED, NK_MODIFIABLE
  CollapseStates* = enum
    ## The states of a tree's content
    minimized, maximized
  SymbolType* = enum
    none, x, underscore, circleSolid, circleOutline, rectSolid, rectOutline,
      triangleUp, triangleDown, triangleLeft, triangleRight, plus, minus, max
  nk_style_item_type* = enum
    NK_STYLE_ITEM_COLOR, NK_STYLE_ITEM_IMAGE
  colorFormat* = enum
    rgb, rgba
  ChartEvent* = enum
    none,
    hovering = 0x01,
    clicked = 0x02
  Buttons* = enum
    left, middle, right, double, max
  nk_style_colors* = enum
    NK_COLOR_TEXT, NK_COLOR_WINDOW, NK_COLOR_HEADER, NK_COLOR_BORDER,
    NK_COLOR_BUTTON, NK_COLOR_BUTTON_HOVER, NK_COLOR_BUTTON_ACTIVE,
    NK_COLOR_TOGGLE, NK_COLOR_TOGGLE_HOVER, NK_COLOR_TOGGLE_CURSOR,
    NK_COLOR_SELECT, NK_COLOR_SELECT_ACTIVE, NK_COLOR_SLIDER,
    NK_COLOR_SLIDER_CURSOR, NK_COLOR_SLIDER_CURSOR_HOVER,
    NK_COLOR_SLIDER_CURSOR_ACTIVE, NK_COLOR_PROPERTY, NK_COLOR_EDIT,
    NK_COLOR_EDIT_CURSOR, NK_COLOR_COMBO, NK_COLOR_CHART,
    NK_COLOR_CHART_COLOR, NK_COLOR_CHART_COLOR_HIGHLIGHT,
    NK_COLOR_SCROLLBAR, NK_COLOR_SCROLLBAR_CURSOR,
    NK_COLOR_SCROLLBAR_CURSOR_HOVER, NK_COLOR_SCROLLBAR_CURSOR_ACTIVE,
    NK_COLOR_TAB_HEADER, NK_COLOR_COUNT
  nk_anti_aliasing* = enum
    NK_ANTI_ALIASING_OFF, NK_ANTI_ALIASING_ON

# -------
# Objects
# -------
type
  nk_color* {.importc: "struct nk_color", nodecl.} = object
    r*, g*, b*, a*: nk_byte
  nk_colorf* {.importc: "struct nk_colorf", nodecl.} = object
    r*, g*, b*, a*: cfloat
  nk_vec2* {.importc: "struct nk_vec2", nodecl.} = object
    x*, y*: cfloat
  nk_style_item_data* {.importc, nodecl.} = object
  nk_style_item* {.importc: "struct nk_style_item", nodecl.} = object
  nk_style_window_header* {.importc, nodecl.} = object
    align*: nk_style_header_align
  nk_style_window* {.importc, nodecl.} = object
    header*: nk_style_window_header
    spacing*: nk_vec2
  nk_style_button* {.importc: "struct nk_style_button", nodecl.} = object
    normal*, hover*, active*: nk_style_item
    border_color*, text_background*, text_normal*, text_hover*,
      text_active*: nk_color
    rounding*: cfloat
    padding*: nk_vec2
  nk_handle* {.bycopy, union.} = object
    `ptr`*: pointer
    id*: cint
  nk_text_width_f* = proc (arg1: nk_handle; h: cfloat; arg3: cstring;
      len: cint): cfloat {.cdecl.}
  nk_user_font* {.importc: "struct nk_user_font", nodecl.} = object
    userdata*: nk_handle
    height*: cfloat
    width*: nk_text_width_f
  nk_style* {.importc, nodecl.} = object
    window*: nk_style_window
    button*: nk_style_button
    font*: ptr nk_user_font
  nk_mouse* {.importc, nodecl.} = object
    delta*: nk_vec2
  nk_input* {.importc, nodecl.} = object
    mouse*: nk_mouse
  nk_buffer* {.importc, nodecl.} = object
  nk_context* {.importc: "struct nk_context", nodecl.} = object
    style*: nk_style
    input*: nk_input
  nk_rect* {.importc: "struct nk_rect", nodecl.} = object
    x*, y*, w*, h*: cfloat
  nk_text_edit* = object
  nk_font* {.importc: "struct nk_font", nodecl.} = object
    handle*: nk_user_font
  nk_font_atlas* {.importc: "struct nk_font_atlas", nodecl.} = object
  nk_font_config* {.importc: "struct nk_font_config", nodecl.} = object
  PContext* = ptr nk_context

# ---------------------
# Procedures parameters
# ---------------------
using ctx: PContext

# -------------------
# Creating structures
# -------------------
proc new_nk_rect*(x, y, w, h: cfloat): nk_rect {.importc: "nk_rect", nodecl.}
proc new_nk_vec2*(x, y: cfloat): nk_vec2 {.importc: "nk_vec2", nodecl.}
proc new_nk_font_config*(pixelHeight: cfloat): nk_font_config {.importc: "nk_font_config", nodecl.}

# -----
# Input
# -----
proc nk_input_begin*(ctx) {.importc, nodecl.}
proc nk_input_end*(ctx) {.importc, nodecl.}

# -------
# General
# -------
proc nk_end(ctx) {.importc, cdecl.}

# ----
# Text
# ----
proc nk_labelf(ctx; flags: nk_flags; fmt: cstring) {.importc,
    varargs, cdecl.}

# -------
# Layouts
# -------
proc nk_layout_row_end(ctx) {.importc, cdecl.}
proc nk_layout_row_begin(ctx; fmt: nk_layout_format;
    rowHeight: cfloat; cols: cint) {.importc, cdecl.}
proc nk_layout_row_push(ctx; width: cfloat) {.importc, cdecl.}
proc nk_layout_row(ctx; fmt: nk_layout_format; height: cfloat;
    cols: cint; ratio: pointer) {.importc, nodecl.}
proc nk_layout_space_begin(ctx; fmt: nk_layout_format;
    height: cfloat; widgetCount: cint) {.importc, cdecl.}
proc nk_layout_space_end(ctx) {.importc, cdecl.}
proc nk_layout_row_template_begin(ctx; height: cfloat) {.importc, cdecl.}
proc nk_layout_row_template_end(ctx) {.importc, cdecl.}

# -----
# Menus
# -----
proc nk_menubar_begin(ctx) {.importc, cdecl.}
proc nk_menubar_end(ctx) {.importc, cdecl.}
proc nk_menu_end(ctx) {.importc, cdecl.}
proc nk_menu_item_label(ctx; text: cstring;
    aligmnent: nk_flags): nk_bool {.importc, cdecl.}

# ------
# Charts
# ------
proc nk_chart_begin(ctx; ctype: ChartType; num: cint; min,
    max: cfloat): nk_bool {.importc, cdecl.}
proc nk_chart_end(ctx) {.importc, cdecl.}

# ------
# Popups
# ------
proc nk_popup_end(ctx) {.importc, nodecl.}

# -----
# Trees
# -----
proc nk_tree_state_push(ctx; ttype: TreeType;
    title: cstring; state: var CollapseStates): nk_bool {.importc, cdecl.}
proc nk_tree_pop(ctx) {.importc, cdecl.}
proc nk_tree_push_hashed(ctx; ttype: TreeType;
    title: cstring; state: CollapseStates; hash: cstring; len,
    id: cint): nk_bool {.importc, cdecl.}
proc nk_tree_element_push_hashed(ctx; ttype: TreeType;
    title: cstring; state: CollapseStates; selected: var nk_bool;
    hash: cstring; len, sed: cint): nk_bool {.importc, cdecl.}
proc nk_tree_element_pop(ctx) {.importc, cdecl.}

# -------
# Buttons
# -------
proc nk_button_label(ctx; title: cstring): nk_bool {.importc, cdecl.}
proc nk_button_symbol(ctx; symbol: SymbolType): nk_bool {.importc, cdecl.}
proc nk_button_symbol_label(ctx; symbol: SymbolType;
    label: cstring; align: nk_flags): nk_bool {.importc, cdecl.}

# -----
# Style
# -----
proc nk_style_item_color(col: nk_color): nk_style_item {.importc, cdecl.}
proc nk_style_set_font*(ctx; font: ptr nk_user_font) {.importc, nodecl.}

# ------
# Combos
# ------
proc nk_combo_begin_color(ctx; color: nk_color;
    size: nk_vec2): nk_bool {.importc, nodecl.}
proc nk_combo_end(ctx) {.importc, cdecl.}

# ------
# Colors
# ------
proc nk_rgb*(r, g, b: cint): nk_color {.importc, nodecl.}
proc nk_rgb_cf*(c: nk_colorf): nk_color {.importc, nodecl.}
proc nk_rgba*(r, g, b, a: cint): nk_color {.importc, nodecl.}

# -------
# Filters
# -------
proc nk_filter_default*(box: ptr nk_text_edit;
    unicode: nk_rune): nk_bool {.importc, cdecl.}
proc nk_filter_decimal*(box: ptr nk_text_edit;
    unicode: nk_rune): nk_bool {.importc, cdecl.}
proc nk_filter_float*(box: ptr nk_text_edit;
    unicode: nk_rune): nk_bool {.importc, cdecl.}
proc nk_filter_hex*(box: ptr nk_text_edit;
    unicode: nk_rune): nk_bool {.importc, cdecl.}
proc nk_filter_oct*(box: ptr nk_text_edit;
    unicode: nk_rune): nk_bool {.importc, cdecl.}
proc nk_filter_binary*(box: ptr nk_text_edit;
    unicode: nk_rune): nk_bool {.importc, cdecl.}
proc nk_filter_ascii*(box: ptr nk_text_edit;
    unicode: nk_rune): nk_bool {.importc, cdecl.}

# ----------
# Contextual
# ----------
proc nk_contextual_end(ctx) {.importc, cdecl.}
proc nk_contextual_item_label(ctx; label: cstring;
    align: nk_flags): nk_bool {.importc, cdecl.}

# --------
# Tooltips
# --------
proc nk_tooltipf(ctx; fmt: cstring) {.importc, nodecl, varargs.}

# ------
# Groups
# ------
proc nk_group_begin(ctx; title: cstring;
    flags: nk_flags): nk_bool {.importc, cdecl.}
proc nk_group_end(ctx) {.importc, cdecl.}

# -----
# Fonts
# -----
proc nk_font_atlas_add_default*(atlas: ptr nk_font_atlas; height: cfloat;
    config: ptr nk_font_config): ptr nk_font {.importc, nodecl.}
proc nk_font_atlas_add_from_file*(atlas: ptr nk_font_atlas; filePath: cstring;
    height: cfloat;  config: ptr nk_font_config): ptr nk_font {.importc, nodecl.}

# ------------------------------------------------------------------
# High level bindings. The new version of the binding
# ------------------------------------------------------------------

# -----
# Types
# -----
type
  NimColor* = object
    ## Used to store information about the selected color. Usually later
    ## converted to Nuklear structure nk_color
    r*, g*, b*, a*: int
  NimColorF* = object
    ## Also used to store information about the selected color, but as a float
    ## values.
    r*, g*, b*, a*: float
  NimRect* = object
    ## Used to store information about UI rectangle. Usually later converted to
    ## Nuklear nk_rect
    x*, y*, w*, h*: cfloat
  NimVec2* = object
    ## Used to store information about UI vector. Usually later converted to
    ## Nuklear nk_vec2
    x*, y*: cfloat
  ButtonStyleTypes* = enum
    ## The types of fields in style's settings for UI buttons
    normal, hover, active, borderColor, textBackground, textNormal, textHover,
        textActive, rounding, padding
  WindowStyleTypes* = enum
    ## The types of fields in style's settings for windows
    spacing
  WindowFlags* {.size: sizeof(cint).} = enum
    ## The settings for windows
    windowNoFlags = 0,
    windowBorder = 1 shl 0,
    windowMoveable = 1 shl 1,
    windowScalable = 1 shl 2,
    windowCloseable = 1 shl 3
    windowMinimizable = 1 shl 4,
    windowNoScrollbar = 1 shl 5,
    windowTitle = 1 shl 6,
    windowScaleLeft = 1 shl 9
  NuklearException* = object of CatchableError
    ## An exception thrown when there is an issue with Nuklear library
  PopupType* = enum
    ## The types of popup windows
    staticPopup, dynamicPopup
  TextAlignment* {.size: sizeof(cint).} = enum
    ## The alignments of a text
    left = NK_TEXT_ALIGN_MIDDLE.int or NK_TEXT_ALIGN_LEFT.int,
    centered = NK_TEXT_ALIGN_MIDDLE.int or NK_TEXT_ALIGN_CENTERED.int,
    right = NK_TEXT_ALIGN_MIDDLE.int or NK_TEXT_ALIGN_RIGHT.int
  EditFlags* {.size: sizeof(cint).} = enum
    ## The edit fields' flags
    default = 0,
    readOnly = 1 shl 0,
    autoSelect = 1 shl 1,
    sigEnter = 1 shl 2,
    allowTab = 1 shl 3,
    noCursor = 1 shl 4,
    selectable = 1 shl 5,
    clipboard = 1 shl 6,
    ctrlEnterNewLine = 1 shl 7,
    noHorizontalScroll = 1 shl 8,
    alwaysInsertMode = 1 shl 9,
    multiline = 1 shl 10,
    gotoEndOnActivate = 1 shl 11
  EditEvent* {.size: sizeof(cint).} = enum
    ## The events which happen in a text field
    none = 0,
    active = 1 shl 0,
    inactive = 1 shl 1,
    activated = 1 shl 2,
    deactivated = 1 shl 3,
    commited = 1 shl 4
  EditTypes* {.size: sizeof(cint).} = enum
    ## The types of edit fields
    simple = alwaysInsertMode,
    field = simple.int or selectable.int or clipboard.int,
    editor = allowTab.int or selectable.int or clipboard.int or multiline.int,
    box = alwaysInsertMode.int or selectable.int or multiline.int or
        allowTab.int or clipboard.int
  PluginFilter* = proc (box: ptr nk_text_edit;
      unicode: nk_rune): nk_bool {.cdecl.}
    ## The procedure used to filter input in edit fields
  StyleColors* = enum
    ## Names of the colors for UI's elements which can be set. The last value
    ## is special, it defines the amount of available colors' settings.
    textColor, windowColor, headerColor, borderColor, buttonColor,
      buttonHoverColor, buttonActiveColor, toggleColor, toggleHoverColor,
      toggleCursorColor, selectColor, selectActiveColor, sliderColor,
      sliderCursorColor, sliderCursorHoverColor, sliderCursorActiveColor,
      propertyColor, editColor, editCursorColor, comboColor, chartColor,
      colorChartColor, colorChartHighlightColor, scrollbarColor,
      scrollbarCursorColor, scrollbarCursorHoverColor,
      scrollbarCursorActiveColor, tabHeaderColor, countColors
  StyleHeaderAlign* = enum
    ## The styles of the window's header
    headerLeft, headerRight
  ButtonBehavior* = enum
    ## The types of buttons behavior
    default, repeater

# ----------
# Converters
# ----------
converter toBool*(x: nk_bool): bool =
  ## Converts Nuklear nk_bool enum to Nim bool
  x == nkTrue
converter toNkFlags*(x: nk_text_alignment): nk_flags =
  ## Converts Nuklear nk_text_alignment enum to Nuklear nk_flags type
  x.ord.cint
converter toNkFlags*(x: EditTypes): nk_flags =
  ## Converts EditTypes enum to Nuklear nk_flags type
  x.ord.cint
converter toCint*(x: bool): cint =
  ## Converts Nim bool type to Nim cint type
  if x: 1 else: 0

# ---------
# Variables
# ---------
var ctx: PContext ## Pointer to the Nuklear context

# -------
# General
# -------
proc setContext*(context: PContext) =
  ## Set the Nuklear lib context
  ##
  ## * context - the pointer to the Nuklear context
  ctx = context

proc getContext*(): PContext =
  ## Get the Nuklear lib context, temporary code
  ##
  ## Returns the pointer to the Nuklear context
  return ctx

proc charArrayToString(charArray: openArray[char]; length: int): string =
  ## Convert a characters' array to Nim string, internal use only, temporary
  ## code
  ##
  ## * charArray - the array of characters to convert
  ##
  ## Returns a string with text converted from the chars' array
  result = ""
  for i in 0 .. length - 1:
    result.add(charArray[i])

proc stringToCharArray(str: string; length: int): tuple[charArray: seq[char];
    length: cint] =
  ## Convert a Nim string to a characters array, internal use only, temporary
  ## code
  ##
  ## * str - the string to convert
  ##
  ## Returns a tuple with two fields, charArray with the converted text from
  ## the string and lenght with the amount of the characters.
  for ch in str:
    result.charArray.add(ch)
  if str.len < length:
    for i in str.len .. length:
      result.charArray.add('\0')
  result.length = str.len.cint

proc createWin(name: cstring; x, y, w, h: cfloat; flags: nk_flags): bool =
  ## Create a new Nuklear window/widget, internal use only, temporary code
  ##
  ## Returns true if window was succesfully created otherwise false.
  proc nk_begin(ctx; title: cstring; bounds: nk_rect;
      flags: nk_flags): nk_bool {.importc, nodecl.}
  return nk_begin(ctx, name, new_nk_rect(x, y, w, h), flags)

proc winSetToInt(flags: set[WindowFlags]): cint =
  result = 0
  {.warning[HoleEnumConv]: off.}
  for flag in flags:
    result = result or flag.cint
  {.warning[HoleEnumConv]: on.}

template window*(name: string; x, y, w, h: float; flags: set[WindowFlags];
    content: untyped) =
  ## Create a new Nuklear window/widget with the content
  ##
  ## * name    - the window title
  ## * x       - the X position of the top left corner of the window
  ## * y       - the Y position of the top left corner of the window
  ## * w       - the width of the window
  ## * h       - the height of the window
  ## * flags   - the flags for the window
  ## * content - the content of the window
  if createWin(name.cstring, x, y, w, h, winSetToInt(flags)):
    content
  nk_end(ctx)

proc getWidgetBounds*(): NimRect =
  ## Get the rectable with the current Nuklear widget coordinates
  ##
  ## Returns a rectangle with the current Nuklear widget coordinates
  ## converted to NimRect
  proc nk_widget_bounds(ctx): nk_rect {.importc, nodecl.}
  let rect = nk_widget_bounds(ctx)
  return NimRect(x: rect.x, y: rect.y, w: rect.w, h: rect.h)

proc getTextWidth*(text: string): float =
  ## Get the width in pixels of the selected text in the current font
  ##
  ## * text - the text which width will be count
  ##
  ## Returns width in pixels of the text paramter
  return ctx.style.font.width(ctx.style.font.userdata, ctx.style.font.height,
      text, text.len.cint)

proc windowIsHidden*(name: string): bool =
  ## Check if the window with the selected name is hidden
  ##
  ## * name - the name of the window to check
  ##
  ## Returns true if the window is hidden, otherwise false
  proc nk_window_is_hidden(ctx; name: cstring): cint {.importc, nodecl.}
  return nk_window_is_hidden(ctx, name.cstring) > 0

proc addSpacing*(cols: int) =
  ## Add spacing in the selected between the row's boundaries in the row
  ##
  ## * cols - the amount of columns to add as the spacing
  proc nk_spacing(ctx; cols: cint) {.importc, nodecl.}
  nk_spacing(ctx, cols.cint)

# ------
# Popups
# ------
proc createPopup(pType: PopupType; title: cstring;
    flags: nk_flags; x, y, w, h: cfloat): bool =
  ## Create a new Nuklear popup window, internal use only, temporary code
  ##
  ## Returns true if the popup was successfully created, otherwise false.
  proc nk_popup_begin(ctx; pType: PopupType; title: cstring;
      flags: nk_flags; rect: nk_rect): nk_bool {.importc, nodecl.}
  return nk_popup_begin(ctx, pType, title, flags, new_nk_rect(
      x, y, w, h))

template popup*(pType: PopupType; title: string; flags: set[WindowFlags]; x,
    y, w, h: float; content: untyped) =
  ## Create a new Nuklear popup window with the selected content
  ##
  ## * pType   - the type of the popup
  ## * title   - the title of the popup
  ## * flags   - the flags for the popup
  ## * x       - the X position of the top left corner of the popup
  ## * y       - the Y position of the top left corner of the popup
  ## * w       - the width of the popup
  ## * h       - the height of the popup
  ## * content - the code executed when the button is pressed
  if not createPopup(pType, title.cstring, winSetToInt(flags), x.cfloat, y, w, h):
    raise newException(NuklearException,
        "Can't create the popup window with title: '" & title & "'.")
  content
  ctx.nk_popup_end

proc closePopup*() =
  ## Close the last popup window
  proc nk_popup_close(ctx) {.importc, nodecl.}
  ctx.nk_popup_close()

# -----
# Trees
# -----
template treeNode*(title: string; state: var CollapseStates;
    current: var Natural; index: Natural; content: untyped) =
  ## Create a new Nuklear tree container with highlighted header and with the
  ## selected content
  ##
  ## * title   - the title of the tree
  ## * state   - the current state of the tree
  ## * current - the current index of the selected tree in the parent.
  ## * index   - the index of the tree in the parent. The tree will be maximized
  ##             when the current parameter is equal to the index parameter
  ## * content - the content of the tree
  ##
  ## Returns modified parameters state and current
  state = (if current == index: maximized else: minimized)
  if ctx.nk_tree_state_push(node, title.cstring, state):
    current = index
    content
    ctx.nk_tree_pop
  else:
    current = (if current == index: 0 else: current)

template treeTab*(title: string; state: var CollapseStates;
    current: var Natural; index: Natural; content: untyped) =
  ## Create a new Nuklear tree container with the selected content
  ##
  ## * title   - the title of the tree
  ## * state   - the current state of the tree
  ## * current - the current index of the selected tree in the parent.
  ## * index   - the index of the tree in the parent. The tree will be maximized
  ##             when the current parameter is equal to the index parameter
  ## * content - the content of the tree
  ##
  ## Returns modified parameters state and current
  state = (if current == index: maximized else: minimized)
  if ctx.nk_tree_state_push(tab, title.cstring, state):
    current = index
    content
    ctx.nk_tree_pop
  else:
    current = (if current == index: 0 else: current)

template treeNode*(title: string; state: CollapseStates; index: Positive;
    content: untyped) =
  ## Create a new Nuklear tree container with highlighted header and with the
  ## selected content
  ##
  ## * title   - the title of the tree
  ## * state   - the current state of the tree
  ## * index   - the index of the tree. Must be unique
  ## * content - the content of the tree
  if nk_tree_push_hashed(ctx, node, title.cstring, state, ($hash(
      index)).cstring, 12, index.cint):
    content
    ctx.nk_tree_pop

template treeTab*(title: string; state: CollapseStates; index: Positive;
    content: untyped) =
  ## Create a new Nuklear tree container with the selected content
  ##
  ## * title   - the title of the tree
  ## * state   - the current state of the tree
  ## * index   - the index of the tree. Must be unique
  ## * content - the content of the tree
  if nk_tree_push_hashed(ctx, tab, title.cstring, state, ($hash(
      index)).cstring, 12, index.cint):
    content
    ctx.nk_tree_pop

template treeElement*(eType: TreeType; title: string; state: CollapseStates;
    selected: var bool; index: Positive; content: untyped) =
  ## Create a new tree's element of the selected type with the selected content
  ##
  ## * eType   - the type of the element
  ## * title   - the title of the element
  ## * state   - the current state of the element
  ## * index   - the index of the element. Must be unique
  ## * content - the content of the element
  var sel: nk_bool = selected.nk_bool
  if nk_tree_element_push_hashed(ctx, eType, title.cstring, state, sel, (
      $hash(index)).cstring, 12, index.cint):
    content
    nk_tree_element_pop(ctx)
  selected = sel

# ------
# Labels
# ------
proc colorLabel*(str: string; r, g, b: int; align: TextAlignment = left) =
  ## Draw a text with the selected color
  ##
  ## * str   - the text to display
  ## * r     - the red value for the text color in RGB
  ## * g     - the green value for the text color in RGB
  ## * b     - the blue value for the text color in RGB
  ## * align - the text aligmnent flags
  proc nk_label_colored(ctx; str: cstring; align: nk_flags;
      color: nk_color) {.importc, nodecl.}
  nk_label_colored(ctx, str.cstring, align.nk_flags, nk_rgb(r.cint, g.cint, b.cint))

proc label*(str: string; alignment: TextAlignment = left) =
  ## Draw the text with the selected alignment
  ##
  ## * str       - the text to draw
  ## * alignment - the alignment of the text. Default is alignment to the left
  proc nk_label(ctx; str: cstring; alignment: nk_flags) {.importc, nodecl.}
  nk_label(ctx, str.cstring, alignment.nk_flags)

proc text*(str: string; len: int = str.len; alignment: TextAlignment = left) =
  ## Draw the part of the text
  ##
  ## * str       - the text to draw
  ## * len       - the lenght of the text to draw. By default it is equal to
  ##               str lenght
  ## * alignment - the alignment of the text. Default is alignment to left
  proc nk_text(ctx; str: cstring; len: cint; alignment: nk_flags) {.importc, nodecl.}
  nk_text(ctx, str.cstring, len.cint, alignment.nk_flags)

proc wrapLabel*(str: string) =
  ## Draw a text and wrap it if its lentgh is bigger than the width of its
  ## container
  ##
  ## * str - the text to draw
  proc nk_label_wrap(ctx; str: cstring) {.importc, nodecl.}
  nk_label_wrap(ctx, str.cstring)

macro fmtLabel*(alignment: TextAlignment; args: varargs[untyped]): untyped =
  ## Draw a text formatted in the same way like the C function printf
  ##
  ## * alignment - the alignment of the text
  ## * args      - the text and its arguments to draw
  result = quote do:
    nk_labelf(ctx, `alignment`.nk_flags, `args`)

# -------
# Buttons
# -------
proc createColorButton(r, g, b: cint): bool =
  ## Draw a button with the selected color background, internal use only, temporary code
  ##
  ## * r   - the red value for the button color in RGB
  ## * g   - the green value for the button color in RGB
  ## * b   - the blue value for the button color in RGB
  ##
  ## Returns true if button was pressed
  proc nk_button_color(ctx; color: nk_color): nk_bool {.importc, nodecl.}
  return nk_button_color(ctx, nk_rgb(r, g, b))

template colorButton*(r, g, b: int; onPressCode: untyped) =
  if createColorButton(r.cint, g.cint, b.cint):
    onPressCode

template labelButton*(title: string; onPressCode: untyped) =
  ## Draw the button with the selected text on it. Execute the selected code
  ## on pressing it.
  ##
  ## * title       - the text to shown on the button
  ## * onPressCode - the Nim code to execute when the button was pressed
  if nk_button_label(ctx, title.cstring):
    onPressCode

proc setButtonBehavior*(behavior: ButtonBehavior) =
  ## Set the behavior of the the next button, when it is clicked
  ##
  ## * behavior - the behavior of a button
  proc nk_button_set_behavior(ctx; behavior: ButtonBehavior) {.importc, nodecl.}
  nk_button_set_behavior(ctx, behavior)

template symbolButton*(symbol: SymbolType; onPressCode: untyped) =
  ## Draw the button with the selected symbol. Execute the selected code
  ## on pressing it.
  ##
  ## * symbol      - the symbol to shown on the button
  ## * onPressCode - the Nim code to execute when the button was pressed
  if nk_button_symbol(ctx, symbol):
    onPressCode

template symbolLabelButton*(symbol: SymbolType; label: string;
    align: TextAlignment; onPressCode: untyped) =
  ## Draw the button with the selected symbol. Execute the selected code
  ## on pressing it.
  ##
  ## * symbol      - the symbol to shown on the button
  ## * label       - the text to display on the button
  ## * align       - the alignment of the button's label
  ## * onPressCode - the Nim code to execute when the button was pressed
  if nk_button_symbol_label(ctx, symbol, label.cstring, align.nk_flags):
    onPressCode

# -------
# Sliders
# -------
proc slide*(min, val, max, step: int): int =
  ## Draw a slide widget with integer values
  ##
  ## * min  - the minimal value on the slider
  ## * val  - the current value on the slider
  ## * max  - the maximum value on the slider
  ## * step - the amount of incrementing or decrementing the value on the
  ##          slider with mouse click
  ##
  ## Returns the new value on the slider
  proc nk_slide_int(ctx; min, val, max, step: cint): cint {.importc, nodecl.}
  return nk_slide_int(ctx, min.cint, val.cint, max.cint, step.cint).int

# -------
# Layouts
# -------
proc layoutSpacePush(ctx; x, y, w, h: cfloat) =
  ## Push the next widget's position and size, internal use only, temporary code
  ##
  ## * ctx - the Nuklear context
  ## * x   - the amount of pixels or ratio to push the position in X axis
  ## * y   - the amount of pixels or ratio to push the position in Y axis
  ## * w   - the amount of pixels or ratio to push the width
  ## * h   - the amount of pixels or ratio to push the height
  proc nk_layout_space_push(ctx; rect: nk_rect) {.importc, nodecl.}
  nk_layout_space_push(ctx, new_nk_rect(x, y, w, h))

proc setLayoutRowDynamic*(height: float; cols: int) =
  ## Set the current widgets layout to divide it into selected amount of
  ## columns with the selected height in rows and grows in width when the
  ## parent window resizes
  ##
  ## * height - the height in pixels of each row
  ## * cols   - the amount of columns in each row
  proc nk_layout_row_dynamic(ctx; height: cfloat; cols: cint) {.importc, cdecl.}
  nk_layout_row_dynamic(ctx, height.cfloat, cols.cint)

proc setLayoutRowStatic*(height: float; width, cols: int) =
  ## Set the current widgets layout to divide it into selected amount of
  ## columns with the selected height in rows but it will not grow in width
  ## when the parent window resizes
  ##
  ## * height - the height in pixels of each row
  ## * width  - the width in pixels of each column
  ## * cols   - the amount of columns in each row
  proc nk_layout_row_static(ctx; height: cfloat; itemWidth,
      cols: cint) {.importc, cdecl.}
  nk_layout_row_static(ctx, height.cfloat, width.cint, cols.cint)

template layoutStatic*(height: float; cols: int; content: untyped) =
  ## Start setting manualy each row of the current widgets layout. The layout
  ## will not resize when the parent window change its size
  ##
  ## * height  - the width in pixels or window's ratio of each row
  ## * cols    - the amount of columns in each row
  ## * content - the content of the layout
  nk_layout_row_begin(ctx, NK_STATIC, height.cfloat, cols.cint)
  content
  nk_layout_row_end(ctx)

template layoutDynamic*(height: float; cols: int; content: untyped) =
  ## Start setting manualy each row of the current widgets layout. The layout
  ## will resize when the parent window change its size
  ##
  ## * height   - the width in pixels or window's ratio of each row
  ## * cols    - the amount of columns in each row
  ## * content - the content of the layout
  nk_layout_row_begin(ctx, NK_DYNAMIC, height.cfloat, cols.cint)
  content
  nk_layout_row_end(ctx)

template row*(width: float; content: untyped) =
  ## Set the content of the row in the current widgets layout
  ##
  ## * width   - the width in the pixels or window's ratio of each column
  ## * content - the content of the row
  nk_layout_row_push(ctx, width.cfloat)
  content

proc setLayoutRowStatic*(height: float; cols: int; ratio: openArray[cfloat]) =
  ## Set the current widgets layout to divide it into selected amount of
  ## columns with the selected height in rows but it will not grow in width
  ## when the parent window resizes
  ##
  ## * height - the height in pixels of each row
  ## * cols   - the amount of columns in each row
  ## * ratio  - the array or sequence of cfloat with width of the colums
  nk_layout_row(ctx, NK_STATIC, height.cfloat, cols.cint, ratio.addr)

proc setLayoutRowDynamic*(height: float; cols: int; ratio: openArray[cfloat]) =
  ## Set the current widgets layout to divide it into selected amount of
  ## columns with the selected height in rows but it will grow in width
  ## when the parent window resizes
  ##
  ## * height - the height in pixels of each row
  ## * cols   - the amount of columns in each row
  ## * ratio  - the array or sequence of cfloat with width of the colums
  nk_layout_row(ctx, NK_DYNAMIC, height.cfloat, cols.cint, ratio.addr)

template layoutSpaceStatic*(height: float; widgetsCount: int;
    content: untyped) =
  ## Start setting manualy each row of the current widgets layout. The layout
  ## will not resize when the parent window change its size
  ##
  ## * height       - the width in pixels or window's ratio of each row
  ## * widgetsCount - the amount of widgets in each row.
  ## * content      - the content of the layout
  nk_layout_space_begin(ctx, NK_STATIC, height.cfloat, widgetsCount.cint)
  content
  nk_layout_space_end(ctx)

template layoutSpaceDynamic*(height: float; widgetsCount: int;
    content: untyped) =
  ## Start setting manualy each row of the current widgets layout. The layout
  ## will resize when the parent window change its size
  ##
  ## * height       - the width in pixels or window's ratio of each row
  ## * widgetsCount - the amount of widgets in each row.
  ## * content      - the content of the layout
  nk_layout_space_begin(ctx, NK_DYNAMIC, height.cfloat, widgetsCount.cint)
  content
  nk_layout_space_end(ctx)

template row*(x, y, w, h: float; content: untyped) =
  ## Set the content of the row in the current widgets layout, used in space
  ## layout
  ##
  ## * x       - the amount of pixels or ratio to push the position in X axis
  ## * y       - the amount of pixels or ratio to push the position in Y axis
  ## * w       - the amount of pixels or ratio to push the width
  ## * h       - the amount of pixels or ratio to push the height
  ## * content - the content of the row
  layoutSpacePush(ctx, x.cfloat, y.cfloat, w.cfloat, h.cfloat)
  content

template setRowTemplate*(height: float; settings: untyped) =
  ## Set the options for the row's template setting for the next rows
  ##
  ## * height   - the height of the each row
  ## * settings - the template settings
  nk_layout_row_template_begin(ctx, height.cfloat)
  settings
  nk_layout_row_template_end(ctx)

proc rowTemplateDynamic*() =
  ## Set the selected column's in the row width in the template's row as dynamic,
  ## which means, the widget will resize with its parent.
  proc nk_layout_row_template_push_dynamic(ctx) {.importc, nodecl.}
  nk_layout_row_template_push_dynamic(ctx)

proc rowTemplateVariable*(minWidth: float) =
  ## Set the selected column's width in the row template as dynamic but with
  ## requirement for minumum width for the widget
  ##
  ## * minWidth - the minimum width in pixels for the widgets in the column
  proc nk_layout_row_template_push_variable(ctx; minWidth: cfloat) {.importc, nodecl.}
  nk_layout_row_template_push_variable(ctx, minWidth.cfloat)

proc rowTemplateStatic*(width: float) =
  ## Set the selected column's width in the row template to static value,
  ## widgets in the column will not resize
  ##
  ## * width - the width of the column in the row template
  proc nk_layout_row_template_push_static(ctx; width: cfloat) {.importc, nodecl.}
  nk_layout_row_template_push_static(ctx, width.cfloat)

# -----
# Menus
# -----
template menuBar*(content: untyped) =
  ## Create a menu bar with the selected content
  ##
  ## * content - the content of the menu bar
  nk_menubar_begin(ctx)
  content
  nk_menubar_end(ctx)

proc createMenu(ctx; text: cstring; align: nk_flags; x, y: cfloat): bool =
  ## Create a Nuklear menu, internal use only, temporary code
  ##
  ## Returns true if the popup was successfully created, otherwise false.
  ##
  ## * ctx   - the Nuklear context
  ## * text  - the label for the menu
  ## * align - the menu alignment
  ## * x     - the X position of the top left corner of the menu
  ## * y     - the Y position of the top left corner of the menu
  ##
  ## Returns true if menu were created, otherwise false
  proc nk_menu_begin_label(ctx; text: cstring; align: nk_flags;
       size: nk_vec2): nk_bool {.importc, nodecl.}
  return nk_menu_begin_label(ctx, text, align, new_nk_vec2(x, y))

template menu*(text: string; align: TextAlignment; x, y: float;
    content: untyped) =
  ## Create a Nuklear menu
  ##
  ## * text    - the label for the menu
  ## * align   - the menu alignment
  ## * x       - the X position of the top left corner of the menu
  ## * y       - the Y position of the top left corner of the menu
  ## * content - the content of the menu
  if createMenu(ctx, text.cstring, align.nk_flags, x, y):
    content
    nk_menu_end(ctx)

template menuItem*(label: string; align: TextAlignment; onPressCode: untyped) =
  ## Create a Nuklear menu's item. Execute the selected code when the user
  ## select the item from a menu.
  ##
  ## * label       - the label of the item
  ## * align       - the alignment of the item's label
  ## * onPressCode - the code executed when the menu was selected by the user
  if nk_menu_item_label(ctx, label.cstring, align.nk_flags):
    onPressCode

# -------
# Sliders
# -------

proc slider*(min: int; val: var int; max, step: int): bool {.discardable.} =
  ## Create a Nuklear slider with integer values
  ##
  ## * min  - the minimal value on the slider
  ## * val  - the current value on the slider
  ## * max  - the maximum value on the slider
  ## * step - the amount which increase or decrease the slider's value when
  ##          the user drag its button
  ##
  ## Returns true if the current value was modified, otherwise false. Also
  ## the modified parameter val
  proc nk_slider_int(ctx; min: cint; val: var cint; max,
      step: cint): nk_bool {.importc, nodecl.}
  var newVal = val.cint
  result = nk_slider_int(ctx = ctx, min = min.cint, val = newVal,
      max = max.cint, step = step.cint) == nkTrue
  val = newVal

proc slider*(min: float; val: var float; max,
    step: float): bool {.discardable.} =
  ## Create a Nuklear slider with float values
  ##
  ## * min  - the minimal value on the slider
  ## * val  - the current value on the slider
  ## * max  - the maximum value on the slider
  ## * step - the amount which increase or decrease the slider's value when
  ##          the user drag its button
  ##
  ## Returns true if the current value was modified, otherwise false. Also
  ## the modified parameter val
  proc nk_slider_float(ctx; min: cfloat; val: var cfloat; max,
    valueStep: cfloat): nk_bool {.importc, nodecl.}
  var newVal = val.cfloat
  result = nk_slider_float(ctx = ctx, min = min.cfloat, val = newVal,
      max = max.cfloat, value_step = step.cfloat) == nkTrue
  val = newVal

# ----------
# Properties
# ----------

proc property*(name: string; min: int; val: var int; max, step: int;
    incPerPixel: float) =
  ## Create a Nuklear property widget with integer values
  ##
  ## * name        - the name of the property and its label to show on it.
  ##                 Using symbol # will generate random string in its place,
  ##                 but it will not show in the property's text
  ## * min         - the minimal value of the property
  ## * val         - the current value of the property
  ## * max         - the maximum value of the property
  ## * step        - the amount which increase or decrease the property value
  ##                 when the user press arrows buttons
  ## * incPerPixel - the amount which increase or decrease the property value
  ##                 when the user drag in it
  ##
  ## Returns the modified parameter val
  proc nk_property_int(ctx; name: cstring; min: cint; val: var cint; max,
      step: cint; incPerPixel: cfloat) {.importc, nodecl.}
  var newVal = val.cint
  nk_property_int(ctx, name.cstring, min.cint, newVal, max.cint, step.cint,
      incPerPixel.cfloat)
  val = newVal.int

proc property*(name: string; min: float; val: var float; max, step: float;
    incPerPixel: float) =
  ## Create a Nuklear property widget with float values
  ##
  ## * name        - the name of the property and its label to show on it.
  ##                 Using symbol # will generate random string in its place,
  ##                 but it will not show in the property's text
  ## * min         - the minimal value of the property
  ## * val         - the current value of the property
  ## * max         - the maximum value of the property
  ## * step        - the amount which increase or decrease the property value
  ##                 when the user press arrows buttons
  ## * incPerPixel - the amount which increase or decrease the property value
  ##                 when the user drag in it
  ##
  ## Returns the modified parameter val
  proc nk_property_float(ctx; name: cstring; min: cfloat;
      val: var cfloat; max, step, incPerPixel: cfloat) {.importc, nodecl.}
  var newVal = val.cfloat
  nk_property_float(ctx, name.cstring, min.cfloat, newVal, max.cfloat,
      step.cfloat, incPerPixel.cfloat)
  val = newVal.float

proc property2*(name: string; min, val, max, step, incPerPixel: float): float =
  ## Create a Nuklear property widget with float values
  ##
  ## * name        - the name of the property and its label to show on it.
  ##                 Using symbol # will generate random string in its place,
  ##                 but it will not show in the property's text
  ## * min         - the minimal value of the property
  ## * val         - the current value of the property
  ## * max         - the maximum value of the property
  ## * step        - the amount which increase or decrease the property value
  ##                 when the user press arrows buttons
  ## * incPerPixel - the amount which increase or decrease the property value
  ##                 when the user drag in it
  ##
  ## Returns the new value of the property
  proc nk_propertyf(ctx; name: cstring; min, val, max, step,
      incPerPixel: cfloat): cfloat {.importc, nodecl.}
  return nk_propertyf(ctx, name.cstring, min.cfloat, val.cfloat, max.cfloat,
      step.cfloat, incPerPixel.cfloat).float

proc property2*(name: string; min, val, max, step: int;
    incPerPixel: float): int =
  ## Create a Nuklear property widget with integer values
  ##
  ## * name        - the name of the property and its label to show on it.
  ##                 Using symbol # will generate random string in its place,
  ##                 but it will not show in the property's text
  ## * min         - the minimal value of the property
  ## * val         - the current value of the property
  ## * max         - the maximum value of the property
  ## * step        - the amount which increase or decrease the property value
  ##                 when the user press arrows buttons
  ## * incPerPixel - the amount which increase or decrease the property value
  ##                 when the user drag in it
  ##
  ## Returns the new value of the property
  proc nk_propertyi(ctx; name: cstring; min, val, max, step: cint;
      incPerPixel: cfloat): cint {.importc, nodecl.}
  return nk_propertyi(ctx, name.cstring, min.cint, val.cint, max.cint,
      step.cint, incPerPixel.cfloat).int

# -----
# Style
# -----
proc headerAlign*(value: StyleHeaderAlign) =
  ## Set the Nuklear windows header alignment
  ##
  ## * value - the new value for the alignment
  ctx.style.window.header.align = value.ord.nk_style_header_align

var buttonStyle: nk_style_button ## Used to store the Nuklear buttons style

proc saveButtonStyle*() =
  ## Save the Nuklear buttons style to variable, so it can be restored later
  buttonStyle = ctx.style.button

proc restoreButtonStyle*() =
  ## Restore previously save to the variable Nuklear buttons style
  ##
  ctx.style.button = buttonStyle

proc setButtonStyle*(field: ButtonStyleTypes; r, g, b: cint) =
  ## Set the color for the selcted field of the Nuklear buttons style
  ##
  ## * field - the style's field which value will be changed
  ## * r     - the red value for the style color in RGB
  ## * g     - the green value for the style color in RGB
  ## * b     - the blue value for the style color in RGB
  case field
  of normal:
    ctx.style.button.normal = nk_style_item_color(nk_rgb(r, g, b))
  of hover:
    ctx.style.button.hover = nk_style_item_color(nk_rgb(r, g, b))
  of active:
    ctx.style.button.active = nk_style_item_color(nk_rgb(r, g, b))
  of borderColor:
    ctx.style.button.border_color = nk_rgb(r, g, b)
  of textBackground:
    ctx.style.button.text_background = nk_rgb(r, g, b)
  of textNormal:
    ctx.style.button.text_normal = nk_rgb(r, g, b)
  of textHover:
    ctx.style.button.text_hover = nk_rgb(r, g, b)
  of textActive:
    ctx.style.button.text_active = nk_rgb(r, g, b)
  else:
    discard

proc setButtonStyle2*(source, destination: ButtonStyleTypes) =
  ## Copy one field of Nuklear buttons style to another
  ##
  ## * source      - the field which value will be copied
  ## * destination - the field to which the source value will be copied
  if source == active:
    if destination == normal:
      ctx.style.button.normal = ctx.style.button.active

proc getButtonStyle*(field: ButtonStyleTypes): NimVec2 =
  ## Get the value of the selected field of Nuklear buttons style
  ##
  ## * field - the field which value will be taken
  ##
  ## Returns vector with the value of the selected field
  if field == padding:
    return NimVec2(x: ctx.style.button.padding.x, y: ctx.style.button.padding.y)

proc stylePushVec2*(field: WindowStyleTypes; x,
    y: cfloat): bool {.discardable.} =
  ## Push the vector value for the selected Nuklear window style on a
  ## temporary stack
  ##
  ## * field - the Nuklear windows style field which will be modified
  ## * x     - the X value of the vector to push
  ## * y     - the Y value of the vector to push
  ##
  ## Returns true if value was succesfully pushed, otherwise false
  proc nk_style_push_vec2(ctx; dest: var nk_vec2;
      source: nk_vec2): nk_bool {.importc, nodecl.}
  if field == spacing:
    return nk_style_push_vec2(ctx, ctx.style.window.spacing, new_nk_vec2(x,
        y))

proc stylePushFloat*(field: ButtonStyleTypes;
    value: cfloat): bool {.discardable.} =
  ## Push the float value for the selected Nuklear buttons style on a
  ## temporary stack
  ##
  ## * ctx   - the Nuklear context
  ## * field - the Nuklear buttons style field which will be modified
  ## * value - the float value to push
  ##
  ## Returns true if value was succesfully pushed, otherwise false
  proc nk_style_push_float(ctx; dest: var cfloat;
      source: cfloat): nk_bool {.importc, nodecl.}
  case field
  of rounding:
    return nk_style_push_float(ctx, ctx.style.button.rounding, value)
  else:
    return false

proc styleFromTable*(table: openArray[NimColor]) =
  ## Set the Nuklear style colors from the table
  ##
  ## * table - the colors table which will be set
  proc nk_style_from_table(ctx; table: pointer) {.importc, nodecl.}
  var newTable: array[countColors.ord, nk_color]
  for index, color in table.pairs:
    newTable[index] = nk_rgba(color.r.cint, color.g.cint, color.b.cint, color.a.cint)
  nk_style_from_table(ctx, newTable.addr)

proc defaultStyle*() =
  ## reset the UI colors to the default Nuklear setting
  proc nk_style_default(ctx) {.importc, nodecl.}
  nk_style_default(ctx)

proc stylePopFloat*() =
  proc nk_style_pop_float(ctx) {.importc, nodecl.}
  nk_style_pop_float(ctx)

proc stylePopVec2*() =
  proc nk_style_pop_vec2(ctx) {.importc, nodecl.}
  nk_style_pop_vec2(ctx)

# ------
# Combos
# ------
proc comboList*(items: openArray[string]; selected, itemHeight: int; x,
    y: float; amount: int = items.len - 1): int =
  ## Create a Nuklear combo widget
  ##
  ## * items       - the list of values for the combo
  ## * selected    - the index of the selected value on the combo's list
  ## * item_height - the height in pixels for the values in the combo's list
  ## * x           - the width of the combo
  ## * y           - the height of the combo's values list
  ## * amount      - the amount of items in the items list. Default to the length
  ##                 of the list
  ##
  ## Returns the index of the currently selected valu on the combo's list
  proc nk_combo(ctx; items: pointer; count,
      selected, itemHeight: cint; size: nk_vec2): cint {.importc, nodecl.}
  var optionsList: seq[cstring]
  for i in 0 .. amount:
    optionsList.add(items[i].cstring)
  return nk_combo(ctx, optionsList[0].addr, amount.cint + 1,
      selected.cint, itemHeight.cint, new_nk_vec2(x.cfloat, y.cfloat)).int

proc createColorCombo(ctx; color: NimColor; x, y: cfloat): bool =
  ## Create a Nuklear combo widget which display color as the value, internal
  ## use only, temporary code
  ##
  ## * ctx   - the Nuklear context
  ## * color - the color displayed as the value of the combo
  ## * x     - the width of the combo
  ## * y     - the height of the combo's values list
  ##
  ## Returns true if combo was successfully created, otherwise false
  return nk_combo_begin_color(ctx, nk_rgb(color.r.cint, color.g.cint,
      color.b.cint), new_nk_vec2(x, y))

template colorCombo*(color: NimColor; x, y: float; content: untyped) =
  ## Create a Nuklear combo widget which display color as the value, internal
  ## use only, temporary code
  ##
  ## * color   - the color displayed as the value of the combo
  ## * x       - the width of the combo
  ## * y       - the height of the combo's values list
  ## * content - the content of the combo widget
  if createColorCombo(ctx, color, x.cfloat, y.cfloat):
    content
    nk_combo_end(ctx)

proc createColorCombo(ctx; color: NimColorF; x, y: cfloat): bool =
  ## Create a Nuklear combo widget which display color with float values as
  ## the value, internal use only, temporary code
  ##
  ## * ctx   - the Nuklear context
  ## * color - the color with float values displayed as the value of the combo
  ## * x     - the width of the combo
  ## * y     - the height of the combo's values list
  ##
  ## Returns true if combo was successfully created, otherwise false
  return nk_combo_begin_color(ctx, nk_rgb_cf(nk_colorf(r: color.r.cfloat,
      g: color.g.cfloat, b: color.b.cfloat, a: color.a.cfloat)), new_nk_vec2(x, y))


template colorCombo*(color: NimColorF; x, y: float; content: untyped) =
  ## Create a Nuklear combo widget which display color with float values as
  ## the value
  ##
  ## * color   - the color with float values displayed as the value of the combo
  ## * x       - the width of the combo
  ## * y       - the height of the combo's values list
  ## * content - the content of the combo widget
  if createColorCombo(ctx, color, x.cfloat, y.cfloat):
    content
    nk_combo_end(ctx)

proc createLabelCombo(ctx; selected: cstring; x, y: cfloat): bool =
  ## Create a Nuklear combo widget which display the custom text as the value,
  ## internal use only, temporary code
  ##
  ## * ctx      - the Nuklear context
  ## * selected - the text to display as the value of the combo
  ## * x        - the width of the combo
  ## * y        - the height of the combo's values list
  ##
  ## Returns true if combo was successfully created, otherwise false
  proc nk_combo_begin_label(ctx; selected: cstring;
      size: nk_vec2): nk_bool {.importc, nodecl.}
  return nk_combo_begin_label(ctx, selected, new_nk_vec2(x, y))

template labelCombo*(selected: string; x, y: float; content: untyped) =
  ## Create a Nuklear combo widget which display the custom text as the value
  ##
  ## * selected - the text to display as the value of the combo
  ## * x        - the width of the combo
  ## * y        - the height of the combo's values list
  ## * content - the content of the combo widget
  if createLabelCombo(ctx, selected.cstring, x.cfloat, y.cfloat):
    content
    nk_combo_end(ctx)

proc comboClose*() =
  ## Stop adding a value to a combo
  proc nk_combo_close(ctx) {.importc, nodecl.}
  nk_combo_close(ctx)

# ------
# Colors
# ------
proc colorfToHsva*(hsva: var array[4, float]; color: NimColorF) =
  ## Convert Nim float color object to HSVA values
  ##
  ## * hsva  - the array of 4 values for HSVA color
  ## * color - the Nim color to convert
  ##
  ## Returns converted color as hsva argument
  proc nk_colorf_hsva_fv(hsva: pointer; color: nk_colorf) {.importc, nodecl.}
  nk_colorf_hsva_fv(hsva.addr, nk_colorf(r: color.r, g: color.g,
      b: color.b, a: color.a))
proc hsvaToColorf*(hsva: array[4, float]): NimColorF =
  ## Convert HSVA values to Nim color with float values
  ##
  ## * hsva - the array with HSVA values to convert
  ##
  ## Returns converted hsva parameter to Nim color with float values
  proc nk_hsva_colorf(h, s, v, a: cfloat): nk_colorf {.importc, nodecl.}
  let newColor = nk_hsva_colorf(hsva[0], hsva[1], hsva[2], hsva[3])
  result = NimColorF(r: newColor.r, g: newColor.g, b: newColor.b, a: newColor.a)

# ------
# Charts
# ------
proc createColorChart(ctx; ctype: ChartType; color,
    higlight: NimColor; count: cint; minValue, maxValue: cfloat): bool =
  ## Create a colored chart, internal use only, temporary code
  ##
  ## * ctx       - the Nuklear context
  ## * ctype     - the type of the chart
  ## * color     - the color used for drawing the chart
  ## * highligh  - the color used for highlighting point when mouse hovering
  ##               over it
  ## * count     - the amount of values on the chart
  ## * min_value - the minimal value of the chart
  ## * max_value - the maximum value of the chart
  ##
  ## Returns true if the chart was succesfully created otherwise false
  proc nk_chart_begin_colored(ctx; ctype: ChartType; color,
      higlight: nk_color; count: cint; minValue,
      maxValue: cfloat): nk_bool {.importc, nodecl.}
  return nk_chart_begin_colored(ctx, ctype, nk_rgb(color.r.cint, color.g.cint,
      color.b.cint), nk_rgb(higlight.r.cint, higlight.g.cint, higlight.b.cint),
          count, minValue, maxValue)

template colorChart*(cType: ChartType; color, highlight: NimColor; count: int;
    minValue, maxValue: float; content: untyped) =
  ## Create a colored chart
  ##
  ## * cType    - the type of the chart
  ## * color    - the color used for drawing the chart
  ## * highligh - the color used for highlighting point when mouse hovering
  ##              over it
  ## * count    - the amount of values on the chart
  ## * minValue - the minimal value of the chart
  ## * maxValue - the maximum value of the chart
  if createColorChart(ctx, cType, color, highlight, count.cint, minValue.cfloat,
      maxValue.cfloat):
    content
    nk_chart_end(ctx)

proc addColorChartSlot*(ctype: ChartType; color,
    higlight: NimColor; count: cint; minValue, maxValue: cfloat) =
  ## Add another chart to the existing one
  ##
  ## * ctype     - the type of the chart
  ## * color     - the color used for drawing the chart
  ## * highligh  - the color used for highlighting point when mouse hovering
  ##               over it
  ## * count     - the amount of values on the chart
  ## * min_value - the minimal value of the chart
  ## * max_value - the maximum value of the chart
  proc nk_chart_add_slot_colored(ctx; ctype: ChartType; color,
      higlight: nk_color; count: cint; minValue, maxValue: cfloat) {.importc, nodecl.}
  nk_chart_add_slot_colored(ctx, ctype, nk_rgb(color.r.cint, color.g.cint,
      color.b.cint), nk_rgb(higlight.r.cint, higlight.g.cint, higlight.b.cint),
          count, minValue, maxValue)

template chart*(cType: ChartType; num: int; min, max: float; content: untyped) =
  ## Create a chart of the selected type
  ##
  ## * cType   - the type of the chart
  ## * num     - the amount of values in the chart
  ## * min     - the minimum value on the chart
  ## * max     - the maximum value on the chart
  ## * content - the content of the chart, usually coe related to adding values
  if nk_chart_begin(ctx, cType, num.cint, min.cfloat, max.cfloat):
    content
    ctx.nk_chart_end

proc chartPush*(value: float): ChartEvent {.discardable.} =
  ## Push, add the value to the current chart
  ##
  ## * value - the value to add
  ##
  ## Returns the mouse event if any happened over the value in the chart
  proc nk_chart_push(ctx; value: cfloat): nk_flags {.importc, nodecl.}

  let res = nk_chart_push(ctx, value.cfloat)
  if (res and clicked.nk_flags) == clicked.nk_flags:
    return clicked
  if (res and hovering.nk_flags) == hovering.nk_flags:
    return hovering
  return none

proc addChartSlot*(ctype: ChartType; count: int; minValue, maxValue: float) =
  ## Add another chart to the existing one
  ##
  ## * ctype     - the type of the chart
  ## * count     - the amount of values on the chart
  ## * min_value - the minimal value of the chart
  ## * max_value - the maximum value of the chart
  proc nk_chart_add_slot(ctx; ctype: ChartType; count: cint;
      minValue, maxValue: cfloat) {.importc, nodecl.}
  nk_chart_add_slot(ctx, ctype, count.cint, minValue.cfloat, maxValue.cfloat)

proc chartPushSlot*(value: float; slot: int): ChartEvent {.discardable.} =
  ## Push, add the value to the current chart at the selected position
  ##
  ## * value - the value to add
  ## * slot  - the slot to which the value will be added
  ##
  ## Returns the mouse event if any happened over the value in the chart
  proc nk_chart_push_slot(ctx; value: cfloat; slot: cint): nk_flags {.importc, nodecl.}

  let res = nk_chart_push_slot(ctx, value.cfloat, slot.cint)
  if (res and clicked.nk_flags) == clicked.nk_flags:
    return clicked
  if (res and hovering.nk_flags) == hovering.nk_flags:
    return hovering
  return none

# ----------
# Contextual
# ----------
proc createContextual(ctx; flags: nk_flags; x, y: cfloat;
    triggerBounds: NimRect): bool =
  ## Create a contextual menu, internal use only, temporary code
  ##
  ## * ctx            - the Nuklear context
  ## * flags          - the flags for the menu
  ## * x              - the width of the menu
  ## * y              - the height of the menu
  ## * trigger_bounds - the rectange of coordinates in the window where clicking
  ##                    cause the menu to appear
  ##
  ## Return true if the contextual menu was created successfully, otherwise
  ## false
  proc nk_contextual_begin(ctx; flags: nk_flags; size: nk_vec2;
      triggerBounds: nk_rect): nk_bool {.importc, nodecl.}
  return nk_contextual_begin(ctx, flags, new_nk_vec2(x, y), new_nk_rect(
      triggerBounds.x, triggerBounds.y, triggerBounds.w,
      triggerBounds.h))

template contextualMenu*(flags: set[WindowFlags]; x, y;
    triggerBounds: NimRect; content: untyped) =
  ## Create a contextual menu
  ##
  ## * flags         - the flags for the menu
  ## * x             - the width of the menu
  ## * y             - the height of the menu
  ## * triggerBounds - the rectange of coordinates in the window where clicking
  ##                   cause the menu to appear
  ## * content       - the content of the menu
  if createContextual(ctx, winSetToInt(flags), x, y, triggerBounds):
    content
    nk_contextual_end(ctx)

template contextualItemLabel*(label: string; align: TextAlignment;
    onPressCode: untyped) =
  ## Add a clickable label to a contextual menu
  ##
  ## * label       - the text to show on the label
  ## * align       - the alignment of the text to show
  ## * onPressCode - the Nim code to execute when the label was pressed
  if nk_contextual_item_label(ctx, label.cstring, align.nk_flags):
    onPressCode

# --------
# Tooltips
# --------

macro fmtTooltip*(args: varargs[untyped]): untyped =
  ## Draw a tooltip formatted in the same way like the C function printf
  ##
  ## * args      - the tooltip's text and its arguments to draw
  result = quote do:
    nk_tooltipf(ctx, `args`)

proc tooltip*(text: string) =
  ## Draw a tooltip with the selected text
  ##
  ## * text - the text to show on the tooltip window
  proc nk_tooltip(ctx; text: cstring) {.importc, nodecl.}
  nk_tooltip(ctx, text.cstring)

# ------
# Groups
# ------

template group*(title: string; flags: set[WindowFlags]; content: untyped) =
  ## Set a group of widgets inside the parent
  ##
  ## * title   - the title of the group
  ## * flags   - the set of WindowFlags for the group
  ## * content - the content of the group
  if nk_group_begin(ctx, title.cstring, winSetToInt(flags)):
    content
    nk_group_end(ctx)

# -----
# Input
# -----
proc isMouseHovering*(rect: NimRect): bool =
  ## Check if mouse is hovering over the selected rectangle
  ##
  ## * x   - the X coordinate of top left corner of the rectangle
  ## * y   - the Y coordinate of top left corner of the rectangle
  ## * w   - the width of the rectangle in pixels
  ## * h   - the height of the rectangle in pixels
  ##
  ## Returns true if the mouse is hovering over the rectangle, otherwise false
  proc nk_input_is_mouse_hovering_rect(i: ptr nk_input;
      rect: nk_rect): nk_bool {.importc, nodecl.}
  return nk_input_is_mouse_hovering_rect(ctx.input.addr, new_nk_rect(rect.x,
      rect.y, rect.w, rect.h))

proc isMousePrevHovering*(x, y, w, h: float): bool =
  ## Check if the mouse was previously hovering over the selected rectangle
  ##
  ## * x   - the X coordinate of top left corner of the rectangle
  ## * y   - the Y coordinate of top left corner of the rectangle
  ## * w   - the width of the rectangle in pixels
  ## * h   - the height of the rectangle in pixels
  ##
  ## Returns true if the mouse was hovering over the rectangle, otherwise false
  proc nk_input_is_mouse_prev_hovering_rect(i: ptr nk_input;
      rect: nk_rect): nk_bool {.importc, nodecl.}
  return nk_input_is_mouse_prev_hovering_rect(ctx.input.addr, new_nk_rect(
      x, y, w, h))

proc isMouseDown*(id: Buttons): bool =
  ## Check if mouse is pressed
  ##
  ## * id  - the mouse button which is pressed
  ##
  ## Returns true if the selected mouse button is pressed, otherwise false
  proc nk_input_is_mouse_down(i: ptr nk_input; id: Buttons): nk_bool {.importc, nodecl.}
  return nk_input_is_mouse_down(ctx.input.addr, id)

proc getMouseDelta*(): NimVec2 =
  ## Get the mouse vector between last check and current position of the mouse
  ##
  ## Returns vector with information about the mouse movement delta
  return NimVec2(x: ctx.input.mouse.delta.x, y: ctx.input.mouse.delta.y)

# ---------
# Edit text
# ---------
proc editString*(text: var string; maxLen: int; editType: EditTypes = simple;
    filter: PluginFilter = nk_filter_default; flags: set[EditFlags] = {}): EditEvent {.discardable.} =
  ## Draw the field of hte selected type and with the selected filter to edit a
  ## text
  ##
  ##  * text     - the text which will be edited in the field
  ##  * maxLen   - the maximum length of the text to edit
  ##  * editType - the type of the edit field. By default it is a simple, one
  ##               line field
  ##  * filter   - the procedure used to filter the user's input in the edit
  ##               field. By default there is no filtering.
  ##  * flags    - the additional flags for the edit field. By default no
  ##               additional flags
  ##
  ## Returns the current state of the edit field and the modified text
  ## parameter.

  proc nk_edit_string(ctx; flags: nk_flags; memory: pointer;
      len: var cint; max: cint; filter: PluginFilter): nk_flags {.importc, nodecl.}

  var
    (cText, length) = stringToCharArray(text, maxLen)
    cFlags: cint = editType.ord.cint
  {.warning[HoleEnumConv]: off.}
  for flag in flags:
    cFlags = cFlags or flag.cint
  result = nk_edit_string(ctx = ctx, flags = cFlags,
      memory = cText[0].addr, len = length.cint, max = maxLen.cint,
      filter = filter).EditEvent
  text = charArrayToString(cText, length)

# -----------
# Selectables
# -----------

proc selectableLabel*(str: string; value: var bool;
    align: TextAlignment = left): bool {.discardable.} =
  ## Draw the text which can be selected with the mouse
  ##
  ## * str   - the text which will be draw
  ## * value - if true, the text is currently selected
  ## * align - the alignment of the text. Default value is left
  ##
  ## Returns true if the selection's state of the text was changed,
  ## otherwise false. Also returns the value parameter with the
  ## current state of the label
  proc nk_selectable_label(ctx; str: cstring; align: nk_flags;
      value: var nk_bool): nk_bool {.importc, nodecl.}
  var newValue = value.nk_bool
  result = nk_selectable_label(ctx, str.cstring, align.nk_flags, newValue) == nkTrue
  discard $newValue
  value = newValue

proc selectableSymbolLabel*(sym: SymbolType; title: string; value: var bool;
    align: TextAlignment = left): bool {.discardable.} =
  ## Draw the text with the symbol which can be selected with the mouse
  ##
  ## * sym   - the symbol which will be draw
  ## * str   - the text which will be draw
  ## * value - if true, the text is currently selected
  ## * align - the alignment of the text. Default value is left
  ##
  ## Returns true if the selection's state of the text was changed,
  ## otherwise false. Also returns the value parameter with the
  ## current state of the label
  proc nk_selectable_symbol_label(ctx; sym: SymbolType;
    title: cstring; align: nk_flags; value: var nk_bool): nk_bool {.importc, nodecl.}
  var newValue = value.nk_bool
  result = nk_selectable_symbol_label(ctx, sym, title.cstring, align.nk_flags,
      newValue) == nkTrue
  discard $newValue
  value = newValue

# -------
# Widgets
# -------
proc colorPicker*(color: NimColorF;
    format: colorFormat): NimColorF =
  ## Create the color picker widget
  ##
  ## * color  - the starting color for the widget
  ## * format - the color format for the widget
  ##
  ## Returns Nim color selected by the user in the widget
  proc nk_color_picker(ctx; color: nk_colorf;
      fmt: colorFormat): nk_colorf {.importc, nodecl.}
  let newColor = nk_color_picker(ctx, nk_colorf(r: color.r, g: color.g,
      b: color.b, a: color.a), format)
  result = NimColorF(r: newColor.r, g: newColor.g, b: newColor.b, a: newColor.a)

proc checkBox*(label: string; checked: var bool): bool {.discardable.} =
  ## Create a Nuklear checkbox widget
  ##
  ## * label   - the text to show with the checkbox
  ## * checked - the state of the checkbox, if true, the checkbox is checked
  ##
  ## Returns true if the state of the checkbox was changed, otherwise false.
  proc nk_checkbox_label(ctx; text: cstring;
      active: var cint): nk_bool {.importc, nodecl.}
  var active: cint = (if checked: 1 else: 0)
  result = nk_checkbox_label(ctx = ctx, text = label.cstring,
      active = active) == nkTrue
  checked = active == 1

proc option*(label: string; selected: bool): bool =
  ## Create a Nuklear option (radio) widget
  ##
  ## * label    - the text show with the option
  ## * selected - the state of the option, if true the option is selected
  ##
  ## Returns true if the option is selected, otherwise false
  proc nk_option_label(ctx; name: cstring; active: cint): nk_bool {.importc, nodecl.}
  var active: cint = (if selected: 1 else: 0)
  return nk_option_label(ctx = ctx, name = label.cstring, active = active) == nkTrue

proc progressBar*(value: var int; maxValue: int;
    modifyable: bool = true): bool {.discardable.} =
  ## Create a Nuklear progress bar widget
  ##
  ## * value      - the current value of the progress bar
  ## * maxValue   - the maximum value of the progress bar
  ## * modifyable - if true, the user can modify the value of the progress bar
  ##
  ## Returns true if the value parameter was changed, otherwise false
  proc nk_progress(ctx; cur: var nk_size; max: nk_size;
      modifyable: nk_bool): nk_bool {.importc, nodecl.}
  return nk_progress(ctx = ctx, cur = value, max = maxValue,
      modifyable = modifyable.nk_bool) == nkTrue

