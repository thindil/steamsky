# Copyright © 2023-2025 Bartek Jasicki
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

## Provides types from nuklear library
import contracts, nimalyzer

# ------------
# Simple types
# ------------
type
  nk_flags* = cint
    ## Internal Nuklear type
  nk_size* = clong
    ## Internal Nuklear type
  nk_byte* = uint8
    ## Internal Nuklear type
  nk_rune* = cuint
    ## Internal Nuklear type
  nk_ushort* = cushort
    ## Internal Nuklear type
  nk_hash* = cuint
    ## Internal Nuklear type
  nk_uint* = cuint
    ## Internal Nuklear type

# ------------
# Enumerations
# ------------
type
  nk_style_header_align* = enum
    ## Internal Nuklear type
    NK_HEADER_LEFT, NK_HEADER_RIGHT
  nk_layout_format* = enum
    ## Internal Nuklear type
    NK_DYNAMIC, NK_STATIC
  nk_text_align* = enum
    ## Internal Nuklear type
    NK_TEXT_ALIGN_LEFT = 0x01,
    NK_TEXT_ALIGN_CENTERED = 0x02,
    NK_TEXT_ALIGN_RIGHT = 0x04,
    NK_TEXT_ALIGN_TOP = 0x08,
    NK_TEXT_ALIGN_MIDDLE = 0x10,
    NK_TEXT_ALIGN_BOTTOM = 0x20
  TreeType* = enum
    ## The types of tree widget
    node, tab
  ChartType* = enum
    ## The types of charts
    lines, column, chartMax
  nk_bool* = enum
    ## Internal Nuklear type
    nkFalse, nkTrue
  nk_modify* = enum
    ## Internal Nuklear type
    NK_FIXED, NK_MODIFIABLE
  CollapseStates* = enum
    ## The states of a tree's content
    minimized, maximized
  SymbolType* = enum
    ## The types of symbolic icons
    none, x, underscore, circleSolid, circleOutline, rectSolid, rectOutline,
      triangleUp, triangleDown, triangleLeft, triangleRight, plus, minus, max
  nk_style_item_type* = enum
    ## Internal Nuklear type
    NK_STYLE_ITEM_COLOR, NK_STYLE_ITEM_IMAGE, NK_STYLE_ITEM_NINE_SLICE
  colorFormat* = enum
    ## Colors formats
    rgb, rgba
  ChartEvent* = enum
    ## Events of charts
    none,
    hovering = 0x01,
    clicked = 0x02
  Buttons* = enum
    ## Types of buttoons
    left, middle, right, double, max
  nk_anti_aliasing* = enum
    ## Internal Nuklear type
    NK_ANTI_ALIASING_OFF, NK_ANTI_ALIASING_ON
  nk_window_flags* = enum
    ## Internal Nuklear type
    NK_WINDOW_DYNAMIC = 1 shl 11
    NK_WINDOW_ROM = 1 shl 12
    NK_WINDOW_HIDDEN = 1 shl 13
    NK_WINDOW_CLOSED = 1 shl 14
    NK_WINDOW_MINIMIZED = 1 shl 15
    NK_WINDOW_REMOVE_ROM = 1 shl 16
  nk_command_type* = enum
    ## Internal Nuklear type
    NK_COMMAND_NOP, NK_COMMAND_SCISSOR, NK_COMMAND_LINE, NK_COMMAND_CURVE,
      NK_COMMAND_RECT, NK_COMMAND_RECT_FILLED, NK_COMMAND_RECT_MULTI_COLOR,
      NK_COMMAND_CIRCLE, NK_COMMAND_CIRCLE_FILLED, NK_COMMAND_ARC,
      NK_COMMAND_ARC_FILLED, NK_COMMAND_TRIANGLE, NK_COMMAND_TRIANGLE_FILLED,
      NK_COMMAND_POLYGON, NK_COMMAND_POLYGON_FILLED, NK_COMMAND_POLYLINE,
      NK_COMMAND_TEXT, NK_COMMAND_IMAGE, NK_COMMAND_CUSTOM
  nk_buffer_allocation_type* = enum
    ## Internal Nuklear type
    NK_BUFFER_FRONT, NK_BUFFER_BACK, NK_BUFFER_MAX
  nk_allocation_type* = enum
    ## Internal Nuklear type
    NK_BUFFER_FIXED, NK_BUFFER_DYNAMIC
  nk_keys* = enum
    ## Internal Nuklear type
    NK_KEY_NONE, NK_KEY_SHIFT, NK_KEY_CTRL, NK_KEY_DEL, NK_KEY_ENTER,
      NK_KEY_TAB, NK_KEY_BACKSPACE, NK_KEY_COPY, NK_KEY_CUT, NK_KEY_PASTE,
      NK_KEY_UP, NK_KEY_DOWN, NK_KEY_LEFT, NK_KEY_RIGHT,
      NK_KEY_TEXT_INSERT_MODE, NK_KEY_TEXT_REPLACE_MODE, NK_KEY_TEXT_RESET_MODE,
      NK_KEY_TEXT_LINE_START, NK_KEY_TEXT_LINE_END, NK_KEY_TEXT_START,
      NK_KEY_TEXT_END, NK_KEY_TEXT_UNDO, NK_KEY_TEXT_REDO,
      NK_KEY_TEXT_SELECT_ALL, NK_KEY_TEXT_WORD_LEFT, NK_KEY_TEXT_WORD_RIGHT,
      NK_KEY_SCROLL_START, NK_KEY_SCROLL_END, NK_KEY_SCROLL_DOWN,
      NK_KEY_SCROLL_UP, NK_KEY_ESCAPE, NK_KEY_MAX
  nk_buttons* = enum
    ## Internal Nuklear type
    NK_BUTTON_LEFT,
    NK_BUTTON_MIDDLE,
    NK_BUTTON_RIGHT,
    NK_BUTTON_DOUBLE,
    NK_BUTTON_MAX
  nk_style_cursor* = enum
    ## Internal Nuklear type
    NK_CURSOR_ARROW,
    NK_CURSOR_TEXT,
    NK_CURSOR_MOVE,
    NK_CURSOR_RESIZE_VERTICAL,
    NK_CURSOR_RESIZE_HORIZONTAL,
    NK_CURSOR_RESIZE_TOP_LEFT_DOWN_RIGHT,
    NK_CURSOR_RESIZE_TOP_RIGHT_DOWN_LEFT,
    NK_CURSOR_COUNT
  nk_button_behavior* = enum
    ## Internal Nuklear type
    NK_BUTTON_DEFAULT,
    NK_BUTTON_REPEATER
  nk_widget_states* = enum
    ## Internal Nuklear type
    NK_WIDGET_STATE_MODIFIED = 1 shl 1
    NK_WIDGET_STATE_INACTIVE = 1 shl 2
    NK_WIDGET_STATE_ENTERED = 1 shl 3
    NK_WIDGET_STATE_HOVER = 1 shl 4
    NK_WIDGET_STATE_ACTIVED = 1 shl 5
    NK_WIDGET_STATE_LEFT = 1 shl 6
    NK_WIDGET_STATE_HOVERED = NK_WIDGET_STATE_HOVER.int or
        NK_WIDGET_STATE_MODIFIED.int
    NK_WIDGET_STATE_ACTIVE = NK_WIDGET_STATE_ACTIVED.int or
        NK_WIDGET_STATE_MODIFIED.int

# -------
# Objects
# -------
{.push ruleOff: "namedParams".}
type
  nk_color* {.importc: "struct nk_color", nodecl.} = object
    ## Internal Nuklear type
    r*, g*, b*, a*: nk_byte
  nk_colorf* {.importc: "struct nk_colorf", nodecl.} = object
    ## Internal Nuklear type
    r*, g*, b*, a*: cfloat
  nk_vec2* {.importc: "struct nk_vec2", nodecl, completeStruct.} = object
    ## Internal Nuklear type
    x*, y*: cfloat
  nk_nine_slice* {.importc: "struct nk_nine_slice", nodecl.} = object
    ## Internal Nuklear type
    image*: nk_image
    l*, t*, r*, b*: nk_ushort
  nk_style_item_data* {.bycopy, union.} = object
    ## Internal Nuklear type
    color*: nk_color
    image*: nk_image
    slice*: nk_nine_slice
  nk_style_item* {.importc: "struct nk_style_item", nodecl.} = object
    ## Internal Nuklear type
    `type`*: nk_style_item_type
    data*: pointer
  nk_style_window_header* {.importc, nodecl.} = object
    ## Internal Nuklear type
    align*: nk_style_header_align
    padding*, label_padding*, spacing*: nk_vec2
    active*, hover*, normal*: nk_style_item
    label_active*, label_hover*, label_normal*: nk_color
    close_symbol*: SymbolType
    close_button*: nk_style_button
  nk_style_window* {.importc, nodecl.} = object
    ## Internal Nuklear type
    header*: nk_style_window_header
    spacing*, scrollbar_size*, padding*, group_padding*, popup_padding*,
      contextual_padding*, combo_padding*, menu_padding*,
      tooltip_padding*: nk_vec2
    background*, group_text_color*: nk_color
    border*, combo_border*, contextual_border*, menu_border*, group_border*,
      tooltip_border*, popup_border*: cfloat
  # FIXME: check nuklear.h needs return type
  nk_draw_begin* = proc(b: ptr nk_command_buffer; userData: nk_handle) {.cdecl.}
    ## Internal Nuklear type
  # FIXME: check nuklear.h needs return type
  nk_draw_end* = proc(b: ptr nk_command_buffer; userData: nk_handle) {.cdecl.}
    ## Internal Nuklear type
  nk_style_button* {.importc: "struct nk_style_button", nodecl.} = object
    ## Internal Nuklear type
    normal*, hover*, active*: nk_style_item
    border_color*, text_background*, text_normal*, text_hover*,
      text_active*: nk_color
    rounding*, border*, color_factor_background*, color_factor_text*: cfloat
    padding*: nk_vec2
    image_padding*: nk_vec2
    touch_padding*: nk_vec2
    # TODO
    # draw_begin*: nk_draw_begin
    # draw_end*: nk_draw_end
    # userdata*: nk_handle
  nk_style_progress* {.importc: "struct nk_style_progress", nodecl.} = object
    cursor_normal*: nk_style_item
    ## Internal Nuklear type
  nk_handle* {.bycopy, union.} = object
    ## Internal Nuklear type
    `ptr`*: pointer
    id*: cint
  nk_text_width_f* = proc (arg1: nk_handle; h: cfloat; arg3: cstring;
      len: cint): cfloat {.cdecl.}
    ## Internal Nuklear type
  nk_user_font* {.importc: "struct nk_user_font", nodecl.} = object
    ## Internal Nuklear type
    userdata*: nk_handle
    height*: cfloat
    width*: nk_text_width_f
  nk_style_text* {.importc: "struct nk_style_text", nodecl.} = object
    ## Internal Nuklear type
    padding*: nk_vec2
  nk_cursor* {.importc: "struct nk_cursor", nodecl.} = object
    ## Internal Nuklear type
  nk_style* {.importc, nodecl.} = object
    ## Internal Nuklear type
    window*: nk_style_window
    button*: nk_style_button
    progress*: nk_style_progress
    font*: ptr nk_user_font
    text*: nk_style_text
    cursor_active*: nk_cursor
    cursors*: pointer
  nk_mouse_button* {.importc: "struct nk_mouse_button", nodecl.} = object
    ## Internal Nuklear type
    down*: nk_bool
    clicked*: cuint
    clicked_pos*: nk_vec2
  nk_mouse* {.importc: "struct nk_mouse", nodecl.} = object
    ## Internal Nuklear type
    delta*: nk_vec2
    buttons*: pointer
  nk_input* {.importc: "struct nk_input", nodecl.} = object
    ## Internal Nuklear type
    mouse*: nk_mouse
  nk_popup_buffer* {.importc: "struct nk_popup_buffer", nodecl.} = object
    ## Internal Nuklear type
    begin*, `end`*, parent*, last*: nk_size
    active*: nk_bool
  PanelType* {.size: sizeof(cint).} = enum
    ## The types of panels
    panelNone = 0,
    panelWindow = 1 shl 0,
    panelGroup = 1 shl 1,
    panelPopup = 1 shl 2,
    panelContextual = 1 shl 4,
    panelCombo = 1 shl 5,
    panelMenu = 1 shl 6,
    panelTooltip = 1 shl 7
  nk_command* {.importc: "struct nk_command", completeStruct.} = object
    ## Internal Nuklear type
    `type`*: nk_command_type
    next*: nk_size
    when defined(nkIncludeCommandUserData):
      userdata*: nk_handle ## Interna Nuklear data
  nk_command_scissor* {.importc: "struct nk_command_scissor".} = object
    ## Internal Nuklear type
    header*: nk_command
    x*, y*: cshort
    w*, h*: cushort
  nk_command_buffer* {.importc: "struct nk_command_buffer".} = object
    ## Internal Nuklear type
    begin*, `end`*, last*: nk_size
    clip*: nk_rect
    base*: ptr nk_buffer
    use_clipping*: cint
  nk_command_image* {.importc: "struct nk_command_image".} = object
    ## Internal Nuklear type
    header*: nk_command
    x*, y*: cshort
    w*, h*: cushort
    img*: nk_image
    col*: nk_color
  nk_command_rect* {.importc: "struct nk_command_rect".} = object
    ## Internal Nuklear type
    header*: nk_command
    rounding*, w*, h*, lineThickness*: cushort
    x*, y*: cshort
    color*: nk_color
  nk_command_rect_filled* {.importc: "struct nk_command_rect_filled".} = object
    ## Internal Nuklear type
    header*: nk_command
    rounding*, w*, h*: cushort
    x*, y*: cshort
    color*: nk_color
  nk_row_layout* {.importc: "struct nk_row_layout".} = object
    ## Internal Nuklear type
    index*, columns*, tree_depth*: cint
    ratio*, item_width*, item_height*, height*: cfloat
  nk_panel* {.importc: "struct nk_panel", nodecl.} = object
    ## Internal Nuklear type
    `type`*: PanelType
    clip*: nk_rect
    flags*: nk_flags
    bounds*: nk_rect
    border*, at_y*, at_x*, max_x*, header_height*, footer_height*: cfloat
    row*: nk_row_layout
    parent*: PNkPanel
    has_scrolling*: cuint
  nk_popup_state* {.importc: "struct nk_popup_state", nodecl.} = object
    ## Internal Nuklear type
    win*: ptr nk_window
    active*: nk_bool
    `type`*: PanelType
    name*: nk_hash
    buf*: nk_popup_buffer
  nk_edit_state* {.importc: "struct nk_edit_state", nodecl.} = object
    ## Internal Nuklear type
    active*: cint
  nk_property_state* {.importc: "struct nk_property_state", nodecl.} = object
    ## Internal Nuklear type
    active*: cint
  nk_window* {.importc: "struct nk_window", nodecl.} = object
    ## Internal Nuklear type
    layout*: PNkPanel
    popup*: nk_popup_state
    parent*: ptr nk_window
    bounds*: nk_rect
    seq*: uint
    flags*: nk_flags
    buffer*: nk_command_buffer
    edit*: nk_edit_state
    property*: nk_property_state
  nk_memory* {.importc: "struct nk_memory", nodecl.} = object
    ## Internal Nuklear type
    `ptr`*: ptr nk_size
    size*: nk_size
  nk_plugin_alloc* = proc (handle: nk_handle; old: pointer;
      size: nk_size): pointer
    ## Internal Nuklear type
  nk_plugin_free* = proc (handle: nk_handle; old: pointer): pointer
    ## Internal Nuklear type
  nk_allocator* {.importc: "struct nk_allocator", nodecl.} = object
    ## Internal Nuklear type
    alloc*: nk_plugin_alloc
    free*: nk_plugin_free
    userdata*: nk_handle
  nk_buffer* {.importc: "struct nk_buffer", nodecl.} = object
    ## Internal Nuklear type
    allocated*, needed*: nk_size
    memory*: nk_memory
    size*: nk_size
    `type`*: nk_allocation_type
    pool*: nk_allocator
    grow_factor*: cfloat
    calls*: nk_size
  nk_context* {.importc: "struct nk_context", nodecl.} = object
    ## Internal Nuklear type
    style*: nk_style
    input*: nk_input
    current*, active*: ptr nk_window
    seq*: uint
    memory*: nk_buffer
    when defined(nkIncludeCommandUserData):
      userdata*: nk_handle ## Interna Nuklear data
  nk_rect* {.importc: "struct nk_rect", nodecl.} = object
    ## Internal Nuklear type
    x*, y*, w*, h*: cfloat
  nk_text_edit* = object
    ## Internal Nuklear type
  nk_font* {.importc: "struct nk_font", nodecl.} = object
    ## Internal Nuklear type
    handle*: nk_user_font
  nk_font_atlas* {.importc: "struct nk_font_atlas", nodecl.} = object
    ## Internal Nuklear type
  nk_font_config* {.importc: "struct nk_font_config", nodecl.} = object
    ## Internal Nuklear type
    `range`*: pointer
  nk_image* {.importc: "struct nk_image", nodecl.} = object
    ## Internal Nuklear type
    handle*: nk_handle
    w*, h*: nk_ushort
    region*: array[4, nk_ushort]
  nk_text* {.importc: "struct nk_text", nodecl.} = object
    ## Internal Nuklear type
    padding*: nk_vec2
    background*, text*: nk_color
  PNkWindow* = ptr nk_window
    ## Pointer to nk_window structure
  PNkPanel* = ptr nk_panel
    ## Pointer to nk_panel structure
  ButtonsArray* = array[NK_BUTTON_MAX, nk_mouse_button]
    ## The array of mouse buttons
  CursorsArray* = array[NK_CURSOR_COUNT, nk_cursor]
    ## The array of mouse buttons

# ---------
# Constants
# ---------
const nkNullRect*: nk_rect = nk_rect(x: -8192.0, y: -8192.0, w: -8192.0, h: -8192.0)
    ## An empty rectangle

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
  ButtonStyle* = object
    ## Used to store information about a button's style.
    borderColor*: NimColor
    rounding*: float
    padding*: NimVec2
    imagePadding*: NimVec2
    touchPadding*: NimVec2
  ButtonStyleTypes* = enum
    ## The types of fields in style's settings for UI buttons
    normal, hover, active, borderColor, textBackground, textNormal, textHover,
      textActive, rounding, padding, border, imagePadding, touchPadding,
      colorFactorBackground, colorFactorText
  FloatStyleTypes* = enum
    ## The types of fields in style's settings with float values
    buttonRounding, popupBorder
  ColorStyleTypes* = enum
    ## The types of fields in style's settings for UI colors
    background
  StyleStyleTypes* = enum
    ## The types of fields in style's settings for UI colors
    progressbar
  WindowStyleTypes* = enum
    ## The types of fields in style's settings for windows
    spacing, padding
  PanelFlags* {.size: sizeof(cint).} = enum
    ## The settings for panels
    windowNoFlags = 0,
    windowBorder = 1 shl 0,
    windowMovable = 1 shl 1,
    windowScalable = 1 shl 2,
    windowClosable = 1 shl 3
    windowMinimizable = 1 shl 4,
    windowNoScrollbar = 1 shl 5,
    windowTitle = 1 shl 6,
    windowScaleLeft = 1 shl 9
    windowNoInput = 1 shl 10
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
      scrollbarCursorActiveColor, tabHeaderColor, knobColor, knobCursorColor,
      knobCursorHoverColor, knobCursorActiveColor, buttonTextColor,
      buttonHoverTextColor, buttonActiveTextColor, editTextColor,
      comboTextColor, tooltipColor, tooltipBorderColor, groupBorderColor,
      headerTextColor, groupTextColor, selectActiveTextColor, propertyTextColor,
      popupColor, popupBorderColor, progressbarColor, progressbarBorderColor,
      countColors
  StyleHeaderAlign* = enum
    ## The styles of the window's header
    headerLeft, headerRight
  ButtonBehavior* = enum
    ## The types of buttons behavior
    default, repeater
  PanelSet* {.size: sizeof(cint).} = enum
    ## The setting of panels
    panelSetNonBlock = panelContextual.int or panelCombo.int or panelMenu.int or
        panelTooltip.int,
    panelSetPopup = panelSetNonBlock.int or panelPopup.int,
    panelSetSub = panelSetPopup.int or panelGroup.int
  UserEvents* = enum
    ## The UI events caused by the user
    noEvent, quitEvent, sizeChangedEvent, keyEvent, mouseButtonEvent, anyEvent
{.pop ruleOn: "namedParams".}

# ----------
# Converters
# ----------
converter toBool*(x: nk_bool): bool =
  ## Converts Nuklear nk_bool enum to Nim bool
  x == nkTrue
converter toNkFlags*(x: TextAlignment): nk_flags =
  ## Converts Nuklear nk_text_alignment enum to Nuklear nk_flags type
  x.ord.cint
converter toNkFlags*(x: EditTypes): nk_flags =
  ## Converts EditTypes enum to Nuklear nk_flags type
  x.ord.cint
converter toCint*(x: bool): cint =
  ## Converts Nim bool type to Nim cint type
  if x: 1 else: 0

# -------------------
# Creating structures
# -------------------
proc new_nk_rect*(x, y, w, h: cfloat): nk_rect {.importc: "nk_rect", nodecl,
    raises: [], tags: [], contractual.}
  ## A binding to Nuklear's function. Internal use only
proc new_nk_vec2*(x, y: cfloat): nk_vec2 {.importc: "nk_vec2", nodecl, raises: [
    ], tags: [], contractual.}
  ## A binding to Nuklear's function. Internal use only
proc new_nk_font_config*(pixelHeight: cfloat): nk_font_config {.importc: "nk_font_config",
    nodecl, raises: [], tags: [], contractual.}
  ## A binding to Nuklear's function. Internal use only

