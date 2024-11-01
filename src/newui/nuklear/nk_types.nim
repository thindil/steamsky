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

## Provides types from nuklear library

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
  nk_ushort* = cushort

# ------------
# Enumerations
# ------------
type
  nk_style_header_align* = enum
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
  nk_image* {.importc: "struct nk_image", nodecl.} = object
    handle: nk_handle
    w, h: nk_ushort
    region: array[4, nk_ushort]

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

