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
  StyleHeaderAlign* = enum
    ## The window's header alignment
    headerLeft, headerRight
  LayoutFormat* = enum
    ## The layout format
    dynamic, static
  TextAlign* = enum
    ## The alignment of a text
    textLeft = 0x01,
    textCentered = 0x02,
    textRight = 0x04,
    textTop = 0x08,
    textMiddle = 0x10,
    textBottom = 0x20
  TreeType* = enum
    ## The types of tree widget
    node, tab
  ChartType* = enum
    ## The types of charts
    lines, column, chartMax
  nk_bool* = enum
    ## Internal Nuklear type
    nkFalse, nkTrue
  Modify* = enum
    ## Modify states
    fixed, modifiable
  CollapseStates* = enum
    ## The states of a tree's content
    minimized, maximized
  SymbolType* = enum
    ## The types of symbolic icons
    none, x, underscore, circleSolid, circleOutline, rectSolid, rectOutline,
      triangleUp, triangleDown, triangleLeft, triangleRight, plus, minus,
      triangleUpOutline, triangleDownOutline, triangleLeftOutline,
      triangleRightOutline, max
  StyleItemType* = enum
    ## Style's item's types
    itemColor, itemImage, itemNineSlice
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
  AntiAliasing* = enum
    ## Antialiasing state
    antiAliasingOff, antiAliasingOn
  WindowFlags* = enum
    ## Flags related to windows
    windowDynamic = 1 shl 11
    windowRom = 1 shl 12
    windowHidden = 1 shl 13
    windowClosed = 1 shl 14
    windowMinimized = 1 shl 15
    windowRemoveRom = 1 shl 16
  CommandType* = enum
    ## Type of command
    commandNop, commandScissor, commandLine, commandCurve,
      commandRect, commandRectFilled, commandRectMultiColor,
      commandCircle, commandCircleFilled, commandArc,
      commandArcFilled, commandTriangle, commandTriangleFilled,
      commandPolygon, commandPolygonFilled, commandPolyline,
      commandText, commandImage, commandCustom
  BufferAllocationType* = enum
    ## Type of buffer's allocation
    bufferFront, bufferBack, bufferMax
  AllocationType* = enum
    ## Another type of buffer allocation
    bufferFixed, bufferDynamic
  Keys* = enum
    ## Special keys
    keyNone, keyShift, keyCtrl, keyDel, keyEnter,
      keyTab, keyBackspace, keyCopy, keyCut, keyPaste,
      keyUp, keyDown, keyLeft, keyRight,
      keyTextInsertMode, keyTextReplaceMode, keyTextResetMode,
      keyTextLineStart, keyTextLineEnd, keyTextStart,
      keyTextEnd, keyTextUndo, keyTextRedo,
      keyTextSelectAll, keyTextWordLeft, keyTextWordRight,
      keyScrollStart, keyScrollEnd, keyScrollDown,
      keyScrollUp, keyEscape, keyMax
  StyleCursor* = enum
    ## Types of cursor's styles
    cursorArrow, cursorText, cursorMove, cursorResizeVertical,
      cursorResizeHorizontal, cursorResizeTopLeftDownRight,
      cursorResizeTopRightDownLeft, cursorCount
  WidgetStates* = enum
    ## States of a widget
    widgetStateModified = 1 shl 1
    widgetStateInactive = 1 shl 2
    widgetStateEntered = 1 shl 3
    widgetStateHover = 1 shl 4
    widgetStateActived = 1 shl 5
    widgetStateLeft = 1 shl 6
    widgetStateHovered = widgetStateHover.int or
        widgetStateModified.int
    widgetStateActive = widgetStateActived.int or
        widgetStateModified.int
  Heading* = enum
    ## Heading diretions
    up, right, down, left
  ButtonBehavior* = enum
    ## The types of buttons behavior
    default, repeater
  DrawVertexLayoutAttribute* = enum
    ## The drawing vertex layout attributes
    vertexPosition, vertexColor, vertexTextCoord, vertexAttributeCount
  DrawVertexLayoutFormat* = enum
    ## The drawing vertext layout formates
    formatSChar, formatSShort, formatSInt, formatUChar, formatUShort,
      formatUInt, formatFloat, formatDouble, formatColorBegin, formatR16G15B16,
      formatR32B32, formatR8G8B8A8, formatB8G8R8A8, formatR16G15B16A16,
      formatR32G32B32A32, formtR32G32B32A32Float, formatR32G32B32A32Double,
      formatRGB32, formatRGBA32, formatCount
  FontCoordType* = enum
    ## The types of fonts coordinates
    coordUv, coordPixel

# ---------
# Constants
# ---------
const
  nkUtfInvalid*: nk_rune = 0xfffd
    ## An invalid UTF-8 rune
  nkUtfSize*: Positive = 4
    ## The number of bytes of UTF glyph
  nkUtfMask*: array[nkUtfSize + 1, nk_byte] = [0xc0, 0x80, 0xe0, 0xf0, 0xf8]
    ## The list of UTF mask bytes
  nkUtfByte*: array[nkUtfSize + 1, nk_byte] = [0x80, 0, 0xc0, 0xe0, 0xf0]
    ## The list of UTF bytes
  nkUtfMin*: array[nkUtfSize + 1, nk_uint] = [0, 0, 0x80, 0x800, 0x10000]
    ## The list of start UTF bytes
  nkUtfMax*: array[nkUtfSize + 1, nk_uint] = [0x10ffff, 0x7f, 0x7ff, 0xffff, 0x10ffff]
    ## The list of end UTF bytes
  nkWindowMaxName: Positive = 64
    ## The maximum lenght of a window's name
  nkMaxLayoutRowTemplateColumns*: Positive = 16
    ## The max amount of columns in row template
  nkChartMaxSlot*: Positive = 4
    ## The max amount of slot in charts
  nkMaxNumberBuffer*: Positive = 64
    ## The max amount of buffers
  nkTextEditUndoStateCount*: Positive = 99
    ## The max amount of text field undo records
  nkTextEditUndoCharCount*: Positive = 999
    ## The max length of text filed undo characters
  nkStyleItemStackSize*: Positive = 16
    ## The size of the stack of style items
  nkFloatStackSize*: Positive = 32
    ## The size of the stack of floats
  nkVectorStackSize*: Positive = 16
    ## The size of the stack of vectors
  nkFlagsStackSize*: Positive = 32
    ## The size of the stack of flags
  nkColorStackSize*: Positive = 32
    ## The size of the stack of colors

# -------
# Objects
# -------

{.push ruleOff: "namedParams".}
type
  nk_color* {.importc: "struct nk_color", completeStruct.} = object
    ## Internal Nuklear type
    r*, g*, b*, a*: nk_byte
  nk_colorf* {.importc: "struct nk_colorf", completeStruct.} = object
    ## Internal Nuklear type
    r*, g*, b*, a*: cfloat
  nk_vec2* {.importc: "struct nk_vec2", completeStruct.} = object
    ## Internal Nuklear type
    x*, y*: cfloat
  nk_vec2i* {.importc: "struct nk_vec2i", completeStruct.} = object
    ## Internal Nuklear type
    x*, y*: cshort
  nk_handle* {.bycopy, union.} = object
    ## Internal Nuklear type
    `ptr`*: pointer
    id*: cint
  nk_image* {.importc: "struct nk_image", completeStruct.} = object
    ## Internal Nuklear type
    handle*: nk_handle
    w*, h*: nk_ushort
    region*: array[4, nk_ushort]
  nk_nine_slice* {.importc: "struct nk_nine_slice", completeStruct.} = object
    ## Internal Nuklear type
    image*: nk_image
    l*, t*, r*, b*: nk_ushort
  nk_style_item_data* {.bycopy, union.} = object
    ## Internal Nuklear type
    color*: nk_color
    image*: nk_image
    slice*: nk_nine_slice
  nk_style_item* {.importc: "struct nk_style_item", completeStruct.} = object
    ## Internal Nuklear type
    `type`*: StyleItemType
    data*: pointer
  nk_draw_f* = proc(b: ptr nk_command_buffer; userData: nk_handle) {.cdecl.}
    ## Internal Nuklear type
  nk_style_button* {.importc: "struct nk_style_button",
      completeStruct.} = object
    ## Internal Nuklear type
    normal*, hover*, active*: nk_style_item
    border_color*, text_background*, text_normal*, text_hover*,
      text_active*: nk_color
    rounding*, border*, color_factor_background*, color_factor_text*,
      disabled_factor*: cfloat
    padding*, image_padding*, touch_padding*: nk_vec2
    alignment*: nk_flags
    # TODO: should be nk_handle, nk_draw_f
    userdata*: cint
    draw_begin*: cint
    draw_end*: cint
  nk_style_window_header* {.importc: "struct nk_style_window_header",
      completeStruct.} = object
    ## Internal Nuklear type
    align*: StyleHeaderAlign
    padding*, label_padding*, spacing*: nk_vec2
    active*, hover*, normal*: nk_style_item
    label_active*, label_hover*, label_normal*: nk_color
    close_symbol*, minimize_symbol*, maximize_symbol*: SymbolType
    close_button*, minimize_button*: nk_style_button
  nk_style_window* {.importc: "struct nk_style_window",
      completeStruct.} = object
    ## Internal Nuklear type
    header*: nk_style_window_header
    fixed_background*, scaler*: nk_style_item
    spacing*, scrollbar_size*, padding*, group_padding*, popup_padding*,
      contextual_padding*, combo_padding*, menu_padding*,
      tooltip_padding*, min_size*: nk_vec2
    background*, group_text_color*, border_color*, popup_border_color*,
      popup_background*, combo_border_color*, contextual_border_color*,
      menu_border_color*, group_border_color*, tooltip_border_color*,
      tooltip_background*, : nk_color
    border*, combo_border*, contextual_border*, menu_border*, group_border*,
      tooltip_border*, popup_border*, rounding*: cfloat
  nk_rect* {.importc: "struct nk_rect", completeStruct.} = object
    ## Internal Nuklear type
    x*, y*, w*, h*: cfloat
  nk_draw_command* {.importc: "struct nk_draw_command",
      completeStruct.} = object
    ## Internal Nuklear type
    elem_count*: cuint
    clip_rect*: nk_rect
    texture*: nk_handle
    when defined(nkIncludeCommandUserData):
      userdata*: nk_handle
  nk_style_progress* {.importc: "struct nk_style_progress",
      completeStruct.} = object
    ## Internal Nuklear type
    cursor_normal*, cursor_hover*, cursor_active*, normal*, hover*,
      active*: nk_style_item
    border_color*, cursor_border_color*: nk_color
    rounding*, border*, cursor_border*, cursor_rounding*, color_factor*,
      disabled_factor*: cfloat
    padding*: nk_vec2
    userdata*: nk_handle
    draw_begin*: ptr nk_draw_f
    draw_end*: ptr nk_draw_f
    ## Internal Nuklear type
  nk_text_width_f* = proc (arg1: nk_handle; h: cfloat; arg3: cstring;
      len: cint): cfloat {.cdecl.}
    ## Internal Nuklear type
  nk_user_font_glyph* {.importc: "struct nk_user_font_glyph",
      completeStruct.} = object
    ## Internal Nuklear type
    uv*: array[2, nk_vec2]
    offset*: nk_vec2
    width*, height*, xadvance*: cfloat
  nk_query_font_glyph_f* = proc(handle: nk_handle; fontHeight: cfloat;
      glyph: nk_user_font_glyph; codepoint, nextCodepoint: nk_rune) {.cdecl.}
    ## Internal Nuklear type
  nk_user_font* {.importc: "struct nk_user_font", completeStruct.} = object
    ## Internal Nuklear type
    userdata*: nk_handle
    height*: cfloat
    width*: nk_text_width_f
    when defined(nkIncludeVertexBufferOutput):
      query*: nk_query_font_glyph_f
      texture*: nk_handle
  PNkUserFont* = ptr nk_user_font
    ## Pointer to nk_user_font structure
  nk_style_text* {.importc: "struct nk_style_text", completeStruct.} = object
    ## Internal Nuklear type
    padding*: nk_vec2
    color*: nk_color
    color_factor*, disabled_factor*: cfloat
  nk_cursor* {.importc: "struct nk_cursor", completeStruct.} = object
    ## Internal Nuklear type
    img*: nk_image
    size*, offset*: nk_vec2
  PNkCursor* = ptr nk_cursor
    ## Pointer to nk_user_font structure
  nk_style_toggle* {.importc: "struct nk_style_toggle",
      completeStruct.} = object
    ## Internal Nuklear type
    normal*, hover*, active*, cursor_normal*, cursor_hover*: nk_style_item
    border_color*, text_normal*, text_hover*, text_active*,
      text_background*: nk_color
    text_alignment*: nk_flags
    padding*, touch_padding*: nk_vec2
    spacing*, border*, color_factor*, disabled_factor*: cfloat
  nk_style_selectable* {.importc: "struct nk_style_selectable",
      completeStruct.} = object
    ## Internal Nuklear type
    normal*, hover*, pressed*, normal_active*, hover_active*,
      pressed_active*: nk_style_item
    text_normal*, text_hover*, text_pressed*, text_normal_active*,
      text_hover_active*, text_pressed_active*, text_background*: nk_color
    text_alignment*: nk_flags
    rounding*, color_factor*, disabled_factor*: cfloat
    padding*, touch_padding, image_padding*: nk_vec2
    userdata*: nk_handle
    draw_begin*: ptr nk_draw_f
    draw_end*: ptr nk_draw_f
  nk_style_slider* {.importc: "struct nk_style_slider",
      completeStruct.} = object
    ## Internal Nuklear type
    normal*, hover*, active*, bar_normal*, bar_hover*, bar_active*,
      cursor_normal*, cursor_hover*, cursor_active*: nk_style_item
    border_color*, bar_filled*: nk_color
    border*, rounding*, bar_height*, color_factor*, disabled_factor*: cfloat
    padding*, spacing*, cursor_size*: nk_vec2
    show_buttons*: cint
    inc_button*, dec_button*: nk_style_button
    inc_symbol*, dec_symbol*: SymbolType
    userdata*: nk_handle
    draw_begin*: ptr nk_draw_f
    draw_end*: ptr nk_draw_f
  nk_style_knob* {.importc: "struct nk_style_knob", completeStruct.} = object
    ## Internal Nuklear type
    normal*, hover*, active*: nk_style_item
    border_color*, knob_normal*, knob_hover*, knob_active*, knob_border_color*,
      cursor_normal*, cursor_hover*, cursor_active*: nk_color
    border*, knob_border*, cursor_width*, color_factor*,
      disabled_factor*: cfloat
    padding*, spacing*: nk_vec2
  nk_style_scrollbar* {.importc: "struct nk_style_scrollbar",
      completeStruct.} = object
    ## Internal Nuklear type
    normal*, hover*, active*, cursor_normal*, cursor_hover*,
      cursor_active: nk_style_item
    border_color*, cursor_border_color*: nk_color
    border*, rounding*, border_cursor*, color_factor*, disabled_factor*: cfloat
    padding*: nk_vec2
    show_buttons*: cint
    inc_button*, dec_button*: nk_style_button
    inc_symbol, dec_symbol*: SymbolType
    userdata*: nk_handle
    draw_begin*: ptr nk_draw_f
    draw_end*: ptr nk_draw_f
  nk_style_edit* {.importc: "struct nk_style_edit", completeStruct.} = object
    ## Internal Nuklear type
    normal*, hover*, active*: nk_style_item
    border_color*, cursor_normal*, cursor_hover*, cursor_text_normal*,
      cursor_text_hover*, text_normal*, text_hover*, text_active*,
      selected_normal*, selected_hover*, selected_text_normal*,
      selected_text_hover*: nk_color
    scrollbar*: nk_style_scrollbar
    border*, rounding*, cursor_size*, row_padding*, color_factor*,
      disabled_factor*: cfloat
    scrollbar_size*, padding*: nk_vec2
  nk_style_chart* {.importc: "struct nk_style_chart", completeStruct.} = object
    ## Internal Nuklear type
    background*: nk_style_item
    border_color*, selected_color*, color*: nk_color
    border*, rounding*, color_factor*, disabled_factor*: cfloat
    padding*: nk_vec2
    show_markers*: nk_bool
  nk_style_tab* {.importc: "struct nk_style_tab", completeStruct.} = object
    ## Internal Nuklear type
    background*: nk_style_item
    border_color*, text*: nk_color
    tab_maximize_button*, tab_minimize_button*, node_maximize_button*,
      node_minimize_button*: nk_style_button
    sym_minimize*, sym_maximize*: SymbolType
    border*, rounding*, indent*, color_factor*, disabled_factor*: cfloat
    padding*, spacing*: nk_vec2
  nk_style_combo* {.importc: "struct nk_style_combo", completeStruct.} = object
    ## Internal Nuklear type
    normal*, hover*, active*: nk_style_item
    border_color*, label_normal*, label_hover*, label_active*, symbol_normal*,
      symbol_hover*, symbol_active*: nk_color
    button*: nk_style_button
    sym_normal*, sym_hover*, sym_active*: SymbolType
    border*, rounding*, color_factor*, disabled_factor*: cfloat
    content_padding*, button_padding*, spacing*: nk_vec2
  nk_style* {.importc: "struct nk_style", completeStruct.} = object
    ## Internal Nuklear type
    window*: nk_style_window
    button*, contextual_button*, menu_button*: nk_style_button
    progress*, property*: nk_style_progress
    font*: PNkUserFont
    text*: nk_style_text
    cursor_active*, cursor_last*: PNkCursor
    cursors*: pointer
    cursor_visible*: cint
    option*, checkbox*: nk_style_toggle
    selectable*: nk_style_selectable
    slider*: nk_style_slider
    knob*: nk_style_knob
    edit*: nk_style_edit
    chart*: nk_style_chart
    scrollh*, scrollv*: nk_style_scrollbar
    tab*: nk_style_tab
    combo*: nk_style_combo
  nk_mouse_button* {.importc: "struct nk_mouse_button",
      completeStruct.} = object
    ## Internal Nuklear type
    down*: nk_bool
    clicked*: cuint
    clicked_pos*: nk_vec2
  ButtonsArray* = array[Buttons.max, nk_mouse_button]
    ## The array of mouse buttons
  nk_mouse* {.importc: "struct nk_mouse", completeStruct.} = object
    ## Internal Nuklear type
    delta*, pos*, prev*, scroll_delta*, : nk_vec2
    buttons*: pointer
    grab*, grabbed*, ungrab*: uint8
    when defined(nkButtonTriggerOnRelease):
      down_pos*: nk_vec2
  nk_key* {.importc: "struct nk_key", completeStruct.} = object
    ## Internal Nuklear type
    down*: nk_bool
    clicked*: cuint
  KeysArray* = array[keyMax, nk_key]
    ## The array of keyboard keys
  nk_keyboard* {.importc: "struct nk_keyboard", completeStruct.} = object
    ## Internal Nuklear type
    keys*, text*: pointer
    text_len*: cint
  nk_input* {.importc: "struct nk_input", completeStruct.} = object
    ## Internal Nuklear type
    mouse*: nk_mouse
    keyboard*: nk_keyboard
  nk_popup_buffer* {.importc: "struct nk_popup_buffer",
      completeStruct.} = object
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
    `type`*: CommandType
    next*: nk_size
    when defined(nkIncludeCommandUserData):
      userdata*: nk_handle ## Interna Nuklear data
  nk_command_scissor* {.importc: "struct nk_command_scissor",
      completeStruct.} = object
    ## Internal Nuklear type
    header*: nk_command
    x*, y*: cshort
    w*, h*: cushort
  PNkBuffer* = ptr nk_buffer
    ## Pointer to nk_buffer type
  nk_command_buffer* {.importc: "struct nk_command_buffer",
      completeStruct.} = object
    ## Internal Nuklear type
    begin*, `end`*, last*: nk_size
    clip*: nk_rect
    base*: PNkBuffer
    use_clipping*: cint
    userdata*: nk_handle
  PNkCommandBuffer* = ptr nk_command_buffer
  nk_command_image* {.importc: "struct nk_command_image",
      completeStruct.} = object
    ## Internal Nuklear type
    header*: nk_command
    x*, y*: cshort
    w*, h*: cushort
    img*: nk_image
    col*: nk_color
  nk_command_rect* {.importc: "struct nk_command_rect",
      completeStruct.} = object
    ## Internal Nuklear type
    header*: nk_command
    rounding*, w*, h*, line_thickness*: cushort
    x*, y*: cshort
    color*: nk_color
  nk_command_rect_filled* {.importc: "struct nk_command_rect_filled",
      completeStruct.} = object
    ## Internal Nuklear type
    header*: nk_command
    rounding*, w*, h*: cushort
    x*, y*: cshort
    color*: nk_color
  nk_command_triangle* {.importc: "struct nk_command_triangle",
      completeStruct.} = object
    ## Internal Nuklear type
    header*: nk_command
    line_thickness*: cshort
    a*, b*, c*: nk_vec2i
    color*: nk_color
  nk_command_triangle_filled* {.importc: "struct nk_command_triangle_filled",
      completeStruct.} = object
    ## Internal Nuklear type
    header*: nk_command
    a*, b*, c*: nk_vec2i
    color*: nk_color
  nk_command_circle_filled* {.importc: "struct nk_command_circle_filled",
      completeStruct.} = object
    ## Internal Nuklear type
    header*: nk_command
    w*, h*: cushort
    x*, y*: cshort
    color*: nk_color
  nk_command_text* {.importc: "struct nk_command_text",
      completeStruct.} = object
    ## Internal Nuklear type
    x*, y*: cshort
    w*, h*: cushort
    background*, foreground*: nk_color
    font*: PNkUserFont
    height*: cfloat
    length*: cint
    `string`*: cstring
    header*: nk_command
  nk_row_layout* {.importc: "struct nk_row_layout", completeStruct.} = object
    ## Internal Nuklear type
    index*, columns*, tree_depth*: cint
    ratio*, item_width*, item_height*, height*, min_height*, item_offset*,
      filled*: cfloat
    templates: array[nkMaxLayoutRowTemplateColumns, cfloat]
  nk_scroll* {.importc: "struct nk_scroll", completeStruct.} = object
    ## Internal Nuklear type
    x*, y*: cuint
  nk_menu_state* {.importc: "struct nk_menu_state", completeStruct.} = object
    x*, y*, w*, h*: cfloat
    offset*: nk_scroll
  nk_chart_slot* {.importc: "struct nk_chart_slot", completeStruct.} = object
    `type`*: ChartType
    color*, highlight*: nk_color
    min*, max*, range*: cfloat
    count*, index*: cint
    last*: nk_vec2
    show_markers: nk_bool
    ## Internal Nuklear type
  nk_chart* {.importc: "struct nk_chart", completeStruct.} = object
    ## Internal Nuklear type
    slot*: cint
    x, y, w, h: cfloat
    slots: pointer
  nk_panel* {.importc: "struct nk_panel", completeStruct.} = object
    ## Internal Nuklear type
    `type`*: PanelType
    clip*, bounds*: nk_rect
    flags*: nk_flags
    border*, at_y*, at_x*, max_x*, header_height*, footer_height*: cfloat
    row*: nk_row_layout
    parent*: PNkPanel
    has_scrolling*, offset_x*, offset_y*: cuint
    menu*: nk_menu_state
    chart*: nk_chart
    buffer*: PNkCommandBuffer
  PNkPanel* = ptr nk_panel
    ## Pointer to nk_panel structure
  nk_popup_state* {.importc: "struct nk_popup_state", completeStruct.} = object
    ## Internal Nuklear type
    win*: PNkWindow
    active*: nk_bool
    `type`*: PanelType
    name*: nk_hash
    buf*: nk_popup_buffer
    combo_count*, con_count*, col_old*, active_con*: cuint
    header*: nk_rect
  nk_edit_state* {.importc: "struct nk_edit_state", completeStruct.} = object
    ## Internal Nuklear type
    active*, prev*, cursor*, sel_start*, sel_end*: cint
    name*: nk_hash
    seq*, old*: cuint
    scrollbar*: nk_scroll
    mode*, single_line*: uint8
  nk_property_state* {.importc: "struct nk_property_state",
      completeStruct.} = object
    ## Internal Nuklear type
    active*, prev*, length*, cursor*, select_start*, select_end*, state*: cint
    buffer*: array[nkMaxNumberBuffer, char]
    name*: nk_hash
    seq*, old*: cuint
  nk_window* {.importc: "struct nk_window", completeStruct.} = object
    ## Internal Nuklear type
    layout*: PNkPanel
    popup*: nk_popup_state
    parent*, next*, prev*: PNkWindow
    bounds*: nk_rect
    seq*, scrolled*, table_count: cuint
    flags*: nk_flags
    buffer*: nk_command_buffer
    edit*: nk_edit_state
    property*: nk_property_state
    scrollbar*: nk_scroll
    name*: nk_hash
    name_string*: array[nkWindowMaxName, char]
    scrollbar_hiding_timer*: cfloat
    widgets_disabled*: nk_bool
    tables*: ptr nk_table
  PNkWindow* = ptr nk_window
    ## Pointer to nk_window structure
  nk_buffer_marker* = object
    ## Internal Nuklear type
    active*: nk_bool
    offset*: nk_size
  nk_memory* {.importc: "struct nk_memory", completeStruct.} = object
    ## Internal Nuklear type
    `ptr`*: ptr nk_size
    size*: nk_size
  nk_plugin_alloc* = proc (handle: nk_handle; old: pointer;
      size: nk_size): pointer {.cdecl.}
    ## Internal Nuklear type
  nk_plugin_free* = proc (handle: nk_handle; old: pointer) {.cdecl.}
    ## Internal Nuklear type
  nk_allocator* {.importc: "struct nk_allocator", completeStruct.} = object
    ## Internal Nuklear type
    alloc*: nk_plugin_alloc
    free*: nk_plugin_free
    userdata*: nk_handle
  nk_buffer* {.importc: "struct nk_buffer", completeStruct.} = object
    ## Internal Nuklear type
    allocated*, needed*: nk_size
    memory*: nk_memory
    size*: nk_size
    `type`*: AllocationType
    pool*: nk_allocator
    grow_factor*: cfloat
    calls*: nk_size
    marker*: array[bufferMax, nk_buffer_marker]
  nk_table* {.importc: "struct nk_table", completeStruct.} = object
    ## Internal Nuklear type
    `seq`*, size*: cuint
    keys*, values*: pointer
    next*, prev*: ptr nk_table
  nk_page_data* {.bycopy, union.} = object
    ## Internal Nuklear type
    tbl*: nk_table
    pan*: nk_panel
    win*: nk_window
  nk_page_element* {.importc: "struct nk_page_element",
      completeStruct.} = object
    ## Internal Nuklear type
    data*: nk_page_data
    next*, prev*: pointer
  nk_str* {.importc: "struct nk_str", completeStruct.} = object
    buffer*: nk_buffer
    len*: cint
  nk_plugin_filter* = proc (text: nk_text_edit; unicode: nk_rune) {.cdecl.}
  nk_text_undo_record* {.importc: "struct nk_text_undo_record",
      completeStruct.} = object
    ## Internal Nuklear type
    where*: cint
    insert_length*, delete_length*, char_storage*: cshort
  nk_text_undo_state* {.importc: "struct nk_text_undo_state",
      completeStruct.} = object
    ## Internal Nuklear type
    undo_rec*: pointer
    undo_point*, redo_point*, undo_char_point*, redo_char_point*: cshort
  nk_text_edit* {.importc: "struct nk_text_edit", completeStruct.} = object
    ## Internal Nuklear type
    clip*: nk_clipboard
    string*: nk_str
    filter*: nk_plugin_filter
    scrollbar*: nk_vec2
    cursor*, select_start*, select_end*: cint
    mode*, cursor_at_end_of_line*, initialized*, has_preferred_x*, single_line*,
      active*, padding1*: uint8
    preferred_x*: cfloat
    undo*: nk_text_undo_state
  nk_plugin_paste* = proc (handle: nk_handle; edit: ptr nk_text_edit) {.cdecl.}
    ## Internal Nuklear type
  nk_plugin_copy* = proc (handle: nk_handle; text: cstring; len: cint) {.cdecl.}
    ## Internal Nuklear type
  nk_clipboard* {.importc: "struct nk_clipboard", completeStruct.} = object
    ## Internal Nuklear type
    userdata*: nk_handle
    copy*: nk_plugin_copy
    paste*: nk_plugin_paste
  nk_config_stack_style_item_element* {.importc: "struct nk_config_stack_style_item_element",
      completeStruct.} = object
    ## Internal Nuklear type
    address*: ptr nk_style_item
    old_value: nk_style_item
  nk_config_stack_float_element* {.importc: "struct nk_config_stack_float_element",
      completeStruct.} = object
    ## Internal Nuklear type
    address*: ptr cfloat
    old_value: cfloat
  nk_config_stack_vec2_element* {.importc: "struct nk_config_stack_vec2_element",
      completeStruct.} = object
    ## Internal Nuklear type
    address*: ptr nk_vec2
    old_value: nk_vec2
  nk_config_stack_flags_element* {.importc: "struct nk_config_stack_flags_element",
      completeStruct.} = object
    ## Internal Nuklear type
    address*: ptr nk_flags
    old_value: nk_flags
  nk_config_stack_color_element* {.importc: "struct nk_config_stack_color_element",
      completeStruct.} = object
    ## Internal Nuklear type
    address*: ptr nk_color
    old_value: nk_color
  nk_config_stack_user_font_element* {.importc: "struct nk_config_stack_user_font_element",
      completeStruct.} = object
    ## Internal Nuklear type
    address*: ptr nk_user_font
    old_value: nk_user_font
  nk_config_stack_button_behavior_element * {.importc: "struct nk_config_stack_button_behavior_element",
      completeStruct.} = object
    ## Internal Nuklear type
    address*: ptr ButtonBehavior
    old_value: ButtonBehavior
  nk_config_stack_style_item* {.importc: "struct nk_config_stack_style_item",
      completeStruct.} = object
    ## Internal Nuklear type
    head*: cint
    elements*: pointer
  nk_config_stack_float* {.importc: "struct nk_config_stack_float",
      completeStruct.} = object
    ## Internal Nuklear type
    head*: cint
    elements*: pointer
  nk_config_stack_vec2* {.importc: "struct nk_config_stack_vec2",
      completeStruct.} = object
    ## Internal Nuklear type
    head*: cint
    elements*: pointer
  nk_config_stack_flags* {.importc: "struct nk_config_stack_flags",
      completeStruct.} = object
    ## Internal Nuklear type
    head*: cint
    elements*: pointer
  nk_config_stack_color* {.importc: "struct nk_config_stack_color",
      completeStruct.} = object
    ## Internal Nuklear type
    head*: cint
    elements*: pointer
  nk_config_stack_user_font* {.importc: "struct nk_config_stack_user_font",
      completeStruct.} = object
    ## Internal Nuklear type
    head*: cint
    elements*: pointer
  nk_config_stack_button_behavior* {.importc: "struct nk_config_stack_button_behavior",
      completeStruct.} = object
    ## Internal Nuklear type
    head*: cint
    elements*: pointer
  nk_configuration_stacks* {.importc: "struct nk_configuration_stacks",
      completeStruct.} = object
    ## Internal Nuklear type
    style_items*: nk_config_stack_style_item
    floats*: nk_config_stack_float
    vectors*: nk_config_stack_vec2
    flags*: nk_config_stack_flags
    colors*: nk_config_stack_color
    fonts*: nk_config_stack_user_font
    button_behaviors*: nk_config_stack_button_behavior
  nk_draw_null_texture* {.importc: "struct nk_draw_null_texture",
      completeStruct.} = object
    texture*: nk_handle
    uv*: nk_vec2
  nk_draw_vertex_layout_element* {.importc: "struct nk_draw_vertex_layout_element",
      completeStruct.} = object
    attribute*: DrawVertexLayoutAttribute
    format*: DrawVertexLayoutFormat
    offset*: nk_size
  nk_convert_config* {.importc: "struct nk_convert_config",
      completeStruct.} = object
    ## Internal Nuklear type
    global_alpha*: cfloat
    line_AA*, shape_AA*: AntiAliasing
    circle_segment_count*, arc_segment_count*, curve_segment_count*: cuint
    tex_null*: nk_draw_null_texture
    vertex_layout*: nk_draw_vertex_layout_element
    vertex_size*, vertex_alignment*: nk_size
  nk_draw_list* {.importc: "struct nk_draw_list", completeStruct.} = object
    ## Internal Nuklear type
    clip_rect*: nk_rect
    circle_vtx: array[12, nk_vec2]
    config*: nk_convert_config
    buffer*, vertices*, elements*: PNkBuffer
    element_count*, vertex_count*, cmd_count*, path_count*, path_offset*: cuint
    cmd_offset*: nk_size
    line_AA, shape_AA: AntiAliasing
    when defined(nkIncludeCommandUserData):
      userdata*: nk_handle
  nk_page* {.importc: "struct nk_pool", completeStruct.} = object
    ## Internal Nuklear type
    size*: cuint
    next*: ptr nk_page
    win*: pointer
  nk_pool* {.importc: "struct nk_pool", completeStruct.} = object
    ## Internal Nuklear type
    alloc: nk_allocator
    `type`: AllocationType
    page_count*, capacity*: cuint
    pages*: ptr nk_page
    freelist*: pointer
    size*, cap*: nk_size
  nk_context* {.importc: "struct nk_context", completeStruct.} = object
    ## Internal Nuklear type
    style*: nk_style
    input*: nk_input
    begin*, `end`*, current*, active*: PNkWindow
    seq*, count*: cuint
    memory*: nk_buffer
    use_pool*: bool
    freelist*: pointer
    clip*: nk_clipboard
    last_widget_state*: nk_flags
    button_behavior*: ButtonBehavior
    stacks*: nk_configuration_stacks
    when defined(nkIncludeCommandUserData):
      userdata*: nk_handle ## Interna Nuklear data
    when defined(nkIncludeVertexBufferOutput):
      draw_list*: nk_draw_list
    text_edit*: nk_text_edit
    overlay*: nk_command_buffer
    build*: cint
    pool*: nk_pool
  nk_font_glyph* {.importc: "struct nk_font_glyph", completeStruct.} = object
    codepoint*: nk_rune
    xadvance, x0, y0, x1, y1, w, h, u0, v0, u1, v1: cfloat
  nk_font* {.importc: "struct nk_font", completeStruct.} = object
    ## Internal Nuklear type
    handle*: PNkUserFont
    next*: ptr nk_font
    scale*: cfloat
    config*: ptr nk_font_config
    glyphs*, fallback*: ptr nk_font_glyph
    fallback_codepoint*: nk_rune
    texture*: nk_handle
  nk_font_atlas* {.importc: "struct nk_font_atlas", nodecl.} = object
    ## Internal Nuklear type
  nk_baked_font* {.importc: "struct nk_baked_font", completeStruct.} = object
    ## Internal Nuklear type
    height*: cfloat
    glyph_offset*, glyph_count*: nk_rune
    ranges*: pointer
  nk_font_config* {.importc: "struct nk_font_config".} = object
    ## Internal Nuklear type
    `range`*, ttf_blob: pointer
    next*, n*, p*: ptr nk_font_config
    ttf_size*: nk_size
    ttf_data_owned_by_atlas*, merge_mode*, pixel_snap*, oversample_v*,
      oversample_h*: uint8
    padding*: array[3, uint8]
    size*: cfloat
    coord_type*: FontCoordType
    spacing*: nk_vec2
    font*: ptr nk_baked_font
    fallback_glyph*: nk_rune
  nk_text* {.importc: "struct nk_text", nodecl.} = object
    ## Internal Nuklear type
    padding*: nk_vec2
    background*, text*: nk_color
  CursorsArray* = array[cursorCount, nk_cursor]
    ## The array of mouse buttons

{.push ruleOff: "namedParams".}
template `+`*[T](p: ptr T; off: nk_size): ptr T =
  ## Pointer artihmetic, adding
  ##
  ## * p   - the pointer to modify
  ## * off - the value to add to the pointer
  ##
  ## Returns the new pointer moved by off.
  cast[ptr type(p[])](cast[nk_size](p) +% off * p[].sizeof)
{.pop ruleOn: "namedParams".}

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
    borderColor*, textNormal*, textHover*: NimColor
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
    left = textMiddle.int or textLeft.int,
    centered = textMiddle.int or textCentered.int,
    right = textMiddle.int or textRight.int
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
  PanelSet* {.size: sizeof(cint).} = enum
    ## The setting of panels
    panelSetNonBlock = panelContextual.int or panelCombo.int or panelMenu.int or
        panelTooltip.int,
    panelSetPopup = panelSetNonBlock.int or panelPopup.int,
    panelSetSub = panelSetPopup.int or panelGroup.int
  UserEvents* = enum
    ## The UI events caused by the user
    noEvent, quitEvent, sizeChangedEvent, keyEvent, mouseButtonEvent, anyEvent
  ShowStates* = enum
    ## When to change the state of a window
    hidden, shown
  # TODO: test code
  MouseButton* = object
    ## Used to store information about a mouse button.
    down*, clicked*: bool
    clickedPos*: NimVec2
  Mouse* = object
    ## Used to store information about a mouse
    delta*, pos*, prev*, scrollDelta*, : NimVec2
    buttons*: array[Buttons.max, MouseButton]
    grab*, grabbed*, ungrab*: bool
  Input* = object
    ## Used to store information about the user's input
    mouse*: Mouse
{.pop ruleOn: "namedParams".}

# ---------
# Constants
# ---------
const
  nkNullRect*: NimRect = NimRect(x: -8192.0, y: -8192.0, w: -8192.0, h: -8192.0)
    ## An empty rectangle

# ----------
# Converters
# ----------
converter toBool*(x: nk_bool): bool =
  ## Converts Nuklear nk_bool enum to Nim bool
  x == nkTrue
converter toNkFlags*(x: TextAlignment): nk_flags =
  ## Converts Nuklear TextAlignment enum to Nuklear nk_flags type
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

