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
  nk_anti_aliasing* = enum
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

# -------
# Objects
# -------
{.push ruleOff: "namedParams".}
type
  nk_color* {.importc: "struct nk_color".} = object
    ## Internal Nuklear type
    r*, g*, b*, a*: nk_byte
  nk_colorf* {.importc: "struct nk_colorf".} = object
    ## Internal Nuklear type
    r*, g*, b*, a*: cfloat
  nk_vec2* {.importc: "struct nk_vec2", completeStruct.} = object
    ## Internal Nuklear type
    x*, y*: cfloat
  nk_vec2i* {.importc: "struct nk_vec2i".} = object
    ## Internal Nuklear type
    x*, y*: cshort
  nk_handle* {.bycopy, union.} = object
    ## Internal Nuklear type
    `ptr`*: pointer
    id*: cint
  nk_image* {.importc: "struct nk_image", nodecl.} = object
    ## Internal Nuklear type
    handle*: nk_handle
    w*, h*: nk_ushort
    region*: array[4, nk_ushort]
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
    `type`*: StyleItemType
    data*: pointer
  nk_style_window_header* {.importc, nodecl.} = object
    ## Internal Nuklear type
    align*: StyleHeaderAlign
    padding*, label_padding*, spacing*: nk_vec2
    active*, hover*, normal*: nk_style_item
    label_active*, label_hover*, label_normal*: nk_color
    close_symbol*, minimize_symbol*, maximize_symbol*: SymbolType
    close_button*, minimize_button*, maximize_button*: nk_style_button
  nk_style_window* {.importc, nodecl.} = object
    ## Internal Nuklear type
    header*: nk_style_window_header
    fixed_background*: nk_style_item
    spacing*, scrollbar_size*, padding*, group_padding*, popup_padding*,
      contextual_padding*, combo_padding*, menu_padding*,
      tooltip_padding*: nk_vec2
    background*, group_text_color*: nk_color
    border*, combo_border*, contextual_border*, menu_border*, group_border*,
      tooltip_border*, popup_border*, rounding*: cfloat
  nk_rect* {.importc: "struct nk_rect", nodecl.} = object
    ## Internal Nuklear type
    x*, y*, w*, h*: cfloat
  nk_draw_command* {.importc: "struct nk_draw_command", nodecl.} = object
    ## Internal Nuklear type
    elem_count*: cuint
    clip_rect*: nk_rect
    texture*: nk_handle
    when defined(nkIncludeCommandUserData):
      userdata*: nk_handle
  nk_draw_f* = proc(b: ptr nk_command_buffer; userData: nk_handle) {.cdecl.}
    ## Internal Nuklear type
  nk_style_button* {.importc: "struct nk_style_button", nodecl.} = object
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
  nk_style_progress* {.importc: "struct nk_style_progress", nodecl.} = object
    cursor_normal*: nk_style_item
    ## Internal Nuklear type
  nk_text_width_f* = proc (arg1: nk_handle; h: cfloat; arg3: cstring;
      len: cint): cfloat {.cdecl.}
    ## Internal Nuklear type
  nk_user_font* {.importc: "struct nk_user_font", nodecl.} = object
    ## Internal Nuklear type
    userdata*: nk_handle
    height*: cfloat
    width*: nk_text_width_f
  PNkUserFont* = ptr nk_user_font
    ## Pointer to nk_user_font structure
  nk_style_text* {.importc: "struct nk_style_text", nodecl.} = object
    ## Internal Nuklear type
    padding*: nk_vec2
  nk_cursor* {.importc: "struct nk_cursor", nodecl.} = object
    ## Internal Nuklear type
  nk_style* {.importc: "struct nk_style", nodecl.} = object
    ## Internal Nuklear type
    window*: nk_style_window
    button*: nk_style_button
    progress*: nk_style_progress
    font*: PNkUserFont
    text*: nk_style_text
    cursor_active*: nk_cursor
    cursors*: pointer
  nk_mouse_button* {.importc: "struct nk_mouse_button",
      completeStruct.} = object
    ## Internal Nuklear type
    down*: nk_bool
    clicked*: cuint
    clicked_pos*: nk_vec2
  ButtonsArray* = array[Buttons.max, nk_mouse_button]
    ## The array of mouse buttons
  nk_mouse* {.importc: "struct nk_mouse", nodecl.} = object
    ## Internal Nuklear type
    delta*, pos*, prev*, scroll_delta*, : nk_vec2
    buttons*: pointer
    grab*, grabbed*, ungrab*: uint8
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
    `type`*: CommandType
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
  PNkCommandBuffer* = ptr nk_command_buffer
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
    rounding*, w*, h*, line_thickness*: cushort
    x*, y*: cshort
    color*: nk_color
  nk_command_rect_filled* {.importc: "struct nk_command_rect_filled".} = object
    ## Internal Nuklear type
    header*: nk_command
    rounding*, w*, h*: cushort
    x*, y*: cshort
    color*: nk_color
  nk_command_triangle* {.importc: "struct nk_command_triangle".} = object
    ## Internal Nuklear type
    header*: nk_command
    line_thickness*: cshort
    a*, b*, c*: nk_vec2i
    color*: nk_color
  nk_command_triangle_filled* {.importc: "struct nk_command_triangle_filled".} = object
    ## Internal Nuklear type
    header*: nk_command
    a*, b*, c*: nk_vec2i
    color*: nk_color
  nk_command_circle_filled* {.importc: "struct nk_command_circle_filled".} = object
    ## Internal Nuklear type
    header*: nk_command
    w*, h*: cushort
    x*, y*: cshort
    color*: nk_color
  nk_command_text* {.importc: "struct nk_command_text".} = object
    ## Internal Nuklear type
    x*, y*: cshort
    w*, h*: cushort
    background*, foreground*: nk_color
    font*: PNkUserFont
    height*: cfloat
    length*: cint
    `string`*: cstring
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
  nk_panel* {.importc: "struct nk_panel", nodecl.} = object
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
  nk_popup_state* {.importc: "struct nk_popup_state", nodecl.} = object
    ## Internal Nuklear type
    win*: PNkWindow
    active*: nk_bool
    `type`*: PanelType
    name*: nk_hash
    buf*: nk_popup_buffer
  nk_edit_state* {.importc: "struct nk_edit_state", nodecl.} = object
    ## Internal Nuklear type
    active*: cint
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
  nk_memory* {.importc: "struct nk_memory", nodecl.} = object
    ## Internal Nuklear type
    `ptr`*: ptr nk_size
    size*: nk_size
  nk_plugin_alloc* = proc (handle: nk_handle; old: pointer;
      size: nk_size): pointer {.cdecl.}
    ## Internal Nuklear type
  nk_plugin_free* = proc (handle: nk_handle; old: pointer) {.cdecl.}
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
    `type`*: AllocationType
    pool*: nk_allocator
    grow_factor*: cfloat
    calls*: nk_size
  nk_table* {.importc: "struct nk_table", nodecl.} = object
    ## Internal Nuklear type
    `seq`*, size*: cuint
    next*, prev*: ptr nk_table
  nk_page_data* {.bycopy, union.} = object
    ## Internal Nuklear type
    tbl*: nk_table
    pan*: nk_panel
    win*: nk_window
  nk_page_element* {.importc: "struct nk_page_element", nodecl.} = object
    data*: nk_page_data
    next*, prev*: pointer
    ## Internal Nuklear type
  nk_context* {.importc: "struct nk_context", nodecl.} = object
    ## Internal Nuklear type
    style*: nk_style
    input*: nk_input
    current*, active*: PNkWindow
    seq*: cuint
    memory*: nk_buffer
    use_pool*: bool
    freelist*: pointer
    when defined(nkIncludeCommandUserData):
      userdata*: nk_handle ## Interna Nuklear data
  nk_text_edit* = object
    ## Internal Nuklear type
  nk_font* {.importc: "struct nk_font", nodecl.} = object
    ## Internal Nuklear type
    handle*: PNkUserFont
  nk_font_atlas* {.importc: "struct nk_font_atlas", nodecl.} = object
    ## Internal Nuklear type
  nk_font_config* {.importc: "struct nk_font_config", nodecl.} = object
    ## Internal Nuklear type
    `range`*: pointer
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

