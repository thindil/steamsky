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
import contracts, nimalyzer
import nk_types, nk_context, nk_tooltip, nk_widget
export nk_types, nk_context, nk_tooltip, nk_widget

## Provides code for Nuklear binding

# -------
# Objects
# -------
type PImage* = pointer

# ---------------------
# Procedures parameters
# ---------------------
using ctx: PContext

# -------------------
# Creating structures
# -------------------
proc new_nk_rect(x, y, w, h: cfloat): nk_rect {.importc: "nk_rect", nodecl,
    raises: [], tags: [], contractual.}
proc new_nk_vec2(x, y: cfloat): nk_vec2 {.importc: "nk_vec2", nodecl, raises: [
    ], tags: [], contractual.}
proc new_nk_font_config*(pixelHeight: cfloat): nk_font_config {.importc: "nk_font_config",
    nodecl, raises: [], tags: [], contractual.}

# -----
# Input
# -----
proc nk_input_begin*(ctx) {.importc, nodecl, raises: [], tags: [], contractual.}
proc nk_input_end*(ctx) {.importc, nodecl, raises: [], tags: [], contractual.}

# -------
# General
# -------
proc nk_end(ctx) {.importc, cdecl, raises: [], tags: [], contractual.}

# ----
# Text
# ----
proc nk_labelf(ctx; flags: nk_flags; fmt: cstring) {.importc,
    varargs, cdecl, raises: [], tags: [], contractual.}

# -------
# Layouts
# -------
proc nk_layout_row_end(ctx) {.importc, cdecl, raises: [], tags: [], contractual.}
proc nk_layout_row_begin(ctx; fmt: nk_layout_format;
    rowHeight: cfloat; cols: cint) {.importc, cdecl, raises: [], tags: [], contractual.}
proc nk_layout_row_push(ctx; width: cfloat) {.importc, cdecl, raises: [],
    tags: [], contractual.}
proc nk_layout_row(ctx; fmt: nk_layout_format; height: cfloat;
    cols: cint; ratio: pointer) {.importc, nodecl, raises: [], tags: [], contractual.}
proc nk_layout_space_begin(ctx; fmt: nk_layout_format;
    height: cfloat; widgetCount: cint) {.importc, cdecl, raises: [], tags: [], contractual.}
proc nk_layout_space_end(ctx) {.importc, cdecl, raises: [], tags: [], contractual.}
proc nk_layout_row_template_begin(ctx; height: cfloat) {.importc, cdecl,
    raises: [], tags: [], contractual.}
proc nk_layout_row_template_end(ctx) {.importc, cdecl, raises: [], tags: [], contractual.}

# -----
# Menus
# -----
proc nk_menubar_begin(ctx) {.importc, cdecl, raises: [], tags: [], contractual.}
proc nk_menubar_end(ctx) {.importc, cdecl, raises: [], tags: [], contractual.}
proc nk_menu_end(ctx) {.importc, cdecl, raises: [], tags: [], contractual.}
proc nk_menu_item_label(ctx; text: cstring;
    aligmnent: nk_flags): nk_bool {.importc, cdecl, raises: [], tags: [], contractual.}

# ------
# Charts
# ------
proc nk_chart_begin(ctx; ctype: ChartType; num: cint; min,
    max: cfloat): nk_bool {.importc, cdecl, raises: [], tags: [], contractual.}
proc nk_chart_end(ctx) {.importc, cdecl, raises: [], tags: [], contractual.}

# ------
# Popups
# ------
proc nk_popup_end(ctx) {.importc, nodecl, raises: [], tags: [], contractual.}

# -----
# Trees
# -----
proc nk_tree_state_push(ctx; ttype: TreeType;
    ctitle: cstring; cstate: var CollapseStates): nk_bool {.importc, cdecl,
        raises: [], tags: [], contractual.}
proc nk_tree_pop(ctx) {.importc, cdecl, raises: [], tags: [], contractual.}
proc nk_tree_push_hashed(ctx; ttype: TreeType;
    ctitle: cstring; cstate: CollapseStates; chash: cstring; len,
    id: cint): nk_bool {.importc, cdecl, raises: [], tags: [], contractual.}
proc nk_tree_element_push_hashed(ctx; ttype: TreeType;
    ctitle: cstring; cstate: CollapseStates; cselected: var nk_bool;
    chash: cstring; len, sed: cint): nk_bool {.importc, cdecl, raises: [],
        tags: [], contractual.}
proc nk_tree_element_pop(ctx) {.importc, cdecl, raises: [], tags: [], contractual.}

# -------
# Buttons
# -------
proc nk_button_label(ctx; title: cstring): nk_bool {.importc, cdecl, raises: [],
    tags: [], contractual.}
proc nk_button_symbol(ctx; symbol: SymbolType): nk_bool {.importc, cdecl,
    raises: [], tags: [], contractual.}
proc nk_button_symbol_label(ctx; symbol: SymbolType; label: cstring;
    align: nk_flags): nk_bool {.importc, cdecl, raises: [], tags: [], contractual.}

# -----
# Style
# -----
proc nk_style_item_color(col: nk_color): nk_style_item {.importc, cdecl,
    raises: [], tags: [], contractual.}
proc nk_style_set_font*(ctx; font: ptr nk_user_font) {.importc, nodecl,
    raises: [], tags: [], contractual.}

# ------
# Combos
# ------
proc nk_combo_begin_color(ctx; color: nk_color;
    size: nk_vec2): nk_bool {.importc, nodecl, raises: [], tags: [], contractual.}
proc nk_combo_end(ctx) {.importc, cdecl, raises: [], tags: [], contractual.}

# ------
# Colors
# ------
proc nk_rgb*(r, g, b: cint): nk_color {.importc, nodecl, raises: [], tags: [], contractual.}
proc nk_rgb_cf*(c: nk_colorf): nk_color {.importc, nodecl, raises: [], tags: [], contractual.}
proc nk_rgba*(r, g, b, a: cint): nk_color {.importc, nodecl, raises: [], tags: [], contractual.}

# -------
# Filters
# -------
proc nk_filter_default*(box: ptr nk_text_edit;
    unicode: nk_rune): nk_bool {.importc, cdecl, raises: [], tags: [], contractual.}
proc nk_filter_decimal*(box: ptr nk_text_edit;
    unicode: nk_rune): nk_bool {.importc, cdecl, raises: [], tags: [], contractual.}
proc nk_filter_float*(box: ptr nk_text_edit;
    unicode: nk_rune): nk_bool {.importc, cdecl, raises: [], tags: [], contractual.}
proc nk_filter_hex*(box: ptr nk_text_edit;
    unicode: nk_rune): nk_bool {.importc, cdecl, raises: [], tags: [], contractual.}
proc nk_filter_oct*(box: ptr nk_text_edit;
    unicode: nk_rune): nk_bool {.importc, cdecl, raises: [], tags: [], contractual.}
proc nk_filter_binary*(box: ptr nk_text_edit;
    unicode: nk_rune): nk_bool {.importc, cdecl, raises: [], tags: [], contractual.}
proc nk_filter_ascii*(box: ptr nk_text_edit;
    unicode: nk_rune): nk_bool {.importc, cdecl, raises: [], tags: [], contractual.}

# ----------
# Contextual
# ----------
proc nk_contextual_end(ctx) {.importc, cdecl, raises: [], tags: [], contractual.}
proc nk_contextual_item_label(ctx; label: cstring;
    align: nk_flags): nk_bool {.importc, cdecl, raises: [], tags: [], contractual.}

# ------
# Groups
# ------
proc nk_group_begin(ctx; title: cstring;
    flags: nk_flags): nk_bool {.importc, cdecl, raises: [], tags: [], contractual.}
proc nk_group_end(ctx) {.importc, cdecl, raises: [], tags: [], contractual.}

# -----
# Fonts
# -----
proc nk_font_atlas_add_default*(atlas: ptr nk_font_atlas; height: cfloat;
    config: ptr nk_font_config): ptr nk_font {.importc, nodecl, raises: [],
        tags: [], contractual.}
proc nk_font_atlas_add_from_file*(atlas: ptr nk_font_atlas; filePath: cstring;
    height: cfloat;  config: ptr nk_font_config): ptr nk_font {.importc,
        nodecl, raises: [], tags: [], contractual.}
proc nk_font_atlas_clear*(atlas: ptr nk_font_atlas) {.importc, nodecl, raises: [
    ], tags: [], contractual.}

# ------------------------------------------------------------------
# High level bindings. The new version of the binding
# ------------------------------------------------------------------

# -------
# General
# -------
proc charArrayToString(charArray: openArray[char];
    length: int): string {.raises: [], tags: [], contractual.} =
  ## Convert a characters' array to Nim string, internal use only, temporary
  ## code
  ##
  ## * charArray - the array of characters to convert
  ##
  ## Returns a string with text converted from the chars' array
  result = ""
  for i in 0 .. length - 1:
    result.add(y = charArray[i])

proc stringToCharArray(str: string; length: int): tuple[charArray: seq[char];
    length: cint] {.raises: [], tags: [], contractual.} =
  ## Convert a Nim string to a characters array, internal use only, temporary
  ## code
  ##
  ## * str - the string to convert
  ##
  ## Returns a tuple with two fields, charArray with the converted text from
  ## the string and lenght with the amount of the characters.
  for ch in str:
    result.charArray.add(y = ch)
  if str.len < length:
    for i in str.len .. length:
      result.charArray.add(y = '\0')
  result.length = str.len.cint

proc getWidgetBounds*(): NimRect {.raises: [], tags: [], contractual.} =
  ## Get the rectable with the current Nuklear widget coordinates
  ##
  ## Returns a rectangle with the current Nuklear widget coordinates
  ## converted to NimRect
  proc nk_widget_bounds(ctx): nk_rect {.importc, nodecl, raises: [], tags: [], contractual.}
  let rect = nk_widget_bounds(ctx = ctx)
  return NimRect(x: rect.x, y: rect.y, w: rect.w, h: rect.h)

proc createWin(title: cstring; wx, wy, ww, wh: cfloat;
    wFlags: nk_flags): bool {.raises: [], tags: [], contractual.} =
  ## Create a new Nuklear window/widget, internal use only, temporary code
  ##
  ## Returns true if window was succesfully created otherwise false.
  proc nk_begin(ctx; title: cstring; bounds: nk_rect;
      flags: nk_flags): nk_bool {.importc, nodecl, raises: [], tags: [], contractual.}
  return nk_begin(ctx = ctx, title = title, bounds = new_nk_rect(x = wx, y = wy,
      w = ww, h = wh), flags = wFlags)

proc winSetToInt(nimFlags: set[WindowFlags]): cint {.raises: [], tags: [],
    contractual.} =
  result = 0
  {.warning[HoleEnumConv]: off.}
  for flag in nimFlags:
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
  if createWin(title = name.cstring, wx = x, wy = y, ww = w, wh = h,
      wFlags = winSetToInt(nimFlags = flags)):
    content
  nk_end(ctx = ctx)

proc getTextWidth*(text: string): float {.raises: [Exception], tags: [
    RootEffect], contractual.} =
  ## Get the width in pixels of the selected text in the current font
  ##
  ## * text - the text which width will be count
  ##
  ## Returns width in pixels of the text paramter
  return ctx.style.font.width(arg1 = ctx.style.font.userdata,
      h = ctx.style.font.height, arg3 = text, len = text.len.cint)

proc windowIsHidden*(name: string): bool {.raises: [], tags: [], contractual.} =
  ## Check if the window with the selected name is hidden
  ##
  ## * name - the name of the window to check
  ##
  ## Returns true if the window is hidden, otherwise false
  proc nk_window_is_hidden(ctx; name: cstring): cint {.importc, nodecl,
      raises: [], tags: [], contractual.}
  return nk_window_is_hidden(ctx = ctx, name = name.cstring) > 0

proc windowClose*(name: string) {.raises: [], tags: [], contractual.} =
  ## Closes the window
  ##
  ## * name - the name of the window to close
  proc nk_window_close(ctx; name: cstring) {.importc, nodecl, raises: [],
      tags: [], contractual.}
  nk_window_close(ctx = ctx, name = name.cstring)

proc addSpacing*(cols: int) {.raises: [], tags: [], contractual.} =
  ## Add spacing in the selected between the row's boundaries in the row
  ##
  ## * cols - the amount of columns to add as the spacing
  proc nk_spacing(ctx; cols: cint) {.importc, nodecl, raises: [], tags: [], contractual.}
  nk_spacing(ctx = ctx, cols = cols.cint)

# ------
# Popups
# ------
{.push ruleOff: "params".}
proc nkPopupBegin(ctx; pType: PopupType; title: string; flags: set[WindowFlags];
    x, y, w, h: float): bool {.raises: [NuklearException], tags: [],
        contractual.} =
  ## Try to create a new popup window. Internal use only.
  ##
  ## * ctx   - the Nuklear context
  ## * pType - the type of the popup
  ## * title - the title of the popup
  ## * flags - the flags for the popup
  ## * x     - the X position of the top left corner of the popup
  ## * y     - the Y position of the top left corner of the popup
  ## * w     - the width of the popup
  ## * h     - the height of the popup
  require:
    ctx != nil
    title.len > 0
    ctx.current != nil
    ctx.current.layout != nil
  body:
    if ctx == nil or ctx.current == nil or ctx.current.layout == nil:
      return false
    let
      win: ptr nk_window = ctx.current
      panel: ptr nk_panel = win.layout
    if panel.`type`.cint != panelSetPopup.cint:
      raise newException(exceptn = NuklearException,
          message = "Popups are not allowed to have popups.")
    return true
{.pop ruleOn: "params".}

proc createPopup(pType2: PopupType; title2: cstring;
    flags2: nk_flags; x2, y2, w2, h2: cfloat): bool {.raises: [], tags: [],
        contractual.} =
  ## Create a new Nuklear popup window, internal use only, temporary code
  ##
  ## Returns true if the popup was successfully created, otherwise false.
  proc nk_popup_begin(ctx; pType: PopupType; title: cstring;
      flags: nk_flags; rect: nk_rect): nk_bool {.importc, nodecl, raises: [],
          tags: [], contractual.}
  return nk_popup_begin(ctx = ctx, pType = pType2, title = title2,
      flags = flags2, rect = new_nk_rect(x = x2, y = y2, w = w2, h = h2))

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
  if not createPopup(pType2 = pType, title2 = title.cstring,
      flags2 = winSetToInt(nimFlags = flags), x2 = x.cfloat, y2 = y, w2 = w, h2 = h):
    raise newException(exceptn = NuklearException,
        message = "Can't create the popup window with title: '" & title & "'.")
  content
  ctx.nk_popup_end

proc closePopup*() {.raises: [], tags: [], contractual.} =
  ## Close the last popup window
  proc nk_popup_close(ctx) {.importc, nodecl, raises: [], tags: [], contractual.}
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
  if ctx.nk_tree_state_push(ttype = node, ctitle = title.cstring, cstate = state):
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
  if ctx.nk_tree_state_push(ttype = tab, ctitle = title.cstring, cstate = state):
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
  if nk_tree_push_hashed(ctx = ctx, ttype = node, ctitle = title.cstring,
      cstate = state, chash = ($hash(x = index)).cstring, len = 12,
      id = index.cint):
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
  if nk_tree_push_hashed(ctx = ctx, ttype = tab, ctitle = title.cstring,
      cstate = state, chash = ($hash(x = index)).cstring, len = 12,
      id = index.cint):
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
  if nk_tree_element_push_hashed(ctx = ctx, ttype = eType,
      ctitle = title.cstring, cstate = state, cselected = sel, chash = ($hash(
      x = index)).cstring, len = 12, sed = index.cint):
    content
    nk_tree_element_pop(ctx = ctx)
  selected = sel

# ------
# Labels
# ------
proc colorLabel*(str: string; r, g, b: int;
    align: TextAlignment = left) {.raises: [], tags: [], contractual.} =
  ## Draw a text with the selected color
  ##
  ## * str   - the text to display
  ## * r     - the red value for the text color in RGB
  ## * g     - the green value for the text color in RGB
  ## * b     - the blue value for the text color in RGB
  ## * align - the text aligmnent flags
  proc nk_label_colored(ctx; str: cstring; align: nk_flags;
      color: nk_color) {.importc, nodecl, raises: [], tags: [], contractual.}
  nk_label_colored(ctx = ctx, str = str.cstring, align = align.nk_flags,
      color = nk_rgb(r = r.cint, g = g.cint, b = b.cint))

proc label*(str: string; alignment: TextAlignment = left) {.raises: [], tags: [
    ], contractual.} =
  ## Draw the text with the selected alignment
  ##
  ## * str       - the text to draw
  ## * alignment - the alignment of the text. Default is alignment to the left
  proc nk_label(ctx; str: cstring; alignment: nk_flags) {.importc, nodecl,
      raises: [], tags: [], contractual.}
  nk_label(ctx = ctx, str = str.cstring, alignment = alignment.nk_flags)

proc text*(str: string; len: int = str.len;
    alignment: TextAlignment = left) {.raises: [], tags: [], contractual.} =
  ## Draw the part of the text
  ##
  ## * str       - the text to draw
  ## * len       - the lenght of the text to draw. By default it is equal to
  ##               str lenght
  ## * alignment - the alignment of the text. Default is alignment to left
  proc nk_text(ctx; str: cstring; len: cint; alignment: nk_flags) {.importc,
      nodecl, raises: [], tags: [], contractual.}
  nk_text(ctx = ctx, str = str.cstring, len = len.cint,
      alignment = alignment.nk_flags)

proc wrapLabel*(str: string) {.raises: [], tags: [], contractual.} =
  ## Draw a text and wrap it if its lentgh is bigger than the width of its
  ## container
  ##
  ## * str - the text to draw
  proc nk_label_wrap(ctx; str: cstring) {.importc, nodecl, raises: [], tags: [], contractual.}
  nk_label_wrap(ctx = ctx, str = str.cstring)

{.push ruleOff: "namedParams".}
macro fmtLabel*(alignment: TextAlignment; args: varargs[untyped]): untyped =
  ## Draw a text formatted in the same way like the C function printf
  ##
  ## * alignment - the alignment of the text
  ## * args      - the text and its arguments to draw
  result = quote do:
    nk_labelf(ctx, `alignment`.nk_flags, `args`)
{.pop ruleOn: "namedParams".}

# -------
# Buttons
# -------
proc createColorButton(r, g, b: cint): bool {.raises: [], tags: [],
    contractual.} =
  ## Draw a button with the selected color background, internal use only, temporary code
  ##
  ## * r   - the red value for the button color in RGB
  ## * g   - the green value for the button color in RGB
  ## * b   - the blue value for the button color in RGB
  ##
  ## Returns true if button was pressed
  proc nk_button_color(ctx; color: nk_color): nk_bool {.importc, nodecl,
      raises: [], tags: [], contractual.}
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

proc setButtonBehavior*(behavior: ButtonBehavior) {.raises: [], tags: [],
    contractual.} =
  ## Set the behavior of the the next button, when it is clicked
  ##
  ## * behavior - the behavior of a button
  proc nk_button_set_behavior(ctx; behavior: ButtonBehavior) {.importc, nodecl,
      raises: [], tags: [], contractual.}
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

proc createStyledButton(bTitle: cstring; bStyle: ButtonStyle): bool {.raises: [
    ], tags: [], contractual.} =
  ## Draw a button with the selected style, internal use only, temporary code
  ##
  ## * bTitle - the text to shown on the button
  ## * bStyle - the button's style settings
  var buttonStyle: nk_style_button = ctx.style.button
  buttonStyle.border_color = nk_rgb(bStyle.borderColor.r.cint,
      bStyle.borderColor.g.cint, bStyle.borderColor.b.cint)
  buttonStyle.rounding = bStyle.rounding.cfloat
  buttonStyle.padding = new_nk_vec2(bStyle.padding.x, bStyle.padding.y)
  proc nk_button_label_styled(ctx; style: var nk_style_button;
      title: cstring): nk_bool {.importc, nodecl, raises: [], tags: [], contractual.}
  return nk_button_label_styled(ctx, buttonStyle, bTitle)

template labelButtonStyled*(title: string; style: ButtonStyle;
    onPressCode: untyped) =
  ## Draw the button with the selected text on it and unique style of the
  ## button. Execute the selected code on pressing it.
  ##
  ## * title       - the text to shown on the button
  ## * style       - the style used to draw the button
  ## * onPressCode - the Nim code to execute when the button was pressed
  if createStyledButton(bTitle = title.cstring, bStyle = style):
    onPressCode

# -------
# Sliders
# -------
proc slide*(min, val, max, step: int): int {.raises: [], tags: [],
    contractual.} =
  ## Draw a slide widget with integer values
  ##
  ## * min  - the minimal value on the slider
  ## * val  - the current value on the slider
  ## * max  - the maximum value on the slider
  ## * step - the amount of incrementing or decrementing the value on the
  ##          slider with mouse click
  ##
  ## Returns the new value on the slider
  proc nk_slide_int(ctx; min, val, max, step: cint): cint {.importc, nodecl,
      raises: [], tags: [], contractual.}
  return nk_slide_int(ctx, min.cint, val.cint, max.cint, step.cint).int

# -------
# Layouts
# -------
proc layoutSpacePush(ctx; x, y, w, h: cfloat) {.raises: [], tags: [],
    contractual.} =
  ## Push the next widget's position and size, internal use only, temporary code
  ##
  ## * ctx - the Nuklear context
  ## * x   - the amount of pixels or ratio to push the position in X axis
  ## * y   - the amount of pixels or ratio to push the position in Y axis
  ## * w   - the amount of pixels or ratio to push the width
  ## * h   - the amount of pixels or ratio to push the height
  proc nk_layout_space_push(ctx; rect: nk_rect) {.importc, nodecl, raises: [],
      tags: [], contractual.}
  nk_layout_space_push(ctx, new_nk_rect(x, y, w, h))

proc setLayoutRowDynamic*(height: float; cols: int) {.raises: [], tags: [],
    contractual.} =
  ## Set the current widgets layout to divide it into selected amount of
  ## columns with the selected height in rows and grows in width when the
  ## parent window resizes
  ##
  ## * height - the height in pixels of each row
  ## * cols   - the amount of columns in each row
  proc nk_layout_row_dynamic(ctx; height: cfloat; cols: cint) {.importc, cdecl,
      raises: [], tags: [], contractual.}
  nk_layout_row_dynamic(ctx, height.cfloat, cols.cint)

proc setLayoutRowStatic*(height: float; width, cols: int) {.raises: [], tags: [
    ], contractual.} =
  ## Set the current widgets layout to divide it into selected amount of
  ## columns with the selected height in rows but it will not grow in width
  ## when the parent window resizes
  ##
  ## * height - the height in pixels of each row
  ## * width  - the width in pixels of each column
  ## * cols   - the amount of columns in each row
  proc nk_layout_row_static(ctx; height: cfloat; itemWidth,
      cols: cint) {.importc, cdecl, raises: [], tags: [], contractual.}
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

proc setLayoutRowStatic*(height: float; cols: int; ratio: openArray[
    cfloat]) {.raises: [], tags: [], contractual.} =
  ## Set the current widgets layout to divide it into selected amount of
  ## columns with the selected height in rows but it will not grow in width
  ## when the parent window resizes
  ##
  ## * height - the height in pixels of each row
  ## * cols   - the amount of columns in each row
  ## * ratio  - the array or sequence of cfloat with width of the colums
  nk_layout_row(ctx, NK_STATIC, height.cfloat, cols.cint, ratio.addr)

proc setLayoutRowDynamic*(height: float; cols: int; ratio: openArray[
    cfloat]) {.raises: [], tags: [], contractual.} =
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

proc rowTemplateDynamic*() {.raises: [], tags: [], contractual.} =
  ## Set the selected column's in the row width in the template's row as dynamic,
  ## which means, the widget will resize with its parent.
  proc nk_layout_row_template_push_dynamic(ctx) {.importc, nodecl, raises: [],
      tags: [], contractual.}
  nk_layout_row_template_push_dynamic(ctx)

proc rowTemplateVariable*(minWidth: float) {.raises: [], tags: [],
    contractual.} =
  ## Set the selected column's width in the row template as dynamic but with
  ## requirement for minumum width for the widget
  ##
  ## * minWidth - the minimum width in pixels for the widgets in the column
  proc nk_layout_row_template_push_variable(ctx; minWidth: cfloat) {.importc,
      nodecl, raises: [], tags: [], contractual.}
  nk_layout_row_template_push_variable(ctx, minWidth.cfloat)

proc rowTemplateStatic*(width: float) {.raises: [], tags: [], contractual.} =
  ## Set the selected column's width in the row template to static value,
  ## widgets in the column will not resize
  ##
  ## * width - the width of the column in the row template
  proc nk_layout_row_template_push_static(ctx; width: cfloat) {.importc, nodecl,
      raises: [], tags: [], contractual.}
  nk_layout_row_template_push_static(ctx, width.cfloat)

proc layoutWidgetBounds*(): NimRect {.raises: [], tags: [], contractual.} =
  ## Get the rectangle of the current widget in the layout
  ##
  ## Returns NimRect with the data for the current widget
  proc nk_layout_widget_bounds(ctx): nk_rect {.importc, nodecl, raises: [],
      tags: [], contractual.}
  let rect = nk_layout_widget_bounds(ctx = ctx)
  result = NimRect(x: rect.x, y: rect.y, w: rect.w, h: rect.h)

proc layoutSetMinRowHeight*(height: float) {.raises: [], tags: [],
    contractual.} =
  ## Set the currently used minimum row height. Must contains also paddings size.
  ##
  ## * height - the new minimum row height for auto generating the row height
  proc nk_layout_set_min_row_height(ctx; height: cfloat) {.importc, nodecl,
      raises: [], tags: [], contractual.}
  nk_layout_set_min_row_height(ctx = ctx, height = height.cfloat)

proc lyoutResetMinRowHeight*() {.raises: [], tags: [], contractual.} =
  ## Reset the currently used minimum row height.
  proc nk_layout_reset_min_row_height(ctx) {.importc, nodecl, raises: [],
      tags: [], contractual.}
  nk_layout_reset_min_row_height(ctx = ctx)

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

proc createMenu(ctx; text: cstring; align: nk_flags; x,
    y: cfloat): bool {.raises: [], tags: [], contractual.} =
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
       size: nk_vec2): nk_bool {.importc, nodecl, raises: [], tags: [], contractual.}
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

proc slider*(min: int; val: var int; max, step: int): bool {.discardable,
    raises: [], tags: [], contractual.} =
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
      step: cint): nk_bool {.importc, nodecl, raises: [], tags: [], contractual.}
  var newVal = val.cint
  result = nk_slider_int(ctx = ctx, min = min.cint, val = newVal,
      max = max.cint, step = step.cint) == nkTrue
  val = newVal

proc slider*(min: float; val: var float; max,
    step: float): bool {.discardable, raises: [], tags: [], contractual.} =
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
    valueStep: cfloat): nk_bool {.importc, nodecl, raises: [], tags: [], contractual.}
  var newVal = val.cfloat
  result = nk_slider_float(ctx = ctx, min = min.cfloat, val = newVal,
      max = max.cfloat, value_step = step.cfloat) == nkTrue
  val = newVal

# ----------
# Properties
# ----------

proc property*(name: string; min: int; val: var int; max, step: int;
    incPerPixel: float) {.raises: [], tags: [], contractual.} =
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
      step: cint; incPerPixel: cfloat) {.importc, nodecl, raises: [], tags: [], contractual.}
  var newVal = val.cint
  nk_property_int(ctx, name.cstring, min.cint, newVal, max.cint, step.cint,
      incPerPixel.cfloat)
  val = newVal.int

proc property*(name: string; min: float; val: var float; max, step: float;
    incPerPixel: float) {.raises: [], tags: [], contractual.} =
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
      val: var cfloat; max, step, incPerPixel: cfloat) {.importc, nodecl,
          raises: [], tags: [], contractual.}
  var newVal = val.cfloat
  nk_property_float(ctx, name.cstring, min.cfloat, newVal, max.cfloat,
      step.cfloat, incPerPixel.cfloat)
  val = newVal.float

proc property2*(name: string; min, val, max, step,
    incPerPixel: float): float {.raises: [], tags: [], contractual.} =
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
      incPerPixel: cfloat): cfloat {.importc, nodecl, raises: [], tags: [], contractual.}
  return nk_propertyf(ctx, name.cstring, min.cfloat, val.cfloat, max.cfloat,
      step.cfloat, incPerPixel.cfloat).float

proc property2*(name: string; min, val, max, step: int;
    incPerPixel: float): int {.raises: [], tags: [], contractual.} =
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
      incPerPixel: cfloat): cint {.importc, nodecl, raises: [], tags: [], contractual.}
  return nk_propertyi(ctx, name.cstring, min.cint, val.cint, max.cint,
      step.cint, incPerPixel.cfloat).int

# -----
# Style
# -----
proc headerAlign*(value: StyleHeaderAlign) {.raises: [], tags: [],
    contractual.} =
  ## Set the Nuklear windows header alignment
  ##
  ## * value - the new value for the alignment
  ctx.style.window.header.align = value.ord.nk_style_header_align

var buttonStyle: nk_style_button ## Used to store the Nuklear buttons style

proc saveButtonStyle*() {.raises: [], tags: [], contractual.} =
  ## Save the Nuklear buttons style to variable, so it can be restored later
  buttonStyle = ctx.style.button

proc restoreButtonStyle*() {.raises: [], tags: [], contractual.} =
  ## Restore previously save to the variable Nuklear buttons style
  ##
  ctx.style.button = buttonStyle

proc setButtonStyle*(field: ButtonStyleTypes; r: cint = 255; g: cint = 255;
    b: cint = 255; a: cint = 255) {.raises: [], tags: [], contractual.} =
  ## Set the color for the selcted field of the Nuklear buttons style
  ##
  ## * field - the style's field which value will be changed
  ## * r     - the red value for the style color in RGBA
  ## * g     - the green value for the style color in RGBA
  ## * b     - the blue value for the style color in RGBA
  ## * a     - the alpha value for the style color in RGBA
  case field
  of normal:
    ctx.style.button.normal = nk_style_item_color(nk_rgba(r, g, b, a))
  of hover:
    ctx.style.button.hover = nk_style_item_color(nk_rgba(r, g, b, a))
  of active:
    ctx.style.button.active = nk_style_item_color(nk_rgba(r, g, b, a))
  of borderColor:
    ctx.style.button.border_color = nk_rgba(r, g, b, a)
  of textBackground:
    ctx.style.button.text_background = nk_rgba(r, g, b, a)
  of textNormal:
    ctx.style.button.text_normal = nk_rgba(r, g, b, a)
  of textHover:
    ctx.style.button.text_hover = nk_rgba(r, g, b, a)
  of textActive:
    ctx.style.button.text_active = nk_rgba(r, g, b, a)
  else:
    discard

proc setButtonStyle2*(source, destination: ButtonStyleTypes) {.raises: [],
    tags: [], contractual.} =
  ## Copy one field of Nuklear buttons style to another
  ##
  ## * source      - the field which value will be copied
  ## * destination - the field to which the source value will be copied
  if source == active:
    if destination == normal:
      ctx.style.button.normal = ctx.style.button.active

proc getButtonStyle*(field: ButtonStyleTypes): NimVec2 {.raises: [], tags: [],
    contractual.} =
  ## Get the value of the selected field of Nuklear buttons style
  ##
  ## * field - the field which value will be taken
  ##
  ## Returns vector with the value of the selected field
  if field == padding:
    return NimVec2(x: ctx.style.button.padding.x, y: ctx.style.button.padding.y)

proc stylePushVec2*(field: WindowStyleTypes; x,
    y: cfloat): bool {.discardable, raises: [], tags: [], contractual.} =
  ## Push the vector value for the selected Nuklear window style on a
  ## temporary stack
  ##
  ## * field - the Nuklear windows style field which will be modified
  ## * x     - the X value of the vector to push
  ## * y     - the Y value of the vector to push
  ##
  ## Returns true if value was succesfully pushed, otherwise false
  proc nk_style_push_vec2(ctx; dest: var nk_vec2;
      source: nk_vec2): nk_bool {.importc, nodecl, raises: [], tags: [], contractual.}
  if field == spacing:
    return nk_style_push_vec2(ctx, ctx.style.window.spacing, new_nk_vec2(x,
        y))

proc stylePushFloat*(field: ButtonStyleTypes;
    value: cfloat): bool {.discardable, raises: [], tags: [], contractual.} =
  ## Push the float value for the selected Nuklear buttons style on a
  ## temporary stack
  ##
  ## * ctx   - the Nuklear context
  ## * field - the Nuklear buttons style field which will be modified
  ## * value - the float value to push
  ##
  ## Returns true if value was succesfully pushed, otherwise false
  proc nk_style_push_float(ctx; dest: var cfloat;
      source: cfloat): nk_bool {.importc, nodecl, raises: [], tags: [], contractual.}
  case field
  of rounding:
    return nk_style_push_float(ctx, ctx.style.button.rounding, value)
  else:
    return false

proc styleFromTable*(table: openArray[NimColor]) {.raises: [], tags: [],
    contractual.} =
  ## Set the Nuklear style colors from the table
  ##
  ## * table - the colors table which will be set
  proc nk_style_from_table(ctx; table: pointer) {.importc, nodecl, raises: [],
      tags: [], contractual.}
  var newTable: array[countColors.ord, nk_color]
  for index, color in table.pairs:
    newTable[index] = nk_rgba(color.r.cint, color.g.cint, color.b.cint, color.a.cint)
  nk_style_from_table(ctx, newTable.addr)

proc defaultStyle*() {.raises: [], tags: [], contractual.} =
  ## reset the UI colors to the default Nuklear setting
  proc nk_style_default(ctx) {.importc, nodecl, raises: [], tags: [], contractual.}
  nk_style_default(ctx)

proc stylePopFloat*() {.raises: [], tags: [], contractual.} =
  proc nk_style_pop_float(ctx) {.importc, nodecl, raises: [], tags: [], contractual.}
  nk_style_pop_float(ctx)

proc stylePopVec2*() {.raises: [], tags: [], contractual.} =
  proc nk_style_pop_vec2(ctx) {.importc, nodecl, raises: [], tags: [], contractual.}
  nk_style_pop_vec2(ctx)

# ------
# Combos
# ------
proc comboList*(items: openArray[string]; selected, itemHeight: int; x,
    y: float; amount: int = items.len - 1): int {.raises: [], tags: [],
        contractual.} =
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
      selected, itemHeight: cint; size: nk_vec2): cint {.importc, nodecl,
          raises: [], tags: [], contractual.}
  var optionsList: seq[cstring]
  for i in 0 .. amount:
    optionsList.add(items[i].cstring)
  return nk_combo(ctx, optionsList[0].addr, amount.cint + 1,
      selected.cint, itemHeight.cint, new_nk_vec2(x.cfloat, y.cfloat)).int

proc createColorCombo(ctx; color: NimColor; x, y: cfloat): bool {.raises: [],
    tags: [], contractual.} =
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

proc createColorCombo(ctx; color: NimColorF; x, y: cfloat): bool {.raises: [],
    tags: [], contractual.} =
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

proc createLabelCombo(ctx; selected: cstring; x, y: cfloat): bool {.raises: [],
    tags: [], contractual.} =
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
      size: nk_vec2): nk_bool {.importc, nodecl, raises: [], tags: [], contractual.}
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

proc comboClose*() {.raises: [], tags: [], contractual.} =
  ## Stop adding a value to a combo
  proc nk_combo_close(ctx) {.importc, nodecl, raises: [], tags: [], contractual.}
  nk_combo_close(ctx)

# ------
# Colors
# ------
proc colorfToHsva*(hsva: var array[4, float]; color: NimColorF) {.raises: [],
    tags: [], contractual.} =
  ## Convert Nim float color object to HSVA values
  ##
  ## * hsva  - the array of 4 values for HSVA color
  ## * color - the Nim color to convert
  ##
  ## Returns converted color as hsva argument
  proc nk_colorf_hsva_fv(hsva: pointer; color: nk_colorf) {.importc, nodecl,
      raises: [], tags: [], contractual.}
  nk_colorf_hsva_fv(hsva.addr, nk_colorf(r: color.r, g: color.g,
      b: color.b, a: color.a))
proc hsvaToColorf*(hsva: array[4, float]): NimColorF {.raises: [], tags: [],
    contractual.} =
  ## Convert HSVA values to Nim color with float values
  ##
  ## * hsva - the array with HSVA values to convert
  ##
  ## Returns converted hsva parameter to Nim color with float values
  proc nk_hsva_colorf(h, s, v, a: cfloat): nk_colorf {.importc, nodecl,
      raises: [], tags: [], contractual.}
  let newColor = nk_hsva_colorf(hsva[0], hsva[1], hsva[2], hsva[3])
  result = NimColorF(r: newColor.r, g: newColor.g, b: newColor.b, a: newColor.a)

# ------
# Charts
# ------
proc createColorChart(ctx; ctype: ChartType; color,
    higlight: NimColor; count: cint; minValue,
        maxValue: cfloat): bool {.raises: [], tags: [], contractual.} =
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
      maxValue: cfloat): nk_bool {.importc, nodecl, raises: [], tags: [], contractual.}
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
    higlight: NimColor; count: cint; minValue, maxValue: cfloat) {.raises: [],
        tags: [], contractual.} =
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
      higlight: nk_color; count: cint; minValue, maxValue: cfloat) {.importc,
          nodecl, raises: [], tags: [], contractual.}
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

proc chartPush*(value: float): ChartEvent {.discardable, raises: [], tags: [],
    contractual.} =
  ## Push, add the value to the current chart
  ##
  ## * value - the value to add
  ##
  ## Returns the mouse event if any happened over the value in the chart
  proc nk_chart_push(ctx; value: cfloat): nk_flags {.importc, nodecl, raises: [
      ], tags: [], contractual.}

  let res = nk_chart_push(ctx, value.cfloat)
  if (res and clicked.nk_flags) == clicked.nk_flags:
    return clicked
  if (res and hovering.nk_flags) == hovering.nk_flags:
    return hovering
  return none

proc addChartSlot*(ctype: ChartType; count: int; minValue,
    maxValue: float) {.raises: [], tags: [], contractual.} =
  ## Add another chart to the existing one
  ##
  ## * ctype     - the type of the chart
  ## * count     - the amount of values on the chart
  ## * min_value - the minimal value of the chart
  ## * max_value - the maximum value of the chart
  proc nk_chart_add_slot(ctx; ctype: ChartType; count: cint;
      minValue, maxValue: cfloat) {.importc, nodecl, raises: [], tags: [], contractual.}
  nk_chart_add_slot(ctx, ctype, count.cint, minValue.cfloat, maxValue.cfloat)

proc chartPushSlot*(value: float; slot: int): ChartEvent {.discardable,
    raises: [], tags: [], contractual.} =
  ## Push, add the value to the current chart at the selected position
  ##
  ## * value - the value to add
  ## * slot  - the slot to which the value will be added
  ##
  ## Returns the mouse event if any happened over the value in the chart
  proc nk_chart_push_slot(ctx; value: cfloat; slot: cint): nk_flags {.importc,
      nodecl, raises: [], tags: [], contractual.}

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
    triggerBounds: NimRect): bool {.raises: [], tags: [], contractual.} =
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
      triggerBounds: nk_rect): nk_bool {.importc, nodecl, raises: [], tags: [], contractual.}
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
proc isMouseHovering*(rect: NimRect): bool {.raises: [], tags: [],
    contractual.} =
  ## Check if mouse is hovering over the selected rectangle
  ##
  ## * x   - the X coordinate of top left corner of the rectangle
  ## * y   - the Y coordinate of top left corner of the rectangle
  ## * w   - the width of the rectangle in pixels
  ## * h   - the height of the rectangle in pixels
  ##
  ## Returns true if the mouse is hovering over the rectangle, otherwise false
  proc nk_input_is_mouse_hovering_rect(i: ptr nk_input;
      rect: nk_rect): nk_bool {.importc, nodecl, raises: [], tags: [], contractual.}
  return nk_input_is_mouse_hovering_rect(ctx.input.addr, new_nk_rect(rect.x,
      rect.y, rect.w, rect.h))

proc isMousePrevHovering*(x, y, w, h: float): bool {.raises: [], tags: [],
    contractual.} =
  ## Check if the mouse was previously hovering over the selected rectangle
  ##
  ## * x   - the X coordinate of top left corner of the rectangle
  ## * y   - the Y coordinate of top left corner of the rectangle
  ## * w   - the width of the rectangle in pixels
  ## * h   - the height of the rectangle in pixels
  ##
  ## Returns true if the mouse was hovering over the rectangle, otherwise false
  proc nk_input_is_mouse_prev_hovering_rect(i: ptr nk_input;
      rect: nk_rect): nk_bool {.importc, nodecl, raises: [], tags: [], contractual.}
  return nk_input_is_mouse_prev_hovering_rect(ctx.input.addr, new_nk_rect(
      x, y, w, h))

proc isMouseDown*(id: Buttons): bool {.raises: [], tags: [], contractual.} =
  ## Check if mouse is pressed
  ##
  ## * id  - the mouse button which is pressed
  ##
  ## Returns true if the selected mouse button is pressed, otherwise false
  proc nk_input_is_mouse_down(i: ptr nk_input; id: Buttons): nk_bool {.importc,
      nodecl, raises: [], tags: [], contractual.}
  return nk_input_is_mouse_down(ctx.input.addr, id)

proc getMouseDelta*(): NimVec2 {.raises: [], tags: [], contractual.} =
  ## Get the mouse vector between last check and current position of the mouse
  ##
  ## Returns vector with information about the mouse movement delta
  return NimVec2(x: ctx.input.mouse.delta.x, y: ctx.input.mouse.delta.y)

# ---------
# Edit text
# ---------
proc editString*(text: var string; maxLen: int; editType: EditTypes = simple;
    filter: PluginFilter = nk_filter_default; flags: set[EditFlags] = {
        }): EditEvent {.discardable, raises: [], tags: [], contractual.} =
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
      len: var cint; max: cint; filter: PluginFilter): nk_flags {.importc,
          nodecl, raises: [], tags: [], contractual.}

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
    align: TextAlignment = left): bool {.discardable, raises: [], tags: [],
        contractual.} =
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
      value: var nk_bool): nk_bool {.importc, nodecl, raises: [], tags: [], contractual.}
  var newValue = value.nk_bool
  result = nk_selectable_label(ctx, str.cstring, align.nk_flags, newValue) == nkTrue
  discard $newValue
  value = newValue

proc selectableSymbolLabel*(sym: SymbolType; title: string; value: var bool;
    align: TextAlignment = left): bool {.discardable, raises: [], tags: [],
        contractual.} =
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
    title: cstring; align: nk_flags; value: var nk_bool): nk_bool {.importc,
        nodecl, raises: [], tags: [], contractual.}
  var newValue = value.nk_bool
  result = nk_selectable_symbol_label(ctx, sym, title.cstring, align.nk_flags,
      newValue) == nkTrue
  discard $newValue
  value = newValue

# ------
# Images
# ------
proc image*(image: PImage) {.raises: [], tags: [], contractual.} =
  ## Draw an image
  ##
  ## * image - pointer to the image which will be drawn
  proc nk_new_image(ctx; img: nk_image) {.importc: "nk_image", nodecl, raises: [
      ], tags: [], contractual.}
  proc nk_image_ptr(iPtr: pointer): nk_image {.importc, nodecl, raises: [],
      tags: [], contractual.}
  nk_new_image(ctx = ctx, img = nk_image_ptr(iPtr = image))

# --------
# Tooltips
# --------
proc showTooltips*() {.raises: [], tags: [], contractual.} =
  ## Check if the mouse is in any of tooltips related widgets bounds. If yes,
  ## update the timer and if delay reached 0, show the selected tooltip. The best
  ## place to call it is at the end of the Nuklear window declaration.
  ## Temporary here due to problems with importing nk_rect.
  var inBounds: bool = false
  for tp in tooltips:
    if isMouseHovering(rect = tp.bounds):
      inBounds = true
      delay -= frameDelay
      if delay <= 0:
        tooltip(text = tp.text)
  if not inBounds:
    delay = tooltipDelay

# -------
# Widgets
# -------
proc colorPicker*(color: NimColorF;
    format: colorFormat): NimColorF {.raises: [], tags: [], contractual.} =
  ## Create the color picker widget. Temporary here due to problems with importing nk_colorf.
  ##
  ## * color  - the starting color for the widget
  ## * format - the color format for the widget
  ##
  ## Returns Nim color selected by the user in the widget
  proc nk_color_picker(ctx; color: nk_colorf;
      fmt: colorFormat): nk_colorf {.importc, nodecl, raises: [], tags: [], contractual.}
  let newColor = nk_color_picker(ctx, nk_colorf(r: color.r, g: color.g,
      b: color.b, a: color.a), format)
  result = NimColorF(r: newColor.r, g: newColor.g, b: newColor.b, a: newColor.a)

