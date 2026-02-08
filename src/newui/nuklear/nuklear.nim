# Copyright © 2023-2026 Bartek Jasicki
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

import std/[colors, hashes, macros, unicode]
import contracts, nimalyzer
import nk_button, nk_colors, nk_context, nk_draw, nk_input, nk_layout, nk_math,
  nk_page, nk_panel, nk_tooltip, nk_types, nk_utf, nk_utils, nk_widget
export nk_button, nk_colors, nk_context, nk_input, nk_layout, nk_tooltip,
  nk_types, nk_widget

## Provides code for Nuklear binding

# -------
# Objects
# -------
type PImage* = pointer ## A pointer to the image type

# ---------------------
# Procedures parameters
# ---------------------
using
  ctx: PContext
  context: ref Context

# -------
# General
# -------
proc nk_end(ctx) {.importc, cdecl, raises: [], tags: [], contractual.}
  ## A binding to Nuklear's function. Internal use only
proc nk_widget_disable_begin(ctx) {.importc, cdecl, raises: [], tags: [], contractual.}
  ## A binding to Nuklear's function. Internal use only
proc nk_widget_disable_end(ctx) {.importc, cdecl, raises: [], tags: [], contractual.}
  ## A binding to Nuklear's function. Internal use only

# -------
# Windows
# -------
proc nk_window_find(ctx; name: cstring): ptr nk_window {.importc, nodecl,
    raises: [], tags: [], contractual.}
  ## A binding to Nuklear's function. Internal use only

# ----
# Text
# ----
proc nk_labelf(ctx; flags: nk_flags; fmt: cstring) {.importc,
    varargs, cdecl, raises: [], tags: [], contractual.}
  ## A binding to Nuklear's function. Internal use only

# -------
# Layouts
# -------
proc nk_layout_row_template_begin(ctx; cheight: cfloat) {.importc, cdecl,
    raises: [], tags: [], contractual.}
  ## A binding to Nuklear's function. Internal use only
proc nk_layout_row_template_end(ctx) {.importc, cdecl, raises: [], tags: [], contractual.}
  ## A binding to Nuklear's function. Internal use only

# -----
# Menus
# -----
proc nk_menubar_begin(ctx) {.importc, cdecl, raises: [], tags: [], contractual.}
  ## A binding to Nuklear's function. Internal use only
proc nk_menubar_end(ctx) {.importc, cdecl, raises: [], tags: [], contractual.}
  ## A binding to Nuklear's function. Internal use only
proc nk_menu_end(ctx) {.importc, cdecl, raises: [], tags: [], contractual.}
  ## A binding to Nuklear's function. Internal use only
proc nk_menu_item_label(ctx; ctext: cstring;
    aligmnent: nk_flags): nk_bool {.importc, cdecl, raises: [], tags: [], contractual.}
  ## A binding to Nuklear's function. Internal use only

# ------
# Charts
# ------
proc nk_chart_begin(ctx; ctype1: ChartType; num1: cint; min1,
    max1: cfloat): nk_bool {.importc, cdecl, raises: [], tags: [], contractual.}
  ## A binding to Nuklear's function. Internal use only
proc nk_chart_end(ctx) {.importc, cdecl, raises: [], tags: [], contractual.}
  ## A binding to Nuklear's function. Internal use only

# ------
# Popups
# ------
proc nk_popup_end(ctx) {.importc, nodecl, raises: [], tags: [], contractual.}
  ## A binding to Nuklear's function. Internal use only
proc nk_tooltip_end(ctx) {.importc, nodecl, raises: [], tags: [], contractual.}
  ## A binding to Nuklear's function. Internal use only

# -----
# Trees
# -----
proc nk_tree_state_push(ctx; ttype: TreeType;
    ctitle: cstring; cstate: var CollapseStates): nk_bool {.importc, cdecl,
        raises: [], tags: [], contractual.}
  ## A binding to Nuklear's function. Internal use only
proc nk_tree_pop(ctx) {.importc, cdecl, raises: [], tags: [], contractual.}
  ## A binding to Nuklear's function. Internal use only
proc nk_tree_push_hashed(ctx; ttype: TreeType;
    ctitle: cstring; cstate: CollapseStates; chash: cstring; len,
    id: cint): nk_bool {.importc, cdecl, raises: [], tags: [], contractual.}
  ## A binding to Nuklear's function. Internal use only
proc nk_tree_element_push_hashed(ctx; ttype: TreeType;
    ctitle: cstring; cstate: CollapseStates; cselected: var nk_bool;
    chash: cstring; len, sed: cint): nk_bool {.importc, cdecl, raises: [],
        tags: [], contractual.}
  ## A binding to Nuklear's function. Internal use only
proc nk_tree_element_pop(ctx) {.importc, cdecl, raises: [], tags: [], contractual.}
  ## A binding to Nuklear's function. Internal use only

# -----
# Style
# -----
proc nk_style_item_color(col: nk_color): nk_style_item {.importc, cdecl,
    raises: [], tags: [], contractual.}
  ## A binding to Nuklear's function. Internal use only
proc nk_style_set_font*(ctx; font: ptr PNkUserFont) {.importc, nodecl,
    raises: [], tags: [], contractual.}
  ## A binding to Nuklear's function. Internal use only

# ------
# Combos
# ------
proc nk_combo_begin_color(ctx; color: nk_color;
    size: nk_vec2): nk_bool {.importc, nodecl, raises: [], tags: [], contractual.}
  ## A binding to Nuklear's function. Internal use only
proc nk_combo_end(ctx) {.importc, cdecl, raises: [], tags: [], contractual.}
  ## A binding to Nuklear's function. Internal use only

# -------
# Filters
# -------
proc nk_filter_default*(box: ptr nk_text_edit;
    unicode: nk_rune): nk_bool {.importc, cdecl, raises: [], tags: [], contractual.}
  ## A binding to Nuklear's function. Internal use only
proc nk_filter_decimal*(box: ptr nk_text_edit;
    unicode: nk_rune): nk_bool {.importc, cdecl, raises: [], tags: [], contractual.}
  ## A binding to Nuklear's function. Internal use only
proc nk_filter_float*(box: ptr nk_text_edit;
    unicode: nk_rune): nk_bool {.importc, cdecl, raises: [], tags: [], contractual.}
  ## A binding to Nuklear's function. Internal use only
proc nk_filter_hex*(box: ptr nk_text_edit;
    unicode: nk_rune): nk_bool {.importc, cdecl, raises: [], tags: [], contractual.}
  ## A binding to Nuklear's function. Internal use only
proc nk_filter_oct*(box: ptr nk_text_edit;
    unicode: nk_rune): nk_bool {.importc, cdecl, raises: [], tags: [], contractual.}
  ## A binding to Nuklear's function. Internal use only
proc nk_filter_binary*(box: ptr nk_text_edit;
    unicode: nk_rune): nk_bool {.importc, cdecl, raises: [], tags: [], contractual.}
  ## A binding to Nuklear's function. Internal use only
proc nk_filter_ascii*(box: ptr nk_text_edit;
    unicode: nk_rune): nk_bool {.importc, cdecl, raises: [], tags: [], contractual.}
  ## A binding to Nuklear's function. Internal use only

# ----------
# Contextual
# ----------
proc nk_contextual_end(ctx) {.importc, cdecl, raises: [], tags: [], contractual.}
  ## A binding to Nuklear's function. Internal use only
proc nk_contextual_item_label(ctx; clabel: cstring;
    calign: nk_flags): nk_bool {.importc, cdecl, raises: [], tags: [], contractual.}
  ## A binding to Nuklear's function. Internal use only
proc nk_contextual_item_image(ctx; img: PImage;): nk_bool {.importc, cdecl,
  raises: [], tags: [], contractual.}
  ## A binding to Nuklear's function. Internal use only

# ------
# Groups
# ------
proc nk_group_begin(ctx; ctitle: cstring;
    cflags: nk_flags): nk_bool {.importc, cdecl, raises: [], tags: [], contractual.}
  ## A binding to Nuklear's function. Internal use only
proc nk_group_end(ctx) {.importc, cdecl, raises: [], tags: [], contractual.}
  ## A binding to Nuklear's function. Internal use only

# -----
# Fonts
# -----
proc nk_font_atlas_add_default*(atlas: ptr nk_font_atlas; height: cfloat;
    config: ptr nk_font_config): ptr nk_font {.importc, nodecl, raises: [],
        tags: [], contractual.}
  ## A binding to Nuklear's function. Internal use only
proc nk_font_atlas_add_from_file*(atlas: ptr nk_font_atlas; filePath: cstring;
    height: cfloat;  config: ptr nk_font_config): ptr nk_font {.importc,
        nodecl, raises: [], tags: [], contractual.}
  ## A binding to Nuklear's function. Internal use only
proc nk_font_atlas_clear*(atlas: ptr nk_font_atlas) {.importc, nodecl, raises: [
    ], tags: [], contractual.}
  ## A binding to Nuklear's function. Internal use only

# ------
# Images
# ------
proc nk_image_ptr(iPtr: pointer): nk_image {.importc, nodecl, raises: [],
    tags: [], contractual.}
  ## A binding to Nuklear's function. Internal use only

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
  for i in 0..length - 1:
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

proc getWidgetBounds*(): Rect {.raises: [], tags: [], contractual.} =
  ## Get the rectable with the current Nuklear widget coordinates
  ##
  ## Returns a rectangle with the current Nuklear widget coordinates
  ## converted to Rect
  proc nk_widget_bounds(ctx): nk_rect {.importc, nodecl, raises: [], tags: [], contractual.}
    ## A binding to Nuklear's function. Internal use only
  let rect: nk_rect = nk_widget_bounds(ctx = ctx)
  return Rect(x: rect.x, y: rect.y, w: rect.w, h: rect.h)

proc createWin(title: cstring; wx, wy, ww, wh: cfloat;
    wFlags: nk_flags): bool {.raises: [], tags: [], contractual.} =
  ## Create a new Nuklear window/widget, internal use only, temporary code
  ##
  ## Returns true if window was succesfully created otherwise false.
  proc nk_begin(ctx; title: cstring; bounds: nk_rect;
      flags: nk_flags): nk_bool {.importc, nodecl, raises: [], tags: [], contractual.}
    ## A binding to Nuklear's function. Internal use only
  return nk_begin(ctx = ctx, title = title, bounds = new_nk_rect(x = wx, y = wy,
      w = ww, h = wh), flags = wFlags)

proc winSetToInt(nimFlags: set[PanelFlags]): cint {.raises: [], tags: [],
    contractual.} =
  ## Convert Nim flags related to windows to C
  ##
  ## * nimFlags - flags to convert
  ##
  ## Returns a result of a bit or on the flags.
  result = 0
  {.warning[HoleEnumConv]: off.}
  {.ruleOff: "assignments".}
  for flag in nimFlags:
    result = result or flag.cint
  {.ruleOn: "assignments".}
  {.warning[HoleEnumConv]: on.}

template window*(name: string; x, y, w, h: float; flags: set[PanelFlags];
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
    ## A binding to Nuklear's function. Internal use only
  return nk_window_is_hidden(ctx = ctx, name = name.cstring) > 0

proc windowClose*(name: string) {.raises: [], tags: [], contractual.} =
  ## Closes the window
  ##
  ## * name - the name of the window to close
  proc nk_window_close(ctx; name: cstring) {.importc, nodecl, raises: [],
      tags: [], contractual.}
    ## A binding to Nuklear's function. Internal use only
  nk_window_close(ctx = ctx, name = name.cstring)

proc addSpacing*(cols: int) {.raises: [], tags: [], contractual.} =
  ## Add spacing in the selected between the row's boundaries in the row
  ##
  ## * cols - the amount of columns to add as the spacing
  proc nk_spacing(ctx; cols: cint) {.importc, nodecl, raises: [], tags: [], contractual.}
    ## A binding to Nuklear's function. Internal use only
  nk_spacing(ctx = ctx, cols = cols.cint)

template disabled*(content: untyped) =
  ## Create disabled widgets list
  ##
  ## * content - the content of the list
  nk_widget_disable_begin(ctx = ctx)
  content
  nk_widget_disable_end(ctx = ctx)

# -------
# Windows
# -------
proc windowHasFocus*(): bool {.raises: [], tags: [], contractual.} =
  ## Check if the currently processed window is currently active
  ##
  ## Returns true if the window is active, otherwise false
  proc nk_window_has_focus(ctx): nk_bool {.importc, nodecl, raises: [], tags: [], contractual.}
    ## A binding to Nuklear's function. Internal use only
  return nk_window_has_focus(ctx = ctx)

proc windowSetFocus*(name: string) {.raises: [], tags: [], contractual.} =
  ## Set the selected window as an active window
  ##
  ## * name - the name of the window to set as active
  proc nk_window_set_focus(ctx; name: cstring) {.importc, nodecl,
      raises: [], tags: [], contractual.}
    ## A binding to Nuklear's function. Internal use only
  nk_window_set_focus(ctx = ctx, name = name.cstring)

proc windowIsActive*(name: string): bool {.raises: [], tags: [], contractual.} =
  ## Check if the selected window is active
  ##
  ## * name - the neme of the window to check
  ##
  ## Returns true if the window is active, otherwise false
  proc nk_window_is_active(ctx; name: cstring): nk_bool {.importc, nodecl,
      raises: [], tags: [], contractual.}
    ## A binding to Nuklear's function. Internal use only
  return nk_window_is_active(ctx = ctx, name = name.cstring)

proc windowIsHovered*(): bool {.raises: [], tags: [], contractual.} =
  ## Check if the currently processed window is hovered by mouse
  ##
  ## Returns true if the window is hovered by mouse, otherwise false
  proc nk_window_is_hovered(ctx): nk_bool {.importc, nodecl, raises: [],
    tags: [], contractual.}
    ## A binding to Nuklear's function. Internal use only
  return nk_window_is_hovered(ctx = ctx)

proc windowFind*(name: string): ptr nk_window {.raises: [], tags: [], contractual.} =
  ## Find the window with the selected name
  ##
  ## * name - the name of the window to find
  ##
  ## Returns the pointer to the selected window
  return nk_window_find(ctx = ctx, name = name.cstring)

proc windowEditActive*(name: string): bool {.raises: [], tags: [],
    contractual.} =
  ## Check if the selected window has active edit widget
  ##
  ## * name - the name of the window to check
  ##
  ## Returns true if the window has active edit widget, otherwise false
  return windowFind(name = name).edit.active == 1

proc windowPropertyActive*(name: string): bool {.raises: [], tags: [],
    contractual.} =
  ## Check if the selected window has active property widget
  ##
  ## * name - the name of the window to check
  ##
  ## Returns true if the window has active property widget, otherwise false
  return windowFind(name = name).property.active == 1

proc windowInput*(name: string; disable: bool = true) {.raises: [], tags: [], contractual.} =
  ## Enable or disable input in the selected window
  ##
  ## * name - the name of the window in which input will be enabled or disabled
  var root: PNkPanel = windowFind(name = name).layout
  if disable:
    while root != nil:
      root.flags = root.flags or windowRom.ord.cint
      root.flags = root.flags and not windowRemoveRom.ord.cint
      root = root.parent
  else:
    while root != nil:
      root.flags = root.flags or windowRemoveRom.ord.cint
      root = root.parent

proc windowDisable*() {.raises: [], tags: [], contractual.} =
  ## Disable the current window
  proc nk_window_disable(ctx) {.importc, nodecl, raises: [], tags: [],
    contractual.}
    ## A binding to Nuklear's function. Internal use only
  nk_window_disable(ctx = ctx)

proc windowShow*(name: string; state: ShowStates) {.raises: [], tags: [],
    contractual.} =
  ## Show or hide the window depending on the state
  ##
  ## * name  - the name of the window to show or hide
  ## * state - the state in which the window will be
  proc nk_window_show(ctx; name: cstring; state: ShowStates) {.importc,
    nodecl, raises: [], tags: [], contractual.}
    ## A binding to Nuklear's function. Internal use only
  nk_window_show(ctx = ctx, name = name.cstring, state = state)

proc createWindow*(context): Window {.raises: [],
  tags: [RootEffect], contractual.} =
  ## Create a new Nuklear widget
  ##
  ## * context - the Nuklear context
  ##
  ## Returns the newly created widget
  var elem: PageElement = nkCreatePageElement(context = context,
    pageType = windowType)
  elem.data.win.seq = context.seq
  return elem.data.win

# ----
# Misc
# ----
proc nkPushScissor(b: var CommandBuffer; r: Rect) {.raises: [], tags: [
    RootEffect], contractual.} =
  ## Clear the rectangle. Internal use only
  ##
  ## b - the command buffer in which scissor will be used
  ## r - the rectangle of the scissor
  ##
  ## Returns the modified parameter b
  body:
    b.clip = r
    var cmd: CommandScissor = cast[CommandScissor](
      nkCommandBufferPush(b = b, t = commandScissor,
      size = CommandScissor.sizeof))
    cmd.x = r.x.int16
    cmd.y = r.y.int16
    cmd.w = max(x = 0.uint16, y = r.w.uint16)
    cmd.h = max(x = 0.uint16, y = r.h.uint16)

proc nkStrokeRect(b: var CommandBuffer, rect: Rect, rounding,
  lineThickness: float, c: NkColor) {.raises: [], tags: [RootEffect],
  contractual.} =
  ## Draw a rectangle. Internal use only
  ##
  ## * b             - the command buffer in which the rectangle will be drawn
  ## * r             - the rectangle to draw
  ## * rounding      - the rouding of the rectangle's corners
  ## * lineThickness - the thinckness of the rectangle's border
  ## * c             - the color used to draw the rectangle
  if c.a == 0 or rect.w == 0 or lineThickness <= 0:
    return
  if b.use_clipping == 1:
    if not nkIntersect(x0 = rect.x, y0 = rect.y, w0 = rect.w, h0 = rect.h,
      x1 = b.clip.x, y1 = b.clip.y, w1 = b.clip.w, h1 = b.clip.h):
      return
  var cmd: CommandRect = cast[CommandRect](nkCommandBufferPush(b = b,
    t = commandRect, size = CommandRect.sizeof))
  cmd.rounding = rounding.cushort
  cmd.line_thickness = lineThickness.cushort
  cmd.x = rect.x.cshort
  cmd.y = rect.y.cshort
  cmd.w = max(x = 0.cushort, y = rect.w.cushort)
  cmd.h = max(x = 0.cushort, y = rect.h.cushort)
  cmd.color = c

proc nkStrokeTriangle(b: var CommandBuffer; x0, y0, x1, y1, x2, y2,
  lineThickness: float; c: NkColor) {.raises: [], tags: [RootEffect], contractual.} =
  ## Draw a triangle. Internal use only
  ##
  ## * b             - the command buffer in which the triangle will be drawn
  ## * x0            - the X coordinate of the first the triangle's vertex
  ## * y0            - the Y coordinate of the first the triangle's vertex
  ## * x1            - the X coordinate of the second the triangle's vertex
  ## * y1            - the Y coordinate of the second the triangle's vertex
  ## * x2            - the X coordinate of the third the triangle's vertex
  ## * y2            - the Y coordinate of the third the triangle's vertex
  ## * lineThickness - the thinckness of the triangle's border
  ## * c             - the color used to draw the triangle
  if c.a == 0:
    return
  if b.use_clipping != 0:
    if not nkInbox(px = x0, py = y0, x = b.clip.x, y = b.clip.y, w = b.clip.w,
      h = b.clip.h) and not nkInbox(px = x1, py = y1, x = b.clip.x, y = b.clip.y,
      w = b.clip.w, h = b.clip.h) and not nkInbox(px = x2, py = y2, x = b.clip.x,
      y = b.clip.y, w = b.clip.w, h = b.clip.h):
      return

  var cmd: CommandTriangle = cast[CommandTriangle](nkCommandBufferPush(b = b,
    t = commandTriangle, size = CommandTriangle.sizeof))
  cmd.line_thickness = lineThickness.uint16
  cmd.a.x = x0.int16
  cmd.a.y = y0.int16
  cmd.b.x = x1.int16
  cmd.b.y = y1.int16
  cmd.c.x = x2.int16
  cmd.c.y = y2.int16
  cmd.color = c

proc nkFillCircle(b: var CommandBuffer; rect: Rect; c: NkColor)
  {.raises: [], tags: [RootEffect], contractual.} =
  ## Fill the circle with the selected color
  ##
  ## * b        - the command buffer in which the rectangle will be drawn
  ## * rect     - the rectangle for the circle
  ## * c        - the color to fill the circle
  if rect.w == 0 or rect.h == 0:
    return
  if b.use_clipping == 1:
    if not nkIntersect(x0 = rect.x, y0 = rect.y, w0 = rect.w, h0 = rect.h,
      x1 = b.clip.x, y1 = b.clip.y, w1 = b.clip.w, h1 = b.clip.h):
      return

  var cmd: CommandCircleFilled = cast[CommandCircleFilled](nkCommandBufferPush(b = b,
    t = commandCircleFilled, size = CommandCircleFilled.sizeof))
  cmd.x = rect.x.int16
  cmd.y = rect.y.int16
  cmd.w = max(x = 0, y = rect.w).uint16
  cmd.h = max(x = 0, y = rect.h).uint16
  cmd.color = c

proc nkFillTriangle(b: var CommandBuffer, x0, y0, x1, y1, x2, y2: float,
  c: NkColor) {.raises: [], tags: [RootEffect], contractual.} =
  ## Fill the circle with the selected color
  ##
  ## * b  - the command buffer in which the triangle will be drawn
  ## * x0 - the X coordinate of the first the triangle's vertex
  ## * y0 - the Y coordinate of the first the triangle's vertex
  ## * x1 - the X coordinate of the second the triangle's vertex
  ## * y1 - the Y coordinate of the second the triangle's vertex
  ## * x2 - the X coordinate of the third the triangle's vertex
  ## * y2 - the Y coordinate of the third the triangle's vertex
  ## * c  - the color to fill the triangle
  if c.a == 0:
    return
  if b.use_clipping != 0:
    if not nkInbox(px = x0, py = y0, x = b.clip.x, y = b.clip.y, w = b.clip.w,
      h = b.clip.h) and not nkInbox(px = x1, py = y1, x = b.clip.x, y = b.clip.y,
      w = b.clip.w, h = b.clip.h) and not nkInbox(px = x2, py = y2, x = b.clip.x,
      y = b.clip.y, w = b.clip.w, h = b.clip.h):
      return

  var cmd: CommandTriangleFilled = cast[CommandTriangleFilled](nkCommandBufferPush(b = b,
    t = commandTriangleFilled, size = CommandTriangleFilled.sizeof))
  cmd.a.x = x0.int16
  cmd.a.y = y0.int16
  cmd.b.x = x1.int16
  cmd.b.y = y1.int16
  cmd.c.x = x2.int16
  cmd.c.y = y2.int16
  cmd.color = c

proc nkDrawImage(b: var CommandBuffer; r: Rect; img: Image; col: NkColor)
  {.raises: [], tags: [RootEffect], contractual.} =
  ## Draw the selected image
  ##
  ## * b   - the command buffer in which the image will be drawn
  ## * r   - the rectangle in which the image will be drawn
  ## * img - the image to draw
  ## * col - the color used as a background for the image
  if b.use_clipping != 0:
    if b.clip.w == 0 or b.clip.h == 0 or not nkIntersect(x0 = r.x, y0 = r.y, w0 = r.w,
      h0 = r.h, x1 = b.clip.x, y1 = b.clip.y, w1 = b.clip.w, h1 = b.clip.h):
      return

  var cmd: CommandImage = cast[CommandImage](nkCommandBufferPush(b = b, t = commandImage,
    size = CommandImage.sizeof))
  cmd.x = r.x.cshort
  cmd.y = r.y.cshort
  cmd.w = max(x = 0.cushort, y = r.w.cushort)
  cmd.h = max(x = 0.cushort, y = r.h.cushort)
  cmd.img = img
  cmd.col = col

proc nkDrawNineSlice(b: var CommandBuffer; r: Rect; slc: NineSlice; col: NkColor)
  {.raises: [], tags: [RootEffect], contractual.} =
  ## Draw the selected fragments of an image
  ##
  ## * b   - the command buffer in which the slice will be drawn
  ## * r   - the rectangle in which the slice will be drawn
  ## * slc - the image's slice to draw
  ## * col - the color used as a background for the slice
  let slcImg: Image = slc.image
  var rgnX, rgnY, rgnW, rgnH: nk_ushort = 0
  rgnX = slcImg.region[0]
  rgnY = slcImg.region[1]
  rgnW = slcImg.region[2]
  rgnH = slcImg.region[3]

  var img: Image = Image()

  # top-left
  img.handle = slcImg.handle
  img.w = slcImg.w
  img.h = slcImg.h
  img.region = [rgnX, rgnY, slc.l, slc.t]

  nkDrawImage(b = b, r = Rect(x: r.x, y: r.y, w: slc.l.float, h: slc.t.float), img = img, col = col)

  # top-center
  img.region = [rgnX + slc.l, rgnY, rgnW - slc.l - slc.r, slc.t]
  nkDrawImage(b = b, r = Rect(x: r.x + slc.l.float, y: r.y, w: r.w - slc.l.float - slc.r.float, h: slc.t.float), img = img, col = col)

  # top-right
  img.region = [rgnX + rgnW - slc.r, rgnY, slc.r, slc.t]
  nkDrawImage(b = b, r = Rect(x: r.x + r.w - slc.r.float, y: r.y, w: slc.r.float, h: slc.t.float), img = img, col = col)

  # center-left
  img.region = [rgnX, rgnY + slc.t, slc.l, rgnH - slc.t - slc.b]
  nkDrawImage(b = b, r = Rect(x: r.x, y: r.y + slc.t.float, w: slc.l.float, h: r.h - slc.t.float - slc.b.float), img = img, col = col)

  # center
  img.region = [rgnX + slc.l, rgnY + slc.t, rgnW - slc.l - slc.r, rgnH - slc.t - slc.b]
  nkDrawImage(b = b, r = Rect(x: r.x + slc.l.float, y: r.y + slc.t.float, w: r.w - slc.l.float - slc.r.float, h: r.h - slc.t.float - slc.b.float), img = img, col = col)

  # center-right
  img.region = [rgnX + rgnW - slc.r, rgnY + slc.t, slc.r, rgnH - slc.t - slc.b]
  nkDrawImage(b = b, r = Rect(x: r.x + r.w - slc.r.float, y: r.y - slc.t.float, w: slc.r.float, h: r.h - slc.t.float - slc.b.float), img = img, col = col)

  # bottom-left
  img.region = [rgnX, rgnY + rgnH - slc.b, slc.l, slc.b]
  nkDrawImage(b = b, r = Rect(x: r.x, y: r.y + r.h - slc.b.float, w: slc.l.float, h: slc.b.float), img = img, col = col)

  # bottom-center
  img.region = [rgnX + slc.l, rgnY + rgnH - slc.b, rgnW - slc.l - slc.r, slc.b]
  nkDrawImage(b = b, r = Rect(x: r.x + slc.l.float, y: r.y + r.h - slc.b.float, w: r.w - slc.l.float - slc.r.float, h: slc.b.float), img = img, col = col)

  # bottom-right
  img.region = [rgnX + rgnW - slc.r, rgnY + rgnH - slc.b, slc.r, slc.b]
  nkDrawImage(b = b, r = Rect(x: r.x + r.w - slc.r.float, y: r.y + r.h - slc.b.float, w: slc.r.float, h: slc.b.float), img = img, col = col)

proc nkTextClamp(font: UserFont; text: string; textLen: int;
  space: float; glyphs: var int; textWidth: var float; sepList: seq[nk_rune];
  sepCount: int): int {.raises: [], tags: [RootEffect], contractual.} =
  ## Clamp the selected text
  ##
  ## * font      - font used to draw the text
  ## * text      - the text to clamp
  ## * textLen   - the lenght of the text
  ## * space     - the amount of pixels used as space between letters
  ## * glyphs    - the amount of glyphs in the text
  ## * textWidth - the width of the text in pixels
  ## * sepList   - the list of separators
  ## * sepCount  - the amount of separators
  ##
  ## Returns the new length of the text
  var
    unicode: nk_rune = 0
    glyphLen: int = nkUtfDecode(c = text, u = unicode)
    width, sepWidth, lastWidth: float = 0.0
    len, sepG, g, sepLen: int = 0
  while glyphLen > 0 and (width < space) and (len < textLen):
    len += glyphLen
    if unicode == '\n'.nk_rune:
      lastWidth = width
      sepWidth = lastWidth
      sepG = g + 1
      sepLen = len
      break
    let s: float = try:
        font.width(arg1 = font.userdata, h = font.height, arg3 = text,
          len = len)
      except Exception:
        return
    var i: Natural = 0
    for sep in sepList:
      i.inc
      if unicode != sep:
        continue
      lastWidth = width
      sepWidth = lastWidth
      sepG = g + 1
      sepLen = len
      break
    if i == sepCount:
      sepWidth = width
      lastWidth = sepWidth
      sepG = g + 1
    width = s
    glyphLen = nkUtfDecode(c = $text[len], u = unicode)
    g.inc
  if len >= textLen:
    glyphs = g
    textWidth = lastWidth
    return len
  glyphs = sepG
  textWidth = sepWidth
  return if sepLen == 0: len else: sepLen

proc nkDrawText(b: var CommandBuffer; r: Rect; str: string; length: var int;
  font: UserFont; bg, fg: NkColor) {.raises: [], tags: [RootEffect],
  contractual.} =
  ## Draw the selected text
  ##
  ## * b    - the command buffer in which the text will be drawn
  ## * r    - the rectangle in which the text will be drawn
  ## * str  - the text to draw
  ## * len  - the length of the text to draw
  ## * font - the font used to draw the text
  ## * bg   - the background color of the text
  ## * fg   - the foreground color of the text
  body:
    if str == "" or length == 0 or (bg.a == 0 and fg.a == 0):
      return
    if b.use_clipping == 1:
      if (b.clip.w == 0 or b.clip.h == 0 or not nkIntersect(x0 = r.x, y0 = r.y,
        w0 = r.w, h0 = r.h, x1 = b.clip.x, y1 = b.clip.y, w1 = b.clip.w,
        h1 = b.clip.h)):
        return

    # make sure text fits inside bounds
    let textWidth: float = try:
        font.width(arg1 = font.userdata, h = font.height, arg3 = str, len = length)
      except Exception:
        return
    if textWidth > r.w:
      var
        glyphs: int = 0
        txtWidth: float = textWidth
      length = nkTextClamp(font = font, text = str, textLen = length,
        space = r.w, glyphs = glyphs, textWidth = txtWidth, sepList = @[], sepCount = 0)

    if length == 0:
      return
    var cmd: CommandText = cast[CommandText](
        nkCommandBufferPush(b = b, t = commandText,
            size = CommandText.sizeof + (length + 1).nk_size))
    cmd.x = r.x.int16
    cmd.y = r.y.int16
    cmd.w = r.w.uint16
    cmd.h = r.h.uint16
    cmd.background = bg
    cmd.foreground = fg
    cmd.font = font
    cmd.length = length
    cmd.height = font.height
    cmd.text = str[0..length]

# -----
# Input
# -----
proc isMouseHovering*(rect: Rect): bool {.raises: [], tags: [],
    contractual.} =
  ## Check if mouse is hovering over the selected rectangle
  ##
  ## * rect - the area in which the mouse will be checked for hovering
  ##
  ## Returns true if the mouse is hovering over the rectangle, otherwise false
  if ctx.input.addr == nil:
    return false
  return nkInbox(px = ctx.input.mouse.pos.x, py = ctx.input.mouse.pos.y,
    x = rect.x, y = rect.y, w = rect.w, h = rect.h)

proc isMousePrevHovering*(rect: Rect): bool {.raises: [], tags: [],
    contractual.} =
  ## Check if the mouse was previously hovering over the selected rectangle
  ##
  ## * rect - the area in which the mouse will be checked for hovering
  ##
  ## Returns true if the mouse was hovering over the rectangle, otherwise false
  proc nk_input_is_mouse_prev_hovering_rect(i: ptr nk_input;
      rect: nk_rect): nk_bool {.importc, nodecl, raises: [], tags: [], contractual.}
    ## A binding to Nuklear's function. Internal use only
  return nk_input_is_mouse_prev_hovering_rect(i = ctx.input.addr,
      rect = new_nk_rect(x = rect.x, y = rect.y, w = rect.w, h = rect.h))

proc isMouseDown*(id: Buttons): bool {.raises: [], tags: [], contractual.} =
  ## Check if mouse is pressed
  ##
  ## * id  - the mouse button which is pressed
  ##
  ## Returns true if the selected mouse button is pressed, otherwise false
  proc nk_input_is_mouse_down(i: ptr nk_input; id: Buttons): nk_bool {.importc,
      nodecl, raises: [], tags: [], contractual.}
    ## A binding to Nuklear's function. Internal use only
  return nk_input_is_mouse_down(i = ctx.input.addr, id = id)

proc getMouseDelta*(): Vec2 {.raises: [], tags: [], contractual.} =
  ## Get the mouse vector between last check and current position of the mouse
  ##
  ## Returns vector with information about the mouse movement delta
  return Vec2(x: ctx.input.mouse.delta.x, y: ctx.input.mouse.delta.y)

proc mouseClicked*(id: Buttons; rect: Rect): bool {.raises: [], tags: [],
    contractual.} =
  ## Check if the selected mouse button was clicked in the selected area
  ##
  ## * id  - the mouse button which was pressed
  ## * rect - the area in which the mouse button was pressed
  ##
  ## Returns true if the selected mouse button was clicked in the selected
  ## area, otherwise false.
  proc nk_input_mouse_clicked(i: ptr nk_input; id: Buttons;
      rect: nk_rect): nk_bool {.importc, nodecl, raises: [], tags: [], contractual.}
    ## A binding to Nuklear's function. Internal use only
  return nk_input_mouse_clicked(i = ctx.input.addr, id = id, rect = new_nk_rect(
      x = rect.x, y = rect.y, w = rect.w, h = rect.h))

proc isMouseClicked*(btn: Buttons): bool {.raises: [], tags: [],
    contractual.} =
  ## Check if the selected mouse button was clicked in the current widget
  ##
  ## * btn  - the mouse button which was pressed
  ##
  ## Returns true if the selected mouse button was clicked in the current
  ## widget, otherwise false.
  proc nk_widget_is_mouse_clicked(ctx; btn: Buttons): nk_bool {.importc, nodecl,
      raises: [], tags: [], contractual.}
    ## A binding to Nuklear's function. Internal use only
  return nk_widget_is_mouse_clicked(ctx = ctx, btn = btn)

proc isKeyPressed*(key: Keys): bool {.raises: [], tags: [], contractual.} =
  ## Check if the selected key is pressed
  ##
  ## * key - the key which was pressed
  ##
  ## Returns true if the selected key is pressed, otherwise false
  proc nk_input_is_key_pressed(i: ptr nk_input;
      key: Keys): nk_bool {.importc, nodecl, raises: [], tags: [], contractual.}
    ## A binding to Nuklear's function. Internal use only
  return nk_input_is_key_pressed(i = ctx.input.addr, key = key)

proc isMousePressed*(id: Buttons): bool {.raises: [], tags: [], contractual.} =
  ## Check if the selected mouse button is pressed now
  ##
  ## * id   - the mouse button which will be checked
  ##
  ## Returns true if the mouse button is pressed, otherwise false
  proc nk_input_is_mouse_pressed(i: ptr nk_input; id: Buttons): nk_bool
    {.importc, nodecl, raises: [], tags: [], contractual.}
    ## A binding to Nuklear's function. Internal use only
  return nk_input_is_mouse_pressed(i = ctx.input.addr, id = id)

proc isMouseReleased*(id: Buttons): bool {.raises: [], tags: [], contractual.} =
  ## Check if the selected mouse button was released
  ##
  ## * id   - the mouse button which will be checked
  ##
  ## Returns true if the mouse button was released, otherwise false
  proc nk_input_is_mouse_released(i: ptr nk_input; id: Buttons): nk_bool
    {.importc, nodecl, raises: [], tags: [], contractual.}
    ## A binding to Nuklear's function. Internal use only
  return nk_input_is_mouse_released(i = ctx.input.addr, id = id)

# ----
# Text
# ----
proc nkWidgetText(o: var CommandBuffer; b: var Rect; str: string; len: var int;
  t: Text; a: nk_flags; f: UserFont) {.raises: [], tags: [RootEffect],
  contractual.} =
  ## Draw a text widget. Internal use only
  ##
  ## * o   - the command buffer in which the widget will be draw
  ## * b   - the bounds of the widget
  ## * str - the text to draw in the widget
  ## * len - the length of the text to draw
  ## * t   - the text style
  ## * a   - the flags related to the widget
  ## * f   - the font used to draw the widget
  body:
    b.h = max(x = b.h, y = 2 * t.padding.y)
    var label: Rect = Rect()
    label.x = 0
    label.w = 0
    label.y = b.y + t.padding.y
    label.h = min(x = f.height, y = b.h - 2 * t.padding.y)
    var textWidth: float = 0.0
    textWidth = try:
        f.width(arg1 = f.userdata, h = f.height, arg3 = str, len = len)
      except Exception:
        return
    textWidth += (2.0 * t.padding.x)

    # align in x-axis
    {.ruleOff: "ifStatements".}
    if (a and textLeft.ord).bool:
      label.x = b.x + t.padding.x
      label.w = max(x = 0, y = b.w - 2 * t.padding.x)
    elif (a and textCentered.ord).bool:
      label.w = max(x = 1, y = 2 * t.padding.x + textWidth.float)
      label.x = (b.x + t.padding.x + ((b.w - 2 * t.padding.x) - label.w) / 2)
      label.x = max(x = b.x + t.padding.x, y = label.x)
      label.w = min(x = b.x + b.w, y = label.x + label.w)
      if label.w >= label.x:
        label.w -= label.x
    elif (a and textRight.ord).bool:
      label.x = max(x = b.x + t.padding.x, y = (b.x + b.w) - (2 * t.padding.x + textWidth.float))
      label.w = textWidth.float + 2 * t.padding.x
    else:
      return
    {.ruleOn: "ifStatements".}

    # align in y-axis
    if (a and textMiddle.ord).bool:
      label.y = b.y + b.h / 2.0 - f.height.float / 2.0
      label.h = max(x = b.h / 2.0, y = b.h - (b.h / 2.0 + f.height / 2.0))
    elif (a and textBottom.ord).bool:
      label.y = b.y + b.h - f.height
      label.h = f.height

    nkDrawText(b = o, r = label, str = str, length = len, font = f,
      bg = t.background, fg = t.text)

# -------
# Buttons
# -------
proc nkButtonBehavior(state: var nk_flags; r: Rect; i: Input;
  behavior: ButtonBehavior): bool {.raises: [], tags: [], contractual.} =
  ## Set the button's behavior. Internal use only
  ##
  ## * state    - the state of the button
  ## * r        - the bounds of the button
  ## * i        - the user input
  ## * behavior - the behavior of the button, normal or repeater
  ##
  ## Returns true if button's behavior was properly set, otherwise false
  nkWidgetStateReset(s = state)
  result = false
  if isMouseHovering(rect = r):
    state = widgetStateHovered.nk_flags
    if isMouseDown(id = left):
      state = widgetStateActive.nk_flags
      if hasMouseClickDownInRect(id = left, rect = r, down = nkTrue):
        if behavior == default:
          when defined(nkButtonTriggerOnRelease):
            result = isMouseReleased(id = left)
          else:
            result = isMousePressed(id = left)
        else:
          result = isMouseDown(id = left)
  if (state and widgetStateHover.ord).nk_bool and not isMousePrevHovering(rect = r):
    state = state or widgetStateEntered.ord
  elif isMousePrevHovering(rect = r):
    state = state or widgetStateLeft.ord

proc nkDoButton(state: var nk_flags; r: Rect;
  style: StyleButton; `in`: Input; behavior: ButtonBehavior;
  content: var Rect): bool {.raises: [], tags: [], contractual.} =
  ## Draw a button. Internal use only
  ##
  ## * state    - the state of the button
  ## * r        - the bounds of the button
  ## * style    - the style of the button
  ## * in       - the user input
  ## * behavior - the behavior of the button, normal or repeater
  ## * content  - the space of the button's content
  ##
  ## Returns true if button was properly drawn, otherwise false
  body:
    # calculate button content space
    content.x = r.x + style.padding.x + style.border + style.rounding
    content.y = r.y + style.padding.y + style.border + style.rounding
    content.w = r.w - (2 * (style.padding.x + style.border + style.rounding))
    content.h = r.h - (2 * (style.padding.y + style.border + style.rounding))

    # execute button behavior
    var bounds: Rect = Rect()
    bounds.x = r.x - style.touch_padding.x
    bounds.y = r.y - style.touch_padding.y
    bounds.w = r.w + 2 * style.touch_padding.x
    bounds.h = r.h + 2 * style.touch_padding.y
    return nkButtonBehavior(state = state, r = bounds, i = `in`, behavior = behavior)

proc nkDrawButton(`out`: var CommandBuffer; bounds: Rect;
  state: nk_flags; style: StyleButton): StyleItem {.raises: [],
  tags: [RootEffect], contractual.} =
  ## Draw a button. Internal use only
  ## * out      - the command buffer in which the button will be drawn
  ## * bounds   - the bounds of the button
  ## * state    - the state of the button
  ## * style    - the style of the button
  ##
  ## Returns the style of the button
  if (state and widgetStateHover.ord).nk_bool:
    result = style.hover
  elif (state and widgetStateActived.ord).nk_bool:
    result = style.active
  else:
    result = style.normal

  let bg: StyleItemData = result.data
  case result.iType
  of itemImage:
    nkDrawImage(b = `out`, r = bounds, img = bg.image, col =
      nkRGBFactor(col = NkColor(r: 255, g: 255, b: 255, a: 255),
      factor = style.colorFactorBackground))
  of itemNineSlice:
    nkDrawNineSlice(b = `out`, r = bounds, slc = bg.slice, col =
      nkRGBFactor(col = NkColor(r: 255, g: 255, b: 255, a: 255),
      factor = style.colorFactorBackground))
  of itemColor:
    nkFillRect(b = `out`, rect = bounds, rounding = style.rounding, c =
      nkRGBFactor(col = bg.color, factor = style.colorFactorBackground))
    nkStrokeRect(b = `out`, rect = bounds, rounding = style.rounding,
      lineThickness = style.border, c = nkRGBFactor(col = bg.color,
      factor = style.colorFactorBackground))

proc nkDrawSymbol(`out`: var CommandBuffer; `type`: SymbolType;
  content: var Rect; background, foreground: NkColor; borderWidth: float;
  font: UserFont) {.raises: [], tags: [RootEffect], contractual.} =
  ## Draw the selected symbol
  ##
  ## * out         - the command buffer in which the symbol will be drawn
  ## * type        - the type of symbol to draw
  ## * content     - the bounds of the symbol's content
  ## * background  - the background color of the symbol
  ## * foreground  - the foreground color of the symbol
  ## * borderWidth - the width of border of the symbol
  ## * font        - the font used to draw on the symbol
  case `type`
  of x, underscore, plus, minus:
    # single character text symbol
    let ch: char = case `type`
      of x:
        'x'
      of underscore:
        '_'
      of plus:
        '+'
      of minus:
        '-'
      else:
        ' '
    var text: Text = Text()
    text.padding = Vec2(x: 0, y: 0)
    text.background = background
    text.text = foreground
    var length: Positive = 1
    nkWidgetText(o = `out`, b = content, str = $ch, len = length, t = text,
      a = centered, f = font)
  of circleSolid, circleOutline, rectSolid, rectOutline:
    var drawRect: Rect = content
    drawRect = nkShrinkRect(r = drawRect, amount = borderWidth)
    # simple empty/filled shapes
    if `type` in [rectSolid, rectOutline]:
      nkFillRect(b = `out`, rect = content, rounding = 0, c = foreground)
      if `type` == rectOutline:
        nkFillRect(b = `out`, rect = drawRect, rounding = 0, c = background)
    else:
      nkFillCircle(b = `out`, rect = content, c = foreground)
      if `type` == circleOutline:
        nkFillCircle(b = `out`, rect = drawRect, c = background)
  of triangleUp, triangleDown, triangleLeft, triangleRight:
    var heading: Heading = right
    var points: array[3, Rect] = [Rect(), Rect(), Rect()]
    case `type`
      of triangleRight:
        heading = right
      of triangleLeft:
        heading = left
      of triangleUp:
        heading = up
      else:
        heading = down
    nkTriangleFromDirection(`result` = points, r = content, padX = 0,
      padY = 0, direction = heading)
    nkFillTriangle(b = `out`, x0 = points[0].x, y0 = points[0].y,
      x1 = points[1].x, y1 = points[1].y, x2 = points[2].x, y2 = points[2].y,
      c = foreground)
  of triangleUpOutline, triangleDownOutline, triangleLeftOutline,
    triangleRightOutline:
    var heading: Heading = right
    var points: array[3, Rect] = [Rect(), Rect(), Rect()]
    case `type`
      of triangleRightOutline:
        heading = right
      of triangleLeftOutline:
        heading = left
      of triangleUpOutline:
        heading = up
      else:
        heading = down
    nkTriangleFromDirection(`result` = points, r = content, padX = 0,
      padY = 0, direction = heading)
    nkStrokeTriangle(b = `out`, x0 = points[0].x, y0 = points[0].y,
      x1 = points[1].x, y1 = points[1].y, x2 = points[2].x, y2 = points[2].y,
      lineThickness = borderWidth, c = foreground)
  else:
    discard

proc nkDrawButtonSymbol(`out`: var CommandBuffer; bounds, content: var Rect;
  state: nk_flags; style: StyleButton; `type`: SymbolType;
  font: UserFont) {.raises: [], tags: [RootEffect], contractual.} =
  ## Draw a button with the selected symbol on it. Internal use only
  ##
  ## * out      - the command buffer in which the button will be drawn
  ## * bounds   - the bounds of the button
  ## * content  - the bounds of the button's content
  ## * state    - the state of the button
  ## * style    - the style of the button
  ## * type     - the type of symbol to draw
  ## * font     - the font used to draw on the button
  # select correct colors/images
  let background: StyleItem = nkDrawButton(`out` = `out`, bounds = bounds,
    state = state, style = style)
  let bg: NkColor = (if background.iType == itemColor: background.data.color else: style.textBackground)

  var sym: NkColor = (if (state and widgetStateHover.ord).bool:
    style.textHover elif (state and widgetStateActive.ord).bool:
      style.textActive else: style.textNormal)

  sym = nkRGBFactor(col = sym, factor = style.colorFactorText)
  nkDrawSymbol(`out` = `out`, `type` = `type`, content = content,
    background = bg, foreground = sym, borderWidth = 1, font = font)

proc nkDoButtonSymbol(state: var nk_flags; `out`: var CommandBuffer; bounds: var Rect,
  symbol: SymbolType; behavior: ButtonBehavior; style: StyleButton;
  `in`: Input; font: UserFont): bool {.raises: [], tags: [RootEffect], contractual.} =
  ## Draw a button with the selected symbol on it. Internal use only
  ##
  ## * state    - the state of the button
  ## * out      - the command buffer in which the button will be drawn
  ## * bounds   - the bounds of the button
  ## * symbol   - the symbol to draw on the button
  ## * behavior - the behavior of the button, normal or repeater
  ## * style    - the style of the button
  ## * in       - the user input
  ## * font     - the font used to draw on the button
  ##
  ## Returns true if button was properly drawn, otherwise false
  body:
    var content: Rect = Rect()
    result = nkDoButton(state = state, r = bounds, style = style,
      `in` = `in`, behavior = behavior, content = content)
    try:
      style.drawBegin(b = `out`, userData = style.userData)
    except Exception:
      discard
    nkDrawButtonSymbol(`out` = `out`, bounds = bounds, content = content,
      state = state, style = style, `type` = symbol, font = font)
    try:
      style.drawEnd(b = `out`, userData = style.userdata)
    except Exception:
      discard

# ------------
# Page element
# ------------

# -----
# Panel
# -----
proc panelHeader(win: ref Window; title: string; style: Style; font: UserFont;
  layout: ref Panel; `out`: var CommandBuffer, `in`: Input): bool {.raises: [],
  tags: [RootEffect], contractual.} =
  ## Start drawing a Nuklear panel's header if needed. Internal use only
  ##
  ## * win    - the panel which header will be draw
  ## * title  - the panel's title
  ## * style  - the current UI style
  ## * font   - the font used by the UI
  ## * layout - the layout of the panel
  ## * out    - the command buffer
  ## * in     - the user's input
  ##
  ## Returns true if the header was drawn, otherwise false
  if nkPanelHasHeader(flags = win.flags, title = title):
    var
      header: Rect = Rect()
      background: StyleItem = StyleItem()
      text: Text = Text()

    # calculate header bounds
    header.x = win.bounds.x
    header.y = win.bounds.y
    header.w = win.bounds.w
    header.h = font.height + 2.0 + style.window.header.padding.y
    header.h += (2.0 + style.window.header.label_padding.y)

    # shrink panel by header
    layout.header_height = header.h
    layout.bounds.y += header.h
    layout.bounds.h -= header.h
    layout.at_y += header.h

    # select correct header background and text color
    if context.active.name == win.name:
      background = style.window.header.active
      if layout.pType == panelGroup:
        text.text = style.window.groupTextColor
      else:
        text.text = style.window.header.labelActive
    elif isMouseHovering(rect = header):
      background = style.window.header.hover
      if layout.pType == panelGroup:
        text.text = style.window.groupTextColor
      else:
        text.text = style.window.header.labelHover
    else:
      background = style.window.header.normal
      if layout.pType == panelGroup:
        text.text = style.window.groupTextColor
      else:
        text.text = style.window.header.labelNormal

    # draw header background
    header.h += 1.0
    case background.iType
    of itemImage:
      text.background = NkColor(r: 0, g: 0, b: 0, a: 0)
      nkDrawImage(b = win.buffer, r = header, img = background.data.image,
        col = NkColor(r: 255, g: 255, b: 255, a: 255))
    of itemNineSlice:
      text.background = NkColor(r: 0, g: 0, b: 0, a: 0)
      nkDrawNineSlice(b = win.buffer, r = header, slc = background.data.slice,
        col = NkColor(r: 255, g: 255, b: 255, a: 255))
    of itemColor:
      text.background = background.data.color
      nkFillRect(b = `out`, rect = header, rounding = 0,
        c = background.data.color)

    # window close button
    var button: Rect = Rect()
    button.y = header.y + style.window.header.padding.y
    button.h = header.h - 2 * style.window.header.padding.y
    button.w = button.h
    if (win.flags and windowClosable.cint).nk_bool:
      var ws: nk_flags = 0
      if style.window.header.align == headerRight:
        button.x = (header.w + header.x) - (button.w + style.window.header.padding.x)
        header.w -= button.w + style.window.header.spacing.x + style.window.header.padding.x
      else:
        button.x = header.x + style.window.header.padding.x
        header.x += button.w + style.window.header.spacing.x + style.window.header.padding.x
      if nkDoButtonSymbol(state = ws, `out` = win.buffer, bounds = button,
        symbol = style.window.header.close_symbol, behavior = default,
        style = style.window.header.close_button, `in` = `in`,
        font = style.font) and not(win.flags and windowRom.cint).nk_bool:
        layout.flags = layout.flags or windowHidden.cint
        layout.flags = layout.flags and not windowMinimized.cint

    # window minimize button
    if (win.flags and windowMinimizable.cint).nk_bool:
      var ws: nk_flags = 0
      if style.window.header.align == headerRight:
        button.x = (header.w + header.x) - button.w
        if not (win.flags and windowClosable.cint).nk_bool:
          button.x -= style.window.header.padding.x
          header.w -= style.window.header.padding.x
        header.w -= button.w + style.window.header.spacing.x
      else:
        button.x = header.x
        header.x += button.w + style.window.header.spacing.x +
          style.window.header.padding.x
      if nkDoButtonSymbol(state = ws, `out` = win.buffer, bounds = button,
        symbol = if (layout.flags and windowMinimized.cint).nk_bool:
        style.window.header.maximizeSymbol else:
        style.window.header.minimizeSymbol, behavior = default,
        style = style.window.header.minimize_button, `in` = `in`,
        font = style.font) and not(win.flags and windowRom.cint).nk_bool:
          layout.flags = if (layout.flags and windowMinimized.cint).nk_bool:
            layout.flags and not windowMinimized.cint else:
            layout.flags or windowMinimized.cint

    # window header title
    var textLen: int = title.len
    let t: float = try:
        font.width(arg1 = font.userdata, h = font.height,
          arg3 = title, len = textLen)
      except Exception:
        return false
    text.padding = Vec2(x: 0, y: 0)
    var label: Rect = Rect(x: 0, y: 0, w: 0, h: 0)

    label.x = header.x + style.window.header.padding.x
    label.x += style.window.header.label_padding.x
    label.y = header.y + style.window.header.label_padding.y
    label.h = font.height + 2 * style.window.header.label_padding.y
    label.w = t + 2 * style.window.header.spacing.x
    label.w = (0.float).clamp(a = label.w, b = header.x + header.w - label.x)
    nkWidgetText(o = `out`, b = label, str = title, len = textLen,
      t = text, a = TextAlignment.left, f = font)
  return true

proc nkPanelBegin(context; title: string; panelType: PanelType): bool {.raises: [
    ], tags: [RootEffect], contractual.} =
  ## Start drawing a Nuklear panel. Internal use only
  ##
  ## * ctx       - the Nuklear context
  ## * title     - the panel's title
  ## * panelType - the type of the panel to draw
  ##
  ## Returns true if the panel was drawn, otherwise false
  body:
    zeroMem(p = context.current.layout.addr, size = Panel.sizeof)
    if (context.current.flags and windowHidden.cint) == 1 or (
        context.current.flags and windowClosed.cint) == 1:
      zeroMem(p = context.current.layout.addr, size = Panel.sizeof)
      context.current.layout.pType = panelType
      return false;
    # pull state into local stack
    let
      style: Style = context.style
      font: UserFont = style.font
    var
      win: ref Window = context.current
      layout: ref Panel = win.layout
    {.ruleOff: "varUplevel"}
    var  `out`: CommandBuffer = win.buffer
    {.ruleOn: "varUplevel"}
    var `in`: Input = (if (win.flags and windowNoInput.cint) ==
          1: Input() else: context.input)
    when defined(nkIncludeCommandUserdata):
      win.buffer.userdata = context.userdata
    # pull style configuration into local stack
    let
      scrollbarSize: Vec2 = style.window.scrollbar_size
      panelPadding: Vec2 = nkPanelGetPadding(style = style,
          pType = panelType)

    # window movement
    if (win.flags and windowMovable.cint) == 1 and (win.flags and
        windowRom.cint) != 1:
      # calculate draggable window space
      var header: Rect = Rect(x: win.bounds.x, y: win.bounds.y,
          w: win.bounds.w, h: 0)
      if nkPanelHasHeader(flags = win.flags, title = title):
        header.h = font.height + 2.0 * style.window.header.padding.y
        header.h += 2.0 * style.window.header.label_padding.y
      else:
        header.h = panelPadding.y
      # window movement by dragging
      var buttons: array[Buttons.max, MouseButton] = `in`.mouse.buttons
      let
        leftMouseDown: bool = buttons[Buttons.left].down
        leftMouseClicked: bool = buttons[Buttons.left].clicked == 1
        leftMouseClickInCursor: bool = hasMouseClickDownInRect(id = left, rect = header, down = nkTrue)
        cursors: CursorsArray = cast[CursorsArray](ctx.style.cursors)
      if leftMouseDown and leftMouseClickInCursor and not leftMouseClicked:
        win.bounds.x += `in`.mouse.delta.x
        win.bounds.y += `in`.mouse.delta.y
        buttons[Buttons.left].clicked_pos.x += `in`.mouse.delta.x
        buttons[Buttons.left].clicked_pos.y += `in`.mouse.delta.y
        ctx.style.cursor_active = cursors[cursorMove].addr
      `in`.mouse.buttons = buttons

    # setup panel
    layout.pType = panelType
    layout.flags = win.flags
    layout.bounds = win.bounds
    layout.bounds.x += panelPadding.x
    layout.bounds.w -= (2 * panelPadding.x)
    if (win.flags and windowBorder.cint).nk_bool:
      layout.border = nkPanelGetBorder(style = style, flags = win.flags, pType = panelType)
      var shrinked: Rect = Rect(x: layout.bounds.x, y: layout.bounds.y,
        w: layout.bounds.w, h: layout.bounds.h)
      shrinked = nkShrinkRect(r = shrinked, amount = layout.border)
      layout.bounds = shrinked
    else:
      layout.border = 0
    layout.at_y = layout.bounds.y
    layout.at_x = layout.bounds.x
    layout.max_x = 0
    layout.header_height = 0
    layout.footer_height = 0
    layoutResetMinRowHeight()
    layout.row.index = 0
    layout.row.columns = 0
    layout.row.ratio = 0
    layout.row.item_width = 0
    layout.row.tree_depth = 0
    layout.row.height = panelPadding.y
    layout.has_scrolling = nkTrue
    if not(win.flags and windowNoScrollbar.cint).nk_bool:
      layout.bounds.w -= scrollbarSize.x
    if nkPanelIsNonblock(`type` = panelType):
      layout.footer_height = 0
      if not(win.flags and windowNoScrollbar.cint).nk_bool or (win.flags and
        windowScalable.cint).nk_bool:
        layout.footer_height = scrollbarSize.y
      layout.bounds.h -= layout.footer_height

    # panel header
    if not panelHeader(win = win, title = title, style = style, font = font,
      layout = layout, `out` = `out`, `in` = `in`):
      return false

    # draw window background
    if not (layout.flags and windowMinimized.cint).nk_bool and not
      (layout.flags and windowDynamic.cint).nk_bool:
      var body: Rect = Rect()
      body.x = win.bounds.x
      body.w = win.bounds.w
      body.y = (win.bounds.y + layout.header_height)
      body.h = (win.bounds.h - layout.header_height)

      let bg: StyleItemData = style.window.fixedBackground.data
      case style.window.fixedBackground.iType
      of itemImage:
        nkDrawImage(b = `out`, r = body, img = bg.image,
          col = NkColor(r: 255, g: 255, b: 255, a: 255))
      of itemNineSlice:
        nkDrawNineSlice(b = `out`, r = body, slc = bg.slice,
          col = NkColor(r: 255, g: 255, b: 255, a: 255))
      of itemColor:
        nkFillRect(b = `out`, rect = body,
          rounding = style.window.rounding, c = bg.color)

    # set clipping rectangle
    var clip: Rect = Rect(x: 0, y: 0, w: 0, h: 0)
    layout.clip = layout.bounds
    let aClip: Rect = Rect(x: win.buffer.clip.x, y: win.buffer.clip.y,
      w: win.buffer.clip.w, h: win.buffer.clip.h)
    nkUnify(clip = clip, a = aClip, x0 = layout.clip.x,
      y0 = layout.clip.y, x1 = layout.clip.x + layout.clip.w,
      y1 = layout.clip.y + layout.clip.h)
    nkPushScissor(b = `out`, r = clip)
    layout.clip = clip
    return not (layout.flags and windowHidden.cint).nk_bool and not
      (layout.flags and windowMinimized.cint).nk_bool

# ------
# Popups
# ------
proc nkStartPopup(win: ref Window) {.raises: [], tags: [],
    contractual.} =
  ## Start setting a popup window. Internal use only
  ##
  ## * win     - the window of a popup
  body:
    var buf: PopupBuffer = win.popup.buf
    buf.begin = win.buffer.cmdEnd
    buf.buffEnd = win.buffer.cmdEnd
    buf.parent = win.buffer.last
    buf.last = buf.begin
    buf.active = nkTrue
    win.popup.buf = buf

proc nkPopupBegin(context; pType: PopupType; title: string; flags: set[PanelFlags];
    x, y, w, h: float): bool {.raises: [NuklearException], tags: [
        RootEffect], contractual.} =
  ## Try to create a new popup window. Internal use only.
  ##
  ## * context - the Nuklear context
  ## * pType   - the type of the popup
  ## * title   - the title of the popup
  ## * flags   - the flags for the popup
  ## * x       - the X position of the top left corner of the popup
  ## * y       - the Y position of the top left corner of the popup
  ## * w       - the width of the popup
  ## * h       - the height of the popup
  require:
    title.len > 0
  body:
    var win: ref Window = context.current
    let panel: ref Panel = win.layout
    if panel.pType.cint != panelSetPopup.cint:
      raise newException(exceptn = NuklearException,
          message = "Popups are not allowed to have popups.")
    var popup: ref Window = win.popup.win
    if popup == nil:
      popup[] = createWindow(context = context)
      popup.parent = win
      win.popup.win = popup
      win.popup.active = nkFalse
      win.popup.pType = panelPopup
    let titleHash: Hash = hash(x = title)
    # make sure we have correct popup
    if win.popup.name != titleHash.nk_hash:
      if win.popup.active:
        return false
      {.ruleOff: "namedParams".}
      nkZero(pData = popup.addr, size = Window.sizeof)
      {.ruleOn: "namedParams".}
      win.popup.name = titleHash.nk_hash
      win.popup.active = nkTrue
      win.popup.pType = panelPopup
    # popup position is local to window
    context.current = popup
    var
      localX: float = x + win.layout.clip.x
      localY: float = y + win.layout.clip.y

    # setup popup data
    popup.parent = win
    popup.bounds = Rect(x: localX, y: localY, w: w, h: h)
    popup.seq = ctx.seq
    popup.layout = nkCreatePanel(context = context)
    popup.flags = winSetToInt(nimFlags = flags)
    {.ruleOff: "assignments".}
    popup.flags = popup.flags or windowBorder.cint
    if (pType == dynamicPopup):
      popup.flags = popup.flags or windowDynamic.cint
    {.ruleOn: "assignments".}

    popup.buffer = win.buffer
    nkStartPopup(win = win)
    var allocated: nk_size = ctx.memory.allocated
    nkPushScissor(b = popup.buffer, r = nkNullRect)

    # popup is running therefore invalidate parent panels
    if nkPanelBegin(context = context, title = title, panelType = panelPopup):
      var root: ref Panel = win.layout
      while root != nil:
        root.flags = root.flags or windowRom.cint
        root.flags = root.flags and not windowRemoveRom.cint
        root = root.parent
      win.popup.active = nkTrue
      popup.layout.offset_x = popup.scrollbar.x
      popup.layout.offset_y = popup.scrollbar.y
      popup.layout.parent = win.layout
      return true

    # popup was closed/is invalid so cleanup
    var root: ref Panel = win.layout
    while root != nil:
      root.flags = root.flags or windowRemoveRom.cint
      root = root.parent
    win.popup.buf.active = nkFalse
    win.popup.active = nkFalse
    context.memory.allocated = allocated
    context.current = win
    nkFreePanel(context = context, pan = popup.layout)
    popup.layout = nil
    return false

proc createPopup(pType2: PopupType; title2: cstring;
    flags2: nk_flags; x2, y2, w2, h2: cfloat): bool {.raises: [], tags: [],
        contractual.} =
  ## Create a new Nuklear popup window, internal use only, temporary code
  ##
  ## Returns true if the popup was successfully created, otherwise false.
  proc nk_popup_begin(ctx; pType: PopupType; title: cstring;
      flags: nk_flags; rect: nk_rect): nk_bool {.importc, nodecl, raises: [],
          tags: [], contractual.}
    ## A binding to Nuklear's function. Internal use only
  return nk_popup_begin(ctx = ctx, pType = pType2, title = title2,
      flags = flags2, rect = new_nk_rect(x = x2, y = y2, w = w2, h = h2))

proc createPopup(pType2: PopupType; title2: string; flags2: set[PanelFlags];
  x2, y2, w2, h2: float): bool {.raises: [NuklearException],
  tags: [RootEffect], contractual.} =
  ## Create a new Nuklear popup window, internal use only, temporary code
  ##
  ## Returns true if the popup was successfully created, otherwise false.
  var con: ref Context = nil
  con[] = context
  return nkPopupBegin(context = con, pType = pType2, title = title2,
    flags = flags2, x = x2, y = y2, w = w2, h = h2)

proc createNonBlocking(flags2: nk_flags; x2, y2, w2, h2: cfloat): bool {.raises: [], tags: [], contractual, discardable.} =
  ## Create a new Nuklear non-blocking popup window, internal use only,
  ## temporary code
  ##
  ## Returns true if the popup is active, otherwise false.
  proc nk_nonblock_begin(ctx; flags: nk_flags; body, header: nk_rect, panelType: PanelType): nk_bool
    {.importc, nodecl, raises: [], tags: [], contractual.}
    ## A binding to Nuklear's function. Internal use only
  return nk_nonblock_begin(ctx = ctx, flags = flags2, body = new_nk_rect(x = x2, y = y2, w = w2, h = h2),
    header = new_nk_rect(x = 0, y = 0, w = 0, h = 0), panel_type = panelPopup)

template popup*(pType: PopupType; title: string; flags: set[PanelFlags]; x,
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

template nonBlocking*(flags: set[PanelFlags]; x, y, w, h: float; content: untyped) =
  ## Create a new Nuklear non-blocking popup window with the selected content
  ##
  ## * flags   - the flags for the popup
  ## * x       - the X position of the top left corner of the popup
  ## * y       - the Y position of the top left corner of the popup
  ## * w       - the width of the popup
  ## * h       - the height of the popup
  ## * content - the code executed when the button is pressed
  discard createNonBlocking(flags2 = winSetToInt(nimFlags = flags),
    x2 = x.cfloat, y2 = y, w2 = w, h2 = h)
  content
  ctx.nk_popup_end

proc closePopup*() {.raises: [], tags: [], contractual.} =
  ## Close the last popup window
  proc nk_popup_close(ctx) {.importc, nodecl, raises: [], tags: [], contractual.}
    ## A binding to Nuklear's function. Internal use only
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
  if ctx.nk_tree_state_push(ttype = node, ctitle = title.cstring,
      cstate = state):
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
  if ctx.nk_tree_state_push(ttype = tab, ctitle = title.cstring,
      cstate = state):
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
proc colorLabel*(str: string; color: Color; align: TextAlignment = left) {.raises: [], tags: [], contractual.} =
  ## Draw a text with the selected color
  ##
  ## * str   - the text to display
  ## * color - the color of the text
  ## * align - the text aligmnent flags
  proc nk_label_colored(ctx; str: cstring; align: nk_flags;
      color: nk_color) {.importc, nodecl, raises: [], tags: [], contractual.}
    ## A binding to Nuklear's function. Internal use only
  var (r, g, b) = color.extractRGB
  nk_label_colored(ctx = ctx, str = str.cstring, align = align.nk_flags,
      color = nk_rgb(r = r.cint, g = g.cint, b = b.cint))

proc colorLabel*(str: string; color, background: Color; align: TextAlignment = left) {.raises: [], tags: [], contractual.} =
  ## Draw a text with the selected color and background
  ##
  ## * str        - the text to display
  ## * color       - the color of the text
  ## * background - the color of the text's background
  ## * align      - the text aligmnent flags
  proc nk_label_colored2(ctx; str: cstring; align: nk_flags;
      color, color2: nk_color) {.importc, nodecl, raises: [], tags: [], contractual.}
    ## A binding to Nuklear's function. Internal use only
  var
    (r, g, b) = color.extractRGB
    (r2, g2, b2) = background.extractRGB
  nk_label_colored2(ctx = ctx, str = str.cstring, align = align.nk_flags,
      color = nk_rgb(r = r.cint, g = g.cint, b = b.cint),
      color2 = nk_rgb(r = r2.cint, g = g2.cint, b = b2.cint))

proc colorLabel*(str: string; background: Color; align: TextAlignment = left) {.raises: [], tags: [], contractual.} =
  ## Draw a text with the selected background color
  ##
  ## * str        - the text to display
  ## * background - the color of the text's background
  ## * align      - the text aligmnent flags
  proc nk_label_colored3(ctx; str: cstring; align: nk_flags;
      color: nk_color) {.importc, nodecl, raises: [], tags: [], contractual.}
    ## A binding to Nuklear's function. Internal use only
  var (r, g, b) = background.extractRGB
  nk_label_colored3(ctx = ctx, str = str.cstring, align = align.nk_flags,
      color = nk_rgb(r = r.cint, g = g.cint, b = b.cint))

proc label*(str: string; alignment: TextAlignment = left) {.raises: [], tags: [
    ], contractual.} =
  ## Draw the text with the selected alignment
  ##
  ## * str       - the text to draw
  ## * alignment - the alignment of the text. Default is alignment to the left
  proc nk_label(ctx; str: cstring; alignment: nk_flags) {.importc, nodecl,
      raises: [], tags: [], contractual.}
    ## A binding to Nuklear's function. Internal use only
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
    ## A binding to Nuklear's function. Internal use only
  nk_text(ctx = ctx, str = str.cstring, len = len.cint,
      alignment = alignment.nk_flags)

proc wrapLabel*(str: string) {.raises: [], tags: [], contractual.} =
  ## Draw a text and wrap it if its lentgh is bigger than the width of its
  ## container
  ##
  ## * str - the text to draw
  proc nk_label_wrap(ctx; str: cstring) {.importc, nodecl, raises: [], tags: [], contractual.}
    ## A binding to Nuklear's function. Internal use only
  nk_label_wrap(ctx = ctx, str = str.cstring)

proc colorWrapLabel*(str: string; color: Color) {.raises: [], tags: [], contractual.} =
  ## Draw a text and wrap it if its lentgh is bigger than the width of its
  ## container
  ##
  ## * str - the text to draw
  proc nk_label_colored_wrap(ctx; str: cstring; color: nk_color) {.importc, nodecl, raises: [], tags: [], contractual.}
    ## A binding to Nuklear's function. Internal use only
  var (r, g, b) = color.extractRGB
  nk_label_colored_wrap(ctx = ctx, str = str.cstring, color = nk_rgb(r = r.cint, g = g.cint, b = b.cint))

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
proc createColorButton(r1, g1, b1: cint): bool {.raises: [], tags: [],
    contractual.} =
  ## Draw a button with the selected color background, internal use only, temporary code
  ##
  ## * r1   - the red value for the button color in RGB
  ## * g1   - the green value for the button color in RGB
  ## * b1   - the blue value for the button color in RGB
  ##
  ## Returns true if button was pressed
  proc nk_button_color(ctx; color: nk_color): nk_bool {.importc, nodecl,
      raises: [], tags: [], contractual.}
    ## A binding to Nuklear's function. Internal use only
  return nk_button_color(ctx = ctx, color = nk_rgb(r = r1, g = g1, b = b1))

template colorButton*(r, g, b: int; onPressCode: untyped) =
  ## Draw a button with the selected color background. Execute the selected code
  ## on pressing it.
  ##
  ## * r           - the red value for the button color in RGB
  ## * g           - the green value for the button color in RGB
  ## * b           - the blue value for the button color in RGB
  ## * onPressCode - the Nim code to execute when the button was pressed
  if createColorButton(r1 = r.cint, g1 = g.cint, b1 = b.cint):
    onPressCode

proc createImageButton(img: PImage): bool {.raises: [], tags: [],
    contractual.} =
  ## Draw the button with the selected image, internal use only, temporary code
  ##
  ## * image - the image to shown on the button
  ##
  ## Returns true if button was created, otherwise false
  proc nk_button_image(ctx; image: nk_image): nk_bool {.importc, nodecl,
      raises: [], tags: [], contractual.}
    ## A binding to Nuklear's function. Internal use only
  return nk_button_image(ctx = ctx, image = nk_image_ptr(iPtr = img))

template imageButton*(image: PImage; onPressCode: untyped) =
  ## Draw the button with the selected image. Execute the selected code
  ## on pressing it.
  ##
  ## * image       - the image to shown on the button
  ## * onPressCode - the Nim code to execute when the button was pressed
  ##
  ## Returns true if button was pressed
  if createImageButton(img = image):
    onPressCode

proc createImageButtonCentered(img: PImage): bool {.raises: [], tags: [],
    contractual.} =
  ## Draw the button with the selected image, internal use only, temporary code
  ##
  ## * image - the image to shown on the button
  ##
  ## Returns true if button was created, otherwise false
  proc nk_button_image_centered(ctx; image: nk_image): nk_bool {.importc, nodecl,
      raises: [], tags: [], contractual.}
    ## A binding to Nuklear's function. Internal use only
  return nk_button_image_centered(ctx = ctx, image = nk_image_ptr(iPtr = img))

template imageButtonCentered*(image: PImage; onPressCode: untyped) =
  ## Draw the button with the selected image. Execute the selected code
  ## on pressing it.
  ##
  ## * image       - the image to shown on the button
  ## * onPressCode - the Nim code to execute when the button was pressed
  ##
  ## Returns true if button was pressed
  if createImageButtonCentered(img = image):
    onPressCode

proc createStyledImageButton(img: PImage; bStyle: ButtonStyle): bool {.raises: [
    ], tags: [], contractual.} =
  ## Draw the button with the selected image, internal use only, temporary code
  ##
  ## * image - the image to shown on the button
  ## * bStyle - the button's style settings
  ##
  ## Returns true if button was created, otherwise false
  var buttonStyle: nk_style_button = ctx.style.button
  buttonStyle.border_color = nk_rgb(r = bStyle.borderColor.r.cint,
      g = bStyle.borderColor.g.cint, b = bStyle.borderColor.b.cint)
  buttonStyle.rounding = bStyle.rounding.cfloat
  buttonStyle.padding = new_nk_vec2(x = bStyle.padding.x, y = bStyle.padding.y)
  buttonStyle.image_padding = new_nk_vec2(x = bStyle.imagePadding.x,
      y = bStyle.imagePadding.y)
  proc nk_button_image_styled(ctx; style: var nk_style_button;
      image: nk_image): nk_bool {.importc, nodecl, raises: [], tags: [], contractual.}
    ## A binding to Nuklear's function. Internal use only
  return nk_button_image_styled(ctx = ctx, style = buttonStyle,
      image = nk_image_ptr(iPtr = img))

template imageButtonStyled*(image: PImage; style: ButtonStyle;
    onPressCode: untyped) =
  ## Draw the button with the selected image. Execute the selected code
  ## on pressing it.
  ##
  ## * image       - the image to shown on the button
  ## * style       - the style used to draw the button
  ## * onPressCode - the Nim code to execute when the button was pressed
  if createStyledImageButton(img = image, bStyle = style):
    onPressCode

proc createImageLabelButton(img: PImage; txt: string; align: TextAlignment): bool {.raises: [], tags: [],
    contractual.} =
  ## Draw the button with the selected image and text, internal use only, temporary code
  ##
  ## * image - the image to show on the button
  ## * text  - the text to show on the button
  ## * align - the alignment of the text to show
  ##
  ## Returns true if button was created, otherwise false
  proc nk_button_image_label(ctx; image: nk_image; text: cstring; textAlignment: nk_flags): nk_bool {.importc, nodecl,
      raises: [], tags: [], contractual.}
    ## A binding to Nuklear's function. Internal use only
  return nk_button_image_label(ctx = ctx, image = nk_image_ptr(iPtr = img), text = txt.cstring, text_alignment = align.nk_flags)

template imageLabelButton*(image: PImage; text: string; alignment: TextAlignment; onPressCode: untyped) =
  ## Draw the button with the selected image and text. Execute the selected code
  ## on pressing it.
  ##
  ## * image       - the image to shown on the button
  ## * text        - the text to show on the button
  ## * align       - the alignment of the text to show
  ## * onPressCode - the Nim code to execute when the button was pressed
  ##
  ## Returns true if button was pressed
  if createImageLabelButton(img = image, txt = text, align = alignment):
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
    ## A binding to Nuklear's function. Internal use only
  return nk_slide_int(ctx = ctx, min = min.cint, val = val.cint, max = max.cint,
      step = step.cint).int

# -------
# Layouts
# -------
proc layoutSpacePush(ctx; x1, y1, w1, h1: cfloat) {.raises: [], tags: [],
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
    ## A binding to Nuklear's function. Internal use only
  nk_layout_space_push(ctx = ctx, rect = new_nk_rect(x = x1, y = y1, w = w1, h = h1))

template row*(x, y, w, h: float; content: untyped) =
  ## Set the content of the row in the current widgets layout, used in space
  ## layout
  ##
  ## * x       - the amount of pixels or ratio to push the position in X axis
  ## * y       - the amount of pixels or ratio to push the position in Y axis
  ## * w       - the amount of pixels or ratio to push the width
  ## * h       - the amount of pixels or ratio to push the height
  ## * content - the content of the row
  layoutSpacePush(ctx = ctx, x1 = x.cfloat, y1 = y.cfloat, w1 = w.cfloat, h1 = h.cfloat)
  content

template setRowTemplate*(height: float; settings: untyped) =
  ## Set the options for the row's template setting for the next rows
  ##
  ## * height   - the height of the each row
  ## * settings - the template settings
  nk_layout_row_template_begin(ctx = ctx, cheight = height.cfloat)
  settings
  nk_layout_row_template_end(ctx = ctx)

proc rowTemplateDynamic*() {.raises: [], tags: [], contractual.} =
  ## Set the selected column's in the row width in the template's row as dynamic,
  ## which means, the widget will resize with its parent.
  proc nk_layout_row_template_push_dynamic(ctx) {.importc, nodecl, raises: [],
      tags: [], contractual.}
    ## A binding to Nuklear's function. Internal use only
  nk_layout_row_template_push_dynamic(ctx = ctx)

proc rowTemplateVariable*(minWidth: float) {.raises: [], tags: [],
    contractual.} =
  ## Set the selected column's width in the row template as dynamic but with
  ## requirement for minumum width for the widget
  ##
  ## * minWidth - the minimum width in pixels for the widgets in the column
  proc nk_layout_row_template_push_variable(ctx; minWidth: cfloat) {.importc,
      nodecl, raises: [], tags: [], contractual.}
    ## A binding to Nuklear's function. Internal use only
  nk_layout_row_template_push_variable(ctx = ctx, minWidth = minWidth.cfloat)

proc rowTemplateStatic*(width: float) {.raises: [], tags: [], contractual.} =
  ## Set the selected column's width in the row template to static value,
  ## widgets in the column will not resize
  ##
  ## * width - the width of the column in the row template
  proc nk_layout_row_template_push_static(ctx; width: cfloat) {.importc, nodecl,
      raises: [], tags: [], contractual.}
    ## A binding to Nuklear's function. Internal use only
  nk_layout_row_template_push_static(ctx = ctx, width = width.cfloat)

# -----
# Menus
# -----
template menuBar*(content: untyped) =
  ## Create a menu bar with the selected content
  ##
  ## * content - the content of the menu bar
  nk_menubar_begin(ctx = ctx)
  content
  nk_menubar_end(ctx = ctx)

proc createMenu(ctx; text1: cstring; align1: nk_flags; x1,
    y1: cfloat): bool {.raises: [], tags: [], contractual.} =
  ## Create a Nuklear menu, internal use only, temporary code
  ##
  ## Returns true if the popup was successfully created, otherwise false.
  ##
  ## * ctx    - the Nuklear context
  ## * text1  - the label for the menu
  ## * align1 - the menu alignment
  ## * x1     - the X position of the top left corner of the menu
  ## * y1     - the Y position of the top left corner of the menu
  ##
  ## Returns true if menu were created, otherwise false
  proc nk_menu_begin_label(ctx; text: cstring; align: nk_flags;
      size: nk_vec2): nk_bool {.importc, nodecl, raises: [], tags: [], contractual.}
    ## A binding to Nuklear's function. Internal use only
  let newSize: nk_vec2 = new_nk_vec2(x = x1, y = y1)
  return nk_menu_begin_label(ctx = ctx, text = text1, align = align1,
      size = newSize)

template menu*(text: string; align: TextAlignment; x, y: float;
    content: untyped) =
  ## Create a Nuklear menu
  ##
  ## * text    - the label for the menu
  ## * align   - the menu alignment
  ## * x       - the X position of the top left corner of the menu
  ## * y       - the Y position of the top left corner of the menu
  ## * content - the content of the menu
  if createMenu(ctx = ctx, text1 = text.cstring, align1 = align.nk_flags,
      x1 = x, y1 = y):
    content
    nk_menu_end(ctx = ctx)

template menuItem*(label: string; align: TextAlignment; onPressCode: untyped) =
  ## Create a Nuklear menu's item. Execute the selected code when the user
  ## select the item from a menu.
  ##
  ## * label       - the label of the item
  ## * align       - the alignment of the item's label
  ## * onPressCode - the code executed when the menu was selected by the user
  if nk_menu_item_label(ctx = ctx, ctext = label.cstring,
      aligmnent = align.nk_flags):
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
    ## A binding to Nuklear's function. Internal use only
  var newVal: cint = val.cint
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
    ## A binding to Nuklear's function. Internal use only
  var newVal: cfloat = val.cfloat
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
    ## A binding to Nuklear's function. Internal use only
  var newVal: cint = val.cint
  nk_property_int(ctx = ctx, name = name.cstring, min = min.cint, val = newVal,
      max = max.cint, step = step.cint, incPerPixel = incPerPixel.cfloat)
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
    ## A binding to Nuklear's function. Internal use only
  var newVal: cfloat = val.cfloat
  nk_property_float(ctx = ctx, name = name.cstring, min = min.cfloat,
      val = newVal, max = max.cfloat, step = step.cfloat,
      incPerPixel = incPerPixel.cfloat)
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
    ## A binding to Nuklear's function. Internal use only
  return nk_propertyf(ctx = ctx, name = name.cstring, min = min.cfloat,
      val = val.cfloat, max = max.cfloat, step = step.cfloat,
      incPerPixel = incPerPixel.cfloat).float

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
    ## A binding to Nuklear's function. Internal use only
  return nk_propertyi(ctx = ctx, name = name.cstring, min = min.cint,
      val = val.cint, max = max.cint, step = step.cint,
      incPerPixel = incPerPixel.cfloat).int

# -----
# Style
# -----
proc headerAlign*(value: StyleHeaderAlign) {.raises: [], tags: [],
    contractual.} =
  ## Set the Nuklear windows header alignment
  ##
  ## * value - the new value for the alignment
  ctx.style.window.header.align = value.ord.StyleHeaderAlign

var buttonStyle: nk_style_button = nk_style_button() ## Used to store the Nuklear buttons style

proc saveButtonStyle*() {.raises: [], tags: [], contractual.} =
  ## Save the Nuklear buttons style to variable, so it can be restored later
  buttonStyle = ctx.style.button

proc restoreButtonStyle*() {.raises: [], tags: [], contractual.} =
  ## Restore previously save to the variable Nuklear buttons style
  ##
  ctx.style.button = buttonStyle

proc setButtonStyle*(field: ButtonStyleTypes; color: Color = colWhite; a: Natural = 255) {.raises: [], tags: [], contractual.} =
  ## Set the color for the selected field of the Nuklear buttons style
  ##
  ## * field - the style's field which value will be changed
  ## * color - the new value for the field
  ## * a     - the intensity (alpha) of the color from 0 to 255
  let (r, g, b) = color.extractRGB
  case field
  of normal:
    ctx.style.button.normal = nk_style_item_color(col = nk_rgba(r = r, g = g,
        b = b, a = a.cint))
  of hover:
    ctx.style.button.hover = nk_style_item_color(col = nk_rgba(r = r, g = g,
        b = b, a = a.cint))
  of active:
    ctx.style.button.active = nk_style_item_color(col = nk_rgba(r = r, g = g,
        b = b, a = a.cint))
  of borderColor:
    ctx.style.button.border_color = nk_rgba(r = r, g = g, b = b, a = a.cint)
  of textBackground:
    ctx.style.button.text_background = nk_rgba(r = r, g = g, b = b, a = a.cint)
  of textNormal:
    ctx.style.button.text_normal = nk_rgba(r = r, g = g, b = b, a = a.cint)
  of textHover:
    ctx.style.button.text_hover = nk_rgba(r = r, g = g, b = b, a = a.cint)
  of textActive:
    ctx.style.button.text_active = nk_rgba(r = r, g = g, b = b, a = a.cint)
  else:
    discard

proc setButtonStyle*(field: ButtonStyleTypes; value: Vec2) {.raises: [],
    tags: [], contractual.} =
  ## Set the vector for the selected field of the Nuklear buttons style
  ##
  ## * field - the style's field which value will be changed
  ## * value - the new value for the style's field
  case field
  of padding:
    ctx.style.button.padding = new_nk_vec2(x = value.x.cfloat,
        y = value.y.cfloat)
  of imagePadding:
    ctx.style.button.image_padding = new_nk_vec2(x = value.x.cfloat,
        y = value.y.cfloat)
  of touchPadding:
    ctx.style.button.touch_padding = new_nk_vec2(x = value.x.cfloat,
        y = value.y.cfloat)
  else:
    discard

proc setButtonStyle*(field: ButtonStyleTypes; value: float) {.raises: [],
    tags: [], contractual.} =
  ## Set the float for the selected field of the Nuklear buttons style
  ##
  ## * field - the style's field which value will be changed
  ## * value - the new value for the style's field
  case field
  of rounding:
    ctx.style.button.rounding = value.cfloat
  of border:
    ctx.style.button.border = value.cfloat
  of colorFactorBackground:
    ctx.style.button.color_factor_background = value.cfloat
  of colorFactorText:
    ctx.style.button.color_factor_text = value.cfloat
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

proc getButtonStyle*(field: ButtonStyleTypes): Vec2 {.raises: [], tags: [],
    contractual.} =
  ## Get the value of the selected field of Nuklear buttons style
  ##
  ## * field - the field which value will be taken
  ##
  ## Returns vector with the value of the selected field
  if field == padding:
    return Vec2(x: ctx.style.button.padding.x, y: ctx.style.button.padding.y)

proc stylePushVec2(fld: WindowStyleTypes; x1,
    y1: cfloat): bool {.discardable, raises: [], tags: [], contractual.} =
  ## Push the vector value for the selected Nuklear window style on a
  ## temporary stack
  ##
  ## * fld - the Nuklear windows style field which will be modified
  ## * x1  - the X value of the vector to push
  ## * y1  - the Y value of the vector to push
  ##
  ## Returns true if value was succesfully pushed, otherwise false
  proc nk_style_push_vec2(ctx; dest: var nk_vec2;
      source: nk_vec2): nk_bool {.importc, nodecl, raises: [], tags: [], contractual.}
    ## A binding to Nuklear's function. Internal use only
  if fld == spacing:
    return nk_style_push_vec2(ctx = ctx, dest = ctx.style.window.spacing,
        source = new_nk_vec2(x = x1, y = y1))
  elif fld == padding:
    return nk_style_push_vec2(ctx = ctx, dest = ctx.style.window.padding,
        source = new_nk_vec2(x = x1, y = y1))

proc stylePushFloat(fld: FloatStyleTypes;
    val: cfloat): bool {.raises: [], tags: [], contractual.} =
  ## Push the float value for the selected Nuklear buttons style on a
  ## temporary stack
  ##
  ## * fld - the Nuklear buttons style field which will be modified
  ## * val - the float value to push
  ##
  ## Returns true if value was succesfully pushed, otherwise false
  proc nk_style_push_float(ctx; dest: var cfloat;
      source: cfloat): nk_bool {.importc, nodecl, raises: [], tags: [], contractual.}
    ## A binding to Nuklear's function. Internal use only
  if fld == buttonRounding:
    return nk_style_push_float(ctx = ctx, dest = ctx.style.button.rounding,
        source = val)
  elif fld == popupBorder:
    return nk_style_push_float(ctx = ctx, dest = ctx.style.window.popup_border,
        source = val)

proc stylePushColor(fld: ColorStyleTypes; col: Color): bool {.raises: [], tags: [], contractual.} =
  ## Push the color value for the selected Nuklear window style on a
  ## temporary stack
  ##
  ## * fld - the Nuklear windows style field which will be modified
  ## * col - the new color for the selected field
  ##
  ## Returns true if value was succesfully pushed, otherwise false
  proc nk_style_push_color(ctx; dest: var nk_color;
      source: nk_color): nk_bool {.importc, nodecl, raises: [], tags: [], contractual.}
    ## A binding to Nuklear's function. Internal use only
  let (r, g, b) = col.extractRGB()
  if fld == background:
    return nk_style_push_color(ctx = ctx, dest = ctx.style.window.background,
      source = nk_rgb(r = r.cint, g = g.cint, b = b.cint))

proc stylePushStyleItem(fld: StyleStyleTypes; col: Color): bool {.raises: [], tags: [], contractual.} =
  ## Push the color value for the selected Nuklear window style on a
  ## temporary stack
  ##
  ## * fld - the Nuklear windows style field which will be modified
  ## * col - the new color for the selected field
  ##
  ## Returns true if value was succesfully pushed, otherwise false
  proc nk_style_push_style_item(ctx; dest: var nk_style_item; source: nk_style_item):
    nk_bool {.importc, nodecl, raises: [], tags: [], contractual.}
    ## A binding to Nuklear's function. Internal use only
  let (r, g, b) = col.extractRGB()
  if fld == progressbar:
    return nk_style_push_style_item(ctx = ctx, dest = ctx.style.progress.cursor_normal,
      source = nk_style_item_color(col = nk_rgb(r = r.cint, g = g.cint, b = b.cint)))

proc styleFromTable*(table: openArray[NkColor]) {.raises: [], tags: [],
    contractual.} =
  ## Set the Nuklear style colors from the table
  ##
  ## * table - the colors table which will be set
  proc nk_style_from_table(ctx; table: pointer) {.importc, nodecl, raises: [],
      tags: [], contractual.}
    ## A binding to Nuklear's function. Internal use only
  {.ruleOff: "varDeclared".}
  var newTable: array[countColors.ord, nk_color]
  {.ruleOn: "varDeclared".}
  for index, color in table:
    newTable[index] = nk_rgba(r = color.r.cint, g = color.g.cint,
        b = color.b.cint, a = color.a.cint)
  nk_style_from_table(ctx = ctx, table = newTable.addr)

proc defaultStyle*() {.raises: [], tags: [], contractual.} =
  ## Reset the UI colors to the default Nuklear setting
  proc nk_style_default(ctx) {.importc, nodecl, raises: [], tags: [], contractual.}
    ## A binding to Nuklear's function. Internal use only
  nk_style_default(ctx = ctx)

proc stylePopFloat() {.raises: [], tags: [], contractual.} =
  ## Reset the UI float setting to the default Nuklear setting
  proc nk_style_pop_float(ctx) {.importc, nodecl, raises: [], tags: [], contractual.}
    ## A binding to Nuklear's function. Internal use only
  nk_style_pop_float(ctx = ctx)

proc stylePopVec2() {.raises: [], tags: [], contractual.} =
  ## reset the UI vector setting to the default Nuklear setting
  proc nk_style_pop_vec2(ctx) {.importc, nodecl, raises: [], tags: [], contractual.}
    ## A binding to Nuklear's function. Internal use only
  nk_style_pop_vec2(ctx = ctx)

proc stylePopColor() {.raises: [], tags: [], contractual.} =
  ## reset the UI color setting to the default Nuklear setting
  proc nk_style_pop_color(ctx) {.importc, nodecl, raises: [], tags: [], contractual.}
    ## A binding to Nuklear's function. Internal use only
  nk_style_pop_color(ctx = ctx)

proc stylePopStyleItem() {.raises: [], tags: [], contractual.} =
  ## reset the UI color setting to the default Nuklear setting
  proc nk_style_pop_style_item(ctx) {.importc, nodecl, raises: [], tags: [], contractual.}
    ## A binding to Nuklear's function. Internal use only
  nk_style_pop_style_item(ctx = ctx)

template changeStyle*(field: WindowStyleTypes; x, y: float; code: untyped) =
  ## Change temporary the vector value for the selected Nuklear window style
  ##
  ## * field - the Nuklear windows style field which will be modified
  ## * x     - the X value of the vector to push
  ## * y     - the Y value of the vector to push
  if stylePushVec2(fld = field, x1 = x.cfloat, y1 = y.cfloat):
    code
    stylePopVec2()

template changeStyle*(field: FloatStyleTypes; value: float; code: untyped) =
  ## Change temporary the float value for the selected Nuklear style
  ##
  ## * field - the Nuklear buttons style field which will be modified
  ## * value - the float value to push
  ## * code  - the code executed when the value will be properly set
  if stylePushFloat(fld = field, val = value.cfloat):
    code
    stylePopFloat()

template changeStyle*(field: ColorStyleTypes; color: Color; code: untyped) =
  ## Change temporary the color value for the selected Nuklear element
  ##
  ## * field - the Nuklear windows style field which will be modified
  ## * color - the new color for the selected field
  ## * code  - the code executed when the color will be properly set
  if stylePushColor(fld = field, col = color):
    code
    stylePopColor()

template changeStyle*(field: StyleStyleTypes; color: Color; code: untyped) =
  ## Change temporary the color value for the selected Nuklear style's item
  ##
  ## * field - the Nuklear windows style field which will be modified
  ## * color - the new color for the selected field
  ## * code  - the code executed when the color will be properly set
  if stylePushStyleItem(fld = field, col = color):
    code
    stylePopStyleItem()

var storedButton: nk_style_button = nk_style_button() ## Used to store temporary button's setting

proc storeButton() {.raises: [], tags: [], contractual.} =
  ## Store the current setting for buttons
  storedButton = ctx.style.button

proc restoreButtonStyle(destination: ButtonStyleTypes) {.raises: [], tags: [], contractual.} =
  ## Restore default setting for the selected field in the button's style
  ##
  ## * destination - the field in the style which will be restored
  if destination == normal:
    ctx.style.button.normal = storedButton.normal

template changeStyle*(src, dest: ButtonStyleTypes; code: untyped) =
  ## Change temporary the setting of the selected button style
  ##
  ## * src      - the field which value will be copied
  ## * dest     - the field to which the source value will be copied
  ## * code     - the code executed when the temporary setting is set
  storeButton()
  setButtonStyle2(source = src, destination = dest)
  code
  restoreButtonStyle(destination = dest)

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
    ## A binding to Nuklear's function. Internal use only
  var optionsList: seq[cstring] = @[]
  for i in 0..amount:
    optionsList.add(y = items[i].cstring)
  return nk_combo(ctx = ctx, items = optionsList[0].addr, count = amount.cint +
      1, selected = selected.cint, itemHeight = itemHeight.cint,
          size = new_nk_vec2(x = x.cfloat, y = y.cfloat)).int

proc createColorCombo(ctx; color1: NkColor; x1, y1: cfloat): bool {.raises: [],
    tags: [], contractual.} =
  ## Create a Nuklear combo widget which display color as the value, internal
  ## use only, temporary code
  ##
  ## * ctx    - the Nuklear context
  ## * color1 - the color displayed as the value of the combo
  ## * x1     - the width of the combo
  ## * y1     - the height of the combo's values list
  ##
  ## Returns true if combo was successfully created, otherwise false
  return nk_combo_begin_color(ctx = ctx, color = nk_rgb(r = color1.r.cint,
      g = color1.g.cint, b = color1.b.cint), size = new_nk_vec2(x = x1, y = y1))

template colorCombo*(color: NkColor; x, y: float; content: untyped) =
  ## Create a Nuklear combo widget which display color as the value, internal
  ## use only, temporary code
  ##
  ## * color   - the color displayed as the value of the combo
  ## * x       - the width of the combo
  ## * y       - the height of the combo's values list
  ## * content - the content of the combo widget
  if createColorCombo(ctx = ctx, color1 = color, x1 = x.cfloat, y1 = y.cfloat):
    content
    nk_combo_end(ctx = ctx)

proc createColorCombo(ctx; color1: NkColorF; x1, y1: cfloat): bool {.raises: [
    ], tags: [], contractual.} =
  ## Create a Nuklear combo widget which display color with float values as
  ## the value, internal use only, temporary code
  ##
  ## * ctx    - the Nuklear context
  ## * color1 - the color with float values displayed as the value of the combo
  ## * x1     - the width of the combo
  ## * y1     - the height of the combo's values list
  ##
  ## Returns true if combo was successfully created, otherwise false
  return nk_combo_begin_color(ctx = ctx, color = nk_rgb_cf(c = nk_colorf(
      r: color1.r.cfloat, g: color1.g.cfloat, b: color1.b.cfloat,
          a: color1.a.cfloat)), size = new_nk_vec2(x = x1, y = y1))


template colorCombo*(color: NkColorF; x, y: float; content: untyped) =
  ## Create a Nuklear combo widget which display color with float values as
  ## the value
  ##
  ## * color   - the color with float values displayed as the value of the combo
  ## * x       - the width of the combo
  ## * y       - the height of the combo's values list
  ## * content - the content of the combo widget
  if createColorCombo(ctx = ctx, color1 = color, x1 = x.cfloat, y1 = y.cfloat):
    content
    nk_combo_end(ctx = ctx)

proc createLabelCombo(ctx; selected1: cstring; x1, y1: cfloat): bool {.raises: [
    ], tags: [], contractual.} =
  ## Create a Nuklear combo widget which display the custom text as the value,
  ## internal use only, temporary code
  ##
  ## * ctx       - the Nuklear context
  ## * selected1 - the text to display as the value of the combo
  ## * x1        - the width of the combo
  ## * y1        - the height of the combo's values list
  ##
  ## Returns true if combo was successfully created, otherwise false
  proc nk_combo_begin_label(ctx; selected: cstring;
      size: nk_vec2): nk_bool {.importc, nodecl, raises: [], tags: [], contractual.}
    ## A binding to Nuklear's function. Internal use only
  return nk_combo_begin_label(ctx = ctx, selected = selected1,
      size = new_nk_vec2(x = x1, y = y1))

template labelCombo*(selected: string; x, y: float; content: untyped) =
  ## Create a Nuklear combo widget which display the custom text as the value
  ##
  ## * selected - the text to display as the value of the combo
  ## * x        - the width of the combo
  ## * y        - the height of the combo's values list
  ## * content - the content of the combo widget
  if createLabelCombo(ctx = ctx, selected1 = selected.cstring, x1 = x.cfloat,
      y1 = y.cfloat):
    content
    nk_combo_end(ctx = ctx)

proc comboClose*() {.raises: [], tags: [], contractual.} =
  ## Stop adding a value to a combo
  proc nk_combo_close(ctx) {.importc, nodecl, raises: [], tags: [], contractual.}
    ## A binding to Nuklear's function. Internal use only
  nk_combo_close(ctx = ctx)

# ------
# Charts
# ------
proc createColorChart(ctx; ctype1: ChartType; color1,
    higlight1: NkColor; count1: cint; minValue1,
        maxValue1: cfloat): bool {.raises: [], tags: [], contractual.} =
  ## Create a colored chart, internal use only, temporary code
  ##
  ## * ctx        - the Nuklear context
  ## * ctype1     - the type of the chart
  ## * color1     - the color used for drawing the chart
  ## * highligh1  - the color used for highlighting point when mouse hovering
  ##                over it
  ## * count1     - the amount of values on the chart
  ## * min_value1 - the minimal value of the chart
  ## * max_value1 - the maximum value of the chart
  ##
  ## Returns true if the chart was succesfully created otherwise false
  proc nk_chart_begin_colored(ctx; ctype: ChartType; color,
      higlight: nk_color; count: cint; minValue,
      maxValue: cfloat): nk_bool {.importc, nodecl, raises: [], tags: [], contractual.}
    ## A binding to Nuklear's function. Internal use only
  return nk_chart_begin_colored(ctx = ctx, ctype = ctype1, color = nk_rgb(
      r = color1.r.cint, g = color1.g.cint, b = color1.b.cint),
      higlight = nk_rgb(r = higlight1.r.cint, g = higlight1.g.cint,
      b = higlight1.b.cint), count = count1, minValue = minValue1,
      maxValue = maxValue1)

template colorChart*(cType: ChartType; color, highlight: NkColor; count: int;
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
  if createColorChart(ctx = ctx, ctype1 = cType, color1 = color,
      higlight1 = highlight, count1 = count.cint, minValue1 = minValue.cfloat,
      maxValue1 = maxValue.cfloat):
    content
    nk_chart_end(ctx = ctx)

proc addColorChartSlot*(ctype: ChartType; color,
    highlight: NkColor; count: cint; minValue, maxValue: cfloat) {.raises: [],
        tags: [], contractual.} =
  ## Add another chart to the existing one
  ##
  ## * ctype     - the type of the chart
  ## * color     - the color used for drawing the chart
  ## * highlight - the color used for highlighting point when mouse hovering
  ##               over it
  ## * count     - the amount of values on the chart
  ## * min_value - the minimal value of the chart
  ## * max_value - the maximum value of the chart
  proc nk_chart_add_slot_colored(ctx; ctype: ChartType; color,
      higlight: nk_color; count: cint; minValue, maxValue: cfloat) {.importc,
          nodecl, raises: [], tags: [], contractual.}
    ## A binding to Nuklear's function. Internal use only
  nk_chart_add_slot_colored(ctx = ctx, ctype = ctype, color = nk_rgb(
      r = color.r.cint, g = color.g.cint, b = color.b.cint), higlight = nk_rgb(
      r = highlight.r.cint, g = highlight.g.cint, b = highlight.b.cint),
      count = count, minValue = minValue, maxValue = maxValue)

template chart*(cType: ChartType; num: int; min, max: float; content: untyped) =
  ## Create a chart of the selected type
  ##
  ## * cType   - the type of the chart
  ## * num     - the amount of values in the chart
  ## * min     - the minimum value on the chart
  ## * max     - the maximum value on the chart
  ## * content - the content of the chart, usually coe related to adding values
  if nk_chart_begin(ctx = ctx, ctype1 = cType, num1 = num.cint,
      min1 = min.cfloat, max1 = max.cfloat):
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
    ## A binding to Nuklear's function. Internal use only

  let res: nk_flags = nk_chart_push(ctx = ctx, value = value.cfloat)
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
    ## A binding to Nuklear's function. Internal use only
  nk_chart_add_slot(ctx = ctx, ctype = ctype, count = count.cint,
      minValue = minValue.cfloat, maxValue = maxValue.cfloat)

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
    ## A binding to Nuklear's function. Internal use only

  let res: nk_flags = nk_chart_push_slot(ctx = ctx, value = value.cfloat,
      slot = slot.cint)
  if (res and clicked.nk_flags) == clicked.nk_flags:
    return clicked
  if (res and hovering.nk_flags) == hovering.nk_flags:
    return hovering
  return none

# ----------
# Contextual
# ----------
proc createContextual(ctx; flags1: nk_flags; x1, y1: cfloat;
    triggerBounds1: Rect; btn: Buttons): bool {.raises: [], tags: [], contractual.} =
  ## Create a contextual menu, internal use only, temporary code
  ##
  ## * ctx            - the Nuklear context
  ## * flags1         - the flags for the menu
  ## * x1             - the width of the menu
  ## * y1             - the height of the menu
  ## * triggerBounds1 - the rectange of coordinates in the window where clicking
  ##                    cause the menu to appear
  ## * btn            - the mouse button which must be pressed to show the menu
  ##
  ## Return true if the contextual menu was created successfully, otherwise
  ## false
  proc nk_contextual_begin(ctx; flags: nk_flags; size: nk_vec2;
      triggerBounds: nk_rect; cButton: Buttons): nk_bool {.importc, nodecl, raises: [], tags: [], contractual.}
    ## A binding to Nuklear's function. Internal use only
  return nk_contextual_begin(ctx = ctx, flags = flags1, size = new_nk_vec2(
      x = x1, y = y1), triggerBounds = new_nk_rect(x = triggerBounds1.x,
      y = triggerBounds1.y, w = triggerBounds1.w, h = triggerBounds1.h),
      cButton = btn.cint.Buttons)

template contextualMenu*(flags: set[PanelFlags]; x, y;
    triggerBounds: Rect; button: Buttons; content: untyped) =
  ## Create a contextual menu
  ##
  ## * flags         - the flags for the menu
  ## * x             - the width of the menu
  ## * y             - the height of the menu
  ## * triggerBounds - the rectange of coordinates in the window where clicking
  ##                   cause the menu to appear
  ## * button        - the mouse button which must be pressed to show the menu
  ## * content       - the content of the menu
  if createContextual(ctx = ctx, flags1 = winSetToInt(nimFlags = flags), x1 = x,
      y1 = y, triggerBounds1 = triggerBounds, btn = button):
    content
    nk_contextual_end(ctx = ctx)

template contextualItemLabel*(label: string; align: TextAlignment;
    onPressCode: untyped) =
  ## Add a clickable label to a contextual menu
  ##
  ## * label       - the text to show on the label
  ## * align       - the alignment of the text to show
  ## * onPressCode - the Nim code to execute when the label was pressed
  if nk_contextual_item_label(ctx = ctx, clabel = label.cstring,
      calign = align.nk_flags):
    onPressCode

template contextualItemImage*(image: PImage; onPressCode: untyped) =
  ## Add a clickable image to a contextual menu
  ##
  ## * image       - the image to show in the menu
  ## * onPressCode - the Nim code to execute when the image was pressed
  if nk_contextual_item_image(ctx = ctx, img = image):
    onPressCode

# ------
# Groups
# ------

template group*(title: string; flags: set[PanelFlags]; content: untyped) =
  ## Set a group of widgets inside the parent
  ##
  ## * title   - the title of the group
  ## * flags   - the set of PanelFlags for the group
  ## * content - the content of the group
  if nk_group_begin(ctx = ctx, ctitle = title.cstring, cflags = winSetToInt(
      nimFlags = flags)):
    content
    nk_group_end(ctx = ctx)

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
    ## A binding to Nuklear's function. Internal use only

  var
    (cText, length) = stringToCharArray(str = text, length = maxLen)
    cFlags: cint = editType.ord.cint
  {.warning[HoleEnumConv]: off.}
  {.ruleOff: "assignments".}
  for flag in flags:
    cFlags = cFlags or flag.cint
  {.ruleOn: "assignments".}
  result = nk_edit_string(ctx = ctx, flags = cFlags,
      memory = cText[0].addr, len = length.cint, max = maxLen.cint,
      filter = filter).EditEvent
  text = charArrayToString(charArray = cText, length = length)

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
    ## A binding to Nuklear's function. Internal use only
  var newValue: nk_bool = value.nk_bool
  result = nk_selectable_label(ctx = ctx, str = str.cstring,
      align = align.nk_flags, value = newValue) == nkTrue
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
    ## A binding to Nuklear's function. Internal use only
  var newValue: nk_bool = value.nk_bool
  result = nk_selectable_symbol_label(ctx = ctx, sym = sym,
      title = title.cstring, align = align.nk_flags, value = newValue) == nkTrue
  discard $newValue
  value = newValue

# ------
# Images
# ------
proc image*(image: PImage; padding: Vec2 = Vec2(x: 0, y: 0)) {.raises: [], tags: [], contractual.} =
  ## Draw an image
  ##
  ## * image   - pointer to the image which will be drawn
  ## * padding - the padding of the image, can be empty
  proc nk_draw_image(b: var nk_command_buffer; r: nk_rect; img: var nk_image; col: nk_color) {.importc: "nk_draw_image", nodecl, raises: [
      ], tags: [], contractual.}
    ## A binding to Nuklear's function. Internal use only
  proc nk_state_widget(bounds: var nk_rect; ctx): int {.importc: "nk_widget", nodecl, raises: [], tags: [], contractual.}
    ## A binding to Nuklear's function. Internal use only
  var bounds: nk_rect = nk_rect(x: padding.x, y: 0, w: 0, h: 0)
  discard nk_state_widget(bounds = bounds, ctx = ctx)
  if padding != Vec2(x: 0, y: 0):
    bounds.x += padding.x
    bounds.y += padding.y
    bounds.w -= 2 * padding.x
    bounds.h -= 2 * padding.y
  var newImage: nk_image = nk_image_ptr(iPtr = image)
  nk_draw_image(b = ctx.current.buffer, r = bounds, img = newImage, col = nk_color(r: 255, g: 255, b: 255, a: 255))

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

proc createTooltip(width2, x2, y2: float): bool {.raises: [], tags: [], contractual.} =
  ## Create a new Nuklear tooltip window, internal use only, temporary code
  ## temporary code
  ##
  ## Returns true if the popup is active, otherwise false.
  proc nk_tooltip_begin2(ctx; width, startx, starty: cfloat): nk_bool {.importc, nodecl, raises: [], tags: [], contractual.}
    ## A binding to Nuklear's function. Internal use only
  return nk_tooltip_begin2(ctx = ctx, width = width2.cfloat, startx = x2, starty = y2)

template tooltip*(x, y, width: float; content: untyped) =
  ## Create a new tooltip window with the selected content
  ##
  ## * x       - the X coordinate of the tooltip window
  ## * y       - the Y coordinate of the tooltip window
  ## * width   - the width of the tooltip window
  ## * content - the content of the window
  if createTooltip(width2 = width, x2 = x, y2 = y):
    content
    ctx.nk_tooltip_end

# -------
# Widgets
# -------
proc colorPicker*(color: NkColorF;
    format: colorFormat): NkColorF {.raises: [], tags: [], contractual.} =
  ## Create the color picker widget. Temporary here due to problems with importing nk_colorf.
  ##
  ## * color  - the starting color for the widget
  ## * format - the color format for the widget
  ##
  ## Returns Nim color selected by the user in the widget
  proc nk_color_picker(ctx; color: nk_colorf;
      fmt: colorFormat): nk_colorf {.importc, nodecl, raises: [], tags: [], contractual.}
    ## A binding to Nuklear's function. Internal use only
  let newColor: nk_colorf = nk_color_picker(ctx = ctx, color = nk_colorf(
      r: color.r, g: color.g, b: color.b, a: color.a), fmt = format)
  result = NkColorF(r: newColor.r, g: newColor.g, b: newColor.b, a: newColor.a)

proc ruleHorizontal*(color: Color, rounding: bool) {.raises: [], tags: [], contractual.} =
  ## Draw a horizontal rule with selected color
  ##
  ## * color    - the color of the rule
  ## * rounding - if true, corners of the rule will be rounded
  proc nk_rule_horizontal(ctx; color: nk_color; rounding: nk_bool) {.importc, nodecl, raises: [], tags: [], contractual.}
    ## A binding to Nuklear's function. Internal use only
  let (r, g, b) = color.extractRGB
  nk_rule_horizontal(ctx = ctx, color = nk_color(r: r.uint8, g: g.uint8, b: b.uint8), rounding = (if rounding: nkTrue else: nkFalse))

# ------
# Colors
# ------
proc colorfToHsva*(hsva: var array[4, float]; color: NkColorF) {.raises: [],
    tags: [], contractual.} =
  ## Convert Nim float color object to HSVA values
  ##
  ## * hsva  - the array of 4 values for HSVA color
  ## * color - the Nim color to convert
  ##
  ## Returns converted color as hsva argument
  proc nk_colorf_hsva_fv(hsva: pointer; color: nk_colorf) {.importc, nodecl,
      raises: [], tags: [], contractual.}
    ## A binding to Nuklear's function. Internal use only
  nk_colorf_hsva_fv(hsva = hsva.addr, color = nk_colorf(r: color.r, g: color.g,
      b: color.b, a: color.a))

proc hsvaToColorf*(hsva: array[4, float]): NkColorF {.raises: [], tags: [],
    contractual.} =
  ## Convert HSVA values to Nim color with float values
  ##
  ## * hsva - the array with HSVA values to convert
  ##
  ## Returns converted hsva parameter to Nim color with float values
  proc nk_hsva_colorf(h, s, v, a: cfloat): nk_colorf {.importc, nodecl,
      raises: [], tags: [], contractual.}
    ## A binding to Nuklear's function. Internal use only
  let newColor: nk_colorf = nk_hsva_colorf(h = hsva[0], s = hsva[1], v = hsva[
      2], a = hsva[3])
  result = NkColorF(r: newColor.r, g: newColor.g, b: newColor.b, a: newColor.a)
