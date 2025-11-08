# Copyright © 2025 Bartek Jasicki
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

## Provides code related to the widgets' layout in nuklear library

import contracts
import nk_types, nk_context

# ---------------------
# Procedures parameters
# ---------------------
using ctx: PContext

# ------------------
# Low level bindings
# ------------------
proc nk_layout_row_end(ctx) {.importc, cdecl, raises: [], tags: [], contractual.}
  ## A binding to Nuklear's function. Internal use only
proc nk_layout_row_begin(ctx; fmt: LayoutFormat;
    rowHeight: cfloat; ccols: cint) {.importc, cdecl, raises: [], tags: [], contractual.}
  ## A binding to Nuklear's function. Internal use only
proc nk_layout_row_push(ctx; cwidth: cfloat) {.importc, cdecl, raises: [],
    tags: [], contractual.}
  ## A binding to Nuklear's function. Internal use only
proc nk_layout_row(ctx; fmt: LayoutFormat; height: cfloat;
    cols: cint; ratio: pointer) {.importc, nodecl, raises: [], tags: [], contractual.}
  ## A binding to Nuklear's function. Internal use only
proc nk_layout_space_begin(ctx; fmt: LayoutFormat;
    cheight: cfloat; widgetCount: cint) {.importc, cdecl, raises: [], tags: [], contractual.}
  ## A binding to Nuklear's function. Internal use only
proc nk_layout_space_end(ctx) {.importc, cdecl, raises: [], tags: [], contractual.}
  ## A binding to Nuklear's function. Internal use only

# -------------------
# High level bindings
# -------------------

proc nkPanelLayout(ctx; win: PNkWindow; height: float; cols: int) {.raises: [
    NuklearException], tags: [], contractual.} =
  ## Set the panel layout
  ##
  ## * ctx    - the Nuklear context
  ## * height - the height in pixels of each row
  ## * cols   - the amount of columns in each row
  let
    layout: PNkPanel = win.layout
    style: nk_style = ctx.style

  if not (layout.flags and windowMinimized.int).bool:
    raise newException(exceptn = NuklearException,
        message = "Window is minimized.")
  if not (layout.flags and windowHidden.int).bool:
    raise newException(exceptn = NuklearException,
        message = "Window is hidden.")
  if not (layout.flags and windowClosed.int).bool:
    raise newException(exceptn = NuklearException,
        message = "Window is closed.")

  # Update the current row and set the current row layout
  layout.row.index = 0
  layout.at_y += layout.row.height
  layout.row.columns = cols.cint
  let itemSpacing: nk_vec2 = style.window.spacing
  if height == 0:
    layout.row.height = max(x = height, y = layout.row.min_height) + itemSpacing.y
  else:
    layout.row.height = height + itemSpacing.y

  layout.row.item_offset = 0
  if (layout.flags and windowDynamic.int).bool:
    # draw background for dynamic panels
    var background: nk_rect = nk_rect()
    background.x = win.bounds.x
    background.w = win.bounds.y
    background.y = layout.at_y - 1.0
    background.h = layout.row.height + 1.0
    let color: nk_color = if layout.type == panelTooltip:
          style.window.tooltip_background
        elif layout.type == panelPopup:
          style.window.popup_background
        else:
          style.window.background

proc nkRowLayout(ctx; fmt: LayoutFormat; height: float; cols,
    width: int) {.raises: [NuklearException], tags: [], contractual.} =
  ## Set the current row layout
  ##
  ## * ctx    - the Nuklear context
  ## * fmt    - the layout format
  ## * height - the height in pixels of each row
  ## * width  - the width in pixels of each column
  ## * cols   - the amount of columns in each row
  require:
    ctx != nil
    ctx.current != nil
    ctx.current.layout != nil
  body:
    if ctx == nil or ctx.current == nil or ctx.current.layout == nil:
      return

    let win: PNkWindow = ctx.current
    nkPanelLayout(ctx = ctx, win = win, height = height, cols = cols)

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
    ## A binding to Nuklear's function. Internal use only
  nk_layout_row_dynamic(ctx = ctx, height = height.cfloat, cols = cols.cint)

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
    ## A binding to Nuklear's function. Internal use only
  nk_layout_row_static(ctx = ctx, height = height.cfloat,
      itemWidth = width.cint, cols = cols.cint)

template layoutStatic*(height: float; cols: int; content: untyped) =
  ## Start setting manualy each row of the current widgets layout. The layout
  ## will not resize when the parent window change its size
  ##
  ## * height  - the width in pixels or window's ratio of each row
  ## * cols    - the amount of columns in each row
  ## * content - the content of the layout
  nk_layout_row_begin(ctx = ctx, fmt = static, rowHeight = height.cfloat,
      ccols = cols.cint)
  content
  nk_layout_row_end(ctx = ctx)

template layoutDynamic*(height: float; cols: int; content: untyped) =
  ## Start setting manualy each row of the current widgets layout. The layout
  ## will resize when the parent window change its size
  ##
  ## * height   - the width in pixels or window's ratio of each row
  ## * cols    - the amount of columns in each row
  ## * content - the content of the layout
  nk_layout_row_begin(ctx = ctx, fmt = dynamic, rowHeight = height.cfloat,
      ccols = cols.cint)
  content
  nk_layout_row_end(ctx = ctx)

template row*(width: float; content: untyped) =
  ## Set the content of the row in the current widgets layout
  ##
  ## * width   - the width in the pixels or window's ratio of each column
  ## * content - the content of the row
  nk_layout_row_push(ctx = ctx, cwidth = width.cfloat)
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
  nk_layout_row(ctx = ctx, fmt = static, height = height.cfloat,
      cols = cols.cint, ratio = ratio.addr)

proc setLayoutRowDynamic*(height: float; cols: int; ratio: openArray[
    cfloat]) {.raises: [], tags: [], contractual.} =
  ## Set the current widgets layout to divide it into selected amount of
  ## columns with the selected height in rows but it will grow in width
  ## when the parent window resizes
  ##
  ## * height - the height in pixels of each row
  ## * cols   - the amount of columns in each row
  ## * ratio  - the array or sequence of cfloat with width of the colums
  nk_layout_row(ctx = ctx, fmt = dynamic, height = height.cfloat,
      cols = cols.cint, ratio = ratio.addr)

template layoutSpaceStatic*(height: float; widgetsCount: int;
    content: untyped) =
  ## Start setting manualy each row of the current widgets layout. The layout
  ## will not resize when the parent window change its size
  ##
  ## * height       - the width in pixels or window's ratio of each row
  ## * widgetsCount - the amount of widgets in each row.
  ## * content      - the content of the layout
  nk_layout_space_begin(ctx = ctx, fmt = static, cheight = height.cfloat,
      widgetCount = widgetsCount.cint)
  content
  nk_layout_space_end(ctx = ctx)

template layoutSpaceDynamic*(height: float; widgetsCount: int;
    content: untyped) =
  ## Start setting manualy each row of the current widgets layout. The layout
  ## will resize when the parent window change its size
  ##
  ## * height       - the width in pixels or window's ratio of each row
  ## * widgetsCount - the amount of widgets in each row.
  ## * content      - the content of the layout
  nk_layout_space_begin(ctx = ctx, fmt = dynamic, cheight = height.cfloat,
      widgetCount = widgetsCount.cint)
  content
  nk_layout_space_end(ctx = ctx)

proc layoutWidgetBounds*(): NimRect {.raises: [], tags: [], contractual.} =
  ## Get the rectangle of the current widget in the layout
  ##
  ## Returns NimRect with the data for the current widget
  proc nk_layout_widget_bounds(ctx): nk_rect {.importc, nodecl, raises: [],
      tags: [], contractual.}
    ## A binding to Nuklear's function. Internal use only
  let rect: nk_rect = nk_layout_widget_bounds(ctx = ctx)
  result = NimRect(x: rect.x, y: rect.y, w: rect.w, h: rect.h)

proc layoutSetMinRowHeight*(height: float) {.raises: [], tags: [],
    contractual.} =
  ## Set the currently used minimum row height. Must contains also paddings size.
  ##
  ## * height - the new minimum row height for auto generating the row height
  proc nk_layout_set_min_row_height(ctx; height: cfloat) {.importc, nodecl,
      raises: [], tags: [], contractual.}
    ## A binding to Nuklear's function. Internal use only
  nk_layout_set_min_row_height(ctx = ctx, height = height.cfloat)

proc layoutResetMinRowHeight*() {.raises: [], tags: [], contractual.} =
  ## Reset the currently used minimum row height.
  proc nk_layout_reset_min_row_height(ctx) {.importc, nodecl, raises: [],
      tags: [], contractual.}
    ## A binding to Nuklear's function. Internal use only
  nk_layout_reset_min_row_height(ctx = ctx)

