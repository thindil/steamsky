# Copyright 2024-2025 Bartek thindil Jasicki
#
# This file is part of Steam Sky.
#
# Steam Sky is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Steam Sky is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Steam Sky.  If not, see <http://www.gnu.org/licenses/>.

## Provides code related to tooltips system, like setting tooltips, showing
## them, etc.

import std/macros
import contracts
import nk_context, nk_types

# ---------------------
# Procedures parameters
# ---------------------
using ctx: PContext

proc nk_tooltipf(ctx; fmt: cstring) {.importc, nodecl, varargs, raises: [],
    tags: [], contractual.}
  ## Internal Nuklear C binding

macro fmtTooltip*(args: varargs[untyped]): untyped =
  ## Draw a tooltip formatted in the same way like the C function printf
  ##
  ## * args      - the tooltip's text and its arguments to draw
  result = quote do:
    nk_tooltipf(ctx = ctx, fmt = `args`)

proc tooltip*(text: string) {.raises: [], tags: [], contractual.} =
  ## Draw a tooltip with the selected text
  ##
  ## * text - the text to show on the tooltip window
  proc nk_tooltip(ctx; text: cstring) {.importc, nodecl, raises: [], tags: [], contractual.}
    ## Internal Nuklear C binding
  nk_tooltip(ctx = ctx, text = text.cstring)

proc tooltip*(text: string; x, y: float) {.raises: [], tags: [], contractual.} =
  ## Create a tooltip with the selected text at the selected position.
  ##
  ## * text - the text to show on the tooltip window
  ## * x    - the X coordinate of the tooltip window
  ## * y    - the Y coordinate of the tooltip window
  proc nk_tooltip2(ctx; text: cstring, startx, starty: cfloat) {.importc, nodecl, raises: [], tags: [], contractual.}
    ## Internal Nuklear C binding
  nk_tooltip2(ctx = ctx, text = text.cstring, startx = x, starty = y)

type
  TooltipData = object
    bounds*: Rect
    text*: string

var
  tooltipDelay*: float = 1000.0
    ## For how long the player hovers the mouse over UI element before a
    ## tooltip will be shown
  frameDelay*: float = 0.0
    ## The length of UI frames. Used to count when to show a tooltip
  tooltips*: seq[TooltipData] = @[]
    ## The list of tooltips available in the window
  delay*: float = tooltipDelay
    ## The current delay before show a tooltip, when reached 0, show a tooltip

proc setTooltips*(tDelay, fDelay: float) {.raises: [], tags: [], contractual.} =
  ## Set the tooltips configuration
  ##
  ## * tDelay - the delay before show a tooltip to the user
  ## * fDelay - the length of UI frame.
  tooltipDelay = tDelay
  frameDelay = fDelay

proc resetTooltips*() {.raises: [], tags: [], contractual.} =
  ## Reset the list of tooltips available in the window. Should be called
  ## at the begining of a Nuklear window declaration.
  tooltips = @[]

proc addTooltip*(bounds: Rect; text: string) {.raises: [], tags: [],
    contractual.} =
  ## Add a tooltip to the list of tooltips. The procedure should be called
  ## before the widget declaration, because it also needs bounds in which the
  ## mouse will be check.
  ##
  ## * bounds - the area in which the widget with the tooltip is
  ## * text   - the text which will be show as the tooltip
  tooltips.add(y = TooltipData(bounds: bounds, text: text))
