# Copyright 2024 Bartek thindil Jasicki
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

## Provides code related to the game's UI, like tooltips

import std/macros
import contracts
import nk_context

# ---------------------
# Procedures parameters
# ---------------------
using ctx: PContext

# --------
# Tooltips
# --------
proc nk_tooltipf(ctx; fmt: cstring) {.importc, nodecl, varargs.}

macro fmtTooltip*(args: varargs[untyped]): untyped =
  ## Draw a tooltip formatted in the same way like the C function printf
  ##
  ## * args      - the tooltip's text and its arguments to draw
  result = quote do:
    nk_tooltipf(ctx, `args`)

proc tooltip*(text: string) {.raises: [], tags: [].} =
  ## Draw a tooltip with the selected text
  ##
  ## * text - the text to show on the tooltip window
  proc nk_tooltip(ctx; text: cstring) {.importc, nodecl.}
  nk_tooltip(ctx, text.cstring)

var
  tooltipDelay: float = 1000.0
    ## For how long the player hovers the mouse over UI element before a
    ## tooltip will be shown

proc showTooltip*(text: string) {.raises: [], tags: [],
    contractual.} =
  ## Show the selected tooltip if the player hovers they mouse over the
  ## selected widget
  ##
  ## * text   - the text to show
#  if isMouseHovering(rect = bounds):
  tooltip(text = text)
