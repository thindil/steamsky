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

## Provides code related to the user's input in nuklear library

import contracts
import nk_context, nk_math, nk_types

# ---------------------
# Procedures parameters
# ---------------------
using ctx: PContext

# ------------------
# Low level bindings
# ------------------
proc nk_input_begin*(ctx) {.importc, nodecl, raises: [], tags: [], contractual.}
  ## A binding to Nuklear's function. Internal use only
proc nk_input_end*(ctx) {.importc, nodecl, raises: [], tags: [], contractual.}
  ## A binding to Nuklear's function. Internal use only
proc nk_input_key*(ctx; key: Keys; down: nk_bool) {.importc, nodecl,
    raises: [], tags: [], contractual.}
  ## A binding to Nuklear's function. Internal use only
proc nk_input_button*(ctx; id: Buttons; x, y: cint; down: nk_bool) {.importc,
    nodecl, raises: [], tags: [], contractual.}
  ## A binding to Nuklear's function. Internal use only

proc hasMouseClickInRect*(id: Buttons; rect: Rect): bool {.raises: [],
  tags: [], contractual.} =
  ## Check if the mouse button was clicked in the selected rectangle
  ##
  ## * id   - the mouse button which will be checked
  ## * rect - the rectangle in which the mouse button will be checked
  ##
  ## Returns true if the mouse button was checked in the selected rectangle,
  ## otherwise false
  if ctx.input.addr == nil:
    return false
  let
    buttons: ButtonsArray = cast[ButtonsArray](ctx.input.mouse.buttons)
    btn: nk_mouse_button = buttons[id]
  return nkInbox(px = btn.clicked_pos.x, py = btn.clicked_pos.y, x = rect.x,
    y = rect.y, w = rect.w, h = rect.h)

proc hasMouseClickInRect2*(id: Buttons; rect: Rect): bool {.raises: [],
  tags: [], contractual.} =
  ## Check if the mouse button was clicked in the selected rectangle
  ##
  ## * id   - the mouse button which will be checked
  ## * rect - the rectangle in which the mouse button will be checked
  ##
  ## Returns true if the mouse button was checked in the selected rectangle,
  ## otherwise false
  let btn: MouseButton = context.input.mouse.buttons[id]
  return nkInbox(px = btn.clickedPos.x, py = btn.clickedPos.y, x = rect.x,
    y = rect.y, w = rect.w, h = rect.h)

proc hasMouseClickDownInRect*(id: Buttons; rect: Rect; down: bool): bool
  {.raises: [], tags: [], contractual.} =
  ## Check if the mouse button is clicked down in the selected rectangle
  ##
  ## * id   - the mouse button which will be checked
  ## * rect - the rectangle in which the mouse button will be checked
  ## * down - if true, the button is clicked down
  ##
  ## Returns true if the mouse button was checked in the selected rectangle, otherwise false
  if ctx.input.addr == nil:
    return false
  let
    buttons: ButtonsArray = cast[ButtonsArray](ctx.input.mouse.buttons)
    btn: nk_mouse_button = buttons[id]
  return hasMouseClickInRect(id = id, rect = Rect(x: rect.x, y: rect.y,
    w: rect.w, h: rect.h)) and btn.down == down

proc hasMouseClickDownInRect2*(id: Buttons; rect: Rect; down: bool): bool
  {.raises: [], tags: [], contractual.} =
  ## Check if the mouse button is clicked down in the selected rectangle
  ##
  ## * id   - the mouse button which will be checked
  ## * rect - the rectangle in which the mouse button will be checked
  ## * down - if true, the button is clicked down
  ##
  ## Returns true if the mouse button was checked in the selected rectangle, otherwise false
  let btn: MouseButton = context.input.mouse.buttons[id]
  return hasMouseClickInRect2(id = id, rect = Rect(x: rect.x, y: rect.y,
    w: rect.w, h: rect.h)) and btn.down == down

proc getInputText*(): string {.raises: [], tags: [], contractual.} =
  ## Get the text inserted by the user
  ##
  ## Returns string with the text inserted by the user
  proc nk_get_input_text(ctx): cstring {.importc, nodecl, raises: [], tags: [], contractual.}
    ## A binding to Nuklear's function.
  return $nk_get_input_text(ctx = ctx)
