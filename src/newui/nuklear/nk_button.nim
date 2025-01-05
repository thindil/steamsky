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

## Provides code related to the button's widget in nuklear library

import contracts
import nk_types, nk_colors, nk_context

# ---------------------
# Procedures parameters
# ---------------------
using ctx: PContext

# ------------------
# Low level bindings
# ------------------
proc nk_button_text(ctx; ctitle: cstring; clen: cint): nk_bool {.importc, cdecl,
    raises: [], tags: [], contractual.}
  ## A binding to Nuklear's function. Internal use only
proc nk_button_label(ctx; ctitle: cstring): nk_bool {.importc, cdecl, raises: [
    ], tags: [], contractual.}
  ## A binding to Nuklear's function. Internal use only
proc nk_button_symbol(ctx; csymbol: SymbolType): nk_bool {.importc, cdecl,
    raises: [], tags: [], contractual.}
  ## A binding to Nuklear's function. Internal use only
proc nk_button_symbol_label(ctx; csymbol: SymbolType; clabel: cstring;
    calign: nk_flags): nk_bool {.importc, cdecl, raises: [], tags: [], contractual.}
  ## A binding to Nuklear's function. Internal use only

# -------------------
# High level bindings
# -------------------
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

template textButton*(title: string; len: Natural; onPressCode: untyped) =
  ## Draw the button and the selected text with selected length on it. Execute
  ## the selected code on pressing it.
  ##
  ## * title       - the text to shown on the button
  ## * len         - the maximum length of the text to show on the button
  ## * onPressCode - the Nim code to execute when the button was pressed
  if nk_button_text(ctx = ctx, ctitle = title.cstring, clen = len.cint):
    onPressCode

template labelButton*(title: string; onPressCode: untyped) =
  ## Draw the button with the selected text on it. Execute the selected code
  ## on pressing it.
  ##
  ## * title       - the text to shown on the button
  ## * onPressCode - the Nim code to execute when the button was pressed
  if nk_button_label(ctx = ctx, ctitle = title.cstring):
    onPressCode

proc setButtonBehavior*(behavior: ButtonBehavior) {.raises: [], tags: [],
    contractual.} =
  ## Set the behavior of the the next button, when it is clicked
  ##
  ## * behavior - the behavior of a button
  proc nk_button_set_behavior(ctx; behavior: ButtonBehavior) {.importc, nodecl,
      raises: [], tags: [], contractual.}
    ## A binding to Nuklear's function. Internal use only
  nk_button_set_behavior(ctx = ctx, behavior = behavior)

template symbolButton*(symbol: SymbolType; onPressCode: untyped) =
  ## Draw the button with the selected symbol. Execute the selected code
  ## on pressing it.
  ##
  ## * symbol      - the symbol to shown on the button
  ## * onPressCode - the Nim code to execute when the button was pressed
  if nk_button_symbol(ctx = ctx, csymbol = symbol):
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
  if nk_button_symbol_label(ctx = ctx, csymbol = symbol,
      clabel = label.cstring, calign = align.nk_flags):
    onPressCode

proc createStyledButton(bTitle: cstring; bStyle: ButtonStyle): bool {.raises: [
    ], tags: [], contractual.} =
  ## Draw a button with the selected style, internal use only, temporary code
  ##
  ## * bTitle - the text to shown on the button
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
  proc nk_button_label_styled(ctx; style: var nk_style_button;
      title: cstring): nk_bool {.importc, nodecl, raises: [], tags: [], contractual.}
    ## A binding to Nuklear's function. Internal use only
  return nk_button_label_styled(ctx = ctx, style = buttonStyle, title = bTitle)

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
