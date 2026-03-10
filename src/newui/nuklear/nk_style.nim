# Copyright © 2026 Bartek Jasicki
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

## Provides code related to Nuklear styles

import contracts, nimalyzer
import nk_context, nk_types

# ---------------------
# Procedures parameters
# ---------------------
using
  ctx: PContext

# ------------------
# High level bindings
# ------------------
{.push ruleOff: "varDeclared".}
let defaultColorStyle {.used.}: array[24, NkColor] = [textColor: NkColor(r: 175,
    g: 175, b: 175, a: 255), windowColor: NkColor(r: 45, g: 45, b: 45, a: 255),
    headerColor: NkColor(r: 40, g: 40, b: 40, a: 255), borderColor: NkColor(
    r: 65, g: 65, b: 65, a: 255), buttonColor: NkColor(r: 65, g: 65, b: 65,
    a: 255), buttonHoverColor: NkColor(r: 40, g: 40, b: 40, a: 255),
    buttonActiveColor: NkColor(r: 35, g: 35, b: 35, a: 255),
    toggleColor: NkColor(r: 100, g: 100, b: 100, a: 255),
    toggleHoverColor: NkColor(r: 120, g: 120, b: 120, a: 255),
    toggleCursorColor: NkColor(r: 45, g: 45, b: 45, a: 255),
    selectColor: NkColor(r: 45, g: 45, b: 45, a: 255),
    selectActiveColor: NkColor(r: 35, g: 35, b: 35, a: 255),
    sliderColor: NkColor(r: 38, g: 38, b: 38, a: 255),
    sliderCursorColor: NkColor(r: 100, g: 100, b: 100, a: 255),
    sliderCursorHoverColor: NkColor(r: 120, g: 120, b: 120, a: 255),
    sliderCursorActiveColor: NkColor(r: 150, g: 150, b: 150, a: 255),
    propertyColor: NkColor(r: 38, g: 38, b: 38, a: 255), editColor: NkColor(
    r: 38, g: 38, b: 38, a: 255), editCursorColor: NkColor(r: 175, g: 175,
    b: 175, a: 255), comboColor: NkColor(r: 45, g: 45, b: 45, a: 255),
    chartColor: NkColor(r: 120, g: 120, b: 120, a: 255),
    colorChartColor: NkColor(r: 45, g: 45, b: 45, a: 255),
    colorChartHighlightColor: NkColor(r: 255, g: 0, b: 0, a: 255),
    scrollbarColor: NkColor(r: 40, g: 40, b: 40, a: 255)]
{.push ruleOn: "varDeclared".}

proc defaultStyle*() {.raises: [], tags: [], contractual.} =
  ## Reset the UI colors to the default Nuklear setting
  proc nk_style_default(ctx) {.importc, nodecl, raises: [], tags: [], contractual.}
    ## A binding to Nuklear's function. Internal use only
  nk_style_default(ctx = ctx)

