# Copyright © 2025-2026 Bartek Jasicki
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

## Provides code for handling colors in nuklear library

import contracts
import nk_types

# ------------------
# Low level bindings
# ------------------
proc nk_rgb*(r, g, b: cint): nk_color {.importc, nodecl, raises: [], tags: [], contractual.}
  ## A binding to Nuklear's function. Internal use only
proc nk_rgb_cf*(c: nk_colorf): nk_color {.importc, nodecl, raises: [], tags: [], contractual.}
  ## A binding to Nuklear's function. Internal use only
proc nk_rgba*(r, g, b, a: cint): nk_color {.importc, nodecl, raises: [], tags: [], contractual.}
  ## A binding to Nuklear's function. Internal use only
proc nk_rgb_factor*(col: nk_color; factor: cfloat): nk_color {.importc, nodecl,
    raises: [], tags: [], contractual.}
  ## A binding to Nuklear's function. Internal use only

# ------------------
# High level bindings
# ------------------
proc nkRGBFactor*(col: NkColor; factor: float): NkColor {.raises: [], tags: [],
    contractual.} =
  ## Get the color after modyfying it by the selected factor
  ##
  ## * col    - the color which will be modified
  ## * factor - the factor about which the color will be modified
  ##
  ## Returns the selected color modified by the selected factor
  if factor == 1.0:
    return col
  result.r = (col.r.float * factor).int
  result.g = (col.g.float * factor).int
  result.b = (col.b.float * factor).int
