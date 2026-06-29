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

{.used.}

## Provides code related to Nuklear styles

import contracts
import nk_types

type
  FontAtlasFormat* = enum
    ## The format of fonts
    atlasAlpha8, atlasRGBA32
  AtlasPtr* = ptr nk_font_atlas
    ## Used to store a pointer to the font data

proc nk_font_atlas_init_default*(atlas: AtlasPtr) {.importc, nodecl,
    raises: [], tags: [], contractual.}
  ## A binding to Nuklear's function. Internal use only
proc nk_font_atlas_begin*(atlas: AtlasPtr) {.importc, nodecl, raises: [],
    tags: [], contractual.}
  ## A binding to Nuklear's function. Internal use only
proc nk_font_atlas_add_default*(atlas: AtlasPtr; height: cfloat;
    config: ptr nk_font_config): ptr nk_font {.importc, nodecl, raises: [],
        tags: [], contractual.}
  ## A binding to Nuklear's function. Internal use only
proc nk_font_atlas_add_from_file*(atlas: AtlasPtr; filePath: cstring;
    height: cfloat;  config: ptr nk_font_config): ptr nk_font {.importc,
        nodecl, raises: [], tags: [], contractual.}
  ## A binding to Nuklear's function. Internal use only
proc nk_font_atlas_clear*(atlas: AtlasPtr) {.importc, nodecl, raises: [
    ], tags: [], contractual.}
  ## A binding to Nuklear's function. Internal use only
proc nk_font_atlas_bake*(atlas: AtlasPtr; width, height: var cint;
    fmt: FontAtlasFormat): ptr {.importc, nodecl, raises: [], tags: [], contractual.}
  ## A binding to Nuklear's function. Internal use only
proc nk_font_atlas_end*(atlas: AtlasPtr; texture: nk_handle;
    texNull: nk_draw_null_texture) {.importc, nodecl, raises: [], tags: [], contractual.}
  ## A binding to Nuklear's function. Internal use only
