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

## Provides code related to the panel widget in nuklear library

import contracts
import nk_alignment, nk_context, nk_page, nk_types

# ---------------------
# Procedures parameters
# ---------------------
using ctx: PContext

# ------------------
# Low level bindings
# ------------------
proc nk_create_panel*(ctx): pointer {.importc, cdecl, raises: [], tags: [], contractual.}
  ## A binding to Nuklear's function. Internal use only

# -------------------
# High level bindings
# -------------------
proc nkPanelGetPadding*(style: nk_style; `type`: PanelType): nk_vec2 {.raises: [
    ], tags: [], contractual.} =
  ## Get the padding for the selected panel, based on its type. Internal use
  ## only
  ##
  ## * style - the whole style of the application
  ## * type  - the selected type of the panel
  ##
  ## Returns vector with information about padding for the selected panel
  case `type`
  of panelWindow:
    return style.window.padding
  of panelGroup:
    return style.window.group_padding
  of panelPopup:
    return style.window.popup_padding
  of panelContextual:
    return style.window.contextual_padding
  of panelCombo:
    return style.window.combo_padding
  of panelMenu:
    return style.window.menu_padding
  of panelTooltip:
    return style.window.tooltip_padding
  else:
    discard

proc nkPanelGetBorder*(style: nk_style; flags: nk_flags;
    `type`: PanelType): cfloat {.raises: [], tags: [], contractual.} =
  ## Get the border size for the selected panel, based on its type. Internal use
  ## only
  ##
  ## * style - the whole style of the application
  ## * type  - the selected type of the panel
  ##
  ## Returns size of the border of the selected panel
  if (flags and windowBorder.ord.int).nk_bool:
    case `type`
    of panelWindow:
      return style.window.border
    of panelGroup:
      return style.window.group_border
    of panelPopup:
      return style.window.popup_border
    of panelContextual:
      return style.window.contextual_border
    of panelCombo:
      return style.window.combo_border
    of panelMenu:
      return style.window.menu_border
    of panelTooltip:
      return style.window.tooltip_border
    else:
      return 0
  else:
    return 0

proc nkPanelHasHeader*(flags: nk_flags; title: string): bool {.raises: [],
    tags: [], contractual.} =
  ## Check if a panel has a header to draw. Internal use only
  ##
  ## * flags - the panel's flags
  ## * title - the panel's  title
  var active: nk_bool = nkFalse
  active = (flags and (windowClosable.ord.int or
      windowMinimizable.ord.int)).nk_bool
  active = (active or (flags and windowTitle.ord.int).nk_bool).nk_bool
  active = (active and not(flags and windowHidden.ord.int).nk_bool and
      title.len > 0).nk_bool
  return active

proc nkPanelIsNonblock*(`type`: PanelType): bool {.raises: [], tags: [],
    contractual.} =
  ## Check if the selected panel's type is non-blocking panel
  ##
  ## * type - the type of panel to check
  ##
  ## Returns true if the panel's type is non-blocking, otherwise false.
  return (`type`.cint and panelSetNonBlock.cint).bool

proc nkFreePanel*(ctx; pan: PNkPanel) {.raises: [], tags: [], contractual.} =
  ## Free memory used by the panel
  ##
  ## * ctx - the Nuklear context
  ## * pan - the panel which memory will be freed
  let
    pd: ptr nk_page_data = nkContainerOf(`ptr` = pan, `type` = nk_page_data, member = "")
    pe: ptr nk_page_element = nkContainerOf(`ptr` = pd, `type` = nk_page_element, member = "data")
  nkFreePageElement(ctx = ctx, elem = pe)

