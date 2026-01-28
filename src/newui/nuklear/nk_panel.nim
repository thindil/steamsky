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

## Provides code related to the panel widget in nuklear library

import contracts
import nk_alignment, nk_context, nk_page, nk_types, nk_utils

# ---------------------
# Procedures parameters
# ---------------------
using ctx: PContext

# -------------------
# High level bindings
# -------------------
proc nkPanelGetPadding*(style: Style; pType: PanelType): Vec2 {.raises: [
    ], tags: [], contractual.} =
  ## Get the padding for the selected panel, based on its type. Internal use
  ## only
  ##
  ## * style - the whole style of the application
  ## * pType  - the selected type of the panel
  ##
  ## Returns vector with information about padding for the selected panel
  case pType
  of panelWindow:
    return style.window.padding
  of panelGroup:
    return style.window.groupPadding
  of panelPopup:
    return style.window.popupPadding
  of panelContextual:
    return style.window.contextualPadding
  of panelCombo:
    return style.window.comboPadding
  of panelMenu:
    return style.window.menuPadding
  of panelTooltip:
    return style.window.tooltipPadding
  else:
    discard

proc nkPanelGetBorder*(style: Style; flags: nk_flags;
    pType: PanelType): float {.raises: [], tags: [], contractual.} =
  ## Get the border size for the selected panel, based on its type. Internal use
  ## only
  ##
  ## * style - the whole style of the application
  ## * pType  - the selected type of the panel
  ##
  ## Returns size of the border of the selected panel
  if (flags and windowBorder.ord.int).nk_bool:
    case pType
    of panelWindow:
      return style.window.border
    of panelGroup:
      return style.window.groupBorder
    of panelPopup:
      return style.window.popupBorder
    of panelContextual:
      return style.window.contextualBorder
    of panelCombo:
      return style.window.comboBorder
    of panelMenu:
      return style.window.menuBorder
    of panelTooltip:
      return style.window.tooltipBorder
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
    pe: ptr nk_page_element = nkContainerOf(`ptr` = pd,
        `type` = nk_page_element, member = "data")
  nkFreePageElement(ctx = ctx, elem = pe)

proc nkCreatePanel*(context: var Context): Panel {.raises: [], tags: [
    RootEffect], contractual.} =
  ## Create a new panel in the selected context
  ##
  ## * context - the Nuklear context in which the panel will be created
  ##
  ## Returns newly created panel
  let elem: PageElement = nkCreatePageElement(context = context,
      pageType = panelType)
  nkZero(pData = elem.addr, size = PageElement.sizeof)
  result = elem.data.pan
