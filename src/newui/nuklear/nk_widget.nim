# Copyright © 2024-2025 Bartek Jasicki
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

## Provides some widgets from nuklear library

import contracts
import nk_context, nk_types

# ---------------------
# Procedures parameters
# ---------------------
using ctx: PContext

proc nkWidgetStateReset*(s: var nk_flags) {.raises: [], tags: [],
    contractual.} =
  ## Reset the state of a widget. Internal use only
  ##
  ## * s - the state to reset
  ##
  ## Returns the modified parameter s
  if (s and widgetStateModified.int).bool:
    s = widgetStateInactive.int or widgetStateModified.int
  else:
    s = widgetStateInactive.ord

proc checkbox*(label: string; checked: var bool): bool {.discardable, raises: [
    ], tags: [], contractual.} =
  ## Create a Nuklear checkbox widget
  ##
  ## * label   - the text to show with the checkbox
  ## * checked - the state of the checkbox, if true, the checkbox is checked
  ##
  ## Returns true if the state of the checkbox was changed, otherwise false.
  proc nk_checkbox_label(ctx; text: cstring;
      active: var cint): nk_bool {.importc, nodecl, raises: [], tags: [], contractual.}
    ## Nuklear C binding
  var active: cint = (if checked: 1 else: 0)
  result = nk_checkbox_label(ctx = ctx, text = label.cstring,
      active = active) == nkTrue
  checked = active == 1

proc option*(label: string; selected: bool): bool {.raises: [], tags: [],
    contractual.} =
  ## Create a Nuklear option (radio) widget
  ##
  ## * label    - the text show with the option
  ## * selected - the state of the option, if true the option is selected
  ##
  ## Returns true if the option is selected, otherwise false
  proc nk_option_label(ctx; name: cstring; active: cint): nk_bool {.importc,
      nodecl, raises: [], tags: [], contractual.}
    ## Nuklear C binding
  var active: cint = (if selected: 1 else: 0)
  return nk_option_label(ctx = ctx, name = label.cstring, active = active) == nkTrue

proc progressBar*(value: var int; maxValue: int; modifyable: bool = true;
    reversed: bool = false): bool {.discardable, raises: [], tags: [],
    contractual.} =
  ## Create a Nuklear progress bar widget
  ##
  ## * value      - the current value of the progress bar
  ## * maxValue   - the maximum value of the progress bar
  ## * modifyable - if true, the user can modify the value of the progress bar
  ## * reversed   - if true, the progress bar should be draw in reverse, from
  ##                the end
  ##
  ## Returns true if the value parameter was changed, otherwise false
  proc nk_progress(ctx; cur: var nk_size; max: nk_size; modifyable,
      reversed: nk_bool): nk_bool {.importc, nodecl, raises: [], tags: [], contractual.}
    ## Nuklear C binding
  return nk_progress(ctx = ctx, cur = value, max = maxValue,
      modifyable = modifyable.nk_bool, reversed = reversed.nk_bool) == nkTrue

