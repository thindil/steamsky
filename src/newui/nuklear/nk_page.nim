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

## Provides code for nuklear page type

import contracts
import nk_types, nk_context

# ---------------------
# Procedures parameters
# ---------------------
using ctx: PContext

# -------------------
# High level bindings
# -------------------
proc nkLinkPageElementIntoFreelist*(ctx; elem: ptr nk_page_element)
  {.raises: [], tags: [], contractual.} =
  ## Link the element into list of items to free
  ##
  ## * ctx  - the Nuklear context
  ## * elem - the page element which will be freed
  # link table into freelist
  if ctx.freelist == nil:
    ctx.freelist = elem
  else:
    elem.next = ctx.freelist
    ctx.freelist = elem

proc nkFreePageElement*(ctx; elem: ptr nk_page_element) {.raises: [], tags: [],
  contractual.} =
  ## Free memory used by the selected page element
  ##
  ## * ctx  - the Nuklear context
  ## * elem - the page element which will be removed
  # we have a pool so just add to free list
  if ctx.use_pool:
    nkLinkPageElementIntoFreelist(ctx = ctx, elem = elem)
    return
  # if possible remove last element from back of fixed memory buffer
  let
    elemEnd: pointer = elem + 1
    bufferEnd: pointer = ctx.memory.memory.`ptr` + ctx.memory.size
  if elemEnd == bufferEnd:
    ctx.memory.size -= elem.sizeOf
  else:
    nkLinkPageElementIntoFreelist(ctx = ctx, elem = elem)

proc nkCreatePageElement*(context: Context): PageElement {.raises: [], tags: [], contractual.} =
  ## Create a new page element
  ##
  ## * context - the Nuklear context
  ##
  ## Returns the newly created element
  return PageElement(data: PageData(pageDataType: windowType))
