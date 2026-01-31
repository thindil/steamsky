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

## Provides code for nuklear page type

import contracts
import nk_buffer, nk_types, nk_context, nk_pool, nk_utils

# ---------------------
# Procedures parameters
# ---------------------
using
  ctx: PContext
  context: ref Context

# -------------------
# High level bindings
# -------------------
proc nkLinkPageElementIntoFreelist*(context; elem: ptr PageElement)
  {.raises: [], tags: [], contractual.} =
  ## Link the element into list of items to free
  ##
  ## * context - the Nuklear context
  ## * elem    - the page element which will be freed
  # link table into freelist
  if context.freeList == nil:
    context.freeList = elem
  else:
    elem.next = context.freeList
    context.freeList = elem

proc nkFreePageElement*(context; elem: ptr PageElement) {.raises: [], tags: [],
  contractual.} =
  ## Free memory used by the selected page element
  ##
  ## * context - the Nuklear context
  ## * elem    - the page element which will be removed
  # we have a pool so just add to free list
  if context.usePool:
    nkLinkPageElementIntoFreelist(context = context, elem = elem)
    return
  # if possible remove last element from back of fixed memory buffer
  let
    elemEnd: pointer = elem.addr + 1
    bufferEnd: pointer = ctx.memory.memory.`ptr` + ctx.memory.size
  if elemEnd == bufferEnd:
    context.memory.size -= elem.sizeOf
  else:
    nkLinkPageElementIntoFreelist(context = context, elem = elem)

proc nkCreatePageElement*(context;
    pageType: PageDataType): PageElement {.raises: [], tags: [RootEffect],
    contractual.} =
  ## Create a new page element
  ##
  ## * context  - the Nuklear context
  ## * pageType - the type of the page element
  ##
  ## Returns the newly created element
  result = PageElement(data: PageData(pageDataType: pageType))
  if context.freeList != nil:
    # Unlink page element from free list
    result = context.freeList[]
    context.freeList = result.next
  elif context.usePool:
    # Allocate page element from memory pool
    result = nkPoolAlloc(pool = context.pool, pageType = pageType)
  else:
    # Allocate new page element from back of fixed size memory buffer
    let
      size: nk_size = PageElement.sizeof
      align: nk_size = PageElement.alignof
    result = cast[PageElement](nkBufferAlloc(b = context.memory,
        bufferAlloc = bufferBack, size = size, align = align))
  nkZero(pData = result.addr, size = PageElement.sizeof)
  result.next = nil
  result.prev = nil
