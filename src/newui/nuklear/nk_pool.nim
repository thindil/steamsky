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

## Provides code for Nuklear pool

import contracts
import nk_types

proc nkPoolAlloc*(pool: var Pool): PageElement {.raises: [
    ], tags: [RootEffect], contractual.} =
  ## Allocate page element from the pool
  ##
  ## * pool - the pool from which the page element will be allocated
  ##
  ## Returns allocated page element
  if pool.pages == nil or pool.pages.size >= pool.capacity:
    var page: Page = Page()
    if pool.aType == bufferFixed:
      return
    var size: nk_size = Page.sizeof
    size += ((pool.capacity - 1) * PageElement.sizeof.uint).nk_size
    try:
      page = cast[Page](pool.alloc.alloc(handle = pool.alloc.userData,
          old = nil, size = size))
      page.next = pool.pages
      page.size = 0
    except Exception:
      discard
  result = pool.pages.win[pool.pages.size]
  pool.pages.size.inc
