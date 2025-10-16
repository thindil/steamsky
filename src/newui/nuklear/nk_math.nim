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

import contracts
import nk_types

proc nkShrinkRect*(r: NimRect; amount: cfloat): NimRect {.raises: [], tags: [],
    contractual.} =
  ## Shrink the selected rectangle. Internal use only
  ##
  ## * r      - the rectangle to shrink
  ## * amount - the size of which the rectangle will be shrinked
  ##
  ## Returns the shrinked rectangle
  let
    w = max(x = r.w, y = 2 * amount)
    h = max(x = r.h, y = 2 * amount)
  result.x = r.x + amount
  result.y = r.y + amount
  result.w = w - 2 * amount
  result.h = h - 2 * amount

proc nkIntersect*(x0, y0, w0, h0, x1, y1, w1, h1: cfloat): bool {.raises: [],
    tags: [], contractual.} =
  ## Check if the rectangle is inside the second rectangle
  ##
  ## * x0, y0, w0, h0 - the coordinates of the rectangle to check
  ## * x1, y1, w1, h1 - the coordinates of the second rectangle
  ##
  ## Returns true if the rectangle is inside the second rectangle, otherwise
  ## false.
  return ((x1 < (x0 + w0)) and (x0 < (x1 + w1)) and (y1 < (y0 + h0)) and (y0 < (
      y1 + h1)))

proc nkTriangleFromDirection*(`result`: var array[3, NimRect]; r: NimRect;
    padX: cfloat; padY: cfloat; direction: Heading) {.raises: [], tags: [],
    contractual.} =
  ## Get the coordinates of a triangle based on its direction
  ##
  ## * result
  ## * r
  ## * padX
  ## * padY
  ## * direction
  var rect: NimRect = NimRect()
  rect.w = max(x = 2 * padX, y = r.w)
  rect.h = max(x = 2 * padY, y = r.h)
  rect.w = rect.w - 2 * padX
  rect.h = rect.h - 2 * padY

  rect.x = r.x + padX
  rect.y = r.y + padY

  let
    wHalf: float = rect.w / 2.0
    hHalf: float = rect.h / 2.0

  case direction
  of up:
    `result`[0] = NimRect(x: rect.x + wHalf, y: rect.y)
    `result`[1] = NimRect(x: rect.x + rect.w, y: rect.y + rect.h)
    `result`[2] = NimRect(x: rect.x, y: rect.y + rect.h)
  of right:
    `result`[0] = NimRect(x: rect.x, y: rect.y)
    `result`[1] = NimRect(x: rect.x + rect.w, y: rect.y + hHalf)
    `result`[2] = NimRect(x: rect.x, y: rect.y + rect.h)
  of down:
    `result`[0] = NimRect(x: rect.x, y: rect.y)
    `result`[1] = NimRect(x: rect.x + rect.w, y: rect.y)
    `result`[2] = NimRect(x: rect.x + wHalf, y: rect.y + rect.h)
  of left:
    `result`[0] = NimRect(x: rect.x, y: rect.y + hHalf)
    `result`[1] = NimRect(x: rect.x + rect.w, y: rect.y)
    `result`[2] = NimRect(x: rect.x + rect.w, y: rect.y + rect.h)

proc nkInbox*(px, py, x, y, w, h: cfloat): bool {.raises: [], tags: [],
    contractual.} =
  ## Check if the selected point is in the box
  ##
  ## * px - the X coordinate of the point
  ## * py - the Y coordinate of the point
  ## * x - the X coordinate of the top left corner of the box
  ## * y - the Y coordinate of the top left corner of the box
  ## * w - the width of the box
  ## * h - the height of the box
  ##
  ## Returns true if the point is in the box, otherwise false
  return ((px >= x and px < x + w) and (py >= y and py < y + h))

proc nkUnify*(clip: var NimRect; a: NimRect; x0, y0, x1, y1: cfloat) {.raises: [
    ], tags: [], contractual.} =
  ## Unify two rectangles
  ##
  ## * clip - the unified rectangle
  ## * a    - the base recrangle
  ## * x0   - the X coordinate of top left point of the second rectangle
  ## * y0   - the Y coordinate of top left point of the second rectangle
  ## * x1   - the X coordinate of bottom right point of the second rectangle
  ## * y1   - the X coordinate of bottom right point of the second rectangle
  ##
  ## Returns modified parameter clip
  clip.x = max(x = a.x, y = x0)
  clip.y = max(x = a.y, y = y0)
  clip.w = min(x = a.x + a.w, y = x1) - clip.x
  clip.h = min(x = a.y + a.h, y = y1) - clip.y
  clip.w = max(x = 0, y = clip.w)
  clip.h = max(x = 0, y = clip.h)
