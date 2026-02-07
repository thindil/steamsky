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

## Provides code for various utility code

import contracts, nimalyzer
import nk_types

proc nkMemSet*(pData: pointer; c0: int; size: nk_size) {.raises: [], tags: [],
    contractual.} =
  ## Set the memory for the selected data
  ##
  ## * pData - the pointer to the data which will be set
  ## * c0    - the index from which the memory will be set
  ## * size  - the size of the data to set
  var c: uint = 0
  const
    nkWsize: int = uint.sizeof
    nkWmask: int = nkWsize - 1

  c = c0.nk_byte
  if c != 0:
    c = (c shl 8) or c # at least 16-bits
    if nkWsize > 2:
      c = (c shl 16) or c # at least 32-bits

  {.ruleOff: "assignments".}
  # too small of a word count
  var dst: ptr nk_byte = cast[ptr nk_byte](pData)
  var localSize: nk_size = size
  if localSize < 3 * nkWsize:
    while localSize > 0:
      dst[] = c0.nk_byte
      dst = dst + 1
      localSize.dec

  # align destination
  var t: nk_size = cast[nk_size](dst)
  if (t and nkWmask) != 0:
    t = nkWsize - t
    localSize -= t
    t.dec
    while t != 0:
      dst[] = c0.nk_byte
      dst = dst + 1
      t.dec
  {.ruleOn: "assignments".}

  # fill word
  t = (size / nkWsize).nk_size
  t.dec
  while t != 0:
    dst[] = c.nk_byte
    t.dec

  # fill trailing bytes
  t = size and nkWmask
  if t != 0:
    while t != 0:
      dst[] = c0.nk_byte
      t.dec

proc nkZero*(pData: pointer; size: nk_size) {.raises: [], tags: [],
    contractual.} =
  ## Set the memory for empty data
  ##
  ## * pData - the pointer to the data which will be set
  ## * size  - the size of the data to set
  nkMemSet(pData = pData, c0 = 0, size = size)
