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

## Provides code related to drawing on the Nuklear buffer

import contracts, nimalyzer
import nk_types

proc nkRoundUpPow2(v: nk_uint): nk_uint {.raises: [], tags: [], contractual.} =
  ## Round up power of 2 in bits. Internal use only
  ##
  ## * v - value to count
  ##
  ## Returns counted value
  result = v - 1
  {.ruleOff: "assignments".}
  result = result or (v shr 1)
  result = result or (v shr 2)
  result = result or (v shr 4)
  result = result or (v shr 8)
  result = result or (v shr 16)
  {.ruleOn: "assignments".}
  result.inc

proc nkBufferAlign*(unaligned: pointer; align: nk_size; alignment: var nk_size;
    bufferAlloc: BufferAllocationType): pointer {.raises: [], tags: [],
    contractual.} =
  ## Align the sekected buffer. Internal use only
  ##
  ## * unaligned   - the pointer to unaligned data
  ## * align       - the size of data to align
  ## * alignment   - the size of data after alignment
  ## * bufferAlloc - the allocation type
  ##
  ## Returns pointer to aligned buffer
  var memory: pointer = nil
  if bufferAlloc == bufferBack:
    if align == 0:
      memory = unaligned
      alignment = 0
    else:
      memory = cast[pointer](cast[nk_size](unaligned) and not(align - 1))
      alignment = (cast[nk_byte](unaligned) - cast[nk_byte](memory)).nk_size
  else:
    if align == 0:
      memory = unaligned
      alignment = 0
    else:
      memory = cast[pointer]((cast[nk_size](unaligned) + (align - 1)) and not(
          align - 1))
      alignment = (cast[nk_byte](memory) - cast[nk_byte](unaligned)).nk_size
  return memory

proc nkBufferRealloc(b: var Buffer; capacity: nk_size;
    size: var nk_size): pointer {.raises: [], tags: [RootEffect],
    contractual.} =
  ## Reallocate memory for the selected buffer. Internal use only
  ##
  ## * b        - the buffer which memory will be reallocated
  ## * capacity - the new capacity of the buffer
  ## * size     - the size of the buffer
  ##
  ## Returns the new pointer to the reallocated memory
  require:
    size != 0
  body:
    if (size == 0 or b.pool.alloc == nil or b.pool.free == nil):
      return nil
    let temp: pointer = try:
        b.pool.alloc(handle = b.pool.userData, old = b.memory.memPtr,
            size = capacity)
      except Exception:
        return nil

    size = capacity
    let bufferSize: nk_size = b.memory.size
    if temp != b.memory.memPtr:
      copyMem(dest = temp, source = b.memory.memPtr, size = bufferSize)
      try:
        b.pool.free(handle = b.pool.userData, old = b.memory.memPtr)
      except Exception:
        discard

    if b.size == bufferSize:
      # no back buffer so just set correct size
      b.size = capacity
      return temp

    # copy back buffer to the end of the new buffer
    let
      backSize: nk_size = bufferSize - b.size
      dst: pointer = cast[pointer](cast[ptr nk_buffer](temp) + (capacity - backSize))
      src: pointer = cast[pointer](cast[ptr nk_buffer](temp) + b.size)
    copyMem(dest = dst, source = src, size = backSize)
    b.size = capacity - backSize
    return temp

proc nkBufferAlloc*(b: var Buffer; bufferAlloc: BufferAllocationType; size,
    align: nk_size): pointer {.raises: [], tags: [RootEffect], contractual.} =
  ## Allocate memory for the selected buffer. Internal use only
  ##
  ## * b           - the buffer in which the memory will be allocated
  ## * bufferAlloc - the allocation type
  ## * size        - the size of memory to allocate
  ## * align       - the align
  ##
  ## Returns pointer to allocated memory
  require:
    size != 0
  body:
    b.needed += size
    var unaligned: ptr nk_size = nil
    # calculate total size with needed alignment + size
    if bufferAlloc == bufferFront:
      unaligned = cast[ptr nk_size](b.memory.memPtr) + b.allocated
    else:
      unaligned = cast[ptr nk_size](b.memory.memPtr) + (b.size - size)
    var alignment: nk_size = 0
    var memory: pointer = nkBufferAlign(unaligned = unaligned, align = align,
        alignment = alignment, bufferAlloc = bufferAlloc)

    var full: bool = false
    # check if buffer has enough memory
    if bufferAlloc == bufferFront:
      full = (b.allocated + size + alignment) > b.size
    else:
      full = (b.size - min(x = b.size, y = (size + alignment))) <= b.allocated

    if full:
      if b.allocType != bufferDynamic:
        return nil
      if b.allocType != bufferDynamic or b.pool.alloc == nil or b.pool.free == nil:
        return nil

      # buffer is full so allocate bigger buffer if dynamic
      var capacity: nk_size = (b.memory.size.cfloat * b.growFactor).nk_size
      capacity = max(x = capacity, y = nkRoundUpPow2(v = (b.allocated.nk_uint +
          size.nk_uint)).nk_size)
      b.memory.memPtr = cast[ptr nk_size](nkBufferRealloc(b = b,
          capacity = capacity, size = b.memory.size))
      if b.memory.memPtr == nil:
        return nil

      # align newly allocated pointer
      if bufferAlloc == bufferFront:
        unaligned = cast[ptr nk_size](b.memory.memPtr) + b.allocated
      else:
        unaligned = cast[ptr nk_size](b.memory.memPtr) + (b.size - size)
      memory = nkBufferAlign(unaligned = unaligned, align = align,
          alignment = alignment, bufferAlloc = bufferAlloc)

    if bufferAlloc == bufferFront:
      unaligned = cast[ptr nk_size](b.memory.memPtr) + b.allocated
    else:
      unaligned = cast[ptr nk_size](b.memory.memPtr) + (b.size - size)
    b.needed += alignment
    b.calls.inc
    return memory

