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

## Provides code related to drawing on the screen

import contracts
import nk_buffer, nk_math, nk_types

proc nkCommandBufferPush*(b: PNkCommandBuffer; t: CommandType;
    size: nk_size): pointer {.raises: [], tags: [RootEffect], contractual.} =
  ## Add a command to the commands buffer. Internal use only
  ##
  ## * b    - the buffer to which to command will be added
  ## * t    - the type of command
  ## * size - the size of command to add
  require:
    b != nil
    b.base != nil
  body:
    if b == nil:
      return nil
    const align: nk_size = alignof(x = Command)
    return nil
#    let cmd: ptr Command = cast[ptr Command](nkBufferAlloc(b = b.base,
#        bufferAlloc = bufferFront, size = size, align = align))
#    if cmd == nil:
#      return nil
#
#    # make sure the offset to the next command is aligned
#    b.last = cast[nk_size](cast[ptr nk_byte](cmd)) - cast[nk_size](cast[
#        ptr nk_byte](b.base.memory.`ptr`))
#    let
#      unaligned: pointer = cast[ptr nk_byte](cmd) + size
#      memory: pointer = cast[pointer]((cast[nk_size](unaligned) + (align -
#          1)) and not(align - 1))
#      alignment: nk_size = cast[nk_size](cast[ptr nk_byte](memory)) - cast[
#          nk_size](cast[ptr nk_byte](unaligned))
#    cmd.cmdType = t
#    cmd.next = b.base.allocated + alignment
#    when defined(nkIncludeCommandUserData):
#      cmd.userdata = b.userdata
#    b.`end` = cmd.next
#    return cmd

proc nkFillRect*(b: PNkCommandBuffer; rect: Rect; rounding: float;
  c: nk_color) {.raises: [], tags: [RootEffect], contractual.} =
  ## Fill the rectangle with the selected color
  ##
  ## * b        - the command buffer in which the rectangle will be drawn
  ## * rect     - the rectangle which will be filled with color
  ## * rounding - if bigger than zero, round the corners of the rectangle
  ## * c        - the color to fill the rectangle
  if b == nil or rect.w == 0 or rect.h == 0:
    return
  if b.use_clipping == 1:
    let clip: nk_rect = b.clip
    if not nkIntersect(x0 = rect.x, y0 = rect.y, w0 = rect.w, h0 = rect.h,
      x1 = clip.x, y1 = clip.y, w1 = clip.w, h1 = clip.h):
      return

  var cmd: ptr nk_command_rect_filled = nil
  cmd = cast[ptr nk_command_rect_filled](nkCommandBufferPush(b = b,
    t = commandRectFilled, size = cmd.sizeof))
  if cmd == nil:
    return
  cmd.rounding = rounding.cushort
  cmd.x = rect.x.cshort
  cmd.y = rect.y.cshort
  cmd.w = max(x = 0, y = rect.w).cushort
  cmd.h = max(x = 0, y = rect.h).cushort
  cmd.color = c

