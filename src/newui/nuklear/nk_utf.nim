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

## Provides code related to handling Unicode like encoding, decoding, etc.

import std/unicode
import contracts, nimalyzer
import nk_types

proc nkUtfValidate(u: var nk_rune; i: int): int {.raises: [], tags: [],
    contractual.} =
  ## Validate UTF rune
  ##
  ## * u - the rune to validate
  ## * i - the index of the rune
  ##
  ## Returns i
  if u notin nkUtfMin[i]..nkUtfMax[i] or u in 0xd800.nk_rune..0xdfff.nk_rune:
    u = nkUtfInvalid
  result = 1
  while u > nkUtfMax[result]:
    result.inc

proc nkUtfDecodeByte(c: Rune; i: var int): nk_rune {.raises: [], tags: [],
  contractual.} =
  ## Decode one UTF rune
  ##
  ## * c - the UTF rune to decode
  ## * i - the lenght of the text
  ##
  ## Returns modified parameter i and UTF code of the rune
  let
    s: string = c.toUTF8
    a: seq[byte] = @(s.toOpenArrayByte(first = 0, last = s.high))
  i = a.len
  return c.nk_rune

proc nkUtfDecode*(c: string; u: var nk_rune): Natural {.raises: [],
  tags: [], contractual.} =
  ## Decode UTF text
  ##
  ## * c    - the text to decode
  ## * u    - the UTF code
  ## * clen - the lenght of the text
  ##
  ## Returns the length of the rune in bytes
  if c == "":
    return 0
  var len: int = 0
  u = nkUtfDecodeByte(c = c.toRunes[0], i = len)
  return nkUtfValidate(u = u, i = len)

# --------------------------------
# Temporary exports for old C code
# --------------------------------

proc nk_utf_validate(u: var nk_rune; i: cint): cint {.raises: [], tags: [],
    contractual, exportc.} =
  ## Temporary C binding. Internal use only
  ##
  ## * u - the rune to validate
  ## * i - the index of the rune
  ##
  ## Returns i
  return nkUtfValidate(u = u, i = i.int).cint

proc nk_utf_decode(c: pointer; u: var nk_rune; clen: cint): cint {.raises: [],
  tags: [], contractual, exportc, ruleOff: "params".} =
  ## Temporary C binding. Internal use only
  ##
  ## * c    - the text to decode
  ## * u    - the UTF code
  ## * clen - the lenght of the text
  ##
  ## Returns the length of the rune in bytes
  let text: cstring = cast[cstring](c)
  return nkUtfDecode(c = $text, u = u).cint
