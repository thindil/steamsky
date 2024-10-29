# Copyright © 2023-2024 Bartek Jasicki
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
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
# DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
# SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
# OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

## Provides code for Nuklear backend for SDL2 renderer

{.emit: """/*INCLUDESECTION*/
#define NK_INCLUDE_FIXED_TYPES
#define NK_INCLUDE_STANDARD_IO
#define NK_INCLUDE_STANDARD_VARARGS
#define NK_INCLUDE_DEFAULT_ALLOCATOR
#define NK_INCLUDE_VERTEX_BUFFER_OUTPUT
#define NK_INCLUDE_FONT_BAKING
#define NK_INCLUDE_DEFAULT_FONT
#define NK_IMPLEMENTATION
#define NK_SDL_RENDERER_IMPLEMENTATION
#include "nuklear.h"
#include "nuklear_sdl_renderer.h"
""".}

include nuklear

# SDL2 bindings

const
  SDL_INIT_VIDEO*: cint = 0x00000020
  SDL_WINDOWPOS_CENTERED*: cint = 0x2FFF0000 or 0
  SDL_WINDOW_SHOWN*: cuint = 0x00000004
  SDL_WINDOW_ALLOW_HIGHDPI*: cuint = 0x00002000
  SDL_RENDERER_ACCELERATED*: cint = 0x00000002
  SDL_RENDERER_PRESENTVSYNC*: cint = 0x0000000
  IMG_INIT_PNG*: cint = 0x00000002

type
  SDL_EventType = enum
    SDL_FIRSTEVENT = 0, SDL_QUIT = 0x100
  SDL_Window {.importc, nodecl.} = object
  SDL_Renderer {.importc, nodecl.} = object
  SDL_Surface {.importc, nodecl.} = object
  WindowPtr = ptr SDL_Window
  RendererPtr = ptr SDL_Renderer
  SurfacePtr = ptr SDL_Surface
  SDL_Event {.importc, nodecl.} = object
    `type`: cuint

proc SDL_SetHint(name, value: cstring) {.importc, nodecl.}
proc SDL_Init(flags: cint): cint {.importc, nodecl.}
proc SDL_CreateWindow(title: cstring; x, y, w, h: cint;
    flags: cuint): WindowPtr {.importc, nodecl.}
proc SDL_Log(fmt: cstring) {.importc, varargs, nodecl.}
proc SDL_GetError(): cstring {.importc, nodecl.}
proc SDL_CreateRenderer(window: WindowPtr; index,
    flags: cint): RendererPtr {.importc, nodecl.}
proc SDL_GetRendererOutputSize(renderer: RendererPtr; w, h: var cint) {.importc, nodecl.}
proc SDL_GetWindowSize(window: WindowPtr; w, h: var cint) {.importc, nodecl.}
proc SDL_RenderSetScale(renderer: RendererPtr; scaleX,
    scaleY: cfloat) {.importc, nodecl.}
proc SDL_PollEvent(event: ptr SDL_Event): cint {.importc, nodecl.}
proc SDL_SetRenderDrawColor(renderer: RendererPtr; r, g, b,
    a: uint8): cint {.importc, nodecl.}
proc SDL_RenderClear(renderer: RendererPtr): cint {.importc, nodecl.}
proc SDL_RenderPresent(renderer: RendererPtr) {.importc, nodecl.}
proc SDL_DestroyRenderer(renderer: RendererPtr) {.importc, nodecl.}
proc SDL_DestroyWindow(window: WindowPtr) {.importc, nodecl.}
proc SDL_Quit() {.importc, nodecl.}
proc SDL_SetWindowIcon(window: WindowPtr, icon: SurfacePtr) {.importc, nodecl.}
proc IMG_Init(flags: cint): cint {.importc, nodecl.}
proc IMG_Load(file: cstring): SurfacePtr {.importc, nodecl.}
proc IMG_Quit() {.importc, nodecl.}

# Nuklear SDL2 backend bindings

proc nk_sdl_init(win: WindowPtr; renderer: RendererPtr): PContext {.importc, nodecl.}
proc nk_sdl_font_stash_begin(atlas: ptr ptr nk_font_atlas) {.importc, nodecl.}
proc nk_sdl_font_stash_end() {.importc, nodecl.}
proc nk_sdl_handle_event(evt: ptr SDL_Event): cint {.importc, nodecl.}
proc nk_sdl_render(aa: nk_anti_aliasing) {.importc, nodecl.}
proc nk_sdl_shutdown() {.importc, nodecl.}

# High level bindings

var
  win: WindowPtr        ## The main X window of the program
  renderer: RendererPtr ## The SDL renderer

proc nuklearInit*(windowWidth, windowHeight: cint; name: cstring = "";
    fontPath: cstring = ""; fontSize: cint = 14; iconPath: cstring = ""): PContext {.discardable.} =
  ## Initialize Nuklear library, create the main program's window with the
  ## selected parameters.
  ##
  ## * windowWidth  - the default main window width
  ## * windowHeight - the default main window height
  ## * name         - the title of the main window
  ## * fontPath     - the full path to the default UI font. If empty, use the
  ##                  default system font. Default value is empty.
  ## * fontSize     - the size of the font used in the UI. Default values is 14.
  ## * iconPath     - the full path to the window's icon. Default value is empty.
  SDL_SetHint("SDL_HINT_VIDEO_HIGHDPI_DISABLED", "0")
  discard SDL_Init(SDL_INIT_VIDEO)
  discard IMG_Init(IMG_INIT_PNG)
  win = SDL_CreateWindow(name, SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED,
      windowWidth, windowHeight, SDL_WINDOW_SHOWN or SDL_WINDOW_ALLOW_HIGHDPI)
  if win == nil:
    SDL_Log("Error SDL_CreateWindow %s", SDL_GetError())
    quit QuitFailure
  renderer = SDL_CreateRenderer(win, -1, SDL_RENDERER_ACCELERATED or SDL_RENDERER_PRESENTVSYNC)
  if renderer == nil:
    SDL_Log("Error SDL_CreateRenderer %s", SDL_GetError())
    quit QuitFailure
  var renderW, renderH, windowW, windowH: cint
  SDL_GetRendererOutputSize(renderer, renderW, renderH)
  SDL_GetWindowSize(win, windowW, windowH)
  if iconPath.len > 0:
    SDL_SetWindowIcon(win, IMG_Load(file = iconPath))
  let scaleX: cfloat = renderW.cfloat / windowW.cfloat
  let scaleY: cfloat = renderH.cfloat / windowH.cfloat
  SDL_RenderSetScale(renderer, scaleX, scaleY)
  let fontScale = scaleY
  setContext(nk_sdl_init(win, renderer))
  var
    atlas: ptr nk_font_atlas
    config = new_nk_font_config(0)
    font: ptr nk_font
  nk_sdl_font_stash_begin(atlas.unsafeAddr)
  if fontPath.len == 0:
    font = nk_font_atlas_add_default(atlas, fontSize.cfloat * fontScale,
        config.unsafeAddr)
  else:
    font = nk_font_atlas_add_from_file(atlas, fontPath, fontSize.cfloat *
        fontScale, config.unsafeAddr)
  nk_sdl_font_stash_end()
  nk_style_set_font(getContext(), font.handle.unsafeAddr)
  return getContext()

proc nuklearInput*(): bool =
  ## Handle the user input
  ##
  ## Returns true if user requested to close the window, otherwise false
  let ctx = getContext()
  var evt: SDL_Event
  nk_input_begin(ctx)
  while SDL_PollEvent(evt.unsafeAddr) != 0:
    if evt.`type` == SDL_QUIT.cuint:
      return true
    discard nk_sdl_handle_event(evt.unsafeAddr)
  nk_input_end(ctx)

proc nuklearDraw*() =
  ## Draw the main window content
  discard SDL_SetRenderDrawColor(renderer, (0.10 * 255).uint8, (0.18 *
      255).uint8, (0.24 * 255).uint8, 255)
  discard SDL_RenderClear(renderer)
  nk_sdl_render(NK_ANTI_ALIASING_ON)
  SDL_RenderPresent(renderer)

proc nuklearClose*() =
  ## Release all resources related to Xlib and Nuklear
  nk_sdl_shutdown()
  SDL_DestroyRenderer(renderer)
  SDL_DestroyWindow(win)
  IMG_Quit()
  SDL_Quit()

proc nuklearLoadImage*(fileName: cstring): nk_image =
  discard
