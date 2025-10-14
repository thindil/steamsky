# Copyright © 2023-2025 Bartek Jasicki
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
  SDL_INIT_VIDEO: cint = 0x00000020
  SDL_WINDOWPOS_CENTERED: cint = 0x2FFF0000 or 0
  SDL_WINDOW_SHOWN: cuint = 0x00000004
  SDL_WINDOW_ALLOW_HIGHDPI: cuint = 0x00002000
  SDL_RENDERER_ACCELERATED: cint = 0x00000002
  SDL_RENDERER_PRESENTVSYNC: cint = 0x0000000
  SDLK_RSHIFT: uint = 0x400000e5u
  SDLK_LSHIFT: uint = 0x400000e1u
  SDLK_DELETE: uint = 0x0000007fu
  SDLK_RETURN: uint = 0x0000000du
  SDLK_TAB: uint = 0x00000009u
  SDLK_BACKSPACE: uint = 0x00000008u
  SDLK_HOME: uint = 0x4000004au
  SDLK_END: uint = 0x4000004du
  SDLK_PAGEDOWN: uint = 0x4000004eu
  SDLK_PAGEUP: uint = 0x4000004bu
  SDLK_Z: uint = 0x0000007au
  SDLK_R: uint = 0x00000072u
  SDLK_C: uint = 0x00000063u
  SDLK_V: uint = 0x00000076u
  SDLK_X: uint = 0x00000078u
  SDLK_B: uint = 0x00000062u
  SDLK_E: uint = 0x00000065u
  SDLK_UP: uint = 0x40000052u
  SDLK_DOWN: uint = 0x40000051u
  SDLK_LEFT: uint = 0x40000050u
  SDLK_RIGHT: uint = 0x4000004fu
  SDLK_ESCAPE: uint = 0x0000001bu
  IMG_INIT_PNG: cint = 0x00000002
  windowCentered*: cint = SDL_WINDOWPOS_CENTERED ## The centered position of a window

type
  SDL_EventType = enum
    SDL_FIRSTEVENT = 0, SDL_QUIT = 0x100, SDL_WINDOWEVENT = 0x200,
        SDL_KEYDOWN = 0x300, SDL_KEYUP, SDL_MOUSEMOTION = 0x400,
        SDL_MOUSEBUTTONDOWN, SDL_MOUSEBUTTONUP
  SDL_WindowEventId = enum
    SDL_WINDOWEVENT_SIZE_CHANGED = 6
  SDL_Window {.importc, nodecl.} = object
  SDL_Renderer {.importc, nodecl.} = object
  SDL_Surface {.importc, nodecl.} = object
  SDL_Texture {.importc, nodecl.} = object
  SDL_RWops {.importc, nodecl.} = object
  WindowPtr = ptr SDL_Window
  RendererPtr = ptr SDL_Renderer
  SurfacePtr = ptr SDL_Surface
  TexturePtr = ptr SDL_Texture
  RWPtr = ptr SDL_RWops
  SDL_Event {.importc, nodecl.} = object
    `type`: cuint
  SDL_WindowEvt {.importc: "SDL_WindowEvent", nodecl.} = object
    `type`: cuint
    event: cuint
  SDL_KeySym {.importc, nodecl.} = object
    sym: cuint
  SDL_KeyboardEvent{.importc, nodecl.} = object
    `type`: cuint
    keysym: SDL_KeySym
  SDL_MouseButtonEvent{.importc, nodecl.} = object
    `type`: cuint
    button, clicks: uint8
    x, y: cint
  SDL_Scancode = enum
    SDL_SCANCODE_LCTRL = 224
  SDL_Mouse_Buttons = enum
    SDL_BUTTON_LEFT = 1, SDL_BUTTON_MIDDLE, SDL_BUTTON_RIGHT

proc SDL_SetHint(name, value: cstring) {.importc, nodecl, raises: [], tags: [], contractual.}
  ## Internal SDL binding
proc SDL_Init(flags: cint): cint {.importc, nodecl, raises: [], tags: [], contractual.}
  ## Internal SDL binding
proc SDL_CreateWindow(title: cstring; x, y, w, h: cint;
    flags: cuint): WindowPtr {.importc, nodecl, raises: [], tags: [], contractual.}
  ## Internal SDL binding
proc SDL_Log(fmt: cstring) {.importc, varargs, nodecl, raises: [], tags: [], contractual.}
  ## Internal SDL binding
proc SDL_GetError(): cstring {.importc, nodecl, raises: [], tags: [], contractual.}
  ## Internal SDL binding
proc SDL_CreateRenderer(window: WindowPtr; index,
    flags: cint): RendererPtr {.importc, nodecl, raises: [], tags: [], contractual.}
  ## Internal SDL binding
proc SDL_GetRendererOutputSize(renderer: RendererPtr; w, h: var cint) {.importc,
    nodecl, raises: [], tags: [], contractual.}
  ## Internal SDL binding
proc SDL_GetWindowSize(window: WindowPtr; w, h: var cint) {.importc, nodecl,
    raises: [], tags: [], contractual.}
  ## Internal SDL binding
proc SDL_RenderSetScale(renderer: RendererPtr; scaleX,
    scaleY: cfloat) {.importc, nodecl, raises: [], tags: [], contractual.}
  ## Internal SDL binding
proc SDL_PollEvent(event: var SDL_Event): cint {.importc, nodecl, raises: [],
    tags: [], contractual.}
  ## Internal SDL binding
proc SDL_SetRenderDrawColor(renderer: RendererPtr; r, g, b,
    a: uint8): cint {.importc, nodecl, raises: [], tags: [], contractual.}
  ## Internal SDL binding
proc SDL_RenderClear(renderer: RendererPtr): cint {.importc, nodecl, raises: [],
    tags: [], contractual.}
  ## Internal SDL binding
proc SDL_RenderPresent(renderer: RendererPtr) {.importc, nodecl, raises: [],
    tags: [], contractual.}
  ## Internal SDL binding
proc SDL_DestroyRenderer(renderer: RendererPtr) {.importc, nodecl, raises: [],
    tags: [], contractual.}
  ## Internal SDL binding
proc SDL_DestroyWindow(window: WindowPtr) {.importc, nodecl, raises: [], tags: [], contractual.}
  ## Internal SDL binding
proc SDL_Quit() {.importc, nodecl, raises: [], tags: [], contractual.}
  ## Internal SDL binding
proc SDL_SetWindowIcon(window: WindowPtr; icon: SurfacePtr) {.importc, nodecl,
    raises: [], tags: [], contractual.}
  ## Internal SDL binding
proc SDL_CreateTextureFromSurface(renderer: RendererPtr;
    surface: SurfacePtr): TexturePtr {.importc, nodecl, raises: [], tags: [], contractual.}
  ## Internal SDL binding
proc SDL_FreeSurface(surface: SurfacePtr) {.importc, nodecl, raises: [], tags: [], contractual.}
  ## Internal SDL binding
proc SDL_RWFromFile(file, mode: cstring): RWPtr {.importc, nodecl, raises: [],
    tags: [], contractual.}
  ## Internal SDL binding
proc SDL_SetWindowSize(window: WindowPtr; w, h: cint) {.importc, nodecl,
    raises: [], tags: [], contractual.}
  ## Internal SDL binding
proc SDL_SetWindowPosition(window: WindowPtr; x, y: cint) {.importc, nodecl,
    raises: [], tags: [], contractual.}
  ## Internal SDL binding
proc SDL_SetWindowResizable(window: WindowPtr; resizable: cint) {.importc,
    nodecl, raises: [], tags: [], contractual.}
  ## Internal SDL binding
proc SDL_GetKeyboardState(numkeys: ptr int = nil): ptr array[512,
    uint8] {.importc, nodecl, raises: [], tags: [], contractual.}
  ## Internal SDL Image binding
proc IMG_Init(flags: cint): cint {.importc, nodecl, raises: [], tags: [], contractual.}
  ## Internal SDL Image binding
proc IMG_Load(file: cstring): SurfacePtr {.importc, nodecl, raises: [], tags: [], contractual.}
  ## Internal SDL Image binding
proc IMG_LoadSizedSVG_RW(src: RWPtr; width, height: cint): SurfacePtr {.importc,
    nodecl, raises: [], tags: [], contractual.}
  ## Internal SDL Image binding
proc IMG_Quit() {.importc, nodecl, raises: [], tags: [], contractual.}
  ## Internal SDL Image binding

# Nuklear SDL2 backend bindings

proc nk_sdl_init(win: WindowPtr; renderer: RendererPtr): PContext {.importc,
    nodecl, raises: [], tags: [], contractual.}
  ## Internal Nuklear binding
proc nk_sdl_font_stash_begin(atlas: ptr ptr nk_font_atlas) {.importc, nodecl,
    raises: [], tags: [], contractual.}
  ## Internal Nuklear binding
proc nk_sdl_font_stash_end() {.importc, nodecl, raises: [], tags: [], contractual.}
  ## Internal Nuklear binding
proc nk_sdl_handle_event(evt: var SDL_Event): cint {.importc, nodecl, raises: [
    ], tags: [], contractual.}
  ## Internal Nuklear binding
proc nk_sdl_render(aa: AntiAliasing) {.importc, nodecl, raises: [], tags: [], contractual.}
  ## Internal Nuklear binding
proc nk_sdl_shutdown() {.importc, nodecl, raises: [], tags: [], contractual.}
  ## Internal Nuklear binding

# High level bindings

type
  FontData* = object
    ## Used to store data about an application's font
    path*: string
    size*: Positive = 14

var
  win: WindowPtr = nil        ## The main X window of the program
  renderer: RendererPtr = nil ## The SDL renderer
  fontScale: cfloat = 0.0     ## The scale used to resize a font

proc nuklearInit*(windowWidth, windowHeight: int; name: string = "";
    iconPath: string = ""): PContext {.discardable, raises: [], tags: [],
        contractual.} =
  ## Initialize Nuklear library, create the main program's window with the
  ## selected parameters.
  ##
  ## * windowWidth  - the default main window width
  ## * windowHeight - the default main window height
  ## * name         - the title of the main window
  ## * iconPath     - the full path to the window's icon. Default value is empty.
  SDL_SetHint(name = "SDL_HINT_VIDEO_HIGHDPI_DISABLED", value = "0")
  discard SDL_Init(flags = SDL_INIT_VIDEO)
  discard IMG_Init(flags = IMG_INIT_PNG)
  win = SDL_CreateWindow(title = name.cstring, x = SDL_WINDOWPOS_CENTERED,
      y = SDL_WINDOWPOS_CENTERED, w = windowWidth.cint, h = windowHeight.cint,
          flags = SDL_WINDOW_SHOWN or SDL_WINDOW_ALLOW_HIGHDPI)
  if win == nil:
    {.ruleOff: "namedparams".}
    SDL_Log("Error SDL_CreateWindow %s", SDL_GetError())
    {.ruleOn: "namedparams".}
    quit QuitFailure
  renderer = SDL_CreateRenderer(window = win, index = -1,
      flags = SDL_RENDERER_ACCELERATED or SDL_RENDERER_PRESENTVSYNC)
  if renderer == nil:
    {.ruleOff: "namedparams".}
    SDL_Log("Error SDL_CreateRenderer %s", SDL_GetError())
    {.ruleOn: "namedparams".}
    quit QuitFailure
  var renderW, renderH, windowW, windowH: cint = 0
  SDL_GetRendererOutputSize(renderer = renderer, w = renderW, h = renderH)
  SDL_GetWindowSize(window = win, w = windowW, h = windowH)
  if iconPath.len > 0:
    SDL_SetWindowIcon(window = win, icon = IMG_Load(file = iconPath.cstring))
  let scaleX: cfloat = renderW.cfloat / windowW.cfloat
  let scaleY: cfloat = renderH.cfloat / windowH.cfloat
  SDL_RenderSetScale(renderer = renderer, scaleX = scaleX, scaleY = scaleY)
  fontScale = scaleY
  setContext(context = nk_sdl_init(win = win, renderer = renderer))
  return getContext()

proc nuklearInput*(): UserEvents {.raises: [], tags: [], contractual.} =
  ## Handle the user input
  ##
  ## Returns true if user requested to close the window, otherwise false
  let ctx: PContext = getContext()
  var evt: SDL_Event = SDL_Event()
  nk_input_begin(ctx = ctx)
  result = noEvent
  while SDL_PollEvent(event = evt) != 0:
    case evt.`type`
    of SDL_QUIT.cuint:
      return quitEvent
    of SDL_WINDOWEVENT.cuint:
      let wEvt: SDL_WindowEvt = cast[SDL_WindowEvt](evt)
      if wEvt.event == SDL_WINDOWEVENT_SIZE_CHANGED.cuint:
        return sizeChangedEvent
    of SDL_KEYUP.cuint, SDL_KEYDOWN.cuint:
      result = keyEvent
      let
        down: nk_bool = (evt.`type` == SDL_KEYDOWN.cuint).nk_bool
        state: ptr array[512, uint8] = SDL_GetKeyboardState()
        kEvnt: SDL_KeyboardEvent = cast[SDL_KeyboardEvent](evt)
      case kEvnt.keysym.sym
      of SDLK_RSHIFT.cuint, SDLK_LSHIFT.cuint:
        nk_input_key(ctx = ctx, key = keyShift, down = down)
      of SDLK_DELETE.cuint:
        nk_input_key(ctx = ctx, key = keyDel, down = down)
      of SDLK_RETURN.cuint:
        nk_input_key(ctx = ctx, key = keyEnter, down = down)
      of SDLK_TAB.cuint:
        nk_input_key(ctx = ctx, key = keyTab, down = down)
      of SDLK_BACKSPACE.cuint:
        nk_input_key(ctx = ctx, key = keyBackspace, down = down)
      of SDLK_HOME.cuint:
        nk_input_key(ctx = ctx, key = keyTextStart, down = down)
        nk_input_key(ctx = ctx, key = keyScrollStart, down = down)
      of SDLK_END.cuint:
        nk_input_key(ctx = ctx, key = keyTextEnd, down = down)
        nk_input_key(ctx = ctx, key = keyScrollEnd, down = down)
      of SDLK_PAGEDOWN.cuint:
        nk_input_key(ctx = ctx, key = keyScrollDown, down = down)
      of SDLK_PAGEUP.cuint:
        nk_input_key(ctx = ctx, key = keyScrollUp, down = down)
      of SDLK_Z.cuint:
        nk_input_key(ctx = ctx, key = keyTextUndo, down = (down and (state[
            SDL_SCANCODE_LCTRL.ord] == 1)).nk_bool)
      of SDLK_R.cuint:
        nk_input_key(ctx = ctx, key = keyTextRedo, down = (down and (state[
            SDL_SCANCODE_LCTRL.ord] == 1)).nk_bool)
      of SDLK_C.cuint:
        nk_input_key(ctx = ctx, key = keyCopy, down = (down and (state[
            SDL_SCANCODE_LCTRL.ord] == 1)).nk_bool)
      of SDLK_V.cuint:
        nk_input_key(ctx = ctx, key = keyPaste, down = (down and (state[
            SDL_SCANCODE_LCTRL.ord] == 1)).nk_bool)
      of SDLK_X.cuint:
        nk_input_key(ctx = ctx, key = keyCut, down = (down and (state[
            SDL_SCANCODE_LCTRL.ord] == 1)).nk_bool)
      of SDLK_B.cuint:
        nk_input_key(ctx = ctx, key = keyTextLineStart, down = (down and (state[
            SDL_SCANCODE_LCTRL.ord] == 1)).nk_bool)
      of SDLK_E.cuint:
        nk_input_key(ctx = ctx, key = keyTextLineEnd, down = (down and (state[
            SDL_SCANCODE_LCTRL.ord] == 1)).nk_bool)
      of SDLK_UP.cuint:
        nk_input_key(ctx = ctx, key = keyUp, down = down)
      of SDLK_DOWN.cuint:
        nk_input_key(ctx = ctx, key = keyDown, down = down)
      of SDLK_LEFT.cuint:
        if state[SDL_SCANCODE_LCTRL.ord] == 1:
          nk_input_key(ctx = ctx, key = keyTextWordLeft, down = down)
        else:
          nk_input_key(ctx = ctx, key = keyLeft, down = down)
      of SDLK_RIGHT.cuint:
        if state[SDL_SCANCODE_LCTRL.ord] == 1:
          nk_input_key(ctx = ctx, key = keyTextWordRight, down = down)
        else:
          nk_input_key(ctx = ctx, key = keyRight, down = down)
      of SDLK_ESCAPE.cuint:
        nk_input_key(ctx = ctx, key = keyEscape, down = down)
      else:
        result = noEvent
    of SDL_MOUSEBUTTONDOWN.cuint, SDL_MOUSEBUTTONUP.cuint:
      result = mouseButtonEvent
      let
        down: nk_bool = (evt.`type` == SDL_MOUSEBUTTONDOWN.cuint).nk_bool
        mEvnt: SDL_MouseButtonEvent = cast[SDL_MouseButtonEvent](evt)
        x: cint = mEvnt.x
        y: cint = mEvnt.y
      case mEvnt.button:
      of SDL_BUTTON_LEFT.uint8:
        if mEvnt.clicks > 1:
          nk_input_button(ctx = ctx, id = double, x = x, y = y, down = down)
        nk_input_button(ctx = ctx, id = left, x = x, y = y, down = down)
      of SDL_BUTTON_MIDDLE.uint8:
        nk_input_button(ctx = ctx, id = middle, x = x, y = y, down = down)
      of SDL_BUTTON_RIGHT.uint8:
        nk_input_button(ctx = ctx, id = right, x = x, y = y, down = down)
      else:
        discard
    else:
      discard nk_sdl_handle_event(evt = evt)
      result = anyEvent
  nk_input_end(ctx = ctx)

proc nuklearDraw*() {.raises: [], tags: [], contractual.} =
  ## Draw the main window content
  discard SDL_SetRenderDrawColor(renderer = renderer, r = (0.10 * 255).uint8,
      g = (0.18 * 255).uint8, b = (0.24 * 255).uint8, a = 255)
  discard SDL_RenderClear(renderer = renderer)
  nk_sdl_render(aa = antiAliasingOn)
  SDL_RenderPresent(renderer = renderer)

proc nuklearClose*() {.raises: [], tags: [], contractual.} =
  ## Release all resources related to Xlib and Nuklear
  nk_sdl_shutdown()
  SDL_DestroyRenderer(renderer = renderer)
  SDL_DestroyWindow(window = win)
  IMG_Quit()
  SDL_Quit()

proc nuklearLoadSVGImage*(filePath: string; width,
    height: int): PImage {.raises: [NuklearException], tags: [], contractual.} =
  ## Load the selected SVG image from a file
  ##
  ## * filePath - the full path to the file from which the image will be loaded
  ##
  ## Returns the nk_image structure
  let img: RWPtr = SDL_RWFromFile(file = filePath.cstring, mode = "r")
  if img == nil:
    raise newException(exceptn = NuklearException, message = $(SDL_GetError()))
  let surface: SurfacePtr = IMG_LoadSizedSVG_RW(src = img, width = width.cint,
      height = height.cint)
  if surface == nil:
    raise newException(exceptn = NuklearException, message = $(SDL_GetError()))
  let image: TexturePtr = SDL_CreateTextureFromSurface(renderer = renderer,
      surface = surface)
  if image == nil:
    raise newException(exceptn = NuklearException, message = $(SDL_GetError()))
  SDL_FreeSurface(surface = surface)
  return image

proc nuklearLoadFont*(font: FontData; glyphsRanges: openArray[nk_rune] = [
    ]): ptr nk_font {.raises: [], tags: [], contractual.} =
  ## Load a font from file with the selected size
  ##
  ## * font         - the font to load. Its path and size
  ## * glyphsRanges - Optional, the list of glyphs ranges to load. The list must
  ##                  be terminated with zero.
  ##
  ## Returns the pointer for the font
  var
    atlas: ptr nk_font_atlas = nil
    config: nk_font_config = new_nk_font_config(pixelHeight = 0)
  if glyphsRanges.len > 0:
    config.`range` = glyphsRanges.addr
  nk_sdl_font_stash_begin(atlas = atlas.unsafeAddr)
  {.ruleOff: "namedParams".}
  result = nk_font_atlas_add_from_file(atlas = atlas,
      filePath = font.path.cstring, height = font.size.cfloat * fontScale, config.addr)
  {.ruleOn: "namedParams".}
  nk_sdl_font_stash_end()

proc nuklearSetDefaultFont*(defaultFont: ptr nk_font = nil;
    fontSize: int = 14) {.raises: [], tags: [], contractual.} =
  ## Set the default font for an application
  ##
  ## * defaultFont - the pointer to the nk_font which will be used as default
  ##                 font. If nil, the default Nuklear font will be used.
  ## * fontSize    - the size of the font used in the UI. Default values is 14.
  var
    atlas: ptr nk_font_atlas = nil
    config: nk_font_config = new_nk_font_config(pixelHeight = 0)
    font: ptr nk_font = nil
  nk_sdl_font_stash_begin(atlas = atlas.unsafeAddr)
  if defaultFont == nil:
    font = nk_font_atlas_add_default(atlas = atlas, height = fontSize.cfloat *
        fontScale, config = config.unsafeAddr)
  else:
    font = defaultFont
  nk_sdl_font_stash_end()
  nk_style_set_font(ctx = getContext(), font = font.handle.unsafeAddr)

proc nuklearResizeWin*(width, height: int) {.raises: [], tags: [],
    contractual.} =
  ## Resize the main window of the application
  ##
  ## * width  - the new width of the main window
  ## * height - the new height of the main window
  SDL_SetWindowSize(window = win, w = width.cint, h = height.cint)

proc nuklearSetWindowPos*(x, y: int) {.raises: [], tags: [], contractual.} =
  ## Set the window position on the screen, related to the upper left corner
  ## of the screen
  ##
  ## * x - the x coordinate of the window in screen coordinates or
  ##       windowCentered
  ## * y - the y coordinate of the window in screen coordinates or
  ##       windowCentered
  SDL_SetWindowPosition(window = win, x = x.cint, y = y.cint)

proc nuklearSetWindowResizable*(resizable: bool = true) {.raises: [], tags: [],
    contractual.} =
  ## Set the main window of application resizable, or not
  ##
  ## * resizable - if true, the window will be resizable, otherwise not
  SDL_SetWindowResizable(window = win, resizable = resizable.ord.cint)

proc nuklearGetWindowSize*(): tuple[w: float; h: float] {.raises: [], tags: [],
    contractual.} =
  ## Get the current size of the main window of the application
  ##
  ## Returns a tuple with width and height of the window.
  var winWidth, winHeight: cint = 0
  SDL_GetWindowSize(window = win, w = winWidth, h = winHeight)
  return (winWidth.float, winHeight.float)
