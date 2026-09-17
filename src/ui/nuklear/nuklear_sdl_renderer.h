/*
 * Nuklear - 4.9.4 - public domain
 */
/*
 * ==============================================================
 *
 *                              API
 *
 * ===============================================================
 */
#ifndef NK_SDL_RENDERER_H_
#define NK_SDL_RENDERER_H_

/*NK_API struct nk_context*   nk_sdl_init(SDL_Window *win, SDL_Renderer *renderer);*/
NK_API void                 nk_sdl_font_stash_begin(struct nk_font_atlas **atlas);

#endif /* NK_SDL_RENDERER_H_ */

/*
 * ==============================================================
 *
 *                          IMPLEMENTATION
 *
 * ===============================================================
 */
#ifdef NK_SDL_RENDERER_IMPLEMENTATION

/*NK_API struct nk_context*
nk_sdl_init(SDL_Window *win, SDL_Renderer *renderer)
{
    sdl.win = win;
    sdl.renderer = renderer;
    nk_buffer_init_default(&sdl.ogl.cmds);
    return &sdl.ctx;
}*/

NK_API void
nk_sdl_font_stash_begin(struct nk_font_atlas **atlas)
{
    *atlas = &sdl.atlas;
}

#endif /* NK_SDL_RENDERER_IMPLEMENTATION */
