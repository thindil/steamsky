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

NK_API void
nk_sdl_font_stash_begin(struct nk_font_atlas **atlas)
{
    *atlas = &sdl.atlas;
}

#endif /* NK_SDL_RENDERER_IMPLEMENTATION */
