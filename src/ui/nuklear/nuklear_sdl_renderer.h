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

#include <SDL2/SDL.h>
#include <SDL2/SDL_image.h>
NK_API struct nk_context*   nk_sdl_init(SDL_Window *win, SDL_Renderer *renderer);
NK_API void                 nk_sdl_font_stash_begin(struct nk_font_atlas **atlas);

#if SDL_COMPILEDVERSION < SDL_VERSIONNUM(2, 0, 22)
/* Metal API does not support cliprects with negative coordinates or large
 * dimensions. The issue is fixed in SDL2 with version 2.0.22 but until
 * that version is released, the NK_SDL_CLAMP_CLIP_RECT flag can be used to
 * ensure the cliprect is itself clipped to the viewport.
 * See discussion at https://discourse.libsdl.org/t/rendergeometryraw-producing-different-results-in-metal-vs-opengl/34953
 */
#define NK_SDL_CLAMP_CLIP_RECT
#endif

#endif /* NK_SDL_RENDERER_H_ */

/*
 * ==============================================================
 *
 *                          IMPLEMENTATION
 *
 * ===============================================================
 */
#ifdef NK_SDL_RENDERER_IMPLEMENTATION

#include <strings.h>

struct nk_sdl_device {
    struct nk_buffer cmds;
    struct nk_draw_null_texture tex_null;
    SDL_Texture *font_tex;
};

struct nk_sdl_vertex {
    float position[2];
    float uv[2];
    nk_byte col[4];
};

static struct nk_sdl {
    SDL_Window *win;
    SDL_Renderer *renderer;
    struct nk_sdl_device ogl;
    struct nk_context ctx;
    struct nk_font_atlas atlas;
} sdl;

NK_API struct nk_context*
nk_sdl_init(SDL_Window *win, SDL_Renderer *renderer)
{
#ifndef NK_SDL_CLAMP_CLIP_RECT
    SDL_RendererInfo info;
    SDL_version runtimeVer;

    /* warn for cases where NK_SDL_CLAMP_CLIP_RECT should have been set but isn't */
    SDL_GetRendererInfo(renderer, &info);
    SDL_GetVersion(&runtimeVer);
    if (strncmp("metal", info.name, 5) == 0 &&
        SDL_VERSIONNUM(runtimeVer.major, runtimeVer.minor, runtimeVer.patch) < SDL_VERSIONNUM(2, 0, 22))
    {
        SDL_LogWarn(
            SDL_LOG_CATEGORY_APPLICATION,
            "renderer is using Metal API but runtime SDL version %d.%d.%d is older than compiled version %d.%d.%d, "
            "which may cause issues with rendering",
            runtimeVer.major, runtimeVer.minor, runtimeVer.patch,
            SDL_MAJOR_VERSION, SDL_MINOR_VERSION, SDL_PATCHLEVEL
        );
    }
#endif
    sdl.win = win;
    sdl.renderer = renderer;
    nk_buffer_init_default(&sdl.ogl.cmds);
    return &sdl.ctx;
}

NK_API void
nk_sdl_font_stash_begin(struct nk_font_atlas **atlas)
{
    *atlas = &sdl.atlas;
}

#endif /* NK_SDL_RENDERER_IMPLEMENTATION */
