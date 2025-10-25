struct SDL_Window;

type SDL_WindowFlags = u64;
enum : SDL_WindowFlags {
  SDL_WINDOW_OPENGL = 0x2,
};

lib fn SDL_CreateWindow(title: i8*, w: i32, h: i32, flags: SDL_WindowFlags) -> SDL_Window*;
lib fn SDL_DestroyWindow(window: SDL_Window*);

struct SDL_GLContextState;
type SDL_GLContext = SDL_GLContextState*;

lib fn SDL_GL_CreateContext(window: SDL_Window*) -> SDL_GLContext;
lib fn SDL_GL_SwapWindow(window: SDL_Window*) -> bool;
lib fn SDL_GL_DestroyContext(context: SDL_GLContext) -> bool;
