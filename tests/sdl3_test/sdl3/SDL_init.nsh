type SDL_InitFlags = u32;
enum : SDL_InitFlags {
  SDL_INIT_VIDEO = 0x20,
};

lib fn SDL_Init(flags: SDL_InitFlags) -> bool;
lib fn SDL_Quit();
