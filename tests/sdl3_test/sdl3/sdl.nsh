type SDL_Scancode = u32;
type SDL_Keycode = u32;

enum SDL_Enum : u32 {
  SDL_INIT_VIDEO = 0x20,
};

struct SDL_Keysym {
    scancode: SDL_Scancode;
    sym: SDL_Keycode;
    mod: u16;
    unused: u32;
};

struct SDL_KeyboardEvent {
    type_: u32;
    timestamp: u32;
    windowID: u32;
    state: u8;
    repeat: u8;
    padding2: u8;
    padding3: u8;
    keysym: SDL_Keysym;
};

struct SDL_Event {
    type_: u32;
    padding: i8[52];
};

lib fn SDL_Init(a: i64) -> i64;
lib fn SDL_CreateWindow(name: i8*, x: i64, y: i64, w: i64, h: i64, f: i64) -> void*;
lib fn SDL_GL_CreateContext(w: void*) -> void*;
lib fn SDL_GL_SwapWindow(w: void*);
lib fn SDL_PollEvent(e: SDL_Event*) -> i64;
lib fn SDL_Quit();
lib fn SDL_GL_DestroyContext(c: void*);
lib fn SDL_DestroyWindow(w: void*);
