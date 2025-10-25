type SDL_Scancode = u32;
type SDL_Keycode = u32;
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

lib fn SDL_PollEvent(event: SDL_Event*) -> bool;
