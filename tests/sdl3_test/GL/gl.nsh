type GLclampf = f32;
type GLbitfield = u32;

enum : GLbitfield {
  GL_DEPTH_BUFFER_BIT = 0x00000100,
  GL_COLOR_BUFFER_BIT = 0x00004000,
};

lib fn glClearColor(red: GLclampf, green: GLclampf, blue: GLclampf, alpha: GLclampf);
lib fn glClear(mask: GLbitfield);
