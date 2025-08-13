#include ir_data.nsh

enum eMemKind : u32 {
  MK_NONE,
  MK_STACK,
  MK_HEAP,
  MK_GLOB,
  MK_COUNT,
};
struct MemKind;

struct ExecCtx {
  prev_ctx: ExecCtx*;
  f: IrFunc*;
  cur_instr: i64;
  vars: i64*; // (u32, u32) offset, size
  vars_len: i64;
  base_stack_offset: i64;
  va_args: i64*;
  va_args_len: i64;
  cur_vaa: i64;
};

struct InterpMemory {
  data: u8*;
  data_len: i64;
  capacity: i64;
};

struct Interpreter {
  memory: InterpMemory[4]; // MK_COUNT
  ir: FullIr*;
  globals: i64*; // (u32, u32) offset, size
  globals_len: i64;
  ctx: ExecCtx*;
  exit_code: i32;
  done: bool;
};

lib fn Interpreter::init(self: Interpreter*, ir: FullIr*);
lib fn Interpreter::destroy(self: Interpreter*);
lib fn Interpreter::run(self: Interpreter*);
lib fn Interpreter::debug(self: Interpreter*);
