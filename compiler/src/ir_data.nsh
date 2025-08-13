#include str.nsh

enum IrInstrKind : u32 {
  // memory/stack
  IR_STV, // STORE_VAR     STV  var_id: int                       => pops value into slot var_id
  IR_STA, // STORE_ADDR    STA  store_size: int                   => pops address and value, then store at that address
  IR_LDA, // LOAD_ADDR     LDA  load_size: int                    => pops address and pushes value found at that address
  IR_PSH, // PUSH          PSH  v: thing                          => push value onto the stack
  IR_DUP, // DUP           DUP                                    => duplicates top of stack
  IR_DRP, // DROP          DRP                                    => drops the top of the stack
  IR_VAA, // VAARG         VAA  is_float: bool                    => push the next va argument onto the stack

  // control
  IR_LBL, // LABEL         LBL  lbl: str                          => creates a label to jump to              
  IR_JMP, // JMP           JMP  lbl: str                          => jumps to a label                        
  IR_JZO, // JZ            JZO  lbl: str                          => pops top of stack and jumps if zero     
  IR_JNZ, // JNZ           JNZ  lbl: str                          => pops top of stack and jumps if not zero 

  // call
  IR_CAL, // CALL          CAL  fn: str       nparams: int        => calls a function by name with nparams on the stack. puts the return value on the stack if it exists 
  IR_BUI, // BUILTIN       BUI  fn: str       nparams: int        => same as call but for builtins                                                                       
  IR_RET, // RET           RET                                    => exits the function                                                                                  
  IR_RTV, // RETVAL        RTV                                    => pops the top of the stack and returns it                                                            

  // arithmetic
  IR_NEG, // NEG           NEG                                    => negates the top of the stack
  IR_INV, // INV           INV                                    => bitwise invert the top of the stack
  IR_NOT, // NOT           NOT                                    => logical not to top of stack
  IR_ADD, // ADD           ADD  v: thing                          => adds the top of the stack (maybe with thing)
  IR_SUB, // SUB           SUB  v: thing                          => subtracts the top of the stack (maybe with thing)
  IR_MUL, // MUL           MUL  v: thing                          => multiply
  IR_DIV, // DIV           DIV  v: thing                          => divide
  IR_REM, // REM           REM  v: thing                          => remainder
  IR_SHL, // SHL           SHL  v: thing                          => left shift
  IR_SHR, // SHR           SHR  v: thing                          => right shift
  IR_AND, // AND           AND  v: thing                          => binary and
  IR_XOR, // XOR           XOR  v: thing                          => binary xor
  IR_IOR, // IOR           IOR  v: thing                          => binary or
  IR_LTH, // LTH           LTH  v: thing                          => less than
  IR_GTH, // GTH           GTH  v: thing                          => greater than
  IR_LEQ, // LEQ           LEQ  v: thing                          => less equal
  IR_GEQ, // GEQ           GEQ  v: thing                          => greater equal
  IR_EQU, // EQU           EQU  v: thing                          => equals
  IR_NEQ, // NEQ           NEQ  v: thing                          => not equals
};

struct IR;

lib fn IR::opcode_name(opcode: IrInstrKind) -> CStr;

enum IrSpOperandKind : u32 {
  IR_OPK_NONE,
  IR_OPK_IMM,
  IR_OPK_VAR,
  IR_OPK_ADDR,
  IR_OPK_GLOB_ADDR,
  IR_OPK_GLOB_I64
};

struct IrInstr {
  opcode: IrInstrKind;
  operand1: u32;
  operand2: u32;
  operand_str: CStr;
};

lib fn IrInstr::special_arg_str(self: IrInstr*, out: String*);
lib fn IrInstr::to_str(self: IrInstr*, out: String*);
lib fn IrInstr::destroy(self: IrInstr*);
lib fn IrInstr::move_from(self: IrInstr*, other: IrInstr*);

struct IrGlobal {
  data: u8*;
  data_len: u64;
  is_lib: bool;
  is_ro: bool;
  name: CStr; // default: "####"
};

lib fn IrGlobal::destroy(self: IrGlobal*);

struct StackFrameEntry {
  size: u32;
  align: u32;
  name: CStr; // for debugging
};

lib fn StackFrameEntry::destroy(self: StackFrameEntry*);

struct ParamEntry {
  size: u32;
  is_float: bool;
};

struct IrFunc {
  returns_value: bool;
  is_vararg: bool;
  is_lib: bool;
  instructions: IrInstr*;
  instructions_len: u64;
  stack_frame: StackFrameEntry*; // includes non VAARGS params and local variables
  stack_frame_len: u64;
  params: ParamEntry*;
  params_len: u64;
  name: CStr;
};

lib fn IrFunc::destroy(self: IrFunc*);

struct FullIr {
  functions: IrFunc*;
  functions_len: u64;
  globs: IrGlobal*;
  globs_len: u64;
};

lib fn FullIr::destroy(self: FullIr*);
lib fn FullIr::has_global(self: FullIr*, name: CStr) -> i64;
lib fn FullIr::print(self: FullIr*);
lib fn FullIr::optimize(self: FullIr*);
