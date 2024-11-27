#ifndef PELEMAY_ENGINE_OPCODE_H
#define PELEMAY_ENGINE_OPCODE_H

#define MASK_INSTRUCTION 0xFFFF
#define SHIFT_INSTRUCTION 0

#define MASK_RESERVED 0xFFFFFFFFFFFF0000
#define SHIFT_RESERVED 16


enum instruction {
    INST_SCAL = 0x0,
    INST_SSCAL = 0x1,
    INST_COPY = 0x2,
    INST_DOT = 0x3,
    INST_AXPY = 0x4,
    INST_GEMV = 0x1000,
    INST_GEMM = 0x2000,
    INST_ALOADT = 0x8000,
    INST_SENDT = 0x8001,
    INST_RETURN = 0x8002,
    INST_SKIP = 0x8003,
    INST_IS_SCALAR = 0x8004,
    INST_DUP = 0x8005,
    INST_POP = 0x8006,
    INST_POP2 = 0x8007,
    INST_SWAP = 0x8008,
    INST_SENDE = 0x8009,
};


enum stack_type {
  type_undefined,
  type_tensor,
  type_scalar,
  type_error,
  type_bool,
};

enum type_binary {
  tb_s = 0,
  tb_u = 1,
  tb_f = 2,
  tb_bf = 3,
  tb_c = 6,
};

enum bit_type_binary {
  btb_8 = 0,
  btb_16 = 1,
  btb_32 = 2,
  btb_64 = 3,
  btb_c16 = 0,
  btb_c32 = 1,
  btb_c64 = 2,
  btb_c128 = 3,
};
#endif // PELEMAY_ENGINE_OPCODE_H
