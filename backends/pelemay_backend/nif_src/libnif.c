#include <stdbool.h>
#include <stdint.h>
#include <string.h>
#include <erl_nif.h>

#ifndef CLOCK_MONOTONIC
#define CLOCK_MONOTONIC 1
#endif

#include <cblas.h>

#include "opcode.h"

#define MAX_STACK 1024

typedef struct code {
    ErlNifUInt64 opcode;
    ERL_NIF_TERM operand;
} code_t;

typedef struct p_stack {
    enum stack_type type;
    ERL_NIF_TERM content;
} p_stack_t;

unsigned int get_degit(ErlNifUInt64 n)
{
    unsigned int digit = 1;

    while(n /= 10) {
        ++digit;
    }

    return digit;
}

bool getcode(ErlNifEnv *env, ERL_NIF_TERM list, code_t **code, unsigned *length, ERL_NIF_TERM *exception)
{
    if(__builtin_expect(!enif_get_list_length(env, list, length), false)) {
        *exception = enif_make_badarg(env);
        return false;
    }
    *code = enif_alloc(*length * sizeof(code_t));
    if(__builtin_expect(*code == NULL, false)) {
        *exception = enif_raise_exception(env, enif_make_string(env, "Fail to alloc memory", ERL_NIF_LATIN1));
        return false;
    }
    ERL_NIF_TERM tail = list;
    code_t *code_p = *code;
    unsigned l = *length;
    while(l > 0) {
        list = tail;
        ERL_NIF_TERM head;
        if(__builtin_expect(!enif_get_list_cell(env, list, &head, &tail), false)) {
            enif_free(*code);
            *exception = enif_raise_exception(env, enif_make_string(env, "Should be list", ERL_NIF_LATIN1));
            return false;
        }
        int arity;
        const ERL_NIF_TERM *array;
        if(__builtin_expect(!enif_get_tuple(env, head, &arity, &array) || arity != 2, false)) {
            enif_free(*code);
            *exception = enif_raise_exception(env, enif_make_string(env, "Should be list of tuple2", ERL_NIF_LATIN1));
            return false;
        }
        if(__builtin_expect(!enif_get_uint64(env, array[0], &code_p->opcode), false)) {
            enif_free(*code);
            *exception = enif_raise_exception(env, enif_make_string(env, "Invalid opcode", ERL_NIF_LATIN1));
            return false;
        }
        code_p->operand = array[1];
        code_p++;
        l--;
    }
    return true;
}

bool execute(ErlNifEnv *env, code_t *code, unsigned code_length, ERL_NIF_TERM *args, unsigned arg_length, ERL_NIF_TERM rpid, ERL_NIF_TERM *reason)
{
    p_stack_t stack[MAX_STACK];

    for(size_t i = 0; i < MAX_STACK; i++) {
        stack[i].type = type_undefined;
    }

    size_t stack_idx = 0;
    
    for(code_t *code_p = code; code_length > 0; code_length--, code_p++) {
        if(__builtin_expect(code_p->opcode & MASK_RESERVED, 0)) {
            *reason = enif_make_string(env, "Should not use reserved bit", ERL_NIF_LATIN1);
            return false;
        }
        uint_fast16_t inst = (code_p->opcode & MASK_INSTRUCTION) >> SHIFT_INSTRUCTION;

        // enif_fprintf(stdout, "instruction: %04X\n", inst);
        switch(inst) {
            case INST_COPY:
                {
                    // enif_fprintf(stdout, "inst: copy\n");

                    /*
                     * Copys the binary of the top of the stack.
                     * 
                     * The stak top should be type_tensor:
                     * {
                     *   Nx.size(args),
                     *   Nx.shape(args),
                     *   Nx.type(args),
                     *   Nx.to_binary(args)
                     * }
                     *
                     * Now, copy supports only in case that the operand is nil.
                     * When the operand is nil, increment of the source adn the destination are 1.
                     * 
                     * Now, copy supports only in case that Nx.type is as follows:
                     * {:f, 32}
                     * {:f, 64}
                     */

                    if(__builtin_expect(stack_idx == 0, false)) {
                        *reason = enif_make_string(env, "Stack limit is less than 0", ERL_NIF_LATIN1);
                        return false;
                    }
                    if(__builtin_expect(stack[stack_idx - 1].type != type_tensor, false)) {
                        *reason = enif_make_string(env, "Should be a tensor in case of copy", ERL_NIF_LATIN1);
                        return false;
                    }
                    int arity;
                    const ERL_NIF_TERM *array;
                    if(__builtin_expect(!enif_get_tuple(env, stack[stack_idx - 1].content, &arity, &array), false)) {
                        *reason = enif_make_string(env, "Stack top should be a tuple in case of copy", ERL_NIF_LATIN1);
                        return false;
                    }
                    if(__builtin_expect(arity != 4, false)) {
                        *reason = enif_make_string(env, "The arity of tuple should be 4 in case of copy", ERL_NIF_LATIN1);
                        return false;
                    }
                    ErlNifUInt64 size;
                    if(__builtin_expect(!enif_get_uint64(env, array[0], &size), false)) {
                        *reason = enif_make_string(env, "Fail to get uint64 in case of copy", ERL_NIF_LATIN1);
                        return false;
                    }
                    int arity_type;
                    const ERL_NIF_TERM *array_type;
                    char *type;
                    unsigned typel;
                    unsigned int type_size;
                    if(__builtin_expect(
                        !enif_get_tuple(env, array[2], &arity_type, &array_type)
                        || arity_type != 2
                        || !enif_get_atom_length(env, array_type[0], &typel, ERL_NIF_LATIN1)
                        || (type = (char *)enif_alloc((typel + 1) * sizeof(char))) == NULL
                        || !enif_get_atom(env, array_type[0], type, typel + 1, ERL_NIF_LATIN1),
                        false)) {
                        *reason = enif_make_string(env, "Fail to get type in case of copy", ERL_NIF_LATIN1);
                        return false;
                    }
                    if(__builtin_expect(
                        strncmp(type, "f", 1) != 0
                        || !enif_get_uint(env, array_type[1], &type_size)
                        || !(type_size == 32 || type_size == 64),
                        false)) {
                        *reason = enif_make_string(env, "Sorry, copy now supports only {:f, 32} or {:f, 64}", ERL_NIF_LATIN1);
                        return false;
                    }
                    ErlNifBinary bin_in;
                    if(__builtin_expect(!enif_inspect_binary(env, array[3], &bin_in), false)) {
                        *reason = enif_make_string(env, "Fail to get binary in case of copy", ERL_NIF_LATIN1);
                        return false;
                    }
                    ErlNifBinary bin_out;
                    if(__builtin_expect(!enif_alloc_binary(bin_in.size, &bin_out), false)) {
                        *reason = enif_make_string(env, "Fail to alloc memory in case of copy", ERL_NIF_LATIN1);
                        return false;
                    }
                    // omit check the operand is nil.
                    
                    switch(type_size) {
                        case 32:
                            cblas_scopy(size, (float *)bin_in.data, 1, (float *)bin_out.data, 1);
                            break;
                        
                        case 64:
                            cblas_dcopy(size, (double *)bin_in.data, 1, (double *)bin_out.data, 1);
                            break;
                        
                        default:
                            *reason = enif_make_string(env, "unexpected", ERL_NIF_LATIN1);
                            return false;
                    }


                    ERL_NIF_TERM b = enif_make_binary(env, &bin_out);
                    stack[stack_idx - 1].content = enif_make_tuple4(env, array[0], array[1], array[2], b);
                }
                break;

            case INST_SCAL:
                {
                    // enif_fprintf(stdout, "inst: scal\n");

                    /*
                     * Scales a tensor by a constant.
                     * 
                     * Pops the two values from the stack, and push the result.
                     * 
                     * The operand should be the positive integer as increment.
                     * 
                     * The stack top should be type_scalar:
                     * {
                     *   Nx.size(args),
                     *   Nx.shape(args),
                     *   Nx.type(args),
                     *   Nx.to_binary(args)
                     * }
                     * 
                     * The next of it should be type_tensor or type_scalar:
                     * {
                     *   Nx.size(args),
                     *   Nx.shape(args),
                     *   Nx.type(args),
                     *   Nx.to_binary(args)
                     * }
                     *
                     * Now, scal supports only in case that Nx.type is as follows:
                     * {:f, 32}
                     * {:f, 64}
                     */

                    if(__builtin_expect(stack_idx <= 1, false)) {
                        *reason = enif_make_string(env, "Stack limit is less than 1", ERL_NIF_LATIN1);
                        return false;
                    }
                    stack_idx -= 2;
                    // stack[stack_idx + 1].type == type_scalar
                    // stack[stack_idx].type == type.tensor or type.scalar

                    // Get the given tensor
                    if(__builtin_expect(
                        !(stack[stack_idx].type == type_tensor || stack[stack_idx].type == type_scalar),
                        false)) {
                        *reason = enif_make_string(env, "Should be a tensor or a scalar in case of scal", ERL_NIF_LATIN1);
                        return false;
                    }
                    int arity;
                    const ERL_NIF_TERM *array;
                    if(__builtin_expect(!enif_get_tuple(env, stack[stack_idx].content, &arity, &array), false)) {
                        *reason = enif_make_string(env, "Stack top should be a tuple in case of scal", ERL_NIF_LATIN1);
                        return false;
                    }
                    if(__builtin_expect(arity != 4, false)) {
                        *reason = enif_make_string(env, "The arity of tuple should be 4 in case of scal", ERL_NIF_LATIN1);
                        return false;
                    }
                    ErlNifUInt64 size;
                    if(__builtin_expect(!enif_get_uint64(env, array[0], &size), false)) {
                        *reason = enif_make_string(env, "Fail to get uint64 in case of scal", ERL_NIF_LATIN1);
                        return false;
                    }
                    int arity_type;
                    const ERL_NIF_TERM *array_type;
                    char *type;
                    unsigned typel;
                    unsigned int type_size;
                    if(__builtin_expect(
                        !enif_get_tuple(env, array[2], &arity_type, &array_type)
                        || arity_type != 2
                        || !enif_get_atom_length(env, array_type[0], &typel, ERL_NIF_LATIN1)
                        || (type = (char *)enif_alloc((typel + 1) * sizeof(char))) == NULL
                        || !enif_get_atom(env, array_type[0], type, typel + 1, ERL_NIF_LATIN1),
                        false)) {
                        *reason = enif_make_string(env, "Fail to get type in case of scal", ERL_NIF_LATIN1);
                        return false;
                    }
                    if(__builtin_expect(
                        strncmp(type, "f", 1) != 0
                        || !enif_get_uint(env, array_type[1], &type_size)
                        || !(type_size == 32 || type_size == 64),
                        false)) {
                        *reason = enif_make_string(env, "Sorry, scal now supports only {:f, 32} or {:f, 64} as a tensor", ERL_NIF_LATIN1);
                        return false;
                    }
                    ErlNifBinary bin;
                    if(__builtin_expect(!enif_inspect_binary(env, array[3], &bin), false)) {
                        *reason = enif_make_string(env, "Fail to get binary in case of scal", ERL_NIF_LATIN1);
                        return false;
                    }

                    // Get the given scalar
                    if(__builtin_expect(
                        !(stack[stack_idx + 1].type == type_scalar),
                        false)) {
                        *reason = enif_make_string(env, "Should be a scalar in case of scal", ERL_NIF_LATIN1);
                        return false;
                    }
                    int arity_s;
                    const ERL_NIF_TERM *array_s;
                    if(__builtin_expect(!enif_get_tuple(env, stack[stack_idx + 1].content, &arity_s, &array_s), false)) {
                        *reason = enif_make_string(env, "Stack top should be a tuple in case of scal", ERL_NIF_LATIN1);
                        return false;
                    }
                    if(__builtin_expect(arity_s != 4, false)) {
                        *reason = enif_make_string(env, "The arity of tuple should be 4 in case of scal", ERL_NIF_LATIN1);
                        return false;
                    }
                    ErlNifUInt64 size_s;
                    if(__builtin_expect(!enif_get_uint64(env, array_s[0], &size_s), false)) {
                        *reason = enif_make_string(env, "Fail to get uint64 in case of scal", ERL_NIF_LATIN1);
                        return false;
                    }
                    if(__builtin_expect(size_s != 1, false)) {
                        *reason = enif_make_string(env, "unexpected scalar but size_s != 1", ERL_NIF_LATIN1);
                        return false;
                    }
                    int arity_type_s;
                    const ERL_NIF_TERM *array_type_s;
                    char *type_s;
                    unsigned typel_s;
                    unsigned int type_size_s;
                    if(__builtin_expect(
                        !enif_get_tuple(env, array_s[2], &arity_type_s, &array_type_s)
                        || arity_type_s != 2
                        || !enif_get_atom_length(env, array_type_s[0], &typel_s, ERL_NIF_LATIN1)
                        || (type_s = (char *)enif_alloc((typel_s + 1) * sizeof(char))) == NULL
                        || !enif_get_atom(env, array_type_s[0], type_s, typel_s + 1, ERL_NIF_LATIN1),
                        false)) {
                        *reason = enif_make_string(env, "Fail to get type in case of scal", ERL_NIF_LATIN1);
                        return false;
                    }
                    if(__builtin_expect(
                        strncmp(type_s, "f", 1) != 0
                        || !enif_get_uint(env, array_type_s[1], &type_size_s)
                        || !(type_size_s == 32 || type_size_s == 64),
                        false)) {
                        *reason = enif_make_string(env, "Sorry, scal now supports only {:f, 32} or {:f, 64} as a tensor", ERL_NIF_LATIN1);
                        return false;
                    }
                    ErlNifBinary bin_s;
                    if(__builtin_expect(!enif_inspect_binary(env, array_s[3], &bin_s), false)) {
                        *reason = enif_make_string(env, "Fail to get binary in case of scal", ERL_NIF_LATIN1);
                        return false;
                    }

                    // get operand
                    ErlNifUInt64 increment;
                    if(__builtin_expect(!enif_get_uint64(env, code_p->operand, &increment), false)) {
                        *reason = enif_make_string(env, "Fail to get uint64 from operand in case of scal", ERL_NIF_LATIN1);
                        return false;
                    }

                    double scalar;
                    switch(type_size_s) {
                        case 32:
                            scalar = (double)((float *)bin_s.data)[0];
                            break;
                        case 64:
                            scalar = (double)((double *)bin_s.data)[0];
                            break;
                        default:
                            *reason = enif_make_string(env, "unexpected", ERL_NIF_LATIN1);
                            return false;
                    }
                    switch(type_size) {
                        case 32:
                            cblas_sscal(size, (float)scalar, (float *)bin.data, increment);
                            break;
                        case 64:
                            cblas_dscal(size, (double)scalar, (double *)bin.data, increment);
                            break;
                        default:
                            *reason = enif_make_string(env, "unexpected", ERL_NIF_LATIN1);
                            return false;
                    }
                    stack_idx++;
                }
                break;

            case INST_SENDT:
                {
                    // enif_fprintf(stdout, "inst: sendt\n");

                    /*
                     * Sends a tensor to the process.
                     * 
                     * The stak top should be type_tensor:
                     * {
                     *   Nx.size(args),
                     *   Nx.shape(args),
                     *   Nx.type(args),
                     *   Nx.to_binary(args)
                     * }
                     * 
                     * Or type_error:
                     * {
                     *   :error,
                     *   enif_make_string(env, reason, ERL_NIF_LATIN1)
                     * }
                     *
                     * The sent message in case of type_tensor is:
                     * {
                     *   :result,
                     *   binary,
                     *   shape,
                     *   type
                     * }
                     * 
                     * The sent message in case of type_error is:
                     * {
                     *   :error,
                     *   reason (Charlist)
                     * }
                     * 
                     */

                    if(__builtin_expect(stack_idx == 0, false)) {
                        *reason = enif_make_string(env, "Stack limit is less than 0", ERL_NIF_LATIN1);
                        return false;
                    }
                    stack_idx--;

                    if(__builtin_expect(stack[stack_idx].type != type_tensor, false)) {
                        *reason = enif_make_string(env, "Should be a tensor in case of sendt", ERL_NIF_LATIN1);
                        return false;
                    }
                    int arity;
                    const ERL_NIF_TERM *array;
                    if(__builtin_expect(!enif_get_tuple(env, stack[stack_idx].content, &arity, &array), false)) {
                        *reason = enif_make_string(env, "Stack top should be a tuple in case of sendt", ERL_NIF_LATIN1);
                        return false;
                    }
                    if(__builtin_expect(arity != 4, false)) {
                        *reason = enif_make_string(env, "The arity of tuple should be 4 in case of sendt", ERL_NIF_LATIN1);
                        return false;
                    }
                    ErlNifUInt64 size;
                    if(__builtin_expect(!enif_get_uint64(env, array[0], &size), false)) {
                        *reason = enif_make_string(env, "Fail to get uint64 in case of sendt", ERL_NIF_LATIN1);
                        return false;
                    }
                    int arity_type;
                    const ERL_NIF_TERM *array_type;
                    char *type;
                    unsigned typel;
                    // unsigned int type_size;
                    if(__builtin_expect(
                        !enif_get_tuple(env, array[2], &arity_type, &array_type)
                        || arity_type != 2
                        || !enif_get_atom_length(env, array_type[0], &typel, ERL_NIF_LATIN1)
                        || (type = (char *)enif_alloc((typel + 1) * sizeof(char))) == NULL
                        || !enif_get_atom(env, array_type[0], type, typel + 1, ERL_NIF_LATIN1),
                        false)) {
                        *reason = enif_make_string(env, "Fail to get type in case of sendt", ERL_NIF_LATIN1);
                        return false;
                    }
                    ErlNifBinary bin;
                    if(__builtin_expect(!enif_inspect_binary(env, array[3], &bin), false)) {
                        *reason = enif_make_string(env, "Fail to get binary in case of sendt", ERL_NIF_LATIN1);
                        return false;
                    }

                    ErlNifPid pid;
                    if(__builtin_expect(!enif_get_local_pid(env, rpid, &pid), false)) {
                        *reason = enif_make_string(env, "Fail to get pid in case sendt", ERL_NIF_LATIN1);
                        return false;
                    }

                    ErlNifEnv *msg_env = enif_alloc_env();
                    if(__builtin_expect(msg_env == NULL, false)) {
                        *reason = enif_make_string(env, "Fail to get new environment in case sendt", ERL_NIF_LATIN1);
                        return false;
                    }
                    ERL_NIF_TERM message = enif_make_tuple4(env,
                        enif_make_atom(env, "result"),
                        array[3],
                        array[1],
                        array[2]
                    );

                    if(__builtin_expect(!enif_send(NULL, &pid, msg_env, message), false)) {
                        *reason = enif_make_string(env, "Fail to send in case sendt", ERL_NIF_LATIN1);
                        return false;
                    }
                }
                break;

            case INST_SENDE:
                {
                    // enif_fprintf(stdout, "inst: sende\n");

                    /*
                     * Sends an error.
                     * 
                     * The operand should be as follows:
                     * Charlist
                     *
                     * The sent message in case of type_tensor is:
                     * 
                     * The sent message in case of type_error is:
                     * {
                     *   :error,
                     *   reason (Charlist)
                     * }
                     * 
                     */

                    ErlNifPid pid;
                    if(__builtin_expect(!enif_get_local_pid(env, rpid, &pid), false)) {
                        *reason = enif_make_string(env, "Fail to get pid from operand in case sende", ERL_NIF_LATIN1);
                        return false;
                    }

                    ErlNifEnv *msg_env = enif_alloc_env();
                    if(__builtin_expect(msg_env == NULL, false)) {
                        *reason = enif_make_string(env, "Fail to get new environment in case sende", ERL_NIF_LATIN1);
                        return false;
                    }
                    ERL_NIF_TERM message = enif_make_tuple2(env,
                        enif_make_atom(env, "error"),
                        code_p->operand
                    );

                    if(__builtin_expect(!enif_send(NULL, &pid, msg_env, message), false)) {
                        *reason = enif_make_string(env, "Fail to send in case sende", ERL_NIF_LATIN1);
                        return false;
                    }
                }
                break;

            case INST_IS_SCALAR:
                {
                    // enif_fprintf(stdout, "inst: is_scalar\n");

                    if(__builtin_expect(stack_idx == 0, false)) {
                        *reason = enif_make_string(env, "Stack limit is less than 0", ERL_NIF_LATIN1);
                        return false;
                    }
                    int arity;
                    const ERL_NIF_TERM *array;
                    if(__builtin_expect(
                        !(stack[stack_idx - 1].type == type_tensor || stack[stack_idx - 1].type == type_scalar)
                        || !enif_get_tuple(env, stack[stack_idx - 1].content, &arity, &array),
                        false)) {
                        *reason = enif_make_string(env, "The stack top should be type_tensor or type_scalar in case of is_scalar", ERL_NIF_LATIN1);
                        return false;
                    }
                    ErlNifUInt64 size;
                    if(__builtin_expect(!enif_get_uint64(env, array[0], &size), false)) {
                        *reason = enif_make_string(env, "Fail to get uint64 in case is_scalar", ERL_NIF_LATIN1);
                        return false;
                    }
                    stack[stack_idx].type = type_bool;
                    if(size == 1) {
                        stack[stack_idx].content = enif_make_uint(env, 1);
                        stack[stack_idx - 1].type = type_scalar;
                    } else {
                        stack[stack_idx].content = enif_make_uint(env, 0);
                    }
                    stack_idx++;
                }
                break;

            case INST_SKIP:
                {
                    // enif_fprintf(stdout, "inst: skip\n");

                    /*
                     * Skips in the given condition by the operand.
                     * 
                     * The operand should be a tuple as follows:
                     * {
                     *   (a non positive number increment of PC),
                     *   {
                     *     :if,
                     *     true or false
                     *   }
                     * }
                     * or 
                     * {
                     *   (a non positive number increment of PC),
                     *   true
                     * }
                     *
                     * If the former, Pops the stack as the condition.
                     * The type of the poped value should be type_bool. 
                     */

                    int arity;
                    const ERL_NIF_TERM *array;
                    if(__builtin_expect(
                        !enif_get_tuple(env, code_p->operand, &arity, &array)
                        || arity != 2,
                        false)) {
                        *reason = enif_make_string(env, "Fail to get tuple2 from the operand in case of skip", ERL_NIF_LATIN1);
                        return false;
                    }
                    ErlNifUInt64 skip;
                    if(__builtin_expect(!enif_get_uint64(env, array[0], &skip), false)) {
                        *reason = enif_make_string(env, "Fail to get uint64 from the increment of PC in case of skip", ERL_NIF_LATIN1);
                        return false;
                    }
                    ERL_NIF_TERM value = array[1];
                    if(enif_is_atom(env, value)) {
                        // case of unconditional branch
                        unsigned len;
                        char *buf;
                        if(__builtin_expect(
                            !enif_get_atom_length(env, value, &len, ERL_NIF_LATIN1)
                            || (buf = enif_alloc(len + 1)) == NULL
                            || !enif_get_atom(env, value, buf, len + 1, ERL_NIF_LATIN1)
                            || strncmp(buf, "true", len + 1) != 0,
                            false)) {
                            *reason = enif_make_string(env, "The conditional value should be true in case of unconditional branch", ERL_NIF_LATIN1);
                            return false;
                        }
                        code_p += skip;
                        code_length -= skip;
                    } else if(__builtin_expect(enif_is_tuple(env, value), true)) {
                        // case of conditional branch
                        if(__builtin_expect(
                            !enif_get_tuple(env, value, &arity, &array)
                            || arity != 2, false)) {
                            *reason = enif_make_string(env, "The conditional value should be tuple2 in case of conditional branch", ERL_NIF_LATIN1);
                            return false;
                        }
                        unsigned len;
                        char *buf;
                        if(__builtin_expect(
                            !enif_get_atom_length(env, array[0], &len, ERL_NIF_LATIN1)
                            || (buf = enif_alloc(len + 1)) == NULL
                            || !enif_get_atom(env, array[0], buf, len + 1, ERL_NIF_LATIN1)
                            || strncmp(buf, "if", len + 1) != 0,
                            false)) {
                            *reason = enif_make_string(env, "The conditional value should be :if in case of conditional branch", ERL_NIF_LATIN1);
                            return false;
                        }
                        int cmp_true, cmp_false;
                        if(__builtin_expect(
                            !enif_get_atom_length(env, array[1], &len, ERL_NIF_LATIN1)
                            || (buf = enif_alloc(len + 1)) == NULL
                            || !enif_get_atom(env, array[1], buf, len + 1, ERL_NIF_LATIN1)
                            || !((cmp_true = strncmp(buf, "true", len + 1)) == 0 
                                || (cmp_false = strncmp(buf, "false", len + 1)) == 0),
                            false)) {
                            *reason = enif_make_string(env, "The conditional value should be true or false in case of conditional branch", ERL_NIF_LATIN1);
                            return false;
                        }
                        if(__builtin_expect(stack_idx == 0, false)) {
                            *reason = enif_make_string(env, "Stack limit is less than 0", ERL_NIF_LATIN1);
                            return false;
                        }
                        stack_idx--;
                        unsigned int bool_branch = 0xff;
                        if(__builtin_expect(
                            stack[stack_idx].type != type_bool
                            || !enif_get_uint(env, stack[stack_idx].content, &bool_branch)
                            || !(bool_branch == 0 || bool_branch == 1), 
                            false)) {
                            // enif_fprintf(stdout, "stack[stack_idx].type: %s\n", stack[stack_idx].type == type_bool ? "type_bool" : "other");
                            *reason = enif_make_string(env, "The stack top should be type_bool in case of conditional branch", ERL_NIF_LATIN1);
                            return false;
                        }
                        if(cmp_true == 0) {
                            if(bool_branch == 1) {
                                code_p += skip;
                                code_length -= skip;
                            }
                        } else if(cmp_false == 0) {
                            if(bool_branch == 0) {
                                code_p += skip;
                                code_length -= skip;
                            }
                        } else {
                            *reason = enif_make_string(env, "unexpected case", ERL_NIF_LATIN1);
                            return false;
                        }
                    } else {
                        *reason = enif_make_string(env, "Unrecognized format of the branch condition in case of skip", ERL_NIF_LATIN1);
                        return false;
                    }
                }
                break;

            case INST_RETURN:
                {
                    // enif_fprintf(stdout, "inst: return\n");
                    // enif_fprintf(stdout, "stack_idx: %u\n", stack_idx);
                    if(__builtin_expect(stack_idx != 0, false)) {
                        *reason = enif_make_string(env, "stack is not zero at return", ERL_NIF_LATIN1);
                        return false;
                    }
                    return true;
                }
                break;

            case INST_DUP:
                {
                    // enif_fprintf(stdout, "inst: dup\n");
                    if(__builtin_expect(stack_idx == 0, false)) {
                        *reason = enif_make_string(env, "stack should be greater than zero in case of dup", ERL_NIF_LATIN1);
                        return false;
                    }
                    stack[stack_idx].type = stack[stack_idx - 1].type;
                    stack[stack_idx].content = stack[stack_idx - 1].content;
                    stack_idx++;
                }
                break;

            case INST_POP:
                {
                    // enif_fprintf(stdout, "inst: pop\n");
                    if(__builtin_expect(stack_idx == 0, false)) {
                        *reason = enif_make_string(env, "stack should be greater than zero in case of pop", ERL_NIF_LATIN1);
                        return false;
                    }
                    stack_idx--;
                    stack[stack_idx].type = type_undefined;
                }
                break;

            case INST_POP2:
                {
                    // enif_fprintf(stdout, "inst: pop2\n");
                    if(__builtin_expect(stack_idx <= 1, false)) {
                        *reason = enif_make_string(env, "stack should be greater than one in case of pop2", ERL_NIF_LATIN1);
                        return false;
                    }
                    stack_idx -= 2;
                    stack[stack_idx + 1].type = type_undefined;
                    stack[stack_idx].type = type_undefined;
                }
                break;

            case INST_SWAP:
                {
                    // enif_fprintf(stdout, "inst: swap\n");
                    if(__builtin_expect(stack_idx <= 1, false)) {
                        *reason = enif_make_string(env, "stack should be greater than one in case of swap", ERL_NIF_LATIN1);
                        return false;
                    }
                    enum stack_type t;
                    ERL_NIF_TERM term;
                    t = stack[stack_idx - 1].type;
                    stack[stack_idx - 1].type = stack[stack_idx - 2].type;
                    stack[stack_idx - 2].type = t;
                    term = stack[stack_idx - 1].content;
                    stack[stack_idx - 1].content = stack[stack_idx - 2].content;
                    stack[stack_idx - 2].content = term;
                }
                break;

            case INST_ALOADT:
                {
                    ErlNifUInt64 local_variable_num;
                    if(__builtin_expect(!enif_get_uint64(env, code_p->operand, &local_variable_num), false)) {
                        *reason = enif_make_string(env, "the operand of aloadt should be unsigned integer", ERL_NIF_LATIN1);
                        return false;
                    }

                    // check the local variable only whether it is tuple4 or not.
                    int arity;
                    const ERL_NIF_TERM *array;
                    if(__builtin_expect(!enif_get_tuple(env, args[local_variable_num], &arity, &array), false)) {
                        const char *error_message = "the local variable %lu should be a tuple in case of aloadt";
                        size_t error_buf_len = strlen(error_message) - 3 + get_degit(UINT64_MAX);
                        char *error_buf = enif_alloc(error_buf_len);
                        enif_snprintf(error_buf, error_buf_len, error_message, local_variable_num);
                        *reason = enif_make_string(env, error_buf, ERL_NIF_LATIN1);
                        enif_free(error_buf);
                        return false;
                    }
                    if(__builtin_expect(arity != 4, false)) {
                        *reason = enif_make_string(env, "The arity of tuple should be 4 in case of aloadt", ERL_NIF_LATIN1);
                        return false;
                    }
                    stack[stack_idx].type = type_tensor;
                    stack[stack_idx].content = args[local_variable_num];
                    stack_idx++;
                }
                break;

            default:
                {
                    const char *err = "unrecognized instruction %04X";
                    size_t length = strlen(err) + 1;
                    char *error_message = enif_alloc(length);
                    enif_snprintf(error_message, length, err, inst);
                    *reason = enif_make_string(env, error_message, ERL_NIF_LATIN1);
                    return false;
                }
        }
        if(stack_idx > MAX_STACK) {
            *reason = enif_make_string(env, "stack limit is over MAX_STACK", ERL_NIF_LATIN1);
            return false;
        }
    }
    // enif_fprintf(stdout, "stack_idx: %u\n", stack_idx);
    if(__builtin_expect(stack_idx != 0, false)) {
        *reason = enif_make_string(env, "stack is not zero at the end of code", ERL_NIF_LATIN1);
        return false;
    }
    return true;
}

static ERL_NIF_TERM execute_engine(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    if(__builtin_expect(argc != 3, false)) {
        return enif_make_badarg(env);
    }
    unsigned length;
    code_t *code;
    ERL_NIF_TERM exception;

    if(__builtin_expect(!getcode(env, argv[0], &code, &length, &exception), false)) {
        return exception;
    }

    unsigned arg_length;
    if(__builtin_expect(!enif_get_list_length(env, argv[1], &arg_length), false)) {
        return enif_make_badarg(env);
    }
    ERL_NIF_TERM *args = enif_alloc(sizeof(ERL_NIF_TERM) * arg_length);
    ERL_NIF_TERM tail = argv[1];
    for(unsigned i = 0; i < arg_length; i++) {
        if(__builtin_expect(!enif_get_list_cell(env, tail, &args[i], &tail), false)) {
            return enif_make_badarg(env);
        }
    }

    ERL_NIF_TERM reason;
    if(execute(env, code, length, args, arg_length, argv[2], &reason)) {
        enif_free(code);
        return enif_make_atom(env, "ok");
    } else {
        enif_free(code);
        return enif_make_tuple2(env, enif_make_atom(env, "error"), reason);
    }
}

static ErlNifFunc nif_funcs [] =
{
    {"execute_engine", 3, execute_engine}
};

ERL_NIF_INIT(Elixir.PelemayBackend.NIF, nif_funcs, NULL, NULL, NULL, NULL)