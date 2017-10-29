# Semantic analysis

Semantic analysis and compilation to IR is implemented in [plank-frontend](../../plank-frontend). The IR itself ir defined in [plank-ir](../../plank-ir).

## IR

Plank program in IR is represented as a collection of functions. Every function has a unique name (symbol). 

A function consists of the following parts:
* Parameter list - a list of registers that hold function parameters.
* Optional output value layout - stores size and alignment of function output value, if it has one.
* Register descriptions - size and alignment for each register that function uses.

If function has an implementation, then there are also:
* A list of blocks that make up the function body.
* Id of the function entry block.

When printed to textual format, function blocks will be listed in arbitrary order. Entry block will be denoted using a `start` pseudo-block, which will contain a `goto` to the actual entry block.

### Function examples

```rust
fn foo(x: bool) -> u32 {
    if !x {
        return 1;
    } else {
        return 2;
    }
}
```

IR of the previous `foo` function:

```
function foo(%0): { 4, 4 }
    register %0: size 1, align 1
    register %1: size 1, align 1
start:
    goto label_0
label_0:
    %1 = xor_8 %0 1_b8
    branch %1 label_1 label_2
label_1:
    drop %1
    return 1_b32
label_2:
    drop %1
    return 2_b32
```

## Blocks

Every function block has its own Id. Ids are unique in a function, but not necessarily unique between functions. Also block has a (possibly empty) list of instructions, and a special block end instruction.

Possible block end instructions:
* Jump (printed as `goto <block-id>`) - continue execution from given block.
* Branch (printer as `branch <value> <then-block> <else-block>`) - if `value` is not zero, continue execution from block `then-block`, otherwise from `else-block`. Value must be 8 bits wide.
* Return (printed as `return`) - return from current function. Cannot be used in functions that return a value.
* Return value (printed as `return <value>`) - return a value from current function. Cannot be used in functions that do not return a value. Size and alignment of value must match those in function declaration.

### Block examples

```
label_1:
    %0 = 1_b8
    %1 = add_i8 %0 2_b8
    branch %1 label_2 label_3
```

```
label_2:
    %0 = 1_b32
    return %0
```

## Instructions

There are 14 kinds of instructions:
* Drop (written as `drop <register>`) - forgets the value stored in the register. Value in the register cannot be used before the next time it is assigned.
* Binary operation (written as `<register> = <op> <value-1> <value-2>`) - performs the operation on two given values, and stores result in the register. Available binary operations are listed below.
* Unary operation (written as `<register> = <op> <value>`) - performs the operation on given value, and stores result in the register. Available unary operations are listed below.
* Call (written as `<register> = call <symbol>(<value-list>)`) - calls a function associated with given symbol, passing given values as parameters. Parameter sizes and alignments must match those in called function declaration. Register size and alignment must match those of called function return value. Stores return value in given register.
* Call procedure (written as `callproc <symbol>(<value-list>)`) - same as call, but for functions that do not return a value. Note only functions that do not return a value cannot be called with `callproc`.
* Virtual call (written as `<register> = callvirt <value>(<value-list>)`) - same as call, but calls not a concrete function, but a function that is pointed to by given value. Value must be function-pointer-sized.
* Virtual procedure call (written as `callprocvirt`) - same as `callproc`, but virtual call.
* Deref store (written as `store (<value> + <offset>) <value-2>`) - store `value-2` at address `value + offset`. `<value>` must be pointer-sized.
* Deref load (written `<register> = deref (<value> + <offset>)`) - read value from address `value + offset`. `<value>` must be pointer-sized.
* Store (written as `<register>[<offset>] = <value>`) - write `value` to register with given offset. Value must fit inside register after the shift.
* Load (written as `<register> = <register-2>[<offset>]`) - read from `register-2` with given offset. `register-2` must be large enough to be possible to fill `register`.
* Take address (written as `<register> = address <register-2>[<offset>]`) - write to `register` the address of `offset`-th byte of `register-2`. `register` must be pointer-sized.
* Assign (written as `<register> = <value>`) - write value to register. Both register and value must have same size and alignment.
* Cast (written as `<register> = cast <value>`) - write value to register. Unlike assignment, only same size requirement must be met.

## Binary operations

The list of allowed binary operations:
* Arithmetic: `{add,sub,mul,div,mod}_{i8,u8,i16,u16,i32,u32}` - integer addition, subtraction, multiplication, division, modulo. Operand and output sizes must be same as in instruction name.
* Bit operations `{and,or,xor}_{8,16,32}`. Operand and output sizes must be same as in instruction name. There are no signed/unsigned variants, because that does not matter for bit operations.
* Ordering comparision: `{le,leq,gt,geq}_{i8,u8,i16,u16,i32,u32}` - less than, less or equal, greater, greater or equal. Operand sizes must be same as in instruction name. Output size is one byte.
* Equality comparision: `eq` and `neq`. Operands must have same size. Output size is one byte.

## Unary operations

Currently there is only one unary operation:
* Negation: `neg_{i8,u8,i16,u16,i32,u32}`. Operand and output sizes must be same as in instruction name.

## Values

There are 4 kinds of values:
* Integer constants (examples: `17_b8`, `91_b32`). Constants always have explicitly written size in bits.
* Registers
* Symbols. They are some numeric constants that refer to addresses of corresponding functions. Their size is that of function pointers.
* Byte sequences. They evaluate to pointers to first byte of the sequence, and thus their size matches that of pointers.

## Registers

In IR functions can use arbitrary number of registers. Registers are written as `%0`, `%1`, and so on (the numbers don't necessarily have to be sequential). Size and alignment must be specified for every register used in a function. At the start of function execution, only registers listed as function parameters are live. Register also becomes live when it is assigned a value. `drop`ped registers are not live and cannot be used unless assigned again. Pointers to registers also become invalid when register is `drop`ped.
