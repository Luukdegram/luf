# Introduction
Currently this is a living document as there's still alot to define and write down regarding the Luf language. However, here you can find the basic internal information regarding its its defintion.
- [Introduction](#introduction)
- [Precedence](#precedence)
- [Arithmetic](#arithmetic)
- [Bytecode](#bytecode)
- [Types](#types)

# Precedence
The order of which expressions are executed is as follow where the highest precedence is executed before a lower precedence.

| Name         | Tokens                       |
| ------------ | ---------------------------- |
| lowest       | Expressions not defined here |
| range        | `..`                         |
| or           | `or`                         |
| and          | `and`                        |
| assign       | `=`, `+=`, `-=`, `*=`, `/=`  |
| equals       | `==`, `!=`,                  |
| less_greater | `<`, `>`, `<=`, `>=`         |
| bitwise_or   | `|`                          |
| bitwise_xor  | `^`                          |
| bitwise_and  | `&`                          |
| shift        | `<<`, `>>`                   |
| sum          | `+`, `-`                     |
| product      | `%`, `/`, `*`                |
| prefix       | `!`, `-` (negation), `~`     |
| call         | `some_function()`            |
| index        | `.` i.e list.len             |

# Arithmetic
Please note that not all arithmetic has yet been implemented. There's still a few assign-and-op shorthands missing

| Name               | Syntax        | Types                   |
| ------------------ | ------------- | ----------------------- |
| Addition           | a + b, a += b | `int`, `string`         |
| Substraction       | a - b, a -= b | `int`                   |
| Multiplication     | a * b, a *= b | `int`                   |
| Division           | a / b, a =/ b | `int`                   |
| Remainder division | a % b         | `int`                   |
| Negation           | -a            | `int`                   |
| Bitwise xor        | a ^ b         | `int`                   |
| Bitwise or         | a &#124; b    | `int`                   |
| Bitwise and        | a & b         | `int`                   |
| Bitwise not        | ~a            | `int`                   |
| Shift left         | a << b        | `int`                   |
| Shift right        | a >> b        | `int`                   |
| and                | a and b       | `bool`                  |
| or                 | a or b        | `bool`                  |
| not bool           | !a            | `bool`                  |
| equal              | a == b        | `bool`, `int`, `string` |
| Not equal          | a != b        | `bool`, `int`, `string` |
| Greater than       | a > b, a >= b | `int`                   |
| Less than          | a < b, a <= b | `int`                   |

# Bytecode

All VM instructions are 24 bits.
The first bit of the instruction contains the opcode,
where the remaining 16 bits can be used to point to another instruction or define a parameter. For example, it can contain the value `2` to the opcode `make_array` to specify its length.

All instructions with its definitions will be documented here soon™.

# Types

Currently the following types are implemented.
| syntax         | Notes                                                |
| -------------- | ---------------------------------------------------- |
| int            | 64 bit **signed** integer                            |
| string         | utf-8 encoded list of characters                     |
| bool           | native zig boolean (u1)                              |
| [5]type        | list of type of length 5                             |
| [5]type1:type2 | map with type1 as key and type2 as value of length 5 |

In the future users can specify their own Types using composable Structs.