```
header: 0x00 0x61 0x73 0x6D
version: 0x01 0x00 0x00 0x00

type section: 0x01
    - size of this section
    - number of types
    - 0:
        - header: 0x60 (function prototype)
        - number of parameters: 0x02
        - parameters type (no need for this if number of parameters is 0):
            - 0:
                i32: 0x7F
            - 1:
                i32: 0x7F
        - number of results: 0x01
        - return type (no need for this if number of results is 0):
            - 0:
                i32: 0x7F

function section: 0x03
    - size of this section
    - number of functions: 0x02
    - 0: index of the type function in section type
    - 1: index of the type function in section type

memory section: 0x05
    - size of this section
    - has maximum size: 0x00 (0x01 if has maximum size)
    - minimum size: 0x13
    - maximum size (if has maximum size): 0x20

export section: 0x07
    - size of this section
    - number of exports: 0x01
    - exports:
        - 0:
            name length: 0x04
            name: "main"
            type (func: 0, table: 1, memory: 2, global: 3): 0x00
            index: 0x00

code section: 0x0A
    - size of this section
    - number of code: 0x01
        - 0:
            size of this code
            number of locals type: 0x01
            - 0:
                number of elements: 0x01
                type of elements: 0x7F (i32)
            end of this code: 0x0B
```

## Exemple

`add.wat`
```wat
(module
  (func $add (param $lhs i32) (param $rhs i32) (result i32) (local $smth i32)
    local.get $lhs
    local.get $rhs
    local.set $smth
    local.get $rhs
    i32.add)
  (export "add" (func $add))
)
```

`add.wasm` (with `wat2wasm` `hexdump -C`)
```
00000000  00 61 73 6d 01 00 00 00  01 07 01 60 02 7f 7f 01  |.asm.......`....|
00000010  7f 03 02 01 00 07 07 01  03 61 64 64 00 00 0a 0f  |.........add....|
00000020  01 0d 01 01 7f 20 00 20  01 21 02 20 01 6a 0b     |..... . .!. .j.|
0000002f
```

`add.wasm.wat` (with `wasm2wat`)
```
(module
  (type (;0;) (func (param i32 i32) (result i32)))
  (func (;0;) (type 0) (param i32 i32) (result i32)
    (local i32)
    local.get 0
    local.get 1
    local.set 2
    local.get 1
    i32.add)
  (export "add" (func 0)))
```
