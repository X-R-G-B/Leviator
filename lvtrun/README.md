# lvtrun

# ----------------------- 0 -----------------------

00 61 73 6d # magic number, always 00 61 73 6d, otherwise the file is invalid
01 00 00 00 # version, always 01 00 00 00, otherwise the file is invalid

# ----------------------- 1 -----------------------

# 01 is the id of the id for type section
01 11 04
# 11 is the length of the section = 17 in decimal
# 04 is the number of types in the section

60 00 01 7f
# 60 is the type "function"
# 00 number of parameters
# 01 number of results
# 0x7f is the type "i32"

60 00 00
# 0 parameter and 0 result
60 01 7f 00
# 1 parameter i32 and 0 result
60 01 7f 01 7f
# 1 parameter i32 and 1 result i32

(type $t0 (func (result i32)))
(type $t1 (func))
(type $t2 (func (param i32)))
(type $t3 (func (param i32) (result i32)))

# ----------------------- 2 -----------------------

02
# 02 is the id of the import section
24 01 16 77
61 73 69 5f
73 6e 61 70
73 68 6f 74
5f 70 72 65
76 69 65 77
31 09 70 72
6f 63 5f 65
78 69 74 00
02

# ----------------------- 3 -----------------------

# section 3 is the function section with 17 bytes of length
# index of the signature (type) of each internal function
# functionsec = section(vec(typeidx))
03 11 10
01
00
01
01
01
02
02
00
01
00
00
00
00
02
03
00

# ----------------------- 4 ----------------------- the table section

04
# 05 = length of the section
05
# 01 = number of table (its a vector)
01
# 70 = reftype {can be 70 = funcref or 6f = externref}
70
# 01 is the indiacator to say "0x00 = min x, max empty or  0x01 = min x, max y"
01
# 02 is the limit min and max of the table
02 02

# ----------------------- 5 ----------------------- the memory section

05
06

# 01 = number of memory (its a vector)
01
01 c7 02 c7 02

# ----------------------- 6 ----------------------- the global section

06 12
# vector of 3 globals
03
# i32 mut = i32.const 66560
7f 01 41 c7 c7 04 0b
7f 01 41 00 0b
7f 01 41 00 0b
# (globalType, expr)
# globalType = (valtype, mut) with mut 0x00 = const, 0x01 = var
# expr = (instructions) 0xb

# ----------------------- 7 -----------------------

# export type id
# 00 = function
# 01 = table
# 02 = memory
# 03 = global


07
# 182 bytes of length
b6
# 010c = 12 exports
01 0c

# 06 is the length of the name
06
# 5f 5f 69 6e 64 65 is the name "memory"
6d 65 6d 6f 72 79
# 02 is export type "memory"
# 00 means export = first vector of memory
02 00


# 19 hex = size in dec = 25 char for the name = __indirect_function
19
# "_ _ i n d i r e c t _ f u n c t i o n _ t a b l e"
5f 5f 69 6e 64 69 72 65 63 74 5f 66 75 6e 63 74 69 6f 6e 5f 74 61 62 6c 65
# 01 is export type "table"
01 00


# size 6
06
# name "start"
5f 73 74 61 72 74
# its a type function with id 03
00 03


# size = 16 name = __errno_location "
10
5f 5f 65 72 72 6e 6f 5f 6c 6f 63 61 74 69 6f 6e
# export function of id 8
00 08

# size = 21 name "emcrypten_stack_init"
15
65 6d 73 63 72 69 70 74 65 6e 5f 73 74 61 63 6b 5f 69 6e 69 74
# export function of id 9
00 09

# size 25 name = "emscripten_stack_get_free"
19
65 6d 73 63 72 69 70 74 65 6e 5f 73 74 61 63 6b 5f 67 65 74 5f 66 72 65 65
# export function of id 10
00 0a

# size 25 name = "emcrypten_stack_get_base"
19 65 6d 73 63 72 69 70 74 65 6e 5f 73 74 61 63 6b 5f 67 65 74 5f 62 61 73 65
# export function of id 11
00 0b

# size = "emcrypten_stack_get_end"
18 65 6d 73 63 72 69 70 74 65 6e 5f 73 74 61 63 6b 5f 67 65 74 5f 65 6e 64
# export function of id 12
00 0c

# size = 9 name = "stackSave"
09 73 74 61 63 6b 53 61 76 65
# export function of id 13
00 0d

# size = 12 name = "stackRestore"
0c 73 74 61 63 6b 52 65 73 74 6f 72 65
# export function of id 14
00 0e

# size = 10 name = "stackAlloc"
0a 73 74 61 63 6b 41 6c 6c 6f 63
# export function of id 15
00 0f

# size = 28 name = "emscripten_get_sbrk_ptr"
1c
65 6d 73 63 72 69 70 74 65 6e 5f 73 74 61 63 6b 5f 67 65 74 5f 63 75 72 72 65 6e 74
# export function of id 16
00 10

# ----------------------- 9 -----------------------

09 07
01 00 41 01
0b 01 01

# ----------------------- 10 -----------------------

# refer to:
https://webassembly.github.io/spec/core/binary/instructions.html

0a
# size = cb = 203
cb 01

# 16 func
10

#                 --- first func ---------

# first function has a size of 4
04
# number of local
00
# 10 = call
10 09 = call func id 9
# 0b means end of expression
0b

#                 --- second func ---------

# 25 = length of 37
25
# number of local variable of type ( see below )
01
# 5 locals variable of type i32 (7f) (because 1 variable local of type x ^)
05 7f
# 23 = global.get = take the index u32 of a symbol in the global section
23 00
# 21 = local.set = extract top stack value and store it in a local variable
21 00
# 
41 10 = i32.const of value 16
21 01 = local.set localidx 1 = localvar
20 00 = local.get localidx 0 = localvar
20 01 = local.get localidx 1 = localvar
6b = i32.sub
21 02 = local.set localidx 2 = localvar
41 00 = i32.const of value 0
21 03 = local.set localidx 3 = localvar
20 02 = local.get localidx 2 = localvar
20 03 = local.get localidx 3 = localvar
36 02 0c = i32.store align 02 offset 0c = 12
41 00 = i32.const of value 0
21 04 = local.set localidx 4 = localvar
20 04 = local.get localidx 4 = localvar
0f = return
0b

#                 --- third func ---------

11
# 0 local variable
00
# block
    02
    40 = memory.grow ?
    41 01 = i32.const of value 1
    45 = i32.eqz
    0d 00 = br_if label 0
    10 01 = call func id 1
    0b
10 02 = call func id 2
10 06 = call func id 6
00 = unreachable
0b

#                 --- fourth func ---------

02
00
0b

#                 --- fifth func ---------

2b = size 43
# 1 local variable of type i32 (7f)
01 01 7f
41 00 = i32.const of value 0
21 00 = local.set id 0 (the local variable)
    02 =  begin block
    40 = memory.grow ?
    41 00 = i32.const of value 0
    41 00 = i32.const of value 0
    4d = i32.le_u
    0d 00 = br_if label 0
        03 = loop
        40 = memory.grow ?
        20 00 = local.get 0
        41 7c = i32.const of value -4 ?
        6a = i32.add
        22 00 = local.tee 0
        28 02 00 = i32.load align 02 offset 00
        11 01 00 = call_indirect index typeidx 01 typeidx 00
        20 00 = local.get 0
        41 00 = i32.const of value 0
        4b = i32.gt_u
        0d 00 = br_if label 0
        0b
    0b
10 04 = call func id 4
0b

#                 --- sixth func ---------

0d
00
10 04 = call func id 4
10 05 = call func id 5
10 04 = call func id 4
20 00 = local.get 0
10 07 = call func id 7
00 = unreachable
0b 07 00 20 00 10 00 00
0b

#                 --- seventh func ---------

06
00
41 = i32.const
c7 c7 04 = value (maybe 65536)
0b

#                 --- eighth func ---------

12
00
41 c7 c7 04 = i32.const value (maybe 65536)
24 02 = global.set 2
41 00 = i32.const of value 0
41 0f = i32.const of value 15
6a = i32.add
41 70 = i32.const of value (maybe -16)
71 = i32.and
24 01 = global.set 1
0b

#                 --- ninth func ---------

07
# no local variable
00
23 00 = global.get 0
23 01 = global.get 1
6b = i32.sub
0b

#                 --- tenth func ---------

04
00
23 02 = global.get 2
0b

#                 --- eleventh func ---------

04
00
23 01 = global.get 1
0b

#                 --- twelfth func ---------

04
00
23 00 = global.get 0
0b

#                 --- thirteenth func ---------

06
00
20 00 = local.get 0
24 00 = global.set 0
0b

#                 --- fourteenth func ---------

12
# number of local variable of type ( see below )
01
# 2 locals variable of type i32 (7f) (because 1 variable local of type x ^)
02 7f
23 00 = global.get 0
20 00 = local.get 0
6b = i32.sub
41 70 = i32.const of value
71 = i32.and
22 01 = local.tee 1
24 00 = global.set 0
20 01 = local.get 1


# ----------------------- 11 -----------------------

# id 11
0b
# size 4
04
# Bitfield. 0 = passive segment, 1 = active segment = presence of an explicit memory index
00
# i32.const
23
# principal memory
00
# i32.store
0b 
