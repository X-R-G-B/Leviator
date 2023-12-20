# Spec for ByteCode

:: All value starting with `0x` are hexadecimal

## Header

1. The file starts with:

```
0x00 0x61 0x73 0x6D
0x01 0x00 0x00 0x00
```

## Type Section

```
0x01
```

### Function Type

```
0x60
```

#### Number of Parameters

- for 2 parameters
    ```
    0x02
    ```

#### Parameters Type

```
0x7F: i32
0x7D: f32
```

#### Return Type

```
0x7F: i32
0x7D: f32
0x00: void
```

## Function Section

```
0x03
```

## Export Section

```
0x07
```

Instructions
------------

## Instructions

### Stack

#### local

##### local.get <index>

```
0x20
```

Get a local variable by its index and add it to the stack.

##### local.set <index>

```
0x21
```

Get the top of the stack and set a local variable by its index.

#### global

##### global.get <index>

```
0x23
```

Get a global variable by its index and add it to the stack.

##### global.set <index>

```
0x24
```

Get the top of the stack and set a global variable by its index.

#### i32

##### i32.const <value>

```
0x41
```

Push an `Int32` to the stack.

### Memory

#### i32

##### i32.store

```
0x36
```

Take the top of the stack. This will be the value stored in memory.
Take the top of the stack. This will be the index in memory.

Store the value in memory at the index.

##### i32.load

```
0x28
```

Take the top of the stack. This will be the index in memory.

Push an `Int32` to the stack with the value at address in memory.

### Condition

#### i32

##### i32.gt_s

```
0x4a
```

Compare the 2 values on the top of the stack.

If the first value is greater than the second value, push `1` to the stack.
else, push `0`

##### i32.eq

```
0x46
```

Compare the 2 values on the top of the stack.

If the first value is equal to the second value, push `1` to the stack.
else, push `0`

#### control

##### if...else...end

- if

    ```
    0x04
    ```

    Enter in the first branch if the top of the stack is 1.

- else

    ```
    0x05
    ```

    Enter in the second branch if the top of the stack is 0.

- end

    ```
    0x0b
    ```

    Exit from the if/else block.

##### loop <label> ... br <label>

- loop <label>

    ```
    0x03
    ```

    Create a label for the loop

    Label is a number

- br <label>

    ```
    0x0c
    ```

    Jump to the label

    Label is a number

### Operations

#### i32

##### i32.add

```
0x6a
```

Add the 2 values on the top of the stack.
Push the result to the stack.
