# Spec for ByteCode

:: All value starting with `0x` are hexadecimal

| Summary                      |
|------------------------------|
| Header                       |
| Struct registration          |
| Function registration        |
| Start function               |

## Header

1. The file starts with:

```
0x55 0x1E 0x53 0x0A
```

## Struct registration


Struct ID for:
- __Int__: `0x00000001`
- __String__: `0x00000002`
- __Bool__: `0x00000003`
- __Void__: `0x00000004`
- __Char__: `0x00000005`

1. The section starts with:

    ```
    0x00 0x06 0x07 0x00
    ```

   - Each struct type `ID` is an (Int32) followed by its name in multiple (Char8), until `0x00`

    ```
    0x00000006MyStruct0x000x00000007SndStruct0x00
    ```

2. Struct are a list of other struct of built-in types

   - Struct start

    ```
    0x01
    ```

   - Struct name (Int32) is an `ID` of the struct name.

     All reference to this struct will be with the `ID`.

   - Number of fields (Int16)

   - Each field is an `ID` of the field type

   - Struct end

    ```
    0x00
    ```

## Function registration

Struct ID for:
- __print__: `0x00000001`
- __printErr__: `0x00000002`
- __getLine__: `0x00000003`
- __str__: `0x00000004`
- __type__: `0x00000005`
- __call__: `0x00000006`

1. The section starts with:

    ```
    0x00 0x06 0x07 0x00
    ```

    - Each function `ID` is an (Int32) followed by its name in multiple (Char8), until `0x00`

    ```
    0x00000006myFunc0x000x00000007sndFunc0x00
    ```

2. Struct are a list of other struct of built-in types

    - Struct start

    ```
    0x01
    ```

    - Struct name (Int32) is an `ID` of the struct name.


1. The section starts with:

    ```
    0x00 0x06 0x07 0x00
    ```

2. Each function is:

    - Function start
   
      ```
      0x01
      ```
      
    - Function name (Int32) is an `ID` of the function name.

      All reference to this function will be with the `ID`.

    - Function name (Char8) is the name of the function and finish with `0x00`

    - 