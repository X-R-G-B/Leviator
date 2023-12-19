# Spec for ByteCode

:: All value starting with `0x` are hexadecimal

|-----------------------------------------------------------------------------|
| Header                                                                      |
|-----------------------------------------------------------------------------|
| Struct registration                                                         |
|-----------------------------------------------------------------------------|
| Global variable registration                                                |
|-----------------------------------------------------------------------------|
| Function registration                                                       |
|-----------------------------------------------------------------------------|
| Start function                                                              |
|-----------------------------------------------------------------------------|

## Header

1. The file starts with:

```
0x55 0x1E 0x53 0x0A
```

## Struct registration

|-----------------------------------------------------------------------------|
| 0x01 0xZZZZZZZZ 0x                                                         |
|-----------------------------------------------------------------------------|

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



2. Struct are a list of other struct of built-in types

- Struct start

```
0x01
```

- Struct name (Int32) is an `ID` of the struct name.

All reference to this struct will be with the `ID`.

- Number of fields (Int16)

- Each fild is a `ID` of the field type

- Struct end

```
0x00
```


```
struct a {
    int
}

struct b {
    a
}
```
