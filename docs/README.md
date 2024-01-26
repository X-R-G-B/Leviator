<div>
    <img src="https://github.com/X-R-G-B/Leviator/assets/87119012/acc77ef3-b39f-4c40-b882-d7e0b0fdefb6" alt="logo" width="150" align="left">
    <h1>🐲 Leviator</h1>
    <p>The opinionated programing language</p>
    <br><br>
    <p><i>Leviator is compiled to wasm binary format.</i></p>
</div>

> [!NOTE]  
> This is a student project (EPITECH school).
> It is not intended to continue this project.

## Language Syntax

> [!IMPORTANT]  
> StringView are not supported.

> [!IMPORTANT]  
> There is no list/array in this language.

- **Comentary**

```c
// This is a comment
```

- **Alias**

```
alias A = Int;
```

- **Variables Declaration**

```hs
@Int a = 1;
@StringView b = "hello";
```

- **Variables Assignment**

```hs
a = 1;
b = "hello";
```

- **Built-in Types**

```hs
@Bool a = True;
@Bool b = False;
@Int c = 1;
@Char e = 'a';
@StringView f = "hello";
```

There is a `Void` type that can be used to return nothing.

- **Function Declaration**

```rust
fn add(a: Int, b: Int) -> Int
{
    // the next line is the `return`
    <- a + b;
};

export fn sub(a: Int, b: Int) -> Int
{
    <- a - b;
};
```

- **Function Call**

```rust
add(1, 2);
```

- **Function Polymorphism**

```rust
fn add(a: Int, b: Int) -> Int
{
    <- a + b;
};

fn add(a: Float, b: Float) -> Float
{
    <- a + b;
};

fn add(a: Int, b: Int, c: Int) -> Int
{
    <- a + b + c;
};
```

- **Conditions**

```c
if (a == 1)
{
    // do something
};

if (a == 1)
{
    // do something
}
else
{
    // do something else
};
```

- **Loops**

```c
@Int i = 0;
while (i < 10)
{
    // do something
    i = i + 1;
};
```

- **Entrypoint**

```rust
// If you don't have this function, the program will not be run
export fn start() -> Int
{
    <- 0;
};
```

- **Operators**

```python
a + b
a - b
a * b
a / b
a == b
a != b
a < b
a <= b
a > b
a >= b
```

- **Priority of Operators**

```c
// realy peticuliar buut we use { for ( and } for )
{a + B} * c
```
