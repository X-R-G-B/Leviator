<div>
    <img src="https://github.com/X-R-G-B/Leviator/assets/87119012/acc77ef3-b39f-4c40-b882-d7e0b0fdefb6" alt="logo" width="150" align="left">
    <h1>🐲 Leviator</h1>
    <p>The opinionated programing language</p>
    <br><br>
</div>

## Documentation

-- **Comentary**

```c
// This is a comment
```

-- **Variables Declaration**

```hs
@Int a = 1;
@String b = "hello";
```

-- **Variables Assignment**

```hs
a = 1;
b = "hello";
```

- **Built-in Types**

```hs
@Bool a = True;
@Bool b = False;
@Int c = 1;
@List[Int] d = [1, 2, 3];
@Char e = 'a';
@String f = "hello";
@List[Char] g = ['a', 'b', 'c'];
```

- **Built-in Global Variables**

```c
@List[String] ARGS = ["programfilepath", "arg1", "arg2"];
```

- **Function Declaration**

```rust
fn add(a: Int, b: Int) -> Int
{
    // the next line is the `return`
    <- a + b;
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

- **Built-in Functions**

```c
// print to stdout
print("hello");
// print to stderr
printErr("hello");
// get a line from stdin
getLine();
// transform a type to a string
str(1);
// get the type of a value in string format
type(a);
// call a function with string
call("add", [1, 2]);
```

- **Generic Functions**

```rust
fn add[A](a: A, b: A) -> A
{
    <- a + b;
};
```

- **Generic Functions Call**

```rust
add[Int](1, 2);
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

```c
@List[Int] lst = [1, 2, 3];
foreach (a in lst)
{
    if (a == 2)
    {
        break;
    };
};
```

- **Imports**

```c
// Circular imports are not allowed
import "path/to/file.lvt"
```

- **Entrypoint**

```rust
// If you don't have this function, the program will not be run
fn start() -> Int
{
    <- 0;
};
```

- **Operators**

```
a + b
a - b
a * b
a / b
a == b
a != b
```

- **Structs**

```c
struct Point
{
    x: Int,
    y: Int,
};
```

- **Structs Initialization**
```
@Point p = {1, 2};
```

- **Structs Access**
```
p:x
```

- **Nested Structs**
```
struct Rect
{
    Point size; 
    Point pos; 
};
@Rect r = {{1, 2}, {3, 4}};
r:size:x
```

- **Generic Structs**

```c
struct Rect[A]
{
    attribute: A,
};
```
