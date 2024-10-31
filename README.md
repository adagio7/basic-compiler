# Compiler

## Description

This is a simple compiler that reads a file and generates a list of tokens. The tokens are then used to generate an abstract syntax tree (AST) which is then used to generate the intermediate code. The intermediate code is then used to generate the final code.

This project uses stack to build and run the project.

## How to run

```bash
$ stack run
```

## How to test

```bash
$ stack test
```

## Syntax

The syntax is somewhat an amalgamation of different imperative languages that I enjoyed, picked and stolen from C, Python, Java.

### Table of Contents
1. [Data Types](#data-types)
1. [Variables](#variables)
1. [Functions](#functions)
1. [If Statements](#if-statements)
1. [Loops](#loops)
1. [Operators](#operators)
1. [Comments](#comments)

#### Data Types
The language supports the following data types:
- int
- float
- char
- string
- list
- null

#### Variables
Variables are declared using the `let` keyword. The type of the variable can be inferred from the value assigned to it.

```c
let a = 10
let b = 'a'
let c = "hello"
let d = -10.0
let e = [1, 2, 3]
```

#### Functions
Functions are declared using the `func` keyword. The return type of the function can be inferred from the return value.

```c
func add(a, b) -> int{
    return a + b
}
```

#### If Statements
If statements are declared using the `if` and `else` keywords are also supported.

```c
if (a > b){
    return a
} else {
    return b
}
```

#### Loops
Loops are declared using the `while` keyword.

```c
while (a < 10){
    a = a + 1
}
```

#### Operators
The language supports the following operators:

- Arithmetic Operators
    - `+`
    - `-`
    - `*`
    - `/`

- Comparison Operators
    - `==`
    - `!=`
    - `>`
    - `<`
    - `>=`
    - `<=`

- Logical Operators
    - `&&`
    - `||`
    - `!`

- Assignment Operators
    - `=`

#### Comments
Inline comments are declared using the `//` symbol. Multi-line comments are indicated by sections enclosed by `'''`.

```c
// This is a comment
'''
    This is a multi-line comment
'''
```

