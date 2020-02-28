# MathLang: Programming Language Fundamentals Project
A stack-based language that performs basic mathematic/logic operations.

## Team Members
Name			 | ONID Username
:---------------:|:--------------:
Melanie Gutzmann | gutzmanm
Cole Swanson     | swanscol
Hannah Vaughan   | vaughanh

## Language Introduction
Our language, named _MathLang_, is a stack-based language with a stack that can have integers, booleans, and tuples as its values. These values can be pushed onto the stack, and mathematical operations can be performed on the integer values in the stack. Conditional logic allows for differing series of commands to be executed with if/else branching; while loops allow for looping to occur on values on the stack, and tuples can be both constructed and deconstructed to provide invertability.

## Usage
### Setup Instructions
_MathLang_ is intended to be run from GHCi, so the _Lang_ module must be loaded to run programs in the language.

### Good Program Examples and their Outputs
```haskell
cmd (Push (I 4)) []
>>> Expected Output: Just [I 4]
```

```haskell
cmd (Push (B True)) [I 4]
>>> Expected Output: Just [B True,I 4]
```

```haskell
cmd (Push (T (I 1) (B False))) [B True,I 4]
>>> Expected Output: Just [T (I 1) (B False),B True,I 4]
```

```haskell
expr Add [I 2,I 3,I 8]
cmd (E Add) [I 2,I 3,I 8]
>>> Expected Output: Just [I 5,I 8]
```

```haskell
expr Add [T (I 1) (I 2),T (I 2) (I 3),T (I 20) (I 40)]
cmd (E Add) [T (I 1) (I 2),T (I 2) (I 3),T (I 20) (I 40)]
>>> Expected Output: Just [T (I 3) (I 5),T (I 20) (I 40)]
```

```haskell
expr Mul [I 2,I 3,I 8]
cmd (E Mul) [I 2,I 3,I 8]
>>> Expected Output: Just [I 6,I 8]
```

```haskell
expr Mul [T (I 2) (I 3),T (I 4) (I 5),T (I 20) (I 40)]
cmd (E Mul) [T (I 2) (I 3),T (I 4) (I 5),T (I 20) (I 40)]
>>> Expected Output: Just [T (I 8) (I 15),T (I 20) (I 40)]
```

```haskell
expr Div [I 10,I 5,I 8]
cmd (E Div) [I 10,I 5,I 8]
>>> Expected Output: Just [I 2,I 8]
```

```haskell
expr Equ [I 2,I 2,I 3]
>>> Expected Output: Just [B True,I 3]
```

```haskell
expr Equ [B True,B False,I 3]
>>> Expected Output: Just [B False,I 3]
```

```haskell
expr (If [Push (I 5)] [Push (B True)]) [B True]
>>> Expected Output: Just [I 5]
```

```haskell
stmt (While Equ (S (Begin [Push (I 5),Push (I 2),E Add]))) [B True,B True]
>>> Expected Output: Just [I 7]
```

```haskell
prog [Push (I 5),Push (I 2),E Add] []
>>> Expected Output: Just [I 7]
```

### Bad Program Examples and their Outputs
```haskell
expr Add [I 1,B True,I 2]
>>> Expected Output: Nothing
```

```haskell
expr Mul [B False,I 1,I 2]
>>> Expected Output: Nothing
```

```haskell
expr Div [I 5,I 0,I 1]
>>> Expected Output: Nothing
```

```haskell
expr Div [I 5,B True,I 1]
>>> Expected Output: Nothing
```

```haskell
expr Equ [B True,T (B False) (B True)]
>>> Expected Output: Nothing
```

```haskell
expr (If [Push (I 5)] [Push (B False)]) [T (B True) (I 1)]
>>> Expected Output: Nothing
```

```haskell
stmt (While Equ [Push (I 5),E Add]) [B True,I 4]
>>> Expected Output: Nothing
```

```haskell
prog [E Add,E Equ] []
>>> Expected Output: Nothing
```