# MathLang: Programming Language Fundamentals Project
A stack-based language that performs basic mathematic/logic operations.

## Team Members
Name			 | ONID Username
:---------------:|:--------------:
Melanie Gutzmann | gutzmanm
Cole Swanson     | swanscol
Hannah Vaughan   | vaughanh

## Language Introduction
Our language, named _MathLang_, is a stack-based language with a stack that can have floating-point number, booleans, and tuples as its values. These values can be pushed onto the stack, and mathematical operations can be performed on the numeric values in the stack. Conditional logic allows for differing series of commands to be executed with if/else branching; while loops allow for looping to occur on values on the stack, and tuples can be both constructed and deconstructed to provide invertability.

## Usage
### Setup Instructions
_MathLang_ is intended to be run from GHCi, so the _MathLang_ module must be loaded to run programs in the language.

### Good Program Examples and their Outputs
```haskell
cmd (Push (D 4)) []
>>> Expected Output: Just [D 4.0]
```

```haskell
cmd (Push (B True)) [D 4]
>>> Expected Output: Just [B True,D 4.0]
```

```haskell
cmd (Push (T (D 1) (B False))) [B True,D 4]
>>> Expected Output: Just [T (D 1.0) (B False),B True,D 4.0]
```

```haskell
expr Add [D 2,D 3,D 8]
cmd (E Add) [D 2,D 3,D 8]
>>> Expected Output: Just [D 5.0,D 8.0]
```

```haskell
expr Add [T (D 1) (D 2),T (D 2) (D 3),T (D 20) (D 40)]
cmd (E Add) [T (D 1) (D 2),T (D 2) (D 3),T (D 20) (D 40)]
>>> Expected Output: Just [T (D 3.0) (D 5.0),T (D 20.0) (D 40.0)]
```

```haskell
expr Mul [D 2,D 3,D 8]
cmd (E Mul) [D 2,D 3,D 8]
>>> Expected Output: Just [D 6.0,D 8.0]
```

```haskell
expr Mul [T (I 2) (I 3),T (I 4) (I 5),T (I 20) (I 40)] []
cmd (E Mul) [T (I 2) (I 3),T (I 4) (I 5),T (I 20) (I 40)] []
>>> Expected Output: Just [T (I 8) (I 15),T (I 20) (I 40)]
```

```haskell
expr Div [I 10,I 5,I 8] []
cmd (E Div) [I 10,I 5,I 8] []
>>> Expected Output: Just [I 2,I 8]
```

```haskell
expr Equ [I 2,I 2,I 3] []
>>> Expected Output: Just [B True,I 3]
```

```haskell
expr Equ [B True,B False,I 3] []
>>> Expected Output: Just [B False,I 3]
```

```haskell
expr (If [Push (I 5)] [Push (B True)]) [B True] []
>>> Expected Output: Just [I 5]
```

```haskell
stmt (While Equ (S (Begin [Push (I 5),Push (I 2),E Add]))) [B True,B True] []
>>> Expected Output: Just [I 7]
```

```haskell
prog [Push (I 5),Push (I 2),E Add] []
>>> Expected Output: Just [I 7]
```

#### ExtractTuple
```haskell
cmd (ExtractTuple 0) [T (I 1) (I 2)] []
>>> Expected Output: Just [I 1]
```

```haskell
cmd (ExtractTuple 1) [T (I 1) (I 2)] []
>>> Expected Output: Just [I 2]
```

```haskell
cmd (ExtractTuple 2) [T (I 1) (I 2)] []
>>> Expected Output: Just [I 1,I 2]
```

#### Percentages
To calculate 20% of 30:
```haskell
prog (percent 20 30) [] []
>>> Expected Output: Just [D 6.0]
```

To calculate 75% of 245:
```haskell
prog (percent 75 245) [] []
>>> Expected Output: Just [D 183.75]
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

#### ExtractTuple
```haskell
cmd (ExtractTuple 5) [T (I 3) (I 4)] []
>>> Expected Output: Nothing
```

```haskell
cmd (ExtractTuple 1) [B True,T (B False) (I 4)] []
>>> Expected Output: Nothing
```