# MathLang: Programming Language Fundamentals Project
A stack-based language that performs basic mathematic/logic operations.

## Team Members
Name			 | ONID Username
:---------------:|:--------------:
Melanie Gutzmann | gutzmanm
Cole Swanson     | swanscol
Hannah Vaughan   | vaughanh

## Language Introduction
Our language, named _MathLang_, is a stack-based language with a stack that can have integers, doubles, booleans, tuples, commands, and functions as its values. These values can be pushed onto the stack, and mathematical operations can be performed on the integer, boolean, and tuple values in the stack. Conditional logic allows for differing series of commands to be executed with if/else branching; while loops allow for looping to occur on values on the stack, and tuples can be both constructed and deconstructed to provide invertability. A "Mathlude" contains functions that perform less common mathematical operations, such as `factorial`, `percent`, and `summation`.

## Usage
### Setup Instructions
_MathLang_ is intended to be run from GHCi, so the _MathLang_ module must be loaded to run programs in the language.
```bash
$ ghci
Prelude> :load MathLang
```

### Good Program Examples and their Outputs
#### Example 1: Convert Integer to Digits
This program deconstructs an integer into its digits. The digits are then pushed back onto the stack as individual integers.
```haskell
-- Prebuilt example:
run int2digit_example i2d_functions
>>> Expected Output: Just [I 2,I 3,I 5,I 2,I 3,I 4]

-- Custom argument:
prog int2digit_example [I 2837] i2d_functions
>>> Expected Output: Just [I 2,I 8,I 3,I 7]

-- Full program:
prog [Call "preprocessing", S (While Less [Call "deconstruct"]), Push (F "cleanup"), CallStackFunc] [I 2837] [  ("preprocessing", [E Dup, Push (I 0)]), ("deconstruct", [E Dup, Push (I 10), E Mod, Swap, Push (I 10), Swap, E Div, E Dup, Push (I 0)]), ("cleanup", [Pop])]
>>> Expected Output: Just [I 2,I 8,I 3,I 7]
```

#### Example 2: TBA

#### Further Examples
Further examples of programs written in our language can be found in our "Mathlude". This standard library contains functions that allow users of our language to perform mathmatical calculations. Users can call functions such as `factorial` and `percent`. These functions are automatically included in the list of accessible functions when programs are run using the `run` keyword.

##### Factorials
Factorial of 6:
```haskell
run [Push (I 6), Call "factorial"] []
>>> Expected Output: Just [I 720]
```
Factorial of 13:
```haskell
run [Push (I 13), Call "factorial"] []
>>> Expected Output: Just [I 6227020800]
```
##### Percentages
To calculate 20% of 30:
```haskell
run [Push (D 20), Push (D 30), Call "percent"] []
>>> Expected Output: Just [D 6.0]
```

To calculate 75% of 245:
```haskell
run [Push (D 75), Push (D 245), Call "percent"] []
>>> Expected Output: Just [D 183.75]
```

### Selected (Short) Good Example Commands
```
cmd (Push (I 4)) [] []
>>> Expected Output: Just [I 4]
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

-- Custom argument:
prog int2digit_example [I 2837] i2d_functions
>>> Expected Output: Just [I 2,I 8,I 3,I 7]

-- Full program:
prog [Call "preprocessing", S (While Less [Call "deconstruct"]), Push (F "cleanup"), CallStackFunc] [I 2837] [  ("preprocessing", [E Dup, Push (I 0)]), ("deconstruct", [E Dup, Push (I 10), E Mod, Swap, Push (I 10), Swap, E Div, E Dup, Push (I 0)]), ("cleanup", [Pop])]
>>> Expected Output: Just [I 2,I 8,I 3,I 7]
```

#### Example 2: TBA

#### Further Examples
Further examples of programs written in our language can be found in our "Mathlude". This standard library contains functions that allow users of our language to perform mathmatical calculations. Users can call functions such as `factorial`, `summation`, and `percent`. These functions are automatically included in the list of accessible functions when programs are run using the `run` keyword.

```haskell
run [Push (I 6), Call "factorial"] []
>>> Expected Output: Just [I 720]
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

```haskell
cmd (ExtractTuple 5) [T (I 3) (I 4)] []
>>> Expected Output: Nothing
```

```haskell
cmd (ExtractTuple 1) [B True,T (B False) (I 4)] []
>>> Expected Output: Nothing
```
