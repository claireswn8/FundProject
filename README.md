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
-- Prebuilt example with input [I 235234]:
run int2digit_example i2d_functions
>>> Expected Output: Just [I 2,I 3,I 5,I 2,I 3,I 4]
```

#### Example 2: Calculate Highest Common Factor
This program calculates the highest common factor of two integers within a tuple. The remaining value on the stack will be the highest common factor of those two integers.
```haskell
-- Prebuilt example with input [T (I 12) (I 16)]:
run hcf_example hcf_functions
>>> Expected Output: Just [I 4]
```

#### Further Examples
Further examples of programs written in our language can be found in our "Mathlude". This standard library contains functions that allow users of our language to perform mathmatical calculations. Users can call functions such as `factorial` and `percent`. These functions are automatically included in the list of accessible functions when programs are run using the `run` keyword.

### Bad Program Examples and their Outputs
To run bad examples, use the following format in GHCi:

`run [S (Begin ExampleName)] []`

For example, to run the `dividebyzero` bad example, enter the following into GHCi:

`run [S (Begin dividebyzero)] []`

The following are bad examples:
* `dividebyzero`

* `mismatchtype`

* `tuplemismatchtype`

* `invalidextracttuple`

* `missingfunctiondefinition`

* `emptystackpop`

* `notenoughargumentsswap`

* `notenoughargumentsistype`

The expected output for all bad examples is `Nothing`
