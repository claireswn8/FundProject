# QueueLang: Programming Language Fundamentals Project
A queue-based language that performs basic mathematic/logic operations.

## Team Members
Name			 | ONID Username
:---------------:|:--------------:
Melanie Gutzmann | gutzmanm
Cole Swanson     | swanscol
Hannah Vaughan   | vaughanh

## Language Introduction
Our language, named _QueueLang_, is a stack-based language that implements the fundamental data structure as a FIFO queue rather than a LIFO stack. Integers, booleans, and tuples are all values that can be enqueued onto the queue, and mathematic operations can be performed on the integer values in the queue. Conditional logic allows for differing series of commands to be executed with if/else branching, while loops allow for looping to occur on values in the queue, and tuples can be both constructed and deconstructed to provide invertability.

## Usage
### Setup Instructions
_QueueLang_ is intended to be run from GHCi, so the _Lang_ module must be loaded to run programs in the language.

### Good Program Examples and their Outputs
#### Example 1
```haskell
prog [Push (I 2), Push (I 3), Add] []
>>> Expected Output: Just [5]
```