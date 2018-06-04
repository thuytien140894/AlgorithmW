# AlgorithmW
This program implements a type inferer for the untyped lambda calculus based on 
Hindley-Milner type inference. To run this program, make sure that Haskell and the 
Stack build tool are installed on the system.

### 1. How to install Stack
    
   For Unix operating systems, run one of these commands:  

        curl -sSL https://get.haskellstack.org/ | sh  
         
   or   
   
        wget -qO- https://get.haskellstack.org/ | sh  

   If [homebrew](https://brew.sh/) is available, run:
   
        brew install haskell-stack  
        
   For Windows, download and install [Windows 64-bit Installer](https://www.stackage.org/stack/windows-x86_64-installer)  

### 2. How to build and run the program

   Go to the program directory, then run the following command to build the program:  
   
        stack build  

   To start the GTLC interpreter, run the command:

<<<<<<< HEAD
        stack exec AlgorithmW-exe  
=======
        stack exec GTLC-exe  
>>>>>>> 3cc9b1963ab07e32b7274e5e00e99a750e4d68d2

### 3. The Language Syntax

   #### Expressions

   Constants include boolean values and 0:
        
        true
        false
        0

   For lambda-abstraction λx. t, use "\" to denote λ. The body expression is placed after a dot. 
   For example, λx. x is written as:   
   
        \x. x
        
   For a function application, insert a space between the operator and the argument. For example, 
   (λx. x) (λx. x)is written as:   
   
        (\x. x) (\x. x) 

   Arithemtic operators (succ, pred, iszero) accept any expression e as the argument:    
   
        succ e
        pred e
        iszero e
        
   Conditionals:
   
        if e1 then e2 else e3
        
### 4. How to exit the program

        Type "exit" at the command prompt.

### 5. How to run tests on the program

        Type "test" at the command prompt.

   **Note**: There are 21 test cases. A well-typed test case either evaluates to a type:

        Test> "Input expression" 
        "Inferred type"
 
   or results in an error:

        Test> "Input expression" 
        Error: "error message"

   Three types of errors are:
   1. "Variable name" is out of bound
   2. Type mismatch: "type" vs. "type"
   3. Type variable "name" occurs in "type"
