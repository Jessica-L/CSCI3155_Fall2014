# Lab 1
####Title:        Writeup for Lab 1 Questions
####Team Members: Jessica Lynch and Noah Dillon


_1. Scala Basics: Binding and Scope.  For each of the following uses of names, give the line where_
   _that name is bound.  Briefly explain your reasoning (1-2 sentences)._

  _(a) The use of pi at line 4 is bound at which line?  The use of pi at line 7 is bound at which_
       _line?_
    ```
    i. The use of pi at line 4 is bound at line 3 where we find "val pi = 3.14159". This is the 
    latest declaration of pi within the same scope as its use on line 4.  The computer will look
    here first to find the value assigned to pi referenced at line 4.
    ```
    ```
    ii. The use of pi at line 7 is bound at line 1 since this location is where pi is last defined
    within the same scope.
    ```







