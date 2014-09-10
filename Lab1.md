# Lab 1
####Title:        Writeup for Lab 1 Questions
####Team Members: Jessica Lynch and Noah Dillon


_1. Scala Basics: Binding and Scope.  For each of the following uses of names, give the line where_
   _that name is bound.  Briefly explain your reasoning (1-2 sentences)._

  _(a) The use of pi at line 4 is bound at which line?  The use of pi at line 7 is bound at which_
       _line?_
    
    i.  The use of pi at line 4 is bound at line 3 where we find "val pi = 3.14159". This is the 
        latest definition of pi within the same scope as its reference on line 4.  
    
    ii. The use of pi at line 7 is bound at line 1 since this location is where pi is last defined
        within the global scope.

  _(b) The use of x at line 3 is bound at which line? The use of x at line 6 is bound at which line?_
      _The use of x at line 10 is bound at which line? The use of x at line 13 is bound at which line?_
    
    i.   The use of x at line 3 is bound at line 2 where x is being passed into the function f() and 
         is therefore declaring x as an Int within the scope of the function f.  The reference of x 
         on line 3 is within the scope of function f.  
    
    ii.  The use of x at line 6 is bound at line 5 since it can only be used within the scope of case x
         where on line 5 x is a new "placeholder" variable being defined to set up this particular case.
         
    iii. The use of x at line 10 is also bound at line 5 since it can only be used within the scope of 
         case x where on line 5 x is a new "placeholder" variable being defined to set up this particular 
         case.
    
    iv.  The use of x at line 13 is bound at line 1 since it is the only definition of x within the global
         scope on which the line 13 reference of x depends.
    
_2. Scala Basics: Typing. In the following, I have left off the return type of function g.  The body_
   _of g is well-typed if we can come up with a valid return type.  Is the body of g well-typed?_
   _If so, give the return type of g (see below in **bold font type**) and explain how you determined_
   _this type._


  _line 1      def g(x: Int) **tuple:( (Int, Int), Int )** = {_
  
  _line 2          val (a, b) = (1, (x, 3))_
  
  _line 3          if( x == 0 ) (b, 1) else (b, a + 2)_
  
  _line 4      }_


  **Explanation for choosing type tuple:( (Int, Int), Int ):**
  As mentioned in the problem, the body of a function is well-typed only if the return type is
  valid.  The validity of the return type and therefore the well-typed status of the function g
  depends on consistency. The detailed breakdown of the typing the body expression of function g
  shows the consistent use of typing throughout ensures the validity of the return type we chose
  and therefore results in the well-typed body of function g. Therefore, our answer to the above 
  question is, **"Yes, the body expression of function g is well-typed."** 
       
      i.  (b, 1):( (Int, Int), Int ) because       // if( x == 0)
               b: (Int, Int) because               // per definition of b on line 2
                   (x, 3): (Int, Int) because 	  
                        x: Int                     // per declaration in parameter field
               1: Int
               
      ii. (b, a + 2): ( (Int, Int), Int ) because  // if( x != 0)
               b: (Int, Int) because               // per definition of b on line 2
                    (x, 3): (Int, Int) because 	 
                         x: Int                    // per declaration in parameter field
               a + 2: Int because
                   a: Int because                  // per definition of a on line 2                
                   2: Int          		
       






