object Lab1 extends jsy.util.JsyApplication {
  import jsy.lab1.ast._
  import jsy.lab1.Parser
  
  /*
   * CSCI 3155: Lab 1
   * Jessica Lynch
   * 
   * Partner: Noah Dillon
   * Collaborators: <Any Collaborators>
   */

  /*
   * Fill in the appropriate portions above by replacing things delimited
   * by '<'... '>'.
   * 
   * Replace the 'throw new UnsupportedOperationException' expression with
   * your code in each function.
   * 
   * Do not make other modifications to this template, such as
   * - adding "extends App" or "extends Application" to your Lab object,
   * - adding a "main" method, and
   * - leaving any failing asserts.
   * 
   * Your lab will not be graded if it does not compile.
   * 
   * This template compiles without error. Before you submit comment out any
   * code that does not compile or causes a failing assert.  Simply put in a
   * 'throws new UnsupportedOperationException' as needed to get something
   * that compiles without error.
   */
  
  /*
   * Example with a Unit Test
   * 
   * A convenient, quick-and-dirty way to experiment, especially with small code
   * fragments, is to use the interactive Scala interpreter.
   * 
   * To run a selection in the interpreter in Eclipse, highlight the code of interest
   * and type Ctrl+Shift+X (on Windows) or Cmd+Shift+X (on Mac).
   * 
   * Highlight the next few lines below to try it out.  The assertion passes, so
   * it appears that nothing happens.  You can uncomment the "bad test specification"
   * and see that a failed assert throws an exception.
   * 
   * You can try calling 'plus' with some arguments, for example, plus(1,2).  You
   * should get a result something like 'res0: Int = 3'.
   * 
   * As an alternative, the testPlus2 function takes an argument that has the form
   * of a plus function, so we can try it with different implementations.  For example,
   * uncomment the "testPlus2(badplus)" line, and you will see an assertion failure.
   * 
   * Our convention is that these "test" functions are testing code that are not part
   * of the "production" code.
   * 
   * While writing such testing snippets are convenient, it is not ideal.  For example,
   * the 'testPlus1()' call is run whenever this object is loaded, so in practice,
   * it should probably be deleted for "release".  A more robust way to maintain
   * unit tests is in a separate file.  For us, we use the convention of writing
   * tests in a file called LabXSpec.scala (i.e., Lab1Spec.scala for Lab 1).
   */
  
  def plus(x: Int, y: Int): Int = x + y
  def testPlus1() {
    assert(plus(1,1) == 2)
    //assert(plus(1,1) == 3) // bad test specification
  }
  testPlus1()

  def badplus(x: Int, y: Int): Int = x - y
  def testPlus2(plus: (Int, Int) => Int) {
    assert(plus(1,1) == 2)
  }
  //testPlus2(badplus)

  /* Exercises */
  
 /* 
  * #3. RUN-TIME LIBRARY FUNCTIONS
  */
 
 /*
  * #3(a)
  * Parameter(s): n: Double
  * Returns the absolute value of n.
  */
  def abs(n: Double): Double = if (n < 0.0) (n*(-1.0)) else n

 /*
  * #3(b)
  * Parameter(s): a: Boolean, b: Boolean
  * Returns a ^ b using truth table logic and probability and 
  * no Boolean operators.
  */
  def xor(a: Boolean, b: Boolean): Boolean = {
    // Handles (i) a true and b true, and (ii) a true and b false
    if( a )
      if( b ) false else true 
    // Handles (i) a false and b true, and (ii) a false and b false
    else
      if( b ) true else false
  }

 /* 
  * #4. RUN-TIME LIBRARY: RECURSION
  */ 
  
 /*
  * #4(a)
  * Parameters: s: String, n: Int
  * Returns a string with n copies of s concatenated together.
  */
  def repeat(s: String, n: Int): String = {
    require( n >= 0 )        // safeguard against n < 0
    if (n == 0) ""           // if n = 0, provides illusion of adding nothing
	else s + repeat(s, n-1)  // recursively call repeat() until n = 0
  }
  
 /*
  * #4(b)
  * Parameters: c: Double, xn: Double
  * Returns a single-step approximation of the square root of c based on
  * Newton's method and an educated guess, xn
  */
  def sqrtStep(c: Double, xn: Double): Double = {
    require( xn > 0.0 )              // safeguard against divide by zero error
    xn - ( ((xn*xn) - c) / (2*xn) )  // square root approximation equation
  }
 
 /*
  * #4(c)
  * Parameters: c: Double, x0: Double, n: Int
  * Returns an n-step approximation of the square root of c based on
  * Newton's method and an educated guess, x0. 
  */
  def sqrtN(c: Double, x0: Double, n: Int): Double = {
    require( n >= 0 )                        // safeguard against -n iterations
    if ( n == 0 ) x0                         // if 0 iterations simply return guess
    else sqrtN( c, sqrtStep( c, x0 ), n-1 )  // if n > 0, recursively call sqrtN() n-1 times
  }

 /*
  * #4(d)
  * Parameters: c: Double, x0: Double, epsilon: Double
  * Returns a multiple-step approximation of the square root of c based on
  * Newton's method and an educated guess, x0. The number of steps needed
  * depends on the approximation being within the margin of error determined
  * by epsilon.
  */
  def sqrtErr(c: Double, x0: Double, epsilon: Double): Double = {
    require( epsilon > 0.0 )					 // safeguard against infinite loop (see next line)
    if ( abs((x0*x0) - c) < epsilon ) x0		 // check to see if approximation < epsilon
    else sqrtErr( c, sqrtStep( c, x0 ), epsilon) // recursively call sqrtErr() until approximation < epsilon
  }

  def sqrt(c: Double): Double = {
    require(c >= 0.0)
    if (c == 0.0) 0.0 else sqrtErr(c, 1.0, 0.0001)
  }
  
  /* Search Tree */
  
 /* 
  * #5. DATA STRUCTURES REVIEW: BINARY SEARCH TREE
  */ 
 
  sealed abstract class SearchTree
  case object Empty extends SearchTree
  case class Node(l: SearchTree, d: Int, r: SearchTree) extends SearchTree

 /*
  * #5(a)
  * Parameters: t: SearchTree
  * Returns a Boolean which indicates whether or not an instance of the 
  * tree is a valid BST.
  */  
  def repOk(t: SearchTree): Boolean = {
    // Helper function to check for Node(Empty, d, Empty), Node(1,d,r), 
    // Node(Empty, d, r), and Node(l, d, Empty) -AND- that 
    // l->data < d <= r->data
    def check(t: SearchTree, min: Int, max: Int): Boolean = t match {
      case Empty => true
      case Node(l, d, r) => {
     	if ( (d < max) && (d >= min) )
     	  (check(l, min, d) && check(r, d, max))
     	else false
      }
    }
    check(t, Int.MinValue, Int.MaxValue)
  }

 /*
  * #5(b)
  * Parameters: t: SearchTree, n: Int
  * Returns revised tree t revised with the inserted node.
  */   
  def insert(t: SearchTree, n: Int): SearchTree = t match {
    case Empty => Node(Empty, n, Empty)
    case Node(l, d, r) => {
      if (n < d) Node(insert(l, n), d, r)
      else Node(l, d, insert(r,n))
    }
  }

 /*
  * #5(c)
  * Parameters: t: SearchTree
  * Returns tuple containing (1) revised tree t with the minimum
  * value node removed, and (2) said node's minimum value.
  */     
  def deleteMin(t: SearchTree): (SearchTree, Int) = {
    require(t != Empty)
    (t: @unchecked) match {
      case Node(Empty, d, r) => (r, d)
      case Node(l, d, r) =>
        val (l1, m) = deleteMin(l)
        (Node(l1, d, r), m)
    }
  }

 /*
  * #5(d)
  * Parameters: t: SearchTree, n: Int
  * Returns revised tree t after node removed.
  */ 
  def delete(t: SearchTree, n: Int): SearchTree = {
    t match {
      case Empty => Empty
      case Node(l, d, r) => if (n < d) Node(delete(l, n), d, r)
      else if (n > d) Node(l, d, delete(r, n))
      else if (l == Empty) r
      else if (r == Empty) l
      else {
         val temp = deleteMin(r)
         Node(l, temp._2, temp._1)    
      }
    } 
  }
  
  /* JavaScripty - a simple interpreter */

 /* 
  * #6. JAVASCRIPTY INTERPRETER: NUMBERS
  */ 
 
 /*
  * #6(a)
  * Parameters: e: Expr
  * Returns the value of expression e. Uses the predefined classes
  * and objects found in ast.scala.
  */  
  def eval(e: Expr): Double = e match {
    case N(n) => n                              // if e is a number, return e
    case Unary( uop, e1 ) => uop match {        // unary operation Neg
      case Neg => -1 * eval(e1)					// interpret Neg - recursively evaluate as needed 
      case _ => throw new UnsupportedOperationException
    }
    case Binary( bop, e1, e2 ) => bop match {   // binary operations
      case Plus  => eval(e1) + eval(e2)         // interpret Plus - recursively evaluate as needed 
      case Minus => eval(e1) - eval(e2)         // interpret Minus - recursively evaluate as needed 
      case Times => eval(e1) * eval(e2)         // interpret Times - recursively evaluate as needed 
      case Div   => eval(e1) / eval(e2)         // interpret Div - recursively evaluate as needed 
      case _ => throw new UnsupportedOperationException
    }
    case _ => throw new UnsupportedOperationException
  }
  
 // Interface to run your interpreter from a string.  This is convenient
 // for unit testing.
 def eval(s: String): Double = eval(Parser.parse(s))



 /* Interface to run your interpreter from the command-line.  You can ignore the code below. */ 
  
 def processFile(file: java.io.File) {
    if (debug) { println("Parsing ...") }
    
    val expr = Parser.parseFile(file)
    
    if (debug) {
      println("\nExpression AST:\n  " + expr)
      println("------------------------------------------------------------")
    }
    
    if (debug) { println("Evaluating ...") }
    
    val v = eval(expr)
    
    println(v)
  }

}
