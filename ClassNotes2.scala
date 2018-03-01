import scala.io.Source

object ClassNotes2{
  
  // Thu 1 Mar 2018
  /*
    In A2, we did eval: ASTNode, env → String
    
    But if we want to do recursion, we need to be careful!
    
    How do we return an perhaps anonymous function?
    We need to do both return a String, or an actual function
    
    "{{  {'||foo'} }}" // invocation of an anonymous function definition. nArgs not relevant here
    INNERITEXT + DEFINE → return nothing, so function name is two spaces, but wait!
    We trim whitespace and end up w "".
    So let's create a binding from name to body.
    But we could have many anon fucntions!
    
    So let's return TWO things instead: eval(node, envr) → returns (String or function)
    
    How to choose? Can't know in advance, so we will always return a pair String, function)
    
    Our lang is dynamically typed like Python. We don't know the explicit type of things
    
    "{{  {'||foo'}{'||bar'} }}"
    INNERITEXT + DEFINE + DEFINE
    
    We could have chose to make this an error, but we choose to take the last definition we found.
    
    
    "{{  x  {'y||foo'}|{{y}} }}" // space space x space space
    
    Definition of x includes y||foo. We'll allow this as "  x  " + "", ie we treat {'y||foo'} as "", and we bind y to "",
    so {{y}} is foo.
    
    // mix of Scala and pseudocode
    
    eval("abc", env) → ("abc", null)
    eval(program (list), env) 
      if(list==Nil) return ("", Nil)// Base case
      else list == head::tail
      let r = eval(head, env)
      and t = eval(tail, env)
        return (r._1 + t._1); 
        if(t._2==null) r._2
        else t._2
    
    eval(  ..(itext, arg1), env)
      let name = eval(itext, env) // eval the args
      if(name._1.trim()=="")
        invoke name._2 // ie the function
      args → evaluation of pair(String, function)
      
    eval(tvar(vname, itext))
      eval vname // Throwaway function, keep only the String (mechanism to allow closures)
        lookup that Srting
        if not found, eval the itext
        
    eval(define(dtextn, dparam, dtextb), env)
      eval(dtextn, env) // must be String (perhaps trimmed), could be ""
      if dtext → ""
        return ("", function)
      else
        return ("", function) and add a binding
      eval dparam too
        Now String must not empty // Handle exception
        return ("", function) // function is an object, RHS of a fct binding in an environment
        
     // That's the last part of WML we'll go over in detail
     
     
     ______________________________________________________________________________________________________________
     "Hard line across here"
     
     Closures:
     A closure is a function + context
     
     foo() // invocation
     
     val x = 1
     println("Hello")
     bar()                    // This whole thing is the CONTINUATION of the foo() invocation
     val y = 17               // We add context 
     //...
     // end of program
     
     Continuation Programming Style
     We call fcts with an extra parameter (continuation + context) ← lazy eval!
     call-with-current-continuation
     
     So we're NOT actually returning from a fct. Instead they invoke their cont, which invoke their cont...
     
     // example
     foo() // pass cont w lazy eval
     val x = 1
     println("Hello")
     bar()
     val y = 17
     
     foo(cont: () => { // cont is a fucntion of everything below foo
      val x = 1
      println("Hello") // ignore this for now
      bar(cont: () => {
        val y = 17
      }) // NB should be CPS
     })
     
     At the end of foo, anywhere and everywhere it would retrun, it instead invokes the cont.
     
  */
  
  def outerFunc() = {
    
    def foo() = {
      println("foo")
      val z = 8
      z * z // return 64
    }
    val q = foo()
    q * q
    
    /* Becomes:
    def foo(c:(Int)=>Unit) = {
      println("foo")
      val z = 8
      c(z * z)
      
    }
    */
    
    def fact1(n: Int): Int = {
      if(n==0) 1
      else fact1(n-1) * n
    }
    
    def fact(n: Int, c:(Int)=>Unit): Unit = {
      if (n==0) 
        c(1) // function ends
      else { // no longer returning, n continuations for n levels
        fact(n-1, (z) => { // create a new continuation
          c(z * n)
        })
      }
    }
    // This is how we call this:
    fact(5, (r) => { println(r) }) // This works IRL!
    
  } // end outerFunc
  
  
}
