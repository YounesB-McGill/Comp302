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
    
    // Why woud we ever want to do this?
    /*
    Performance is an issue, so we'll have deeply nested recursive calls (need more (~double) stack space)
    Making func calls and never return means we go deep in the stack and never go back up. Aha!
    
    Since we're never coming back, we don't care about the stack! We can do this with a goto construct similar to tail recursion, 
    so performance is actually good!
    
    CPS is used in some intermediate compilers to optimize code we write in a hi level language.
    */
    
    /**
    Get slope of a line
    */
    def getLine(x1: Double, y1: Double, x2: Double, y2: Double) = {
      val slope = (y2-y1)/(x2-x1)
      val intercept = y1 - slope * x1
      (slope, intercept) // return a tuple
    }
    
    // CPS version
    def getLine(x1: Double, y1: Double, x2: Double, y2: Double, c: (Double, Double) => Unit) = { // We are NOT constructing an object
      val slope = (y2-y1)/(x2-x1) 
      val intercept = y1 - slope * x1
      c(slope, intercept)
    }
    
    // If slope was zero, we'd have an error
    def getLine(x1: Double, y1: Double, x2: Double, y2: Double, c: (Double, Double) => Unit, e:() => Unit) = {
      if(x2 - x1 == 0) e() // exception
      else{ 
        val slope = (y2-y1)/(x2-x1) 
        val intercept = y1 - slope * x1
        c(slope, intercept)
      }
    }
    
    // Call like this:
    getLine(1,4,3,8,
      (s, i) = {println("Slope = " + s + ", Intercept = " + i)} 
      () => {println("Failure!")}
    )
    
    // Some esoteric langs we'll see at the end will use CPS
    
    
    
    /*********************************************/
    
    // LAMBDA CALCULUS
    /*
    λ-calculus developed in 1930's by Alonzo Church to express computation 
    (contrast to Turning machine, which is mathematically equivalent)
    How minimal can we get in lang features and syntax?
    
    Everything is going to be func creation and application
    
    Define a recursively formed language:
    1) Variable names (assume finite size): By convention we usually have one-letter var names, like a, b, x.
    2) Lambda λ
    3) The dot .
    4) Brackets ()
    
    What are lambda terms?
    1) Variables are lambda terms // base case
    2) If M is a lambda term, and N is a lambda term, then (M N) is a lambda term // Amplication (recursive)
    3) If M is a lambda term, and x is a variable, then (λx.M) // func abstraction. This how we create functions
    
    eg:
    x (xy) (λx.y) (λx.x) (λx.(λy.z))
    
    Assume left-assoc: MNP = (MN)P
    and λx.xyz = (λx.(xyz)) // Continue till u hit a bracket
    
    Tue 13 Mar 2018
    MN means function application, ie call sth, ie term rewriting
    
    Beta-reduction: 
    BODY is whatever happens after the dot
    
    (λx.x)y. First thing MUST be an abstraction (w a λ), so (λx.x)y means Identity Function applied to y,
    so result of (λx.x)y is y.
    
    Formally, this is a rewriting process (wo worrying abt scopes, envr, ...)
    
    (λx.M)N → M[x→N] // map every x to N
    
    eg (λx.x)y → x[x→y] // change every x to y
    (λx.x(λz.w))y → x(λz.w)[x→y] = y(λz.w)
    
    (λx.xx)y → xx[x→y] → yy // take the body xx, then replace every x with y
    
    A bit trickier if we have a lot of nesting if stuff is being redefined:
    (λx.x(λx.x))y // Different x's! First two and last two don't refer to the same thing, so:
    (λx.x(λx.x))y → x(λx.x)[x→y] = y(λx.x) // Only look at OUTSIDE. Inside is SHADOWED so it should not be touched
    
    In Scala:
    */
    def foo(x: Int, y: Int) = {
      def bar(x: Int) = {
        x + y // x comes from bar, y comes from foo
      }
    }
    /*
    This works the way we expect from nested function calls
    
    We can distinguish between x's bsed on whether there is a lambda in font of them.
    
    As a rewrite system, we need to know what is free from what is bound
    
    Recursive definition:
    1. x is free in x // Base case
    2. if F is a set of free variables in term M // inductive step
          then F - {x} is free in (λx.M)
    3. if F is a set of 3 variables in M and G is a set of 3 variables in N
          then F U G is free in application (MN)
    4. What is not free is bound
    
    // example for 2:
    x is free, λx.x is not free. This is backwards from what we are used to. Write the body first, then see what is free or not.
    
    // Big example:
    (λx.z(λq.qz)(λz.xq)x)
    
    q is free in q, z free in z, x free in x // base cases
    
    Buildup with induction. Start with innermost:
    {q} is free in q AND {z} is free in z → {q, z} is free in qz
    {q, z} is free in qz → {q, z}-{q}={z} is free in (λq.qz)
    
    Formally, beta reduction uses this free/bound distiction, but we understand it already.
    
    (λx.BODY)N
    When we beta reduce (MN), watch out for implicit brackets!
    
    eg (λx.x(λy.y)w)z // nb: ((λx.n)N) is a redex → M[x→N] is a contraction
    
    → (λx.{x(λy.y)}w)z // implicit () written as {}
    
    Sometimes we get stuck if we apply beta-reduction
    
    (λx.x((λx.x)y))z // 2 redexes
    
    Inner: (λx.x(y))x → zy...
    
    Churh-Rossa thm = Diamond property, Confluence shapee
    
    M →* N, M →* P, where →* is some set of reductions, ie order does not matter
    then there exists a Q st N →* Q, P →* Q.
    
    This does NOT guarantee normal from exists, but if normal reduction terminates, then there is a unique normal form
    
    We can rename variables to make life easier: λx.x = λy.y
    
    Some terms indeed never terminate under beta reduction:
    ω = (λx.xx), Ω = ωω. Beta reduce Ω: xx[x→(λx.xx)] = (λx.xx)(λx.xx) = Ω → Ω → Ω → ... // stable
    
    w = (λx.xxx) // remember everything is left associative
    ww = (λx.xxx)(λx.xxx) 
       = (λx.xxx)(λx.xxx)(λx.xxx)
       = ww(λx.xxx) = www → wwww → wwwwww → ... // infinitely growing!
    
    Does the order in which we do the redexes matter?
    If a solution is possible, we'll get there, but not all solutions are equally efficient!
    
    F = (xx.(λy.y)) can be written as λxy.y // Throw away 1st arg, do 2nd. This is in curry form!
    
    {F(ww)}I → I // I is the identity
    
    If we expanded x instead, we'll be wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww forever!
    But if at some point we stop, we can throw them all out and get I. Moral is eval things lazily, not eagerly! 
    
    So do the leftmost, outermost reduction first!
    
    How do we do computations in λ caluculus?
    Encode integers as functions! Church numerals:
    0 = λfx.x → (λf.(λx.x)) // zero applications of f
    1 = λfx.(fx)
    2 = λfx.f(fx)
    3 = λfx.f(f(fx))
    
    
    
    
    
    
    
    
    
    
    
    */
    
  } // end outerFunc
 
  
  
}
