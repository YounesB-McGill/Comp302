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
    
    
    Thu 15 Mar+ 2018
    
    // TODO first 30 min
    
    PLUS 1 2
                        m         n
    = (λmnfx.nf(mfx))(λgy.gy)(λhz.h(hx))
    = λfx.(λhz.h(hz))f(`(λgy.gy)fx`) // `...` is fx
    = λfx.(λHZ.h(hz))f(fx) // the last fx is bound to the Capital Z, the TODO 13:36 is bound to the H
    = λfx.(λhz.h(hz))f(`(λgy.gy)fx`
    =...
    
    MULT Doable: excercise!
    
    PRED: Difficult // Predecessor, eg PRED 2 is 1
    
    
    CONDintionals?
    We need lazy evals.
    We need true and false values, in functional forms:
    TRUE  = λxy.x // returns first argument
    FALSE = λxy.y // this is zero
    
    Want if(cond) thenside; elseside // 3 args
    cond must reduce to either TRUE or FALSE
    
    Notice symetry with defs of T/F.
    So we can define:
    IF = (λctf.ctf) // implicit brackets, t/f is true/false
    
    Need predicates like ISZERO // base case for many recursive functions
    // this what is bound on:
    //               f        x
    ISZERO = λn.n(λx: FALSE)TRUE
    
    ISZERO 0 = (λn.n(λx: FALSE)TRUE)(λfx.x) // beta reduction
             = (λfx.x)(λx.FALSE)TRUE // bind 0, throwaway f, returns 0; ; 
             = TRUE
             
    ISZERO 2 = (λn.n(λx: FALSE)TRUE)(λfx.x) // any number w f will turn into false
                              f      x
             = (λfx.f(fx))(λx.FALSE)TRUE
             
             = (λx.FALSE)(FALSE... `dc much abt this`) 
             
    Logical AND: // 'cute'
    AND = (λxy.xyx)
    
    AND tt = (λxy.xyx)tt = ttt = t // Remember TRUE is a function that returns 1 arg
    AND tf = (λxy.xyx)tf = (λxy.xyx)(λxy.x)(λxy.y) = λxy.y = f
    
    We can also make ORs, etc
    
    // Equality of numbers
    EQ(m, n) = AND(LEQ(m, n), LEQ(n, m))
    
    We still need to iterate, and will do that by 
    RECURSION!
    f(f(f(f(... x))))
    
    To build this, f will take itself as a parameter.
    We can have terms that build recursions for us, called combinators.
    Based on idea of fixed point
    x = f(x)
    f(f(x = f(x))) // No matter how many times we call f, we'll always get fixed point x
    
    We need a function F st: F(f) = f(F(f))
    
    Y-Combinator:
    Y = (λf.(λx.f(xx))(λx.f(xx))) // try this out
    This returns an infinite nested sequence that stops when fct says so
    
    Yg = g(Yg) = g(g(Yg)) = g(g(g(Yg))) = ... // expands forever 
    
    So we don't need to make a function recursive. We just wrap around a Y-Combinator
    
    if we want fct to stop:
    g(r, n) = if(n==0) basecase; else r(n-1)
    
    Interesting Combinators:
    Identity I = λx.x
    True K = λxy.x
    S = λxyz.xy(yz)
    
    We can represent lots of terms with just these three (I, K, S)
    λx.xx = SII
          = (λxyz.xy(yz))II
          = (λyz.Iz(yz))I
          = (λyz.z(yz))I
          = λz.z(Iz)
          = λz.zz // same as what we started with, by alpha substitution!
    
    We don't even need I:
    
    SKK = ((λxyz.xy(yz)))KK
        = (λyz.Kz(yz))K
        = λz.Kz(Kz) // Since K only returns the first arg, we dc abt the second Kz
        = λz.z = I
        
   Exercice: Express λx.x(xx) using S, K, I
   
   We can express every lambda term with just S, K, (I).
   We can build a computer this way, but maybe less efficient
   We can make negative numbers by making pairs, like (-, 3), "Objects" which are n-tuples
   
   
   
   
   
   //  T Y P E S
   Most practical languages have types.
   Often have static types, like in Java, eg String foo(Object o, ArrayList<String> a, ...)
   
   Nominative type system: Type name is important, eg int != char
   Structural type system: What type allows // Some dynamic languages allow this. Scala kinda does this if we want. 
   Sluggish and slow but still possible
        
   Tue 20 Mar 2018
   Static vs dynamic types:
   Static types: Must specify types, eg C or Java: String foo; Scala does this for the most part. We'll focus on this. Static typing
   allows us to validate types visually, eg we know in Java that int i = "foo"; is wrong!
   Dynamic types: Are determined at runtime
          
   Early reasons for types were to know the number of bits/words needed to store a variable, eg in C, a char is 8 bits, 
   an int is a machine word (16/32/64 bits). long is at least a word, more if possible
   
   Back to the 1900's, an abstract reasoning about types due to Russell's Paradox
   We can define sets of things. What is (dis)allowed in a set?
   
   Russell:
   R = {x | x is a set, and x is not a member of itself}
   
   Is {0, 1, 2, 3} in R? Yes, since {0, 1, 2, 3} is not a member of {0, 1, 2, 3}.
   Is (𝕫 U {𝕫}) in R? Nope!
   Is R in R?
   Assume R is in R. Now let's double check that:
   if R is inside R:
      then R is not in R since R is not supposed to be a member of itself
   if R is not in R:
      then R is a set, and R is not a member of itself, so R is in R
    
   Contradiction!
   
   This is known as the impredictivity/viscious circle problem. Reason is that the definition refers to the entity being defined
   
   Philosophical/logical problem. 
   
   So we need to define things on levels/strata of types. We can't refer to things above. Only below or at same level.
   
   Is there sth analogous in computation?
   
   P = (λf.if(f(f)==0) then 1 else 0)
   
   (PP) diverges (infinite loop!). So inside computation we need to make levels to avoid divergent (bad) programs by using types.
   
   If we do so, does not negatively affect expressivity? ie, is this too strict that will block legal ops we want to do?
   
   
   Base types are sets of things:
   int, char, float, boolean. eg char = {..., 'a', 'b',...}
   
   Type constructors:
   Package things into a record (Pascal), struct (C), Object (Java et al)
    
   We have specific rules that specify what is allows or not, eg in Java "abc"/7; is illegal
   
   There is a hierarchy of types (class hierarchy!). Sometimes this shows up in base types. eg is a float "bigger" than an int?
   What is 7.0 + 3? There is implicit conversion, not because an int IS a float, but we convert it. In many cases we can coerce 
   one type into another.
    
    Type constructors are used to build new types. eg, */
    enum day {sunday, monday, tuesday, wednesday, thursday, friday, saturday}
    /*
    We cannot do something like monday + 9
    
    In C, we were just defining constants:
    #define monday 0
    
    Then monday + 2 is wednesday
    
    In Pascal, enus are types
    
    In C: An aggregate type:
    struct point {
      int x;
      int y;
      double z;
    }
    
    In Scala:
    */
    class point{
      x: Int
      y: Int
      z: Int
    }
    /*
    A struct is layed out like this in memory:
    [ x:  32 b] [ y:  32 b]
    [ z:      64 b        ]
    
    Note that the memory is not always contiguous, for performance reasons:
    [ x:  32 b] [   ///   ]
    [ y:  32 b] [   ///   ]
    [ z:      64 b        ]
    
    Aggregators also define types:
    point: int x int x double // x is \times, type constructs for aggregation
    
    Unions (variants) // Don't worry too much abt this
    
    union number {
      int i;
      double d;
    }
    
    Put stuff on top of each other
    
    number: int + double // This is OR instead of AND
    
    This is used to convert, eg float to int.
    
    
    Arrays are type constructors
    x: int[] // array of ints != int
    
    In many languages we have recursive types: In Java 
    
    class List{
      int x;
      List next;
    }
    
    What is a List type? We know this an aggregation and List = int x List. But this cycle makes sense since we can know the size 
    of the data
    
    
    Functions have types too.
    
    int square(int x){
      return x * x;
    }
    
    What is the type of square()? Takes an int and returns an int, so we can write: int → int, 
    where the arrow is a function type constructor.
    
    double[] foo(int x, String y){ 
      return null;
    }
    
    foo: int x String → double[]
    
    In Scala, we can have arrays of functions!
    
    Note that this still works with recursion:
    
    int fact(int n){
      if(n==0) return 1;
      else return fact(n-1)*n;
    }
    
    fact: int → int
    
    In Comp302 we will use this notation: x, →, [].
    
    Simply typed lambda calculus: Done in 1940's
    We could make base types in λ-calculus, eg int or boolean.
    We only need 1 base type!
    We'll use the phi symbol Φ, but u can replace it with int.
    
    Inductive contact types:
    if s & t are types:
      then s→t is a type
    
    eg: Φ, Φ→Φ, (Φ→Φ)→Φ, Φ→(Φ→Φ) // left assoc by default
    
    Each type has associative 
    x:
    
    
    A var of type t is a term of type t.
    x^Φ has type Φ, x^(Φ→Φ) has type (Φ→Φ), x^whatever has type whatever
    
    Application: if M is of type s→t, and N^s, then (MN)^t
    
    Abstraction: if M^t and x^s, then (λx.M)^(s→t)
    
    eg base case: x^0: 0, z^(0→0): 0→0
    (λx^0.y^0): 0→0
    ((λx^0.y^0)z^0): 0 // From rule above
    
    Can we make a typed version of (λxy.yx)?
    ((λx.(y.yx)) x: 0, y: 0→0 // Here the prof cheated by just saying y is 0→0
    
    (λx^0:(λy^[0→0](y^[0→0x^0]))) // Innermost bracket has type 0, inner is a (0→0)→0, outermost is 0→((0→0)→0)
    
    If rules are respected, then we say it's well typed
    
    Strongly normalizing → every computation beta-reduces and terminates, which is cool!
    But this means some (divergent terms) cannot be typed! eg (λx.xx)
    Suppose that x: s, then x: s→t, but that goes on forever! (inf recursion!) So we cannot type this
    
    This also means we can't type a legal program:
    (λx.y)(λx.xx)
    
    Thu 22 Mar 2018
    
    Let's give everything in λ calculus a type
    
    The base type is 0.
    
    Goal of STLC is:
    No divergence, and all types normalize via beta-reduction
    No fixed-point combinator (since that's like an infinite recursion) // No divergence
    Church nums can be typed, but they'll give us too many restrictions
    
    We can do λx^0.x^0: 0 -> 0
    But we can also do this x^(0->0).x^(0-0): This is an identity fct that can be typed
    This works for any type, say alpha.
    
    Polymorphic types
    (λxy.y) =def (λx.(λy^0.y^0)) // Throw out x
    
    More abstact way to think abt this:
    (λx^a y^b.y): (a → (b →b))
        
    // TODO
    
    Fri 27 Mar 2018
    
    Type proofs
    
    Gamma is a set of variables.
    ',' is a union, but is often a set. 
    End product is empty assumptions, because the type statement declaration is true under all circumstances
    But keep in mind that each rule has to match EXACTLY 
    So we need rules for everything.
    
    Functions:
    Denoted by the arrow →
    that of STLC
    Axiom:
    
    _______________________
    Γ, x: T1 ⊢ x: T1
    
    Abstract
    
            Γ, x: T1 ⊢ expr: T2
    Rabs_______________________________
            Γ ⊢ (λx.expr): T1 → T2
            
    Application

            Γ ⊢ a: T1 → T2  
    Rapp_______________________________
            Γ ⊢ (a b): T2
    
    eg: λa(λb.a)
    
    
                        Γ(a): 0
                        _______
            a: 0, b: 0 ⊢ a: 0
      Rabs_________________________
          a: 0 ⊢ (λb.a): 0 → 0
    Rabs________________________________
          ⊢ (λa(λb.a)): 0 → (0 → 0)
    
    
    Thu 29 Mar 2018
    
    
    
    
    */
    def scalaTypingExample() = {
      /* This is a symbol, in lambda calculus this is
      let foo = λx.x+3 // abuse of notation
      */
      def foo(x: Int): Int = {
        x + 3
      }
      
      val foo2 = (x: Int) => { x + 3 } // Anonymous function binded to foo2

      val y = 7
      foo(y)
    }
    
    /*
    
                                            Γ ... 0
    Rabs____________________________        _________________________
            ⊢ (x) => x+3: Int -> Int        foo: Int -> Int ⊢ let 3 in 
                                                foo(y): Int
    Rlet________________________________________________
            ⊢ let foo = (x) => x + 3 in
                let y = 3 in
                   foo(y): Int      .
                                  .
                                .
                              0
                              
     How do we do this with recursive functions?
     
              (       ): T
     R _________________________________________
        let: foo = λ. ... in foo()
        
     This goes on forever, we need to know foo's type in order to know foo's type
     
     Soulution: Recursive variation on let rule (recursion inside recursion!)
     
     
     
              Γ, f: T' ⊢ expr: T'       Γ: f: T' ⊢ body: T
     Rlet_r __________________________________________________
              Γ ⊢ let f = expr in body 
              
     So it's safe to always use the recursive form, but we don't have to do that
    
    
    eg, our fave function:
    */
    def fact(x: Int) = {
      if(x==0) 1 
      else x*fact(x-1)
    }
    
    /*
                                                                                          // Trivial (axiom)          // Trivial
                                                                                   Rv_________________________      _____________________
                                                                                        fact: I->I ⊢ fact: I->I     fact: I->I ⊢ 100: I
                                                                                   Rapp__________________________________
              fact: Int -> Int ⊢ (x)=> {if(x==0) 1 else x*fact(x-1)}: Int -> Int        fact: Int -> Int ⊢ fact(100): Int
    Rlet_r _________________________________________________________________________________________________________________________________
            . ⊢ let fact = (x) => {if(x==0) 1 else x*fact(x-1)} in fact(100): Int
            
    
          Γ,x:I ⊢ // TODO
    Rabs_______________________________________________________________________
          Γ = {f:I->I} ⊢ (x) => {if(x==0) 1 else x*fact(x-1)}: I->I
    
    
    The interesting part is saying that x * fact(x-1) is an Int.
    It's a multiplication. x is an Int by Γ' 
    Now the recusive application: x-1 is an Int since x is an Int (Use R- rule). fact is therfore an Int.
    
    We need to add fact(): Int into Γ', so we have to add it to our type assumptions at the beginning
    
    This how Scala/Eclipse determines validity of types at compile time. In Java we have to specify types for everything.
    This is one way of dertermining if the program is "well-typed"
    
    But in Scala, we don't always specify val types or function return types.
    
    How does this magic work?
    */
    val x = 3
    /*
    Scala know that 3 is an Int, therefore x must also be an Int
    
    In many cases the type of data is constrained. How can we figure this out?
    
    Type Inference:
    Algorithmically derive types
    
    Scala does not always get this rite, but it gives us the idea.
    
    eg if(x) ... -> x must be a Boolean
    
    How could we do this?
    
    It's by a process of refining things, using the idea of a type variable
    Start w a crude idea of associating data with type vars, then apply constraints to narrow down the types
    (or any declared types, eg x: Int means  y = x is an Int too, and so is y + 3)
    
    Scala does this, so does
    *ML, like OCaML, SML, Haskell // all functional languages
    JavaScript: dynamically typed, but can be restricted by TypeScript, Flow (better type inference) etc
    
    How?
    Most common algs all based on:
    Damas-Hindley-Milner Algorithm = Algorithm W
    start from type vars and then narrow it down 
    (type rules resolve our constraints) + (unification (focus on this))
    
    
    Unification:
    Robinson's Algorithm:
    Hard to describe, easy to implement
    
    find a mapping between variables and type st certain equality constarinst are guaranteed. 
    
    eg, Ta: t1 // Tx (Tau x) is a type, tn is a type var
        Tb: Int 
    
    constraint Ta == Tb, true iff t1 is an Int
    
    Ta = t1 -> String
    Tb = Boolean -> t2
    
    To have Ta == Tb, we must have t1 is a Boolean and t2 is a String
    
    
    Ta = t1 -> t2
    Tb = t3 -> t4
    
    To have Ta == Tb, we must have t1, t3 AND t2, t4 both of the same types
    
    
    Not everything unifies:
    Ta: Boolean; Tb: Int // Can't do it, sorry!
    
    ta: Int -> t1
    Tb: t2 -> Boolean
    Tc: Int -> String
    
    Can unify pairs, but not all 3, since t1 would have to be both a Boolean and a String 
    
    Notice that our types include:
      - Base types: Int, Boolean, etc
      - Type vars: t1, t2 etc
      - Type ...: The arrow ->
   
    We'll need a mapping between expressions and types
    (using Gamma)
    
    Must also map from type vars to other types (concrete/type vars). This is a substitution mapping: which is a fucntion
    
    S: typesvars -> types
    
    eg: S = {Int -> Boolean / t1, String/t2} (substitute t2 with String)
    S = {t1 ⊢> Int -> Boolean, t2 ⊢> String} // where ⊢> is NOT a function arrow
    
    eg: 
    S = {t1 ⊢> Int -> Boolean, t2 ⊢> Int}
    Apply substitution to a type, ie apply all the amppings in S to the argument types: Give back a new type
    
    
    Function composition is recursive!
    S2 o S1 = S2(S1(...))
    
    
    Now we can state Robinson's Unification Algorithm:
    
    def unify(t1: Type, t2: Type): Substitution = { 
      cases{
        // Base cases
        T1 = t1: return {t1 ⊢> T2}* // Recall T is Tau, t is tee
        T2 = t2: return {t2 ⊢> T1}*
        
        T1 = base1 && T2 = base2 // if basetypes, either trivial or impossible!
        if(base1 == base2) return {} // unified! Return an empty Substitution
        else throw new UnunifiableException()
        
        // Constructed types = function types
        T1 = (T11 -> T12) &&
        T2 = (T21 -> T22)
        
        S = unify() // unify the arguments to get a Substitution (recursive call)
        S' = unify(S(T12), S(T22)) // Notice we apply the args subs to the indices here
        
        return  S o S // ie S() Composition // TODO
        
        *Caveat:
        eg, T1 = t1
            T2 = t1 -> String
                      S = {t1 ⊢> (t1 -> String)} // recursive type that never ends!
        if t1 is free
              in T2 throw new UnunifiableException() // occurs check
      }
      
      Tue 3 Apr 2018
      
      We can have T1 = t1 {t1 ⊢> t2}; 
      Ok, since term does not expand infinitely
      
      unify(T1 = (t1 → int), T2 = (t2 → t2)) == {
        // case 4
        
        // TODO 13:17
        
        unify(t1, t2) == sArgs = {t1 ⊢> t2}
        
        Now we unify the body types after applying the Substitution
        sArgs == int
        sArgs == t2
        unify(int, t2) == sb == {t2 ⊢> int}
        
        // This is our unifier:
        sb o sa == sb(sa(...)) == {t2 → int} o {t1 → t2} == {t1 → int, t2 → int} // Now it's a set. No need to worry abt order
        
        Now this unifies our original terms
        s(t1 → int) = int → int // Unified
        s(t2 → t2) = int → int  // terms
      } 
      
      btw, cases 1 and 2 could have been done in the reverse order. Does that make a difference? Yes, then we would have gotten 
      sa' = {t2 → t1}. If we continued, sa'(int) = int; sa'(t2) = t1; sb' == unify(int, t1) = {t1 ⊢> int}
      sb' o sa' == ... int → int // Flip things around. ORDER OF THE COMPOSITION MATTERS!!!
      
    }
    
    More complex example:
    T1 = int → (t1 → t2)
    T2 = t3 → ((int → t3) → int)
    
    First unify the arg types
    unify(int, t3) {t3 ⊢> int}: sa
    
    Then unify the body types under sa:
    sa(t1 → t2) = t1 → t2
    sa((int → t3) → int) = (int → int) → int
    Recursively unify these types:
    `t1` → t2
    `(int → int)` → int
    unify arg types sa {t1 ⊢> (int, int)}
    unify the body internals:
    sa'(t2) = t2
    sa'(int) = int
    sb' = {t2 ⊢> int}
    
    sb' o sa' = {t2 ⊢> int} o {t1 ⊢> (int → int)} = {t2 ⊢> int, t1 ⊢> (int → int)}
    
    Back one level:
    sb o sa = {t2 ⊢> int, t1 ⊢> (int → int)} o {t3 ⊢> int} 
      = {t1 ⊢> (int → int), t2 ⊢> int, t3 ⊢> int} // unifies
      
    Use s to unify terms
    s(int → (t1 → t2)) = int → ((int → int) → int)
    s(t3 → ((int → t3) → int)) = int → ((int → int) → int ) // They're the same!
    
    Robinson's alg computes a unifier t1 → t2, int → t2. Multiple unifiers are possible. Which should we use? Neat property:
    Robinson returns the Most General Unifier (MGU), let's call it U.
    U has the property that for any other (more specific) Substitution V, there is a Substitution W such that 
      W o U = V
      
    Example: {t1 ⊢> int} = U; {t1 ⊢> int, t2 ⊢> int}: V;
    W o U = V
    W = {t2 ⊢> int}
    
    
    So we can unify type terms.
    
    Type Inference:
    Hindley-Milna = W
    Uses unification internally
    W(G, expr) return (S, T) st S(G) ⊢ expr: T
    
    
    
    Another important course concept:
    
    SEMANTICS OF LANGUAGES
    
    We'll use structural induction to show behavior, exp in 1 case
    
    We did this in proofs, by breaking things down (see Comp240), like recursion, until we reach a base case
    
    We'll also use this startegy for showing how our language "works."
    
    Language semantics give meaning to the execution to a language (what a program means). 2 main kinds - must be very formal!
    
    1) Operational sematics: very explicit in explaining meaning of program as it executes. We'll focus on this.
       Assumes a specific machine execution model, eg x + y means LDR X; LDR Y, ADD x y
       
    2) Denotational semantics: // Not covered!
       What it implies in another domain? [[x + y]]_|N = sum of two mumbers
       [[ 3 ]]_(+/-) = positive
    
    We'll consider the imperative language IMP
    No functions!
    3 types of statements:
      Arithmetic expressions: A, a1, a2, a3, ...
      Boolean expressions: B b1, b2, ...
      Commands: C c1, c2, ...
      
      <program> ::= <C>
      <C> ::= skip // Same as NOP
            | x = <A> // variable x1, x2, ... = assignment (allowed here)
            | <C>; <C> // sequence of 2 things. Not left recursive
            | if <B> then <C> else <C>
            | while <B> do <C>
      
      // 
      <B> ::= T | F | <A> (.) <A> | <B> (/) <B> | ~<B>
      
      //   constants or vars or operations on A
      <A> ::= n | x | <A> (+) <A>
      
      (+) ∈ {+, -, *} // Dont wan't divByZero!
      (.) ∈ {<=, ==, >=, <, >, }
      ()
      
      Structural operational semantics (SOS) [1981]
      Simplified machine model can do basic operations, some memory store (a var → value mapping)
      Actual seantics are given by rules, similar to typing rules we saw before, in the form
      if A is true then B can be concluded. We'll add to that by saying B can be transformed into B'
      
                      A
      ______________________________________
                    B → B'
      
      We have our context (was Gamma, now is our memory store S)
      
      
      
                <A, S>
      __________________________________
         <B, S>     →       <B', S'>
         program        modified part of prog
      
      Rules:
      
      
      var___________________________________________
              <x, S>        →       <S(x), S>
      
      ie the meaning of var x is the value of x in the current store S.
      
      Operations: // Not all rules will given. U have to know how to derive!
      
      
      _______________________________________
        <n1 (+) n2, S>     →     <n3, S> // where n3 = n1 (+) n2, eg "3+4" The plus here is a char, the (+) in the machine is an ADD
                                         // NOT THE SAME +!
       
      Ops on arithmetic expressions:
      
                <a1, S>   →   <a1', S>
      ________________________________________
         <a1 (+) a2, S>   →   <a1' (+) a2, S> // Reduce left hand side


                <a2, S>   →   <a2', S>
      ________________________________________
         <n (+) a2, S>   →   <n (+) a2', S> // Reduce rite hand side. a2 != n
         
         
         
     
      Boolean:
      
      __________________________________
         <b1 (.) b2, S>   →   <b3, S> 
         
                  
      ________________________
        <!T, S>   →   <F, S>
        
      ________________________
        <!F, S>   →   <T, S>
      */
    

    
  } // end outerFunc
 
  
  
}
