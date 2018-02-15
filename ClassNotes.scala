import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.Sorting.quickSort

object Food {
  def main(args: Array[String])={
    println("Hi!")
  }
  
  def foo(x:Int) : Int = { // : Int not rly needed
    x+1 // last thing in the function is the return value
  }
  
  def bar(x:Int) = x + 1 // {} not needed for {}
  
  /*
   * 
 var a = 2

 a: Int = 2

 a+b

 <console>:13: error: not found: value b
       a+b
         ^

 val b = 7

 b: Int = 7

 8.to(11)

 res1: scala.collection.immutable.Range.Inclusive = Range 8 to 11

 8 to 11

 res2: scala.collection.immutable.Range.Inclusive = Range 8 to 11

 /*Comment*/

 incomplete statements not supported yet, sorry, you'll have to retype...

 /*Comment*/
8.to(13)

 res4: scala.collection.immutable.Range.Inclusive = Range 8 to 13

 /* There is no ++ or -- in Scala, due to philosophical motivations */
a = a + 1

 a: Int = 3

 println("Ranges are inclusive by default, ie res1 is 8, 9, 10, 11. Spaces work for binary operators like +, but not for unary ops like toString")

 Ranges are inclusive by default, ie res1 is 8, 9, 10, 11. Spaces work for binary operators like +, but not for unary ops like toString

 println("Semicolons r not needed in Scala, putting them wont make a diference"); print("!")

 Semicolons r not needed in Scala, putting them wont make a diference
!

 println("Put operators at the end of the line")

 Put operators at the end of the line
   * 
   * */
  
  def ifStatement(){
    /* In Scala, if statements are like expressions which return something */
    val w = if(3<4) 17 else 89
    val x : Any = if(3<4) 17 else "food" // type is Any
  }
  
  def loops(){
    // While loop
    while(true){
      
    } // returns Unit, so can't do val x = while...
    
    // Do not use for
    for( i <- 0 to 10){ // keyword exists. Usage: for()
      println(i)
    }
    
    for( i <- "Hello"){ 
      println(i) // Prints H \n e \n l \n...
    }
    
    for( i <- "cdt"; v <- "aeo"; d <- "bx"){ 
      println("-"+i+v+d)//prints -cab -cax - ceb in a nested for loop
    }
    
    // Comprehension: Mathematical sets that come from constructing new values from old
    val hex = 
      for(c <- "spatula") yield (((c-'a')+13)%26+'a').toChar // Encrypts everyone's fave String to fcnghyn
    
    //val s = for(i<-'a' to 'f') println()
      
    val b = ArrayBuffer[Int]()
    // b+=1 gives AB(1)
    // b+=(range) or b++=(range) appends the range
    // can treat b as a collection, so we can iterate over it using for (i <- b)
    
  } // end loops
  
  // An array itself is immutable, but its element may not be. 
  // In this course, we'll construct new things like collections instead of 
  
  /**
   * Text I/O and regex
   */
  def textManipulation(){
    val text = "Some text from file";
    val words = text.split("\\s+");
    val lowers = for (w <- words if (w(0).isLower)) yield w
    val caseless = for(w <- words) yield w.toLowerCase // create a new collection
    val c2 = caseless.clone
    
    // Want to sort caseless
    quickSort(c2) // Modifies c2: BAD!
    val sorted = caseless.sorted // sorted is a function call that does not change original
  }
  
  /**
   * Tuples: can be pairs, triples, quadruples, 5-tuples...
   */
  def tuples(){
    val t = ("baloon", 5, 2.0) // Has type (String, Int, Double)
    // t._1 is "balloon", t._2 is 2, t._3 is 2.0, not using the . gives a warning
    
    // To take apart the tuple, we destuct (de-struct) it:
    val (s,i,d) = t // s is "balloon", i is 5...
    
    val grades = List("A", "A-", "B+", "B") 
    
  }

  // Daniel's notes (Thu 18 Jan 2018)
  /*

Scala Types:

Int , Bool , Byte , Char , Double , Short , Float

int Java , int , Integer

Everything is an object --> no primitive types like in java

every operation is a "function call"

8.toString()

8.toInt --> not toInt() // no brackets.....
because it doesn't change things (8 was an int still an int) and doesn't take anything as input

toString() also works as toString. without brackets

a + b is the same as a.+(b)
operators are functions

Strings , Arrays , Lists , Sets , ....


8.to(11) gives range from 8 to 11 in scala collection format
also can do 8 to 11

there is no ++ or -- in scala

can make function calls to Java but can't mix syntax

Semi-colons , are inferered : it does it for you

val x = 3 +  <-- doesn't make sens eot have a semi colon there , so scala will no put it
val x = 3   \n  +4 ;  <-- makes sense in Java for example

val x = 3 \n +4      <-- doesn't make sense in scala since val x = 3 makes sense to put a semi colon , but then the + 4 will get fucked

function:

def foo(x:Int) : Int = {
x + 1
}

takes x , an int as an argument , and returns an Int
doesn't have a return function
return function is the last thing done inside the function

return type doesn't really need to be defined in this case

def ++ (x:Int) = x + 1 is a valid way to define the function before. No need for the brackets in this case , no need to return type

functions that dont return things are called procedures.  def foo(x:Int) = println(x)

return type = Unit (similar to void in Java)

Conditionals:

If-Statements: they are expressions: they return something of type Unit

val x = if (3 > 4) 17 else 25
x: Int = 25

Similar to ternary operations in java : int x = (3 > 4) ? 17 :

if (3 < 4) {
if (2 < 3) 7 else 9

}else 88

nested ifs


val x = if ( 3 < 4 ) "foo" else 9
x: Any = foo  return type is "Any"  types are incompatible (String vs Int) then the return is "Any"
when both were Ints inside the ternary operation (if) then the return type was Int
--> this can be problematic , try to avoid


val x = if ( 3 < 4 ) {println("foo") } else 9
foo
x: AnyVal = ()


returns nothing since the first expression just prints something
returns AnyVal , as opposed to Any.  Any means any type

functiosn return the last line
x = {
...
...
... <-- returns whatever is there



Loops:

while(){
...
...
..
}    return type is Unit

can't type val x = while()...

Java: for(int i = 0 , i < 10 , i ++){
....
....
....
}

Scala:

for-loops --> iterators

for( i <- 0 to 10 ) println(i)
0
1
2
3
4
5
6
7
8
9
10

i isn't being UPDATED , it's being reCREATED at every iteration


for ( var <- expression (something we iterate over) {
body
}


for (i <- "Hello" ) println(i)    // what the fuck is this language
H
e
l
l
o

}


for( i <- "cdt" ; v <- "aeo" ; d <- "bx") println("."+i+v+d) // nested for loops this is retared
.cab
.cax
.ceb
.cex
.cob
.cox
.dab
.dax
.deb
.dex
.dob
.dox
.tab
.tax
.teb
.tex
.tob
.tox


for( i <- "cdt" ; v <- "aeo"  if(i< v) ; d <- "bx") println("."+i+v+d)  //conditional nested loops
.ceb
.cex
.cob
.cox
.deb
.dex
.dob
.dox


for( i <- "cdt" ; v <- "aeo"  if(i< v) ; z="bx" ;  d <- z ) println("."+i+v+d) // assign the string of d beforehand with the creation of a local variable z
.ceb
.cex
.cob
.cox
.deb
.dex
.dob
.dox


for loops --> comprehensions --> new collections from old


for (c <- "spatula") yield (((c-'a')+13)%26 + 'a').toChar
res6: String = fcnghyn

taking the value of C , adding the ascii value 13 to the character , taking the modulo 26  , then reverting it back to a char


for (c <- "fcnghyn") yield (((c-'a')+13)%26 + 'a').toChar
res7: String = spatula


val hex = for (i <- 'a' to 'f' ; j <- 'a' to 'f' ) yield "" +i+j   // creating a collection
hex: scala.collection.immutable.IndexedSeq[String] = Vector(aa, ab, ac, ad, ae, af, ba, bb, bc, bd, be, bf, ca, cb, cc, cd, ce, cf, da, db, dc, dd, de, df, ea, eb, ec, ed, ee, ef, fa, fb, fc, fd, fe, ff)

for (h <- hex) yield "0x"+h
res8: scala.collection.immutable.IndexedSeq[String] = Vector(0xaa, 0xab, 0xac, 0xad, 0xae, 0xaf, 0xba, 0xbb, 0xbc, 0xbd, 0xbe, 0xbf, 0xca, 0xcb, 0xcc, 0xcd, 0xce, 0xcf, 0xda, 0xdb, 0xdc, 0xdd, 0xde, 0xdf, 0xea, 0xeb, 0xec, 0xed, 0xee, 0xef, 0xfa, 0xfb, 0xfc, 0xfd, 0xfe, 0xff)



Arrays :

Val x = new Array[Int](5)


val x = new Array[String](5)
x: Array[String] = Array(null, null, null, null, null)

x(0)    // need round brackets to find the value at an index of an array
res9: String = null


x(2) = "Hello"
scala> x
res11: Array[String] = Array(null, null, Hello, null, null)


x = 7 <-- can't do that since it's immutable
BUT , contents of the Array are immutable.

can't resize array , it's fixed

Array Buffers:

import scala.collection.mutable.ArrayBuffer
>import scala.collection.mutable.ArrayBuffer


val b = ArrayBuffer[Int[()
b += 1
b += (1 , 2 , 3 , 4)
b ++= Array(8 , 10 12)

for(i <- 0 until b.length) println((b(i))

for (i <- b) println(i)


More on arrays:

import scala.io.Source
val file = Source.fromFile("alice.txt", "UTF-8") // load a file into a variable

val text = file.toString

val words = text.split("\\s+") // create an array of strings, all the words in the file

val lowers = for(w <- words if(w(0).isLower)) yield w  // lowers not only contains all the words that start with a lower case

val caseless = for(w <- words) yield w.toLowerCase     // return all the words in lower case

val c2 = caseless.clone // clone caseless into c2

scala.util.Sorting.quickSort(c2)  // c2 is now sorted. calling a builtin quick sort function

val sorted = caseless.sorted  //sorted is now a sorted version of caseless


Tuples :  pairs , triples , quadruples , etc...

val t = ("balloon" , 5 , 2.0)
(String, Int, Double) = (balloon,5,2.0)   //tuples start at 1

 t._1
res1: String = balloon

val (s , i , d) = t   //Destructuring
s: String = balloon
i: Int = 5
d: Double = 2.0



List:

() -> () -> () -> () -> null.   //linked list

val grades = List ("A" , "A-" , "B+" , "B")
grades: List[String] = List(A, A-, B+, B)

val grades = "A"::"A-"::"B+"::"B"::Nil
grades: List[String] = List(A, A-, B+, B)

A::B::C::Nil  ==> A:: (B::(C::Nil))

grades.head
res2: String = A

grades.tail
res4: List[String] = List(A-, B+, B)

grades.tail.head
res5: String = A-

Functional programming
--> Functions are "1st class citizens"  --> we can pass to functions as parameters
                                        --> we can return functions from functions
                                        --> create dynamically

--> Immutability
            --> We don't update data

--> recursion as iteration

def plus(a:Int, b:Int) : Int = (a+b)
plus: (a: Int, b: Int)Int

def plus(a:Int, b:Int) : = a + b   // Equivalent
plus: (a: Int, b: Int)Int

def minus(a:Int, b:Int) : Int = (a-b)
minus: (a: Int, b: Int)Int

def times(a:Int, b:Int) : Int = (a*b)
times: (a: Int, b: Int)Int

def dec (a:Int) = a - 1
dec: (a: Int)Int

 def dec (a:Int) = minus(a , 1)
dec: (a: Int)Int

def square (a:Int) = times(a , a)
square: (a: Int)Int

def sumofsquares(x: Int , y: Int) = plus(square(x) , square(y))
sumofsquares: (x: Int, y: Int)Int

def prefixSum(n:Int) : Int = {
  if(n==0) 0
  else (prefixSum(n-1)+n)
}
     |      |      | prefixSum: (n: Int)Int

prefixSum(5)
res10: Int = 15



def fac (n:Int) : Int = {
if(n==0) 1
else (fac(n-1)*n)
}
     |      |      | fac: (n: Int)Int    // regular recursive call , stacking on the way down then computing in the way up
                                         // head recursion
fac(5)
res11: Int = 120

def tailfac(n:Int , m:Int) : Int = {
  if ( n==0) m
  else tailfac(n-1 , n*m)
}
     |      |      | tailfac: (n: Int, m: Int)Int

tailfac(5 , 1)
res12: Int = 120                    // here tailfac is different , computing on the way down
                                    // tail recursion , here the return up just returns the correct answer all the way up several times

tail recursion --> optimize iteration


def fac3(n:Int , m:Int = 1) : Int = {    // force m to be 1 if you only give 1 parameter , otherwise just take what you give it
  if ( n == 0 ) m
  else fac3(n-1 , n*m)
}
     |      |      | fac3: (n: Int, m: Int)Int

scala> fac3(5)
res14: Int = 120



def fac2(n:Int) : Int = {
  def helper(count: Int, prod: Int) : Int = {
    if(count==0) prod
    else helper(count-1 , prod*count)
  }
  helper(n ,1)
}
     |      |      |      |      |      | fac2: (n: Int)Int    //declare a function inside a function



def <name> (n:Int):Int = {
if(n == baseCase) <baseResult>
else <combine> (n , <name>(<reduce> (n)))
}



 def iterPattern(n:Int , basecase:Int , baseresult: Int , combine:(Int , Int)=>Int, reduce:(Int)=>Int) : Int = {
  if(n==basecase) baseresult
  else combine(n, iterPattern(reduce(n) ,  basecase , baseresult , combine , reduce))
}
     |      |      | iterPattern: (n: Int, basecase: Int, baseresult: Int, combine: (Int, Int) => Int, reduce: Int => Int)Int


iterPattern(5 , 0 , 1 , times, dec)
res16: Int = 120

iterPattern(5 , 0 , 0 , plus, dec)
res17: Int = 15

 */

  def iterPattern(n: Int, basecase: Int, baseresult: Int, combine: (Int, Int) => Int, reduce: (Int) => Int): Int = {
    if (n == basecase) baseresult
    else combine(n, iterPattern(reduce(n), basecase, baseresult, combine, reduce))
  }
  
  // Tue 23 Jan 2018
  def fac5(n: Int): Int = {
    // Last 2 arguments are anonymous functions. We define them 
    iterPattern(n, 0, 1, (a: Int, b: Int) => { a * b }, (a: Int) => { a - 1 })
  }
  
  // Can have functions as values
  val t = (a: Int, b: Int) => { a * b }
  
  // We can call iterpattern like this: 
  iterPattern(5, 0, 1, (a: Int, b: Int) => { a * b }, (a: Int) => { a - 1 }) // should return 5!= 120
  
  // Or like this (underscore and space)
  iterPattern(5, 0, 1, _ * _, _ - 1) // should return 5!= 120
  
  /**
   * @return <b>a function</b> that takes an Int and returns an Int
   */
  def addFactory(n: Int): (Int) => Int = _ + n
  
  val add5 = addFactory(5)
  
  def addFactory2(n: Int): (Int) => Int = {
    def adder(x: Int) = x + n
    adder // return adder. Could also say 'adder _' (without quotes). That would return a function
  }
  
  val callAddFactory2 = addFactory2(5)(6) // Should be 11

  // Play with this to get a feel for it

  val file = Source.fromFile("alice.txt", "UTF-8") // load a file into a variable

  val text = file.toString

  val words = text.split("\\s+") // create an array of strings, all the words in the file

  val lowers = for (w <- words if (w(0).isLower)) yield w // lowers not only contains all the words that start with a lower case

  val caseless = for (w <- words) yield w.toLowerCase // return all the words in lower case

  val c2 = caseless.clone // clone caseless into c2

  scala.util.Sorting.quickSort(c2) // c2 is now sorted. calling a builtin quick sort function

  val sorted = caseless.sorted  //sorted is now a sorted version of caseless
  
  // These do the same thing:
  words.sortWith((x, y) => (x > y)) // Here we didn't specify the types. Generally we should
  words.sortWith(_ > _) // Sort in the reverse order (ZYX...)
  
  // If we want to get a Boolean:
  words.sortWith((x, y) => (x > y): Boolean)
  words.sortWith(((x: String, y: String) => (x > y)): (String, String) => Boolean)
  
  // Then we went over Scala class hierarchy.
  // An instance of Unit is ()
  val void = () // Returns Unit
  
  //An instance of Null is null
  
  val ifStatement3 = if (3>4) 5 else true // returns an AnyVal function
  
  val ifStatement4 = if (3>4) 5 else "Hello!" // returns Any because "String"
    
  def divide(x: Int, y: Int): Int = {
    if(y != 0) x/y // Int
    else throw new RuntimeException() // Has type Nothing
  } // So we can merge Int and Nothing to an Int
  
  // Maps
  val grades = Map("Alice" -> 0.5, "Bob" -> 0.83) // Map
  
  // What if we want to increase Alice's grade?
  val gradesM = scala.collection.mutable.Map("Alice" -> 0.5, "Bob" -> 0.83) // Map
  gradesM("Alice") = 0.7 // Ok
  // But we can't do this in Comp302
  
  // Prof does not want to bell curve our grades!
  def bell(pct: Double, grades: Map[String, Double]): Map[String, Double] = {
    for((name, mark) <- grades) yield (name, mark * (1 - pct) + pct)
  }
  
  // We'll create a new kind of Map
  def bell2(pct: Double, grades: Map[String, Double]): Map[String, Double] = {
    //grades.keySet.zip(grades.keySet.map((name) => grades(name) * (1 - pct) + pct))
    grades // Prof couldn't figure out the syntax -_-'
  }
  
  // Map is a set of pairs
  // map() takes one argument
  
  
  
  //val (first, second) = 1 //x // Destruct
  
  // enhanced switch
  /* variable match {
   *   case
   * 
   * }
   * */
  
  val name = "Bart"
  name match {
    case "Bart" => "Yes"
    case "Lisa" => "Lisa"
    case _ => "default"
  }
  
  def testIt(name: String): String = {
      name match {
      case "Bart" => "Yes"
      case "Lisa" => "Lisa"
      case _ => "default"
    }
  }
  
  def checker(x: Any): String = {
    x match {
      case v: String => "Str"
      case (t1,t2) => "Tuple" + t1
      case _ => "dunno"
    }
  }
  
  
  // Thu 25 Jan 2018
  // Map: map each element x of a collection to some f(x).
  
  def mapFunc(): Unit ={
    (1 to 5).reduceLeft(_ + _) // does (((1+2)+3)+4)+5, recursively from the left
    (1 to 5).reduceLeft(_ * _) // = 5! = 120. reduceLeft needs 2 args (binary) 
    
    (1 to 5).mkString // "12345"
    (1 to 5).mkString(",") // "1,2,3,4,5". This is also done recursively like reduceLeft
    
    // We can mkString with reduceLeft:
    //(1 to 5).reduceLeft((s1,s2) => { s1 + "," + s2 }) // does not work. type mismatch
    
    // Two fixes:
    (1 to 5).reduceLeft((s1: Any,s2) => { s1 + "," + s2 })
    (1 to 5).map(_.toString).reduceLeft((s1,s2) => { s1 + "," + s2 })
    
    
    // There is also a reduceRight
    
    // What if we want to start from a predefined value? eg !,1,2,3,4,5
    ("!" ++ (1 to 5)).reduceLeft((s1: Any,s2) => { s1 + "," + s2 }) // ++ is for adding a collection
    
    // foldLeft lets us pass the start value and . Takes one arg and returns a func with one arg
    // foldLeft: ? 
    (1 to 5).map(_.toString).foldLeft("!")((s1,s2) => s1 + "," + s2)
    // Looks like a tree :/
    ("!" /: (1 to 5).map(_.toString))((s1,s2) => s1 + "," + s2)
    
  }
  
  def currying(): Unit = { // After the person, not the food lol
    // f(x,y) = [f(x)](y). [] is a function that we make from f(x) that takes y
    def prefix(p: String, s: String) = {p + s}
    prefix("hello", " bye") // "hello bye"
    
    val prefix2 = (p: String) => { (s: String) => {p + s} }
    prefix2("hello")(" buy")
    
    (Map[Char,Int]() /: "Mississippi"){
      (m,c) => m + (c -> (m.getOrElse(c,0)+1)) // if c not in map, return 0. We add 1 since char was found
    } // = Map(M -> 1, i ->4, ...)
    
    val s125 = 1::2::3::4::5::Nil
    val t125 = 1::2::4::5::6::Nil
    
    (false /: s125)((b, i) => { if(!b && i==3) true else b }) // true
    (false /: t125)((b, i) => { if(!b && i==3) true else b }) // false
  }
  
  def bindingFunc(): Unit = {
    /* Instead of val, we can use the let keyword for local vars. But we we don't need local vars,
     * Coz we can replace them with functions!
     * 
     * eg f(x) = (1 + xy)² + (1 + xy). Redundant, may cause double calculations
     * */
    def f (x: Double, y: Double) = {
      def fHelper(x: Double, y: Double, a: Double, b: Double) = {
        x*a*a + y*b + a*b
      }
    }
    
    // Let abstraction lets us use inner function parameters for local vars 
  }
  
  /** We refer to vars by names. Vars have values
   * Vars have lifetimes. They are born and expire.
   * Consider the pseudocode: <code>
   * function foo(a){
   *   var i
   *   ...
   *   // i dies here
   * } // a dies here </code>
   * 
   * Lifespan of a var: declaration ? until end of method/class
   * 
   * But when does foo() die?
   * def defines function even before it is declared. Mutual recursion possible
   * 
   * Values also have lifetimes: <code>
   * void char *x = malloc();
   * // ...
   * free(x); </code>
   * 
   * Same thing with new and garbage collector in Java.
   * 
   * ***
   * 
   * We associate name:value in scopes.
   * <code> var x=1; x=2 </code> binds and re-binds x 
   * 
   * */
  def scope(): Unit = {
    // Direct binding:
    val x = 2
    var w = 5
    
    // We consider effects of binding in an <i>environment</i>. 
    // We look it up (value or address) to execute code
    
    /* When are bindings actually applied in memory?
     * Lots of choices! *
     */
    val y = 2
    def foo(x: Int) = {
      x + y
    }
    foo(3) 
    
    // Can we change the order to:
    val z = 2 // 1 Can't switch these 2 tho
    foo2(3) // 2
    def foo2(x: Int) = {
      x + z
    }
    
    // The answer is yes: linker will create symbols for foo (N/A for REPL)
    
    /* Scope: area in which a set of bindings is active 
     * 2 main styles:
     * Static scoping: C, Java, Scala
     * Dymanic scoping: mostly older/obscure languages. Originally proposed for functional
     * languages like early LISP
     * 
     * Static scope = Lexical scope: 
     * We can tell from the code which values/variables can be accessed. No dynamic info
     * is needed. The simplest model is early BASIC (1964): One scope! No procedures or funcs:
     * 
     * // They had line numbers back then.
     * 1 a = 7
     * 2 GOSUB 10 // goto line 10
     * 3 a = x
     * 4 END
     * ...
     * 10 a = 9
     * 11 x = 11
     * 12 RETURN // This is bad an error-prone
     * 
     * So now we have local vs global scope:
     */
    
    val a = 1
    def bar(b: Int) = {val c = 2; a + b + c}
    def baz(b: Int) = {val c = 2; val a = 5; a + b + c} // Not the same scope for a!
    
    // Nesting scope
    def foo4()={
      val i = 3
      val j = { // open subscope
        val k = 2
        k + k
      } 
      // 5 + k // error: can't access k
    }
    
    /* Algol style: A name declared in a scope is known to its scope and all subscopes.
     * Shadowing means the same symbol represents many values (eg in C/Java for loops)
     * 
     * So we nest on a chain of environments:
     * 
     * Global →(parent pointer)→ foo() → bar() → ping()
     * 
     * This can be more complex than it looks!
     * Not necessarily a tree.
     *  
     */
    
    def example() = {
      var x = 10;
      {
        var x = 1
        x = x + x // 2
      }
      x = x + x
      x // 20
    }
    
    /* JavaScript has weird scoping:
     * 
     * function foo() {
     *   var x = 1
     *   { var x = 2 }
     *   return x // returns 2
     * } 
     * 
     * // is equivalent to:
     * 
     * function foo() {
     *   var x
     *   x = 1
     *   { x = 2 }
     *   return x // still 2
     * }
     * 
     * But Scala is more sane (ie we'll get 1)
     */
    
    /*
     * Declaration order:
     * Modular-3 style: order doesn't matter! 
     * But Scala complains sometimes
     */
    
    def error() = {
      val y = 8
      def foo() = {
        val x = y // Will complain here
        val y = 2
        x
      }
      foo()
    }
    
    /* Why care? Mutually recursive functions! 
     * 
     * val x = y // won't work
     * val y = x
     * 
     * Thinking in terms of sex, uh sets:
     * 
     * X = Y U {a}
     * Y = X U {b}
     * 
     * So we get X = Y = {a,b}. Scala does not allow this
     * 
     */
    
    def mutability() = {
      var n = 0
      def X() = {
        n = 1 // Chase n up to the upper environment and change it there. Think of this as a setter
      }
      def Y() = {
        var n = 2
        X()
      }
      X() // This function call creates an environment
    }
    
    // env: stack following func calls
    // parent env: where func is defined
    
    // look-up symbol: follow parent pointers
    
    
    /* Dynamic scoping:
     * Our parent linkers are based on the *current* env, not where func was defined
     */
    
  }
  
  // Thu 1 Feb 2018
  
  def recrusiveScoping()={
    def fact(n: Int): Int = {
      if(n==0) 1
      else fact(n-1)*n
    }
    fact(2) 
    
    // Here's what happens in that method call:
    /*
     * Global environment (GE)
     * fact
     * 
     * fact(2), points to GE
     * n = 2
     * 
     * fact(1), points to GE
     * n = 1
     * 
     * fact(0), points to GE (All point to GE)
     * n = 0
     * 
     * This is NOT the same as the callstack. This depends only on scome
     * 
     */
    
  }
  
    /**
     * When we execute code, we need the code itself, and 
     * and environment, and its parent, grandparent, ggp...
     * 
     * But what if we have all the ancestors? Then we can package things in a way to 
     * make things (funcs).
     * 
     * In a function, we have <parameters+body , environment where it is invoked >
     * We call this <> a CLOSURE.
     * 
     * Do not confuse this with a continuation, which is something (code bodies + environment)
     * that happens after the func
     */
  def closures() = {
    // In GE, define addMaker (param + body)
    def addMaker(x: Int): (Int) => Int = {
      (y: Int) => {x + y} // anonymous function
    }
    // When addMaker is called, we create an addMaker environment which has GE as a parent
    // Inside the addMaker environment, we define the anonymous func
    val add11 = addMaker(11) // bound to anonymous function
    add11(3) //14
    // This runs by creating an add11 envr, whose parent is addMaker
    
    // forbidden vars!
    var x = 8
    // Define in GE
    /** Constructs a new function and returns it */
    def incMaker() = {
      () => {x = x + 1}
    }
    // create new envr for incMaker, whose parent is GE
    val s = incMaker()
    x
    
    // Use encapsulation of local data ??? to closures
    // Returns a pair of things (a getter and setter)
    def makeObj() = {
      var name = ""
      // Getter     and       setter
      ( () => {name}, (n: String) => {name = n} )
    }
    
    val n1 = makeObj()
    
    // Invoke setter
    n1._2("Clark")
    // Getter
    n1._1() // Gets Clark
    
    def hMaker() = { // Never do this!
      var words = Array("hello", "hi", "yo")
      var helloF = new Array[() => String](3) // Array of functions
      var i = 0
      while (i < 3){
        helloF(i) = () => (words(i))
        i = i + 1 //↑ This is the function helloF(0,1,2)
      }
      helloF
    }
    def allHellos = hMaker() // hMaker envr
    allHellos(0)() // Fails! This is because the environment is wrong. i has been incremented
    
    def hMaker2() = {
      val words = Array("hello", "hi", "yo")
      // Each version has its own environment
      (for(i <- 0 until 3) yield { () => (words(i)) }).toArray // can still be improved
      /**
       * GE:
       * 0: creates an environment for anon, which creates another envr
       * 1: creates an environment for anon, which creates another envr
       * 2: creates an environment for anon, which creates another envr
       * */
    }
  }
  
  /**
   * You will make your own (functional) language!
   * Must be very formal.
   * Formal spec → implementation
   * Formal spec has the syntax (representation)
   * and semantics (what it means to execute it). We'll do this later
   * 
   * Programs are massive symbols, could be a UTF-8, ASCII, or even a picture!
   * We have a stream of characters that can be compiled into something that can be executed 
   */
  def LanguageConstruction() = {
    // Tue 6 Feb 2018: 2 interviews tmw!
    /* Languages have special values called tokens.
     * Some are keywords like val, for...
     * Some are identifiers, like method and variable names, eg to
     * There r rules to see if sth is a token or not, like = vs ==, so we need to look ahead 
     * We have to do this recursively, ie no C style iteration
     * Recognizing patterns will be done using regex, to find/match tokens
     * Our scanner will take a stream of chars. It'll branch off and look for every possible regex
     * in the language, eg does this mean "val" or identifier, or whitespace, then it emits tokens
     */
    
    // Regular expressions
    /* Language for recognizing patterns. Power comes from simplicity (ltd expressiveness)
     * Definition: A regex (re) is:
     * 0) empty string, which we'll symbolize with epsilon
     * 1) A single character, like 'a', 'b', ':',...
     * 2) If R1 and R2 are regexes, then the concat R1R2 is a re // recursive definition
     * 3) If R1 and R2 are regexes, then R1|R2 is a choice re // recursive
     * 4) If R1 is a re, then so is R1*, where * is the Kleene star, for 0 or more repitions
     * 
     * Does "x|y*" mean "x|(y*)" or "(x|y)*"? So use brackets in case of ambiguity
     * "a|b" matches "b", "c" and "a" don't match
     * "(abc)*" matches "", "abc", "abcabc"
     * if we want at least one abc: (abc)(abc)*
     * 
     * Extra syntax: makes life less awkward. We'll use these later
     * Plus operator (abc)+ = (abc)(abc)*
     * 0 or 1 of a string: (abc)? = (abc)|epsilon
     * Any lowercase char: [a-z] = a|b|c|...|y|z 
     * Any alphabetic char: [a-zA-Z] = a|b|c|...|y|z|A|B|...|Z
     * Any char from this set: [aqwf3!]
     * Anything except a-z: [^a-z]
     * To re the dash symbol: Put at beginning or end: [abc-]
     * whitespace [w\n\t\r] \s for any whitespace.
     * Escape sequences
     * Anchors ^ start of str, $ end of str, eg ^abc$
     */
    val numberPattern = "[0-9]+".r // .r is the Scala operator to convert to a regex
    // To seach for this pattern in a String:
    numberPattern.findAllIn("abc 123 8 ppp 8") // finds 123, 8, 8123. Returns a non-empty iterator, which we can convert toArray()
    numberPattern.findFirstIn("abc 123 8 ppp 8") // returns Some(123)
    numberPattern.findAllIn("abc ppp") // returns None
    // So we get an Option[String], either Some(thing) or None. These are subclasses of the case class
    numberPattern.findFirstIn("abc 123 8 ppp 8") match {
      
    }
    // Matching numbers is usually easy, but should we allow 000007?
    // Matching identifiers 
    val id = "^[_a-zA-Z][_a-zA-Z0-9]*".r 

    
    
    val key_val = "^val\b".r // \b means boundary of a word
    // In the Alice example we did "\\s+". First \ is to esc the backslash, then '\s' is for a space, followed by a +
    
    // Convert a language stream to tokens, using regex. See java.ulil.regex.Pattern since that's what Scala delegates to
    
    // To build a Scanner, test against all regex and branch based on possibilities
    // Some tokens overlap, ie val vs value
    
    def patternMatch1() = {
      val re_id = "^[_a-zA-Z][_a-zA-Z0-9]*".r // regex for an identifier, ^[_a-zA-Z][_a-zA-Z0-9]*
      val re_ws = "^\\s+".r // regex for whitespace, ^\s+. Note the one \ here
      
      def scan(input: String, tokens: List[String]): List[String] = {
        if(input=="") tokens
        else{
          re_ws.findFirstIn(input) match {
            case Some(x) => return scan(input.substring(x.lenghth), "WS"::tokens)
            case None => // do nothing
          }
          re_id.findFirstIn(input) match {
            case Some(x) => return scan(input.substring(x.lenghth), "ID(" + x + ")" ::tokens)
            case None => // do nothing
          }
          // ...
          
          // If we get a garbage char
          "ERROR"::tokens
        }
        
        scan("foo bar      ping  ").reverse // recursion will give us a backwards string
        
        // Take Comp330/520 for better ways to do this
        
       
      }
    }
    
    // 2nd part of language is the PARSER
    // program will have var declarations, eg 
    var vardecl = 0
    def functiondecl_id ( paramList: Int ) = { /* expression */ } // Not actually an Int
    // expression breaks down into list of expressions
    
    // Use BNF (Backus-Naur Form) grammars, we'll use CFG (context-free grammar)
    // List of rules, some may be recursive, composed of symbols=identifiers, which can be terminal or not.
    // Rule: LHS (non terminal) ::= RHS (list of term/nonterm)
    // every non term must be on atl one LHS
    
    // example
    //for (foo <- (1 to 10)) yield foo
    
    // forStmt ::= FOR "(" ID "<-" expr ")" YIELD expr
    // Grammar does not tell us if prog is correct, it just give the format. We can't know just using grammar if sth is an iterator or not
    // so we just say expr
    
    // expr ::= binexpr | unexpr | fncall | ifexpr | ...
    // OR
    // exper ::= binexpr; exper ::= unexpr; ...
    
    // blockexpr ::= "{" exprList "}"
    // exprList ::= expr exprList | e // e is epsilon, ie nothing
    
    // Feb 15
    
    //....
    
    // 13:13 Now we're using Int to make a calculator
    
    // { case _ ~ e ~ _ => e } // if I get sth I don't care abt (_), return the expr as an Int without brackets
    // t is for term, recursive
    // Specify constants with caps, like Minus, so Scala will assume that it's a constant
    // last expr case is to avert compiler warnings
   
    /*
    def term: Parser[Int] = factor ~ rep(Times ~ factor)^^{
      case f ~ list => f * list.map((x) => x._2).product // throw away extra times symbols we don't need
      // product is built-in
    }
    // factor removes ()
    
    // Invoke like this:
    // val p = new ExprParser(); p.parseAll(p.expr,"3*4+1-2").get
    
    */
    
    // In assignment, use a tree structure based on this hierarchy:
    abstract class ASTNode // That's it! No body
    case class ASTNumber(val value: Int) extends ASTNode // That's it! case keyword is optional here
    case class ASTBinOp(val op: String /* + - or * */, val right: ASTNode, val left: ASTNode) extends ASTNode
    // case indicates a deeper meaning than regular inheritance
    
    
    // In 3rd parser, we use these ASTNodes
    
    // Case class. An Option is either Some or None
    
    /* Consider this case:
    
          (f)         (*,TreeNode)::(*,TreeNode)::Nil // TreeNode = 🦑
         /   \
         
         Gets converted to this List: (f)::🦑::🦑::Nil
         
                          binOp
                         / \  \
                      binOp 🦑 🦑  
                       / \  
                      binOp 🦑
                          \...
        ASTBinOp(-,ASTBinOp(*,ASTNumber(3),ASTNumber(2)),ASTNumber(1)) // This is just like Comp250 ExpressionNode
        
                  -
                /  \
               *    1
             /   \ 
            3     2
    */
    
    def pp(a: ASTNode): String = { // pretty printer
      a match{
        case ASTNumber(n) => n.toString
        case ASTBinOp(op,left,right) => "(" + pp(left) + op + pp(right) + ")" // left assoc eg 1*2*3 = (1*2)*3 since we used reduceLeft
      }
    }
    
    // That's a nice way of validating/fixing things
    
    // Expression parser is boring, so we'll make a new functional lanuguage using a functional langugae! WML
    // Wiki markup language. Format webpages in convenient nice syntax for an HTML page
    // Use a simplified restricted form, known as Wikitext. Standardization is still being attempted
    // We will use MediaWiki's style to make a platform for non-technical users to make wiki pages for TV shows, games etc
    // We will make commands from unlikely char sequences
    /* In HTML, "abc\ndef" -> "abc def", but "abc\ndef\n" -> <p>abc</p>
    *x *y means <ul><li>x</li><li>y</li></ul> 
    */
    
    // We're more interested in the template language. Eg, The way we describe Simpsons characters should be the same
    // gamepedia.com. Infobox on the rite. Can edit page to view source
    /* example: Beer (https://elex.gamepedia.com/index.php?title=Beer&action=edit)
    
    {{Infobox inventory
    | name = Beer
    | icon = Icon_Food.png
    | image = Icon_beer.png
    | size = 60px
    | desc = Brewed using whatever came to hand, this doesn't come closes to complying with any purity law.
    | inventory = item
    | slot = food
    | cost = 10
    | value = 2
    | health = +10
    | duration = 10
    }}
    
    {{ is reserved so {{ foo }} will find the page called foo, copyingt the contents -> "transclusion"
    
    Parameters:
    {{ foo | blah | yadda | /*emptyString/ }} // treat this like a function call
    
    bar 
    Hello {{ nameify | Clark | Verbrugge }}
    
    namify 
    Mr {{{1}}} {{{2}}} // These are parameters refered by index number
    But if we do {{{8}}}, it won't crash or give an error, so it will just return {{{8}}}
    example: Mr {{{1}}} {{{8}}} gives "Mr Clark {{{8}}}"
    
    We can also name our parameters, eg Hello {{ nameify | first=Clark | last=Verbrugge }}
    In nameify: Mr {{{first}}} {{{last}}}
    
    In wikis, this falls short of a programming language.
    What would happen if foo transcluded itself?
    foo {{ foo }} is banned! Can't do recursion directly
    
    Also, there is no real scoping model. Nor can we pass functions and envrionments or make anonymous pages
    
    So we're gonna fill in these gaps by making a real functional language! WML
    
    We won't use the wiki param passing. Instead, we will do this:
    {{ foo | bar | ping }}
    
    foo: abc, def
        {{{ ab }}}
        
    We will do this in a file (like a page)
    So special synatx for defining templates. Everything lives in the same file!
    
    In order to do this, we need a grammar. See A2
    
    
    */
    
    
  } // end language construction
  
  
} // end Food
