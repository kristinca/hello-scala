//this is scala 2.13 ^^'

import scala.math.Pi
//
// 1+1
//val x = 555
//x * x

// same as python: string in ""
// string.lenght -> present yay
// operations:
1 > -8
2.max(0) //max value between left and right
3.max(2)
-6.abs
//concatenation of strings
"hello "+" scala"+" ! ^^ "
//n *
val n = 2
n * '*'
//uppercase
"Hello, Is It Me you're looking for? :D".toUpperCase()
4<2 && 3>2
(4<2).&&(3>2)
//large expressions are HARD TO READ AND WRITE
// DON'T WRITE LARGE EXPRESSIONS
//val some_name
//Boolean, Int 32-signed int, Double 64-bit floating point number, String ...
//Methods -> functions
//in scala 2: {} in methods
def nsquared(n: Int): Int =
  n * n
nsquared(5)
val a = 1
// if ... then ...
//else if ... then...
// else .. .
//in scala 2: {} and no then
case class Square(side: Int){
  val area = side * side}

val facade = Square(20)
facade.area + 2

case class Circle(r: Int){
    val area = Pi * r * r}
val facade2 = Circle(20)
facade2.area

//case classes CAN'T BE CHANGED

val smallCircle = Circle(r = 2)
val largeCircle = smallCircle.copy(r = smallCircle.r * 10)

val squaresix = Square(4).copy(6)
squaresix.side

//sealed trait Shape
//case class Rectangle(width: Int, height: Int) extends Shape
//case class Circle(radius: Int) extends Shape

//val shape: Shape = Rectangle(4,3)


//val circle = Circle(2)
//
//enum -> defines a type w/ exactly known values
// enum ONLY IN SCALA 3 ! check the scala version AAA! :D