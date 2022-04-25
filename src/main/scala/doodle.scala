package x

import doodle.image._
import doodle.core._
import doodle.image.syntax.all._
import doodle.java2d._
import cats.effect.unsafe.implicits.global
import doodle.effect.Writer._
import doodle.core.PathElement._
import scala.math


val blackSquare = Image.rectangle(40, 40).fillColor(Color.black)
val orchidSquare = Image.rectangle(30,30).fillColor(Color.darkOrchid)

val twoByTwo = 
  (orchidSquare.beside(blackSquare))
    .above(blackSquare.beside(orchidSquare))

val fourByFour = 
  (twoByTwo.beside(twoByTwo))
    .above(twoByTwo.beside(twoByTwo))

val chessboard = 
  (fourByFour.beside(fourByFour))
    .above(fourByFour.beside(fourByFour))

val circleImage = 
  Image.circle(300).strokeWidth(10).fillColor(Color.aquamarine)
    
object DrawStuff extends App:
  chessboard.draw()
  circleImage.draw()

  val angleTriangle = Angle(Math.toRadians(60))
  val angleAB = Math.toRadians(120)
  val angleAC = Angle(Math.toRadians(30))

  val a = 100
  val b = 100
  val c = Math.sqrt((Math.pow(a, 2) + Math.pow(b, 2)) - (2 * a * b * Math.cos(angleAB)))

  // start arrowhead construction sierpinski gasket
  val seq = Seq(lineTo(a, angleTriangle), 
                lineTo(c, angleAC),
                lineTo(a * 2, 0))

  Image.openPath(seq).draw()

  // for fun: draw some triangles in triangles
  val seq2 = Seq(lineTo(a * 2, angleTriangle),
                 lineTo(c, angleAC), 
                 lineTo(a, angleTriangle),
                 lineTo(a, 0),
                 lineTo(c, angleAC),
                 lineTo(a * 2, 0),
                 lineTo(0, 0)
                 )

  Image.openPath(seq2).draw()

  //circleImage.write[Png]("circleImage.png")
  