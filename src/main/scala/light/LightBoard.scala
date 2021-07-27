package light

import light.LightBoard.{Coordinate, Matrix}

case class LightBoard(matrix: Matrix) {

  def count(f: LightState => Boolean) =
    matrix.count{ case (_, light) => f(light.stat) }

  def turnOff(start: Coordinate, end: Coordinate): LightBoard= {
    eval(start, end, (x, y) =>
      matrix(Coordinate(x, y)).turnOff)
    this
  }

  def turnOn(start: Coordinate, end: Coordinate): LightBoard= {
      eval(start, end, (x, y) =>
        matrix(Coordinate(x, y)).turnOn)
      this
  }

  def toggle(start: Coordinate, end: Coordinate): LightBoard = {
    eval(start, end, (x,y) =>
      matrix(Coordinate(x, y)).toggle)
    this
  }
  def find(light: LightState): Option[(Coordinate, Light)] =
    matrix.find{ case (_, l) => l.stat == light}

  /**
   * 1 2 3 4
   * 2 3 4 2
   * 3 1 3 3 => find (0,2) (2,2) => iterator through coordinates => if found match return
   * */
  def findIn(start: Coordinate, end: Coordinate, state: LightState): Option[(Coordinate, Light)] = {
    val xRange = start.x to end.x
    val yRange = start.y to end.y
    matrix.find{
      case (Coordinate(x, y), light) =>
        (xRange contains x) &&
          (yRange contains y) &&
            (light.stat == state)
    }
  }

  def totalBrightness: Int = matrix.foldLeft(0){
    case (acc, (_,light)) => acc + light.brightns
  }

  def size: (Int, Int) = {
    implicit object ord extends Ordering[Coordinate] {
      /** Ordered by Y and then X:
       * if cord.Y > anotherCord.Y => return cord
       * else => evaluates X
       * */
      override def compare(cord: Coordinate, anotherCord: Coordinate): Int =
        ( cord.x compare anotherCord.x, cord.y compare anotherCord.y ) match {
          case (_, yComparison) if yComparison != 0 => yComparison
          case (xComparison, yComparison) if yComparison == 0 => xComparison
        }
    }

      val Coordinate(maxX, maxY) = matrix.keySet.max

      val indexOffset = 1
      (maxX + indexOffset, maxY + indexOffset)
  }

  override def toString: String =
    matrix.foldLeft("") { case (acc: String, (Coordinate(x: Int, y: Int), l: Light)) =>
      s"$acc | ($x,$y) -> $l"
    }

  def eval[A](start: Coordinate, end: Coordinate, f: (Int, Int) => A): LightBoard = {
    for {
      x <- start.x to end.x
      y <- start.y to end.y
    } f(x, y)
    this
  }
}
object LightBoard {
  import collection.mutable.Map

  type Matrix = Map[Coordinate, Light]
  case class Coordinate(x: Int, y: Int)

  def apply(x: Int, y: Int): LightBoard = {
    LightBoard(
      collection.mutable.Map.from(
        for {
          x <- 0 until x
          y <- 0 until y
        } yield Coordinate(x,y) -> Light()
      ))
  }
}
