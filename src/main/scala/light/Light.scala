package light

import light.LightBoard.{Coordinate, Matrix, eval}

sealed trait Light {
  override def equals(obj: Any): Boolean =
    this.getClass == obj.getClass
}

case class LightOn() extends Light {
  override def toString: String = "O"
}

case class LightOff() extends Light {
  override def toString: String = "X"
}

case class LightBoard(matrix: Matrix) {
  def count(f: Light => Boolean) =
    matrix.count{ case (_, light) => f(light) }

  def turnOff(start: Coordinate, end: Coordinate): LightBoard= {
    eval(start, end, (x, y) =>
      matrix((x, y)) = LightOff())
    this
  }

  def turnOn(start: Coordinate, end: Coordinate): LightBoard= {
    eval(start, end, (x, y) =>
      matrix((x, y)) = LightOn())
    this
  }

  override def toString: String =
    matrix.foldLeft("") { case (acc: String, ((x: Int, y: Int), l: Light)) =>
      s"$acc | ($x,$y) -> $l"
    }

  def find(light: Light): Option[Light] =
    matrix.values.find(_ == light)

  def findIn(begin: Coordinate, end: Coordinate, light: Light): Option[(Coordinate, Light)] = {
    val xRange = begin._1 until end._1
    val yRange = begin._2 until end._2
    matrix.find{
      case ((x, y), l) =>
        (xRange contains x) &&
          (yRange contains y) &&
            (l == light)
    }
  }

  def size: (Int, Int) = {
    val (maxX, maxY) = matrix.keySet.max
    (maxX + 1, maxY + 1)
  }

  def toggle(start: Coordinate, end: Coordinate): LightBoard = {
    def toggle(light: Light): Light =
      light match {
        case LightOn() => LightOff()
        case LightOff() => LightOn()
      }

    val xRange = start._1 until end._1
    val yRange = start._2 until end._2

    matrix.foreach { case ((x, y), light) =>
      if ((xRange contains x) && (yRange contains y))
        matrix((x, y)) = toggle(light)
    }

    this
  }

}

/**
 * List ( 0: List(0: 1,
 * 1: 2,
 * 2: 3),
 * 1: List(0: 1,
 * 1: 2,3),
 * 2: List(1,2,3),
 * 3: List(1,2,3),
 * 4: List(1,2,3),
 * 5: List(1,2,3)
 */
object LightBoard {
  type Matrix = collection.mutable.Map[Coordinate, Light]
  type Coordinate = (Int, Int)

  def apply(x: Int, y: Int): LightBoard = {
    LightBoard(
      collection.mutable.Map.from(
        (0 until x).flatMap(xCord =>
          (0 until y).map(yCord =>
            (xCord, yCord) -> LightOff()))
          .toMap))
  }

  def eval[A](start: Coordinate, end: Coordinate, f: (Int, Int) => A): Unit = {
    val xRange = start._1 until end._1
    val yRange = start._2 until end._2
    xRange.foreach(x =>
      yRange.foreach(y =>
        f(x, y)
      ))
  }

}