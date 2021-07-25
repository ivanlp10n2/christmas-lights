package light

import light.LightBoard.{Coordinate, Matrix, eval}

case class Light(private var state: LightState = LightOff,
                 private var brightness: Int = 0){
  def turnOff = {
    state = LightOff
    brightness = if (brightness > 0) brightness - 1 else 0
    this
  }

  def turnOn = {
    state = LightOn
    brightness = brightness + 1
    this
  }

  def toggle = {
    state = state match {
        case LightOn => LightOff
        case LightOff => LightOn
      }
    brightness = brightness + 2
    this
  }

  def brightns = brightness

  def stat = state
}

sealed trait LightState {
  override def equals(obj: Any): Boolean =
    this.getClass == obj.getClass
}

case object LightOn extends LightState {
  override def toString: String = "O"
}

case object LightOff extends LightState {
  override def toString: String = "X"
}

case class LightBoard(matrix: Matrix) {
  def totalBrightness: Int = matrix.foldLeft(0){
    case (acc, (_,light)) => acc + light.brightns
  }

  def count(f: LightState => Boolean) =
    matrix.count{ case (_, light) => f(light.stat) }

  def turnOff(start: Coordinate, end: Coordinate): LightBoard= {
    eval(start, end, (x, y) =>
      matrix((x, y)).turnOff)
    this
  }

  def turnOn(start: Coordinate, end: Coordinate): LightBoard= {
    eval(start, end, (x, y) =>
      matrix((x, y)).turnOn)
    this
  }

  override def toString: String =
    matrix.foldLeft("") { case (acc: String, ((x: Int, y: Int), l: Light)) =>
      s"$acc | ($x,$y) -> $l"
    }

  def find(light: LightState): Option[(Coordinate, Light)] =
    matrix.find{ case (_, l) => l.stat == light}

  /**
   * 1 2 3 4
   * 2 3 4 2
   * 3 1 3 3 => find (0,2) (2,2) => iterator through coordinates => if found match return
   * */
  def findIn(begin: Coordinate, end: Coordinate, state: LightState): Option[(Coordinate, Light)] = {
    val xRange = begin._1 until end._1
    val yRange = begin._2 until end._2
    matrix.find{
      case ((x, y), l) =>
        (xRange contains x) &&
          (yRange contains y) &&
            (l.stat == state)
    }
  }

  def size: (Int, Int) = {
    val (maxX, maxY) = matrix.keySet.max
    (maxX + 1, maxY + 1)
  }

  def toggle(start: Coordinate, end: Coordinate): LightBoard = {

    val xRange = start._1 to end._1
    val yRange = start._2 to end._2

    matrix.foreach { case ((x, y), light) =>
      if ((xRange contains x) && (yRange contains y))
        matrix((x, y)).toggle
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
            (xCord, yCord) -> Light()
          ))
        .toMap))
  }

  def eval[A](start: Coordinate, end: Coordinate, f: (Int, Int) => A): Unit = {
    val xRange = start._1 to end._1
    val yRange = start._2 to end._2
    xRange.foreach(x =>
      yRange.foreach(y =>
        f(x, y)
      ))
  }

}