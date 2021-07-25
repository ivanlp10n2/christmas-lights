package light

import light.LightBoard.{Coordinate, Matrix}

case class Light(private var state: LightState = LightOff,
                 private var brightness: Int = 0){
  def turnOff: Light = {
    state = LightOff
    brightness = if (brightness > 0) brightness - 1 else 0
    this
  }

  def turnOn: Light = {
    state = LightOn
    brightness = brightness + 1
    this
  }

  def toggle: Light = {
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

  def toggle(start: Coordinate, end: Coordinate): LightBoard = {
    eval(start, end, (x,y) => matrix(x, y).toggle)
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
    val xRange = start._1 to end._1
    val yRange = start._2 to end._2
    matrix.find{
      case ((x, y), l) =>
        (xRange contains x) &&
          (yRange contains y) &&
            (l.stat == state)
    }
  }

  def totalBrightness: Int = matrix.foldLeft(0){
    case (acc, (_,light)) => acc + light.brightns
  }

  def size: (Int, Int) = {
    val (maxX, maxY) = matrix.keySet.max
    val indexOffset = 1
    (maxX + indexOffset, maxY + indexOffset)
  }

  override def toString: String =
    matrix.foldLeft("") { case (acc: String, ((x: Int, y: Int), l: Light)) =>
      s"$acc | ($x,$y) -> $l"
    }

  def eval[A](start: Coordinate, end: Coordinate, f: (Int, Int) => A): LightBoard = {
    for {
      x <- start._1 to end._1
      y <- start._2 to end._2
    } f(x, y)
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
  import collection.mutable.Map

  type Matrix = Map[Coordinate, Light]
  type Coordinate = (Int, Int)

  def apply(x: Int, y: Int): LightBoard = {
    LightBoard(
      collection.mutable.Map.from(
        for {
          x <- 0 until x
          y <- 0 until y
        } yield (x,y) -> Light()
      ))
  }


}