package light

import cats.data.State
import light.LightBoard.Coordinate

final case class LightBoard(matrix: Map[Coordinate, Light])

object LightBoard extends WriteOps with ReadOps{
  case class Coordinate(x: Int, y: Int)

  def apply(x: Int, y: Int): LightBoard =
    new LightBoard(
      Map.from(
        for {
          x <- 0 until x
          y <- 0 until y
        } yield Coordinate(x, y) -> Light()
      )
    )
}

sealed trait WriteOps extends MatrixNavigationUtils {
  case class Command(coordinate: Coordinate, operation: Operation)

  def toggle(start: Coordinate, end: Coordinate): State[LightBoard, Unit] =
    modifyState(start, end, Toggle)

  def turnOn(start: Coordinate, end: Coordinate): State[LightBoard, Unit] =
    modifyState(start, end, TurnOn)

  def turnOff(start: Coordinate, end: Coordinate): State[LightBoard, Unit] =
    modifyState(start, end, TurnOff)

  def modifyState(start: Coordinate,
                  end: Coordinate,
                  operation: Operation) : State[LightBoard, Unit] =
    State.modify { buildCommands(start, end, operation)
        .foldLeft(_) (updateLightBoard)
    }

  private def buildCommands(start: Coordinate,
                            end: Coordinate,
                            operation: Operation) : LazyList[Command] = {
    traverseCoordinates(start, end)
      .map(Command(_, operation))
  }

  private def updateLightBoard(lightBoard: LightBoard, command: Command): LightBoard =
    lightBoard.copy(matrix = lightBoard.matrix +
      (command.coordinate -> updateLight(lightBoard, command))
    )

  private def updateLight(lightBoard: LightBoard, command: Command): Light =
    (lightBoard.matrix(command.coordinate), command.operation) match {
      case (light, TurnOn) => light.turnOn
      case (light, TurnOff) => light.turnOff
      case (light, Toggle) => light.toggle
    }
}

sealed trait ReadOps extends MatrixNavigationUtils {
  type Record = (Coordinate, Light)

  def count(f: LightState => Boolean):State[LightBoard, Int]=
    State.inspect { lightBoard =>
      lightBoard.matrix
        .count { case (_, light) => f(light.state) }
    }

  def find(state: LightState): State[LightBoard, Option[(Coordinate, Light)]] =
    State.inspect{ lightBoard =>
      lightBoard.matrix
        .find { case (_, l) => l.state == state }
    }

  def findIn(start: Coordinate, end: Coordinate, state: LightState): State[LightBoard, Option[Record]] =
    State.inspect{ lightBoard =>
      traverseCoordinates(start, end)
        .takeWhile{ position => lightBoard.matrix(position).state == state }
        .map( position => (position, lightBoard.matrix(position)) )
        .headOption
    }

  def totalBrightness: State[LightBoard, Int] =
    State.inspect { lightBoard =>
      lightBoard.matrix.foldLeft(0) {
        case (acc, (_, light)) => acc + light.brightness
      }
  }

  def size: State[LightBoard, (Int, Int)] =
    State.inspect{ lightBoard =>
    implicit object ord extends Ordering[Coordinate] {
      /** Ordered by Y and then X:
       * if cord.Y > anotherCord.Y => return cord
       * else => evaluates cord.X and anotherCord.X
       * */
      override def compare(cord: Coordinate, anotherCord: Coordinate): Int =
        (cord.x compare anotherCord.x, cord.y compare anotherCord.y) match {
          case (_, yComparison) if yComparison != 0 => yComparison
          case (xComparison, yComparison) if yComparison == 0 => xComparison
        }
    }

    val Coordinate(maxX, maxY) = lightBoard.matrix.keySet.max
    val indexOffset = 1
    (maxX + indexOffset, maxY + indexOffset)
  }

}

trait MatrixNavigationUtils {
  def traverseCoordinates(start: Coordinate, end: Coordinate): LazyList[Coordinate] = {
    val indexOffset = 1
    for {
      x <- LazyList.range(start.x, end.x + indexOffset)
      y <- LazyList.range(start.y, end.y + indexOffset)
    } yield Coordinate(x,y)
  }
}
