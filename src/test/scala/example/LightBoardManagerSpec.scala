package example

import cats.data.State
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class LightBoardSpec extends AnyWordSpec with Matchers {

  import scala.language.postfixOps
  import LightBoardUtils.CoordinateBuilder
  import light.LightBoard._
  import light._

  "A (1000x1000) light board" when {
    val rows = 1000
    val columns = 1000
    "instantiated" should {
      "be completely turned off" in {
        val init = LightBoard(rows, columns)

        val exec = find(LightOn).runA(init)

        exec.value should {
          matchPattern { case None => }
        }

        val exec2 = find(LightOff).runA(init)
        exec2.value should matchPattern { case Some(_) => }
      }
      "evaluates as a matrix of the same size" in {
        val aLightBoard = LightBoard(rows, columns)
        (rows, columns) shouldBe LightBoard.size.runA(aLightBoard).value
      }
    }
    "turn on 0,0 through 999,999 " should {
      val start = Coordinate(0, 0)
      val end = Coordinate(999, 999)

      "turn on (or leave on) every light" in {
        val init = LightBoard(rows, columns)

        val ops = for {
          _ <- turnOn(start, end)
          lightOff <- findIn(start, end, LightOff)
        } yield lightOff

        val exec = ops.runA(init).value

        exec should {
          matchPattern { case None => }
        }
      }
    }
    "toggling 0,0 through 999,0" should {
      val start = Coordinate(0, 0)
      val end = Coordinate(999, 0)

      "if toggled once, return the first line of 1000 lights as LightOn" in {
        val init = LightBoard(rows, columns)

        val ops = for {
        _       <- toggle(start, end)
        founded <- findIn(start, end, LightOff)
        } yield founded

        val exec = ops.runA(init).value

        exec should {
          matchPattern { case None => }
        }
      }
      "if toggled twice, return the first line of 1000 lights as LightOff" in {
        val init = LightBoard(rows, columns)

        val ops = for {
          _       <- toggle(start, end)
          _       <- toggle(start, end)
          founded <- findIn(start, end, LightOn)
        } yield founded

        val exec = ops.runA(init).value

        exec should matchPattern {
          case None =>
        }
      }
      "if toggled twice, return the first line of 1000 lights as LightOn" in {
        val init = LightBoard(rows, columns)

        val ops = for {
          _ <- toggle(start, end)
          _ <- toggle(start, end)
          _ <- toggle(start, end)
          founded <- findIn(start, end, LightOff)
        } yield founded

        val exec = ops.runA(init).value

        exec should matchPattern {
          case None =>
        }
      }
    }
    "turn off 499,499 through 500,500" should {
      import cats.implicits._
      import cats.kernel.Semigroup
      implicit val combineCoordinate = new Semigroup[Coordinate] {
        override def combine(first: Coordinate, second: Coordinate): Coordinate =
          Coordinate(first.x + second.x, first.y + second.y)
      }

      val start = Coordinate(499, 499)
      val end = Coordinate(500, 500)
      "turn off the middle four lights" in {
        val init = LightBoard(rows, columns)

        def turnOnAllLights(rows: Int, columns: Int): State[LightBoard, Unit] = {
          val start = Coordinate(0, 0)
          val end = Coordinate(rows - 1, columns - 1)
          turnOn(start, end)
        }

        val ops = for {
          _ <- turnOnAllLights(rows, columns)
          _ <- turnOff(start, end)
          lightOn <- findIn(start, end, LightOn)
          lightOff <- findIn(start |+| Coordinate(2, 2), end |+| Coordinate(2, 2), LightOn)
        } yield (lightOn, lightOff)

        ops.runA(init).value should matchPattern {
          case (None, Some(_)) =>
        }
      }
    }
    "execute multiple commands" should {
      "return 998,996 lights on after instructions" in {
        val init = LightBoard(rows, columns)

        val exec = for {
          _ <- turnOn((0, 0) toCord, (999, 999) toCord)
          _ <- toggle((0, 0) toCord, (999, 0) toCord)
          _ <- turnOff((499, 499) toCord, (500, 500) toCord)
          brightness <- totalBrightness
        } yield brightness

        val turnOnBrightness = rows * columns
        val toggleBrightness = rows * 2
        val turnOffBrightness = -2 * 2
        val expectedBrightness = turnOnBrightness + toggleBrightness + turnOffBrightness
        assert(exec.runA(init).value == expectedBrightness)
      }
      "knows how many lights are lit" in {
        val init = LightBoard(rows, columns)
        val brightnessExpected: Int = 539560
        val lightsOnExpected: Int = 230022
        val exec = (for {
          _ <- turnOn(Coordinate(887, 9), Coordinate(959, 629))
          _ <- turnOn(Coordinate(454, 398), Coordinate(844, 448))
          _ <- turnOff(Coordinate(539, 243), Coordinate(559, 965))
          _ <- turnOff(Coordinate(370, 819), Coordinate(676, 868))
          _ <- turnOff(Coordinate(145, 40), Coordinate(370, 997))
          _ <- turnOff(Coordinate(301, 3), Coordinate(808, 453))
          _ <- turnOn(Coordinate(351, 678), Coordinate(951, 908))
          _ <- toggle(Coordinate(720, 196), Coordinate(897, 994))
          _ <- toggle(Coordinate(831, 394), Coordinate(904, 860))
          brightness <- totalBrightness
          lightsOn <- count((light: LightState) => light == LightOn)
        } yield (brightness, lightsOn))
          .runA(init)
          .value

        exec should matchPattern {
          case (brightness: Int, lightsOn: Int)
            if (brightness == brightnessExpected) && (lightsOn == lightsOnExpected) =>
        }
      }
    }
    "turn on 0,0 through 0,0" should {
      "increase the total brightness by 1" in {
        import LightBoardUtils.CoordinateBuilder
        import scala.language.postfixOps

        val init = LightBoard(rows, columns)
        val ops = for {
          _ <- turnOn((0, 0) toCord, (0, 0) toCord)
          brightness <- totalBrightness
        } yield brightness

        assert(ops.runA(init).value == 1)

        val tt = (
          for {
            _ <- turnOn((0, 0) toCord, (1, 1) toCord)
            brightness2 <- totalBrightness
            s <- LightBoard.size
          } yield (brightness2, s))
          .runA(init)
          .value
        assert(tt._1 == 4)
        assert(tt._2 == (1000, 1000))
      }
    }
    "toggle 0,0 through 999,999" should {
      "increase the total brightness by 2000000" in {
        val init = LightBoard(rows, columns)
        (for {
          _ <- toggle((0, 0) toCord, (999, 999) toCord)
          brightness <- totalBrightness
        } yield brightness)
          .runA(init)
          .value shouldBe 2000000

        //            toggle(Coordinate (0,0), Coordinate(999,999))
        //              .flatMap(a => totalBrightness.flatMap( brightness => brightness)
        //              .runA(init)
        //              .value

      }
    }
    "toggle 0,0 through 1,1 and then turn off" should {
      "returns 4" in {
        val init = LightBoard(3, 3)
        val start = Coordinate(0, 0)
        val end = Coordinate(1, 1)

        (for {
          _ <- toggle(start, end)
          _ <- turnOff(start, end)
          brightness <- totalBrightness //4
          _ <- toggle(start, end)
          brightness2 <- totalBrightness //12
          _ <- turnOff(start, end)
          brightness3 <- totalBrightness //8
          _ <- turnOn(start,end)
          brightness4 <- totalBrightness //12
        } yield (brightness, brightness2, brightness3, brightness4))
          .runA(init).value should matchPattern {
            case (4,12,8,12) =>
          }
      }
    }
  }
}

object LightBoardUtils {

  import light.LightBoard.Coordinate

  implicit class CoordinateBuilder(tuple: (Int, Int)) {
    def toCord: Coordinate = Coordinate(tuple._1, tuple._2)
  }
}