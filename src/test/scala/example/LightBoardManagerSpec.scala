package example

import cats.data.State
import light._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class LightBoardSpec extends AnyWordSpec with Matchers {

  import light.LightBoard._

  "A Light" when {
    "compare subclass" should {
      "differentiate if they're the same" in {
        assert(LightOff == LightOff)
        assert(LightOn == LightOn)

        assert(LightOn != LightOff)
      }
    }
  }


  "A light board configured with 1000x1000 board" when {
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

    "turn on 0,0 through 999,999" should {
      val start = Coordinate(0, 0)
      val end = Coordinate(999, 999)

      "have only turned on lights" in {
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

      "toggle 0,0 through 999,0" should {
        val start = Coordinate(0, 0)
        val end = Coordinate(999, 0)

        "set lights on when toggled " in {
          val init = LightBoard(rows, columns)

          val ops = for {
            _         <- toggle(start, end)
            founded   <- findIn(start, end, LightOff)
          } yield founded

          val exec = ops.runA(init).value

          exec should {
            matchPattern { case None => }
          }

        }

        "set lights off when toggled twice" in {
          val init = LightBoard(rows, columns)

          val ops = for {
            _ <- toggle(start, end)
            _ <- toggle(start, end)
            founded <- findIn(start, end, LightOn)
          } yield founded

          val exec = ops.runA(init).value

          exec should matchPattern {
            case None =>
          }
        }
        "set lights on when toggled thrice" in {
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

          def turnOnAllLights(rows: Int, columns: Int): State[LightBoard, Unit] ={
            val start = Coordinate(0, 0)
            val end = Coordinate(rows, columns)
            turnOn(start, end)
          }

          val ops = for {
            _         <- turnOnAllLights(rows, columns)
            _         <- turnOff(start, end)
            lightOn   <- findIn(start, end, LightOn)
            lightOff  <- findIn( start |+| Coordinate(2, 2), end |+| Coordinate(2, 2), LightOn)
          } yield (lightOn, lightOff)

          ops.runA(init).value should matchPattern{
            case (None, Some(_)) =>
          }
        }
      }

      "execute multiple commands" should {
        "return 998,996 lights on after instructions" in {
          val init = LightBoard(rows, columns)
          import LightBoardUtils.CoordinateBuilder
          import scala.language.postfixOps

          val exec = for {
            _ <- turnOn((0, 0) toCord, (999, 999) toCord)
            _ <- toggle((0, 0) toCord, (999, 0) toCord)
            _ <- turnOff((499, 499) toCord, (500, 500) toCord)
            brightness <- totalBrightness
          } yield brightness

          val realExpectedResult = 998996
          val expectedBrightness = (rows * columns) + 2 * (rows * 1) - (2 * 2)
          assert(exec.runA(init).value == realExpectedResult)
        }
      }
//                  "knows how many lights are lit" in {
//                    val aLightBoard = LightBoard(rows, columns)
//                    aLightBoard
//                      .turnOn(Coordinate(887, 9), Coordinate(959, 629))
//                      .turnOn(Coordinate(454, 398), Coordinate(844, 448))
//                      .turnOff(Coordinate(539, 243), Coordinate(559, 965))
//                      .turnOff(Coordinate(370, 819), Coordinate(676, 868))
//                      .turnOff(Coordinate(145, 40), Coordinate(370, 997))
//                      .turnOff(Coordinate(301, 3), Coordinate(808, 453))
//                      .turnOn(Coordinate(351, 678), Coordinate(951, 908))
//                      .toggle(Coordinate(720, 196), Coordinate(897, 994))
//                      .toggle(Coordinate(831, 394), Coordinate(904, 860))
//
//                    val assumptionLights = 230022
//                    assert(aLightBoard.count((light: LightState) => light == LightOn) == assumptionLights)
//                    assert(aLightBoard.totalBrightness == 539560)
//                  }
                }

//                "test" in {
//                    val s: State[LightBoard, Record] = for {
//                      _ <- updateLightBoard((Coordinate(0,0), Light()))
//                      ma <- updateLightBoard((Coordinate(0,1), Light(LightOn, 1)))
//                    } yield ma
//
//                  val a = s.run(LightBoard(3,3).matrix).value
//                  print(a._1)
//                }

//                "turn on 0,0 through 0,0" should{
//                  "increase the total brightness by 1" in {
//                    val aLightBoard = LightBoard(rows, columns)
//                    aLightBoard.turnOn(Coordinate (0,0), Coordinate(0,0))
//                    assert(aLightBoard.totalBrightness == 1)
//
//                    val aLightBoard2 = LightBoard(rows, columns)
//                    aLightBoard2.turnOn(Coordinate (0,0), Coordinate(1,1))
//                    assert(aLightBoard2.totalBrightness == 4)
//
//                    assert( (0 to 0).size == 1)
//                  }
//                }
//                "toggle 0,0 through 999,999" should{
//                  "increase the total brightness by 2000000" in{
//                    val aLightBoard = LightBoard(rows, columns)
//                    aLightBoard.toggle(Coordinate (0,0), Coordinate(999,999))
//
//                    assert(aLightBoard.totalBrightness == 2000000)
//                  }
//                }

//                "toggle 0,0 through 1,1 and then turn off" should{
//                  "returns 4" in{
//                    val aLightBoard = LightBoard(3,3)
//                    val start = Coordinate(0, 0)
//                    val end = Coordinate(1, 1)
//
//                    aLightBoard
//                      .toggle( start, end)
//                      .turnOff( start, end)
//
//                    assert(aLightBoard.totalBrightness == 4)
//                    assert(aLightBoard.toggle(start, end).totalBrightness == 12)
//                    assert(aLightBoard.turnOff(start, end).totalBrightness == 8)
//                    assert(aLightBoard.turnOn(start, end).totalBrightness == 12)
//                  }
//    }

  }
}

object LightBoardUtils {
  import light.LightBoard.Coordinate
  implicit class CoordinateBuilder(tuple: (Int, Int)){
    def toCord: Coordinate = Coordinate(tuple._1, tuple._2)
  }
}