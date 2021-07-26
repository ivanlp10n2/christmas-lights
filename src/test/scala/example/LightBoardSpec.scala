package example

import example.LightBoardUtils._
import light.LightBoard.Coordinate
import light.{LightBoard, LightOff, LightOn, LightState}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class LightBoardSpec extends AnyWordSpec with Matchers {

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
        val aLightBoard = LightBoard(rows, columns)
        aLightBoard.find(LightOn) should matchPattern { case None => }
        aLightBoard.find(LightOff) should matchPattern { case Some(_) => }
      }
      "evaluates as a matrix of the same size" in {
        val aLightBoard = LightBoard(rows, columns)
        assert((rows, columns) == aLightBoard.size)
      }
    }

    "turn on 0,0 through 999,999" should {
      val start = Coordinate(0, 0)
      val end = Coordinate(999, 999)

      "have only turned on lights" in {
        val aLightBoard = LightBoard(rows, columns)
        aLightBoard.turnOn(start, end)

        val founded = aLightBoard.findIn(start, end, LightOff)
        founded should
          matchPattern { case None => }
      }
    }

    "toggle 0,0 through 999,0" should {
      val start = Coordinate(0, 0)
      val end = Coordinate(999, 0)

      "set lights on when toggled " in {
        val aLightBoard = LightBoard(rows, columns)
        assert(allLightsAreOff(aLightBoard, start, end))

        //when toggled
        val toggledFromOff = aLightBoard.toggle(start, end)

        //returns turned on lights for the range
        assert(allLightsAreOn(toggledFromOff, start, end))
      }
      "set lights off when toggled twice" in {
        val aLightBoard = LightBoard(rows, columns)
        //given turned off lights for the range
        assert(aLightBoard.findIn(start, end, LightOn) == None)

        //when toggled twice
        val toggledTwice = aLightBoard
          .toggle(start, end)
          .toggle(start, end)

        //returns turned on lights for the range
        assert(allLightsAreOff(toggledTwice, start, end))
      }
      "set lights on when toggled thrice" in {
        val aLightBoard = LightBoard(1000, 1000)
        //given turned off lights for the range
        assert(aLightBoard.findIn(start, end, LightOn) == None)

        //when toggled thrice
        val toggledTwice = aLightBoard
          .toggle(start, end)
          .toggle(start, end)
          .toggle(start, end)

        //returns turned on lights for the range
        assert(allLightsAreOn(toggledTwice, start, end))
      }
    }

    "turn off 499,499 through 500,500" should {
      import cats.kernel.Semigroup
      import cats.implicits._
      implicit val combineCoordinate = new Semigroup[Coordinate] {
        override def combine(first: Coordinate, second: Coordinate): Coordinate =
          Coordinate(first.x + second.x, first.y + second.y)
      }

      val start = Coordinate(499, 499)
      val end = Coordinate(500, 500)
      "turn off the middle four lights" in {
        val lightBoard = LightBoard(rows, columns)

        lightBoard.turnOn(Coordinate(0, 0), Coordinate(999, 999))
        lightBoard.turnOff(start, end)

        allLightsAreOff(lightBoard, start, end) shouldBe true
        allLightsAreOff(lightBoard, start |+| Coordinate(2, 2), end |+| Coordinate(2, 2)) shouldBe false

      }
    }

    "execute multiple commands" should{
      "return 998,996 lights on after instructions" in{
        val aLightBoard = LightBoard(rows, columns)

        aLightBoard
          .turnOn(Coordinate (0,0), Coordinate(999,999))
          .toggle(Coordinate (0,0), Coordinate(999,0))
          .turnOff(Coordinate (499,499), Coordinate(500,500))

        val expectedBrightness = (rows * columns) + 2 * (rows * 1) - (2 * 2)
        assert(aLightBoard.totalBrightness == expectedBrightness)
      }
      "knows how many lights are lit" in {
        val aLightBoard = LightBoard(rows, columns)
        aLightBoard
          .turnOn(Coordinate(887, 9), Coordinate(959, 629))
          .turnOn(Coordinate(454, 398), Coordinate(844, 448))
          .turnOff(Coordinate(539, 243), Coordinate(559, 965))
          .turnOff(Coordinate(370, 819), Coordinate(676, 868))
          .turnOff(Coordinate(145, 40), Coordinate(370, 997))
          .turnOff(Coordinate(301, 3), Coordinate(808, 453))
          .turnOn(Coordinate(351, 678), Coordinate(951, 908))
          .toggle(Coordinate(720, 196), Coordinate(897, 994))
          .toggle(Coordinate(831, 394), Coordinate(904, 860))

        val assumptionLights = 230022
        assert(aLightBoard.count((light: LightState) => light == LightOn) == assumptionLights)
        assert(aLightBoard.totalBrightness == 539560)
      }
    }

    "turn on 0,0 through 0,0" should{
      "increase the total brightness by 1" in {
        val aLightBoard = LightBoard(rows, columns)
        aLightBoard.turnOn(Coordinate (0,0), Coordinate(0,0))
        assert(aLightBoard.totalBrightness == 1)

        val aLightBoard2 = LightBoard(rows, columns)
        aLightBoard2.turnOn(Coordinate (0,0), Coordinate(1,1))
        assert(aLightBoard2.totalBrightness == 4)

        assert( (0 to 0).size == 1)
      }
    }
    "toggle 0,0 through 999,999" should{
      "increase the total brightness by 2000000" in{
        val aLightBoard = LightBoard(rows, columns)
        aLightBoard.toggle(Coordinate (0,0), Coordinate(999,999))

        assert(aLightBoard.totalBrightness == 2000000)
      }
    }

    "toggle 0,0 through 1,1 and then turn off" should{
      "returns 4" in{
        val aLightBoard = LightBoard(3,3)
        val start = Coordinate(0, 0)
        val end = Coordinate(1, 1)

        aLightBoard
          .toggle( start, end)
          .turnOff( start, end)

        assert(aLightBoard.totalBrightness == 4)
        assert(aLightBoard.toggle(start, end).totalBrightness == 12)
        assert(aLightBoard.turnOff(start, end).totalBrightness == 8)
        assert(aLightBoard.turnOn(start, end).totalBrightness == 12)
      }
    }

  }


}

object LightBoardUtils {
  def allLightsAreOff(lightBoard: LightBoard, start: Coordinate, end: Coordinate): Boolean =
    lightBoard.findIn(start, end, LightOn) == None

  def allLightsAreOn(lightBoard: LightBoard, start: Coordinate, end: Coordinate): Boolean =
    lightBoard.findIn(start, end, LightOff) == None
}