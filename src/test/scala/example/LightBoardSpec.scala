package example

import light.LightBoard.Coordinate
import light.{LightBoard, LightOff, LightOn}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class LightBoardSpec extends AnyWordSpec with Matchers {

  "A Light" when {
    "compare subclass" should {
      "differentiate if they're the same" in {
        assert(LightOff() == LightOff())
        assert(LightOn() == LightOn())

        assert(LightOn() != LightOff())
      }
    }
  }

  "A light board configured with 1000x1000 board" when {
    val rows = 1000
    val columns = 1000
    "instantiated" should {
      "be completely turned off" in {
        val aLightBoard = LightBoard(rows, columns)
        aLightBoard.find(LightOn()) should matchPattern { case None => }
        aLightBoard.find(LightOff()) should matchPattern { case Some(_) => }
      }
      "evaluates as a matrix of the same size" in {
        val aLightBoard = LightBoard(rows, columns)
        assert((rows, columns) == aLightBoard.size)
      }
    }

    def allLightsAreOff(lightBoard: LightBoard, start: Coordinate, end: Coordinate): Boolean =
      lightBoard.findIn(start, end, LightOn()) == None

    def allLightsAreOn(lightBoard: LightBoard, start: Coordinate, end: Coordinate): Boolean =
      lightBoard.findIn(start, end, LightOff()) == None

    "turn on 0,0 through 999,999" should {
      val start = (0, 0)
      val end = (999, 999)

      "have only turned on lights" in {
        val aLightBoard = LightBoard(rows, columns)
        aLightBoard.turnOn(start, end)
        val founded = aLightBoard.findIn(start, end, LightOff())
        founded should
          matchPattern { case None => }
      }
    }

    "toggle 0,0 through 999,0" should {
      val start = (0, 0)
      val end = (999, 0)

      "set lights ON when toggled " in {
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
        assert(aLightBoard.findIn(start, end, LightOn()) == None)

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
        assert(aLightBoard.findIn(start, end, LightOn()) == None)

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
      import cats.syntax.semigroup._
      val start = (499, 499)
      val end = (500,500)
      "turn off the middle four lights" in {
        val lightBoard = LightBoard(rows, columns)

        lightBoard.turnOn( (0,0), (999,999))
        lightBoard.turnOff(start, end)

        allLightsAreOff(lightBoard, start, end) shouldBe true
        allLightsAreOff(lightBoard, start |+| (2,2), end |+| (2,2)) shouldBe false

      }
    }

  }
}
