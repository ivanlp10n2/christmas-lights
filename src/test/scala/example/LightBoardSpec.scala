package example

import example.LightBoardUtils._
import light.LightBoard.Coordinate
import light.{Light, LightBoard, LightOff, LightOn}
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
      val end = (500, 500)
      "turn off the middle four lights" in {
        val lightBoard = LightBoard(rows, columns)

        lightBoard.turnOn((0, 0), (999, 999))
        lightBoard.turnOff(start, end)

        allLightsAreOff(lightBoard, start, end) shouldBe true
        allLightsAreOff(lightBoard, start |+| (2, 2), end |+| (2, 2)) shouldBe false

      }
    }

    "execute multiple commands" should{
      "knows how many lights are lit" in {
        val aLightBoard = LightBoard(rows, columns)
        aLightBoard
          .turnOn((887, 9), (959, 629))
          .turnOn((454, 398), (844, 448))
          .turnOff((539, 243), (559, 965))
          .turnOff((370, 819), (676, 868))
          .turnOff((145, 40), (370, 997))
          .turnOff((301, 3), (808, 453))
          .turnOn((351, 678), (951, 908))
          .toggle((720, 196), (897, 994))
          .toggle((831, 394), (904, 860))

        val assumptionLights = 228698
        assert(aLightBoard.count((light: Light) => light == LightOn()) == assumptionLights)
      }
    }
  }

}

object LightBoardUtils {
  def allLightsAreOff(lightBoard: LightBoard, start: Coordinate, end: Coordinate): Boolean =
    lightBoard.findIn(start, end, LightOn()) == None

  def allLightsAreOn(lightBoard: LightBoard, start: Coordinate, end: Coordinate): Boolean =
    lightBoard.findIn(start, end, LightOff()) == None
}