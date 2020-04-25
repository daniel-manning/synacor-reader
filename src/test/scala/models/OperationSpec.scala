package models

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

import scala.collection.mutable

class OperationSpec extends AnyFreeSpec with Matchers {

  "Add Operation" - {
    implicit val runningSettings: RunningSettings = RunningSettings("test", debugOutput = false)

    "when given a number of a register must use the register's value" in {
      val programme = Machine(Value(0), Memory(Map.empty, Vector(8,2,0,0,0,0,0,0).map(Value)), mutable.Stack.empty)
      val finalProgramme = AddOperation(Value(32768), Value(32769), Value(4)).run()(programme, runningSettings)

      finalProgramme.memory.get(Value(32768)) mustBe Value(6)
    }
  }

}
