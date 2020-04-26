package models

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

class MachineSpec extends AnyFreeSpec with Matchers {

  "Machine" - {
    "when given a simple programme should operate correctly" in {
      implicit val runningSettings: RunningSettings = RunningSettings("test", debugOutput = false)

      val beginningState = Machine(
        pointer = Value(0),
        Memory(
          programme = Map.empty,
          registers = Vector(0,0,0,0,0,0,0,0).map(Value)
        ),
        stack = Seq.empty
       ).construct("9,32768,32769,4,19,32768")

      val finalState = beginningState.runProgramme()
      finalState.memory.registers(0)  mustBe Value(4)
    }
  }


}
