package models

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

import scala.collection.mutable

class MachineSpec extends AnyFreeSpec with Matchers {

  "Machine" - {
    "when given a simple programme should operate correctly" in {
      val beginningState = Machine(
        pointer = Value(0),
        programme = Map.empty,
        registers = List(0,0,0,0,0,0,0,0).map(Value(_)),
        stack = mutable.Stack.empty
       ).construct("9,32768,32769,4,19,32768")

      val finalState = beginningState.runProgramme()
      finalState.registers.head  mustBe Value(4)
    }
  }


}
