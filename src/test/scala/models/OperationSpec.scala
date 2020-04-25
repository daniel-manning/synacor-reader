package models

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

import scala.collection.mutable

class OperationSpec extends AnyFreeSpec with Matchers {
  implicit val runningSettings: RunningSettings = RunningSettings("test", debugOutput = false)

  "Add Operation" - {

    "when given a number of a register must use the register's value" in {
      val programme = Machine(Value(0), Memory(Map.empty, Vector(8,2,0,0,0,0,0,0).map(Value)), Seq.empty)
      val finalProgramme = AddOperation(Value(32768), Value(32769), Value(4)).run()(programme, runningSettings)

      finalProgramme.memory.get(Value(32768)) mustBe Value(6)
    }
  }

  "Jump Operation" - {
    "when given an address must move pointer to that address" in {
      val programme = Machine(Value(0), Memory(Map.empty, Vector(0,0,0,0,0,0,0,0).map(Value)), Seq.empty)
      val finalProgramme = JumpOperation(Value(4)).run()(programme, runningSettings)
      finalProgramme.pointer mustBe Value(4)
    }
  }

  "JumpIfTrue Operation" - {
    "when given a nonzero value must move pointer to supplied address" in {
      val programme = Machine(Value(0), Memory(Map.empty, Vector(0,0,0,0,0,0,0,0).map(Value)), Seq.empty)
      val finalProgramme = JumpIfTrueOperation(Value(4), Value(5)).run()(programme, runningSettings)
      finalProgramme.pointer mustBe Value(5)
    }

    "when given a register with nonzero value  must move pointer to supplied address" in {
      val programme = Machine(Value(0), Memory(Map.empty, Vector(1,0,0,0,0,0,0,0).map(Value)), Seq.empty)
      val finalProgramme = JumpIfTrueOperation(Value(32768), Value(5)).run()(programme, runningSettings)
      finalProgramme.pointer mustBe Value(5)
    }

    "when given a zero value must move pointer after instruction set" in {
      val programme = Machine(Value(0), Memory(Map.empty, Vector(0,0,0,0,0,0,0,0).map(Value)), Seq.empty)
      val finalProgramme = JumpIfTrueOperation(Value(0), Value(5)).run()(programme, runningSettings)
      finalProgramme.pointer mustBe Value(3)
    }

    "when given a register with zero value  must move pointer after instruction set" in {
      val programme = Machine(Value(0), Memory(Map.empty, Vector(0,0,0,0,0,0,0,0).map(Value)), Seq.empty)
      val finalProgramme = JumpIfTrueOperation(Value(32768), Value(5)).run()(programme, runningSettings)
      finalProgramme.pointer mustBe Value(3)
    }
  }

  "JumpIfFalse Operation" - {
    "when given a zero value must move pointer to supplied address" in {
      val programme = Machine(Value(0), Memory(Map.empty, Vector(0,0,0,0,0,0,0,0).map(Value)), Seq.empty)
      val finalProgramme = JumpIfFalseOperation(Value(0), Value(5)).run()(programme, runningSettings)
      finalProgramme.pointer mustBe Value(5)
    }

    "when given a register with zero value  must move pointer to supplied address" in {
      val programme = Machine(Value(0), Memory(Map.empty, Vector(0,0,0,0,0,0,0,0).map(Value)), Seq.empty)
      val finalProgramme = JumpIfFalseOperation(Value(32768), Value(5)).run()(programme, runningSettings)
      finalProgramme.pointer mustBe Value(5)
    }

    "when given a nonzero value must move pointer after instruction set" in {
      val programme = Machine(Value(0), Memory(Map.empty, Vector(0,0,0,0,0,0,0,0).map(Value)), Seq.empty)
      val finalProgramme = JumpIfFalseOperation(Value(1), Value(5)).run()(programme, runningSettings)
      finalProgramme.pointer mustBe Value(3)
    }

    "when given a register with nonzero value  must move pointer after instruction set" in {
      val programme = Machine(Value(0), Memory(Map.empty, Vector(1,0,0,0,0,0,0,0).map(Value)), Seq.empty)
      val finalProgramme = JumpIfFalseOperation(Value(32768), Value(5)).run()(programme, runningSettings)
      finalProgramme.pointer mustBe Value(3)
    }
  }

  "Set Operation" - {
    "when given a value must set register to value" in {
      val programme = Machine(Value(0), Memory(Map.empty, Vector(0,0,0,0,0,0,0,0).map(Value)), Seq.empty)
      val finalProgramme = SetOperation(Value(32768), Value(5)).run()(programme, runningSettings)
      finalProgramme.memory.get(Value(32768)) mustBe Value(5)
    }

    "when given a register address must set register to value" in {
      val programme = Machine(Value(0), Memory(Map.empty, Vector(0,9,0,0,0,0,0,0).map(Value)), Seq.empty)
      val finalProgramme = SetOperation(Value(32768), Value(32769)).run()(programme, runningSettings)
      finalProgramme.memory.get(Value(32768)) mustBe Value(9)
    }
  }

  "Eq Operation" - {
      "when test values are registers and are equal set address to 1" in {
        val programme = Machine(Value(0), Memory(Map.empty, Vector(9,9,0,0,0,0,0,0).map(Value)), Seq.empty)
        val finalProgramme = EqualOperation(Value(32768), Value(32768), Value(32769)).run()(programme, runningSettings)
        finalProgramme.memory.get(Value(32768)) mustBe Value(1)
      }

    "when test values are values and are equal set address to 1" in {
      val programme = Machine(Value(0), Memory(Map.empty, Vector(9,9,0,0,0,0,0,0).map(Value)), Seq.empty)
      val finalProgramme = EqualOperation(Value(32768), Value(155), Value(155)).run()(programme, runningSettings)
      finalProgramme.memory.get(Value(32768)) mustBe Value(1)
    }

    "when test values are registers and are not equal set address to 0" in {
      val programme = Machine(Value(0), Memory(Map.empty, Vector(5,9,0,0,0,0,0,0).map(Value)), Seq.empty)
      val finalProgramme = EqualOperation(Value(32768), Value(32768), Value(32769)).run()(programme, runningSettings)
      finalProgramme.memory.get(Value(32768)) mustBe Value(0)
    }

    "when test values are values and are not equal set address to 0" in {
      val programme = Machine(Value(0), Memory(Map.empty, Vector(9,9,0,0,0,0,0,0).map(Value)), Seq.empty)
      val finalProgramme = EqualOperation(Value(32768), Value(144), Value(155)).run()(programme, runningSettings)
      finalProgramme.memory.get(Value(32768)) mustBe Value(0)
    }
  }

  "Push operation" - {
    "when a value is pushed it must go on the stack" in {
      val programme = Machine(Value(0), Memory(Map.empty, Vector(0,0,0,0,0,0,0,0).map(Value)), Seq.empty)
      val finalProgramme = PushOperation(Value(155)).run()(programme, runningSettings)
      finalProgramme.stack.head mustBe Value(155)
    }

    "when a register value is pushed it must go on the stack" in {
      val programme = Machine(Value(0), Memory(Map.empty, Vector(9,9,0,0,0,0,0,0).map(Value)), Seq.empty)
      val finalProgramme = PushOperation(Value(32768)).run()(programme, runningSettings)
      finalProgramme.stack.head mustBe Value(9)
    }
  }

  "Pop Operation" - {
    "when a value is on the stack it must be popped and stored" in {
      val programme = Machine(Value(0), Memory(Map.empty, Vector(0,0,0,0,0,0,0,0).map(Value)), Seq(Value(155)))
      val finalProgramme = PopOperation(Value(32768)).run()(programme, runningSettings)
      finalProgramme.memory.get(Value(32768)) mustBe Value(155)
    }

    "when there is no value on the stack an error is thrown" in {
      val programme = Machine(Value(0), Memory(Map.empty, Vector(9,9,0,0,0,0,0,0).map(Value)), Seq.empty)
      intercept[NoSuchElementException](
        PopOperation(Value(32768)).run()(programme, runningSettings)
      )
    }
  }

}
