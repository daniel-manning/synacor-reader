package models

import java.util.Scanner

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

class OperationSpec extends AnyFreeSpec with Matchers {
  implicit val runningSettings: RunningSettings = RunningSettings("test", debugOutput = false)
  implicit val scanner: Scanner = new Scanner("")



  "Add Operation" - {
    "when given a number of a register must use the register's value" in {
      val programme = Machine(Value(0), Memory(Map.empty, Vector(8,2,0,0,0,0,0,0).map(Value)), Seq.empty)
      val finalProgramme = AddOperation(Value(32768), Value(32769), Value(4)).run()(programme, scanner, runningSettings)

      finalProgramme.memory.get(Value(32768)) mustBe Value(6)
    }
  }

  "Multiply Operation" - {

    "when given a number of a register must use the register's value" in {
      val programme = Machine(Value(0), Memory(Map.empty, Vector(8,2,0,0,0,0,0,0).map(Value)), Seq.empty)
      val finalProgramme = MultiplyOperation(Value(32768), Value(32769), Value(4)).run()(programme, scanner, runningSettings)

      finalProgramme.memory.get(Value(32768)) mustBe Value(8)
    }
  }

  "Mod Operation" - {

    "when given a number of a register must use the register's value" in {
      val programme = Machine(Value(0), Memory(Map.empty, Vector(8,5,0,0,0,0,0,0).map(Value)), Seq.empty)
      val finalProgramme = ModOperation(Value(32768), Value(32769), Value(3)).run()(programme, scanner, runningSettings)

      finalProgramme.memory.get(Value(32768)) mustBe Value(2)
    }
  }

  "Read Memory Operation" - {

    "when given a main memory address must store it's value in another address" in {
      val programme = Machine(Value(0), Memory(Map(Value(155) -> Value(233)), Vector(8,2,0,0,0,0,0,0).map(Value)), Seq.empty)
      val finalProgramme = ReadMemoryOperation(Value(32768), Value(155)).run()(programme, scanner, runningSettings)

      finalProgramme.memory.get(Value(32768)) mustBe Value(233)
    }

    "when given a register address  must store it's value in another address" in {
      val programme = Machine(Value(0), Memory(Map(Value(155) -> Value(233)), Vector(8,155,0,0,0,0,0,0).map(Value)), Seq.empty)
      val finalProgramme = ReadMemoryOperation(Value(32768), Value(32769)).run()(programme, scanner, runningSettings)

      finalProgramme.memory.get(Value(32768)) mustBe Value(233)
    }
  }

  "Write Memory Operation" - {
     "when given a main memory address write to it from value" in {
       val programme = Machine(Value(0), Memory(Map(Value(155) -> Value(233)), Vector(8,2,0,0,0,0,0,0).map(Value)), Seq.empty)
       val finalProgramme = WriteMemoryOperation(Value(155), Value(177)).run()(programme, scanner, runningSettings)

       finalProgramme.memory.get(Value(155)) mustBe Value(177)
     }

    "when given a main memory address write to it from a register stored value" in {
      val programme = Machine(Value(0), Memory(Map(Value(155) -> Value(233), Value(177) -> Value(277)), Vector(177,2,0,0,0,0,0,0).map(Value)), Seq.empty)
      val finalProgramme = WriteMemoryOperation(Value(155), Value(32768)).run()(programme, scanner, runningSettings)

      finalProgramme.memory.get(Value(155)) mustBe Value(177)
    }

    "when given a register stored main memory address write to it from value" in {
      val programme = Machine(Value(0), Memory(Map(Value(155) -> Value(233), Value(177) -> Value(277)), Vector(177,155,0,0,0,0,0,0).map(Value)), Seq.empty)
      val finalProgramme = WriteMemoryOperation(Value(32769), Value(32768)).run()(programme, scanner, runningSettings)

      finalProgramme.memory.get(Value(155)) mustBe Value(177)
    }
  }

  "Ret Operation" - {
    "when there is a value in the stack jump to it" in {
      val programme = Machine(Value(0), Memory(Map.empty, Vector(177,155,0,0,0,0,0,0).map(Value)), Seq(Value(177)))
      val finalProgramme = RetOperation.run()(programme, scanner, runningSettings)

      finalProgramme.pointer mustBe Value(177)
      finalProgramme.stack.length mustBe 0
    }

    "when there is a register in the stack jump to it's value" in {
      val programme = Machine(Value(0), Memory(Map.empty, Vector(177,155,0,0,0,0,0,0).map(Value)), Seq(Value(32768)))
      val finalProgramme = RetOperation.run()(programme, scanner, runningSettings)

      finalProgramme.pointer mustBe Value(177)
      finalProgramme.stack.length mustBe 0
    }

    "when there is a no value in the stack halt" in {
      val programme = Machine(Value(0), Memory(Map.empty, Vector(177,155,0,0,0,0,0,0).map(Value)), Seq.empty)

      intercept[NoSuchElementException](
        RetOperation.run()(programme, scanner, runningSettings)
      )
    }
  }

  "Input Operation" - {
    "take multiple inputs and process one" in {
      //character by character
      implicit val scanner: Scanner = new Scanner("down")
      scanner.useDelimiter("")

      val programme = Machine(Value(0), Memory(Map.empty, Vector(179,155,0,0,0,0,0,0).map(Value)), Seq.empty)
      val finalProgramme = InputOperation(Value(177)).run()(programme, scanner, runningSettings)
      finalProgramme.memory.get(Value(177)) mustBe Value(100)
    }
  }

  "Jump Operation" - {
    "when given an address must move pointer to that address" in {
      val programme = Machine(Value(0), Memory(Map.empty, Vector(0,0,0,0,0,0,0,0).map(Value)), Seq.empty)
      val finalProgramme = JumpOperation(Value(4)).run()(programme, scanner, runningSettings)
      finalProgramme.pointer mustBe Value(4)
    }
  }

  "JumpIfTrue Operation" - {
    "when given a nonzero value must move pointer to supplied address" in {
      val programme = Machine(Value(0), Memory(Map.empty, Vector(0,0,0,0,0,0,0,0).map(Value)), Seq.empty)
      val finalProgramme = JumpIfTrueOperation(Value(4), Value(5)).run()(programme, scanner, runningSettings)
      finalProgramme.pointer mustBe Value(5)
    }

    "when given a register with nonzero value  must move pointer to supplied address" in {
      val programme = Machine(Value(0), Memory(Map.empty, Vector(1,0,0,0,0,0,0,0).map(Value)), Seq.empty)
      val finalProgramme = JumpIfTrueOperation(Value(32768), Value(5)).run()(programme, scanner, runningSettings)
      finalProgramme.pointer mustBe Value(5)
    }

    "when given a zero value must move pointer after instruction set" in {
      val programme = Machine(Value(0), Memory(Map.empty, Vector(0,0,0,0,0,0,0,0).map(Value)), Seq.empty)
      val finalProgramme = JumpIfTrueOperation(Value(0), Value(5)).run()(programme, scanner, runningSettings)
      finalProgramme.pointer mustBe Value(3)
    }

    "when given a register with zero value  must move pointer after instruction set" in {
      val programme = Machine(Value(0), Memory(Map.empty, Vector(0,0,0,0,0,0,0,0).map(Value)), Seq.empty)
      val finalProgramme = JumpIfTrueOperation(Value(32768), Value(5)).run()(programme, scanner, runningSettings)
      finalProgramme.pointer mustBe Value(3)
    }
  }

  "JumpIfFalse Operation" - {
    "when given a zero value must move pointer to supplied address" in {
      val programme = Machine(Value(0), Memory(Map.empty, Vector(0,0,0,0,0,0,0,0).map(Value)), Seq.empty)
      val finalProgramme = JumpIfFalseOperation(Value(0), Value(5)).run()(programme, scanner, runningSettings)
      finalProgramme.pointer mustBe Value(5)
    }

    "when given a register with zero value  must move pointer to supplied address" in {
      val programme = Machine(Value(0), Memory(Map.empty, Vector(0,0,0,0,0,0,0,0).map(Value)), Seq.empty)
      val finalProgramme = JumpIfFalseOperation(Value(32768), Value(5)).run()(programme, scanner, runningSettings)
      finalProgramme.pointer mustBe Value(5)
    }

    "when given a nonzero value must move pointer after instruction set" in {
      val programme = Machine(Value(0), Memory(Map.empty, Vector(0,0,0,0,0,0,0,0).map(Value)), Seq.empty)
      val finalProgramme = JumpIfFalseOperation(Value(1), Value(5)).run()(programme, scanner, runningSettings)
      finalProgramme.pointer mustBe Value(3)
    }

    "when given a register with nonzero value  must move pointer after instruction set" in {
      val programme = Machine(Value(0), Memory(Map.empty, Vector(1,0,0,0,0,0,0,0).map(Value)), Seq.empty)
      val finalProgramme = JumpIfFalseOperation(Value(32768), Value(5)).run()(programme, scanner, runningSettings)
      finalProgramme.pointer mustBe Value(3)
    }
  }

  "Set Operation" - {
    "when given a value must set register to value" in {
      val programme = Machine(Value(0), Memory(Map.empty, Vector(0,0,0,0,0,0,0,0).map(Value)), Seq.empty)
      val finalProgramme = SetOperation(Value(32768), Value(5)).run()(programme, scanner, runningSettings)
      finalProgramme.memory.get(Value(32768)) mustBe Value(5)
    }

    "when given a register address must set register to value" in {
      val programme = Machine(Value(0), Memory(Map.empty, Vector(0,9,0,0,0,0,0,0).map(Value)), Seq.empty)
      val finalProgramme = SetOperation(Value(32768), Value(32769)).run()(programme, scanner, runningSettings)
      finalProgramme.memory.get(Value(32768)) mustBe Value(9)
    }
  }

  "Eq Operation" - {
      "when test values are registers and are equal set address to 1" in {
        val programme = Machine(Value(0), Memory(Map.empty, Vector(9,9,0,0,0,0,0,0).map(Value)), Seq.empty)
        val finalProgramme = EqualOperation(Value(32768), Value(32768), Value(32769)).run()(programme, scanner, runningSettings)
        finalProgramme.memory.get(Value(32768)) mustBe Value(1)
      }

    "when test values are values and are equal set address to 1" in {
      val programme = Machine(Value(0), Memory(Map.empty, Vector(9,9,0,0,0,0,0,0).map(Value)), Seq.empty)
      val finalProgramme = EqualOperation(Value(32768), Value(155), Value(155)).run()(programme, scanner, runningSettings)
      finalProgramme.memory.get(Value(32768)) mustBe Value(1)
    }

    "when test values are registers and are not equal set address to 0" in {
      val programme = Machine(Value(0), Memory(Map.empty, Vector(5,9,0,0,0,0,0,0).map(Value)), Seq.empty)
      val finalProgramme = EqualOperation(Value(32768), Value(32768), Value(32769)).run()(programme, scanner, runningSettings)
      finalProgramme.memory.get(Value(32768)) mustBe Value(0)
    }

    "when test values are values and are not equal set address to 0" in {
      val programme = Machine(Value(0), Memory(Map.empty, Vector(9,9,0,0,0,0,0,0).map(Value)), Seq.empty)
      val finalProgramme = EqualOperation(Value(32768), Value(144), Value(155)).run()(programme, scanner, runningSettings)
      finalProgramme.memory.get(Value(32768)) mustBe Value(0)
    }
  }

  "Greater Than Operation" - {
    "when test values are registers and a is greater than b set address to 1" in {
      val programme = Machine(Value(0), Memory(Map.empty, Vector(9,8,0,0,0,0,0,0).map(Value)), Seq.empty)
      val finalProgramme = GreaterThanOperation(Value(32768), Value(32768), Value(32769)).run()(programme, scanner, runningSettings)
      finalProgramme.memory.get(Value(32768)) mustBe Value(1)
    }

    "when test values are values and a is greater than b set address to 1" in {
      val programme = Machine(Value(0), Memory(Map.empty, Vector(9,9,0,0,0,0,0,0).map(Value)), Seq.empty)
      val finalProgramme = GreaterThanOperation(Value(32768), Value(155), Value(122)).run()(programme, scanner, runningSettings)
      finalProgramme.memory.get(Value(32768)) mustBe Value(1)
    }

    "when test values are registers and a is smaller than b set address to 0" in {
      val programme = Machine(Value(0), Memory(Map.empty, Vector(5,9,0,0,0,0,0,0).map(Value)), Seq.empty)
      val finalProgramme = GreaterThanOperation(Value(32768), Value(32768), Value(32769)).run()(programme, scanner, runningSettings)
      finalProgramme.memory.get(Value(32768)) mustBe Value(0)
    }

    "when test values are values and a is smaller than b set address to 0" in {
      val programme = Machine(Value(0), Memory(Map.empty, Vector(9,9,0,0,0,0,0,0).map(Value)), Seq.empty)
      val finalProgramme = GreaterThanOperation(Value(32768), Value(144), Value(155)).run()(programme, scanner, runningSettings)
      finalProgramme.memory.get(Value(32768)) mustBe Value(0)
    }
  }


  "Push operation" - {
    "when a value is pushed it must go on the stack" in {
      val programme = Machine(Value(0), Memory(Map.empty, Vector(0,0,0,0,0,0,0,0).map(Value)), Seq.empty)
      val finalProgramme = PushOperation(Value(155)).run()(programme, scanner, runningSettings)
      finalProgramme.stack.head mustBe Value(155)
    }

    "when a register value is pushed it must go on the stack" in {
      val programme = Machine(Value(0), Memory(Map.empty, Vector(9,9,0,0,0,0,0,0).map(Value)), Seq.empty)
      val finalProgramme = PushOperation(Value(32768)).run()(programme, scanner, runningSettings)
      finalProgramme.stack.head mustBe Value(9)
    }
  }

  "Pop Operation" - {
    "when a value is on the stack it must be popped and stored" in {
      val programme = Machine(Value(0), Memory(Map.empty, Vector(0,0,0,0,0,0,0,0).map(Value)), Seq(Value(155)))
      val finalProgramme = PopOperation(Value(32768)).run()(programme, scanner, runningSettings)
      finalProgramme.memory.get(Value(32768)) mustBe Value(155)
    }

    "when there is no value on the stack an error is thrown" in {
      val programme = Machine(Value(0), Memory(Map.empty, Vector(9,9,0,0,0,0,0,0).map(Value)), Seq.empty)
      intercept[NoSuchElementException](
        PopOperation(Value(32768)).run()(programme, scanner, runningSettings)
      )
    }
  }

  "Bitwise AND Operation" - {
    "two values have bitwise AND done and value stored in address" in {
      val programme = Machine(Value(0), Memory(Map.empty, Vector(0,0,0,0,0,0,0,0).map(Value)), Seq.empty)
      val finalProgramme = BitwiseANDOperation(Value(32768), Value(111), Value(121)).run()(programme, scanner, runningSettings)
      finalProgramme.memory.get(Value(32768)) mustBe Value(105)
    }

    "two registers have bitwise AND done and value stored in address" in {
      val programme = Machine(Value(0), Memory(Map.empty, Vector(111,121,0,0,0,0,0,0).map(Value)), Seq.empty)
      val finalProgramme = BitwiseANDOperation(Value(32768), Value(32768), Value(32769)).run()(programme, scanner, runningSettings)
      finalProgramme.memory.get(Value(32768)) mustBe Value(105)
    }
  }

  "Bitwise OR Operation" - {
    "two values have bitwise AND done and value stored in address" in {
      val programme = Machine(Value(0), Memory(Map.empty, Vector(0,0,0,0,0,0,0,0).map(Value)), Seq.empty)
      val finalProgramme = BitwiseOROperation(Value(32768), Value(111), Value(121)).run()(programme, scanner, runningSettings)
      finalProgramme.memory.get(Value(32768)) mustBe Value(127)
    }

    "two registers have bitwise OR done and value stored in address" in {
      val programme = Machine(Value(0), Memory(Map.empty, Vector(111,121,0,0,0,0,0,0).map(Value)), Seq.empty)
      val finalProgramme = BitwiseOROperation(Value(32768), Value(32768), Value(32769)).run()(programme, scanner, runningSettings)
      finalProgramme.memory.get(Value(32768)) mustBe Value(127)
    }
  }

  "Bitwise NOT Operation" - {
    "a value have bitwise NOT done and value stored in address" in {
      val programme = Machine(Value(0), Memory(Map.empty, Vector(0,0,0,0,0,0,0,0).map(Value)), Seq.empty)
      val finalProgramme = BitwiseNOTOperation(Value(32768), Value(178)).run()(programme, scanner, runningSettings)
      finalProgramme.memory.get(Value(32768)) mustBe Value(32589)
    }

    "two registers have bitwise OR done and value stored in address" in {
      val programme = Machine(Value(0), Memory(Map.empty, Vector(111,178,0,0,0,0,0,0).map(Value)), Seq.empty)
      val finalProgramme = BitwiseNOTOperation(Value(32768), Value(32769)).run()(programme, scanner, runningSettings)
      finalProgramme.memory.get(Value(32768)) mustBe Value(32589)
    }

    "a zero value should be converted appropriately" in {
      val programme = Machine(Value(0), Memory(Map.empty, Vector(0,0,0,0,0,0,0,0).map(Value)), Seq.empty)
      val finalProgramme = BitwiseNOTOperation(Value(32768), Value(32769)).run()(programme, scanner, runningSettings)
      finalProgramme.memory.get(Value(32768)) mustBe Value(32767)
    }
  }

  "Call Operation" - {
    "must put the next instruction in stack and jump to address" in {
      val programme = Machine(Value(0), Memory(Map.empty, Vector(0, 0, 0, 0, 0, 0, 0, 0).map(Value)), Seq.empty)
      val finalProgramme = CallOperation(Value(144)).run()(programme, scanner, runningSettings)
      finalProgramme.stack.head mustBe Value(2)
      finalProgramme.pointer mustBe Value(144)
    }
  }

}
