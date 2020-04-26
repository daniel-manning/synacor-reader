package models

sealed trait Operation extends Logable {
  val instructionLength: Value
  def run()(implicit programme: Machine, settings: RunningSettings): Machine

  def interpret(value: Value)(implicit programme: Machine, settings: RunningSettings): Value = {
    if(value.value < 32768) {
      value
    } else {
      programme.memory.get(value)
    }
  }
}

case object ExitOperation extends Operation {
  val instructionLength = Value(1)

  def run()(implicit programme: Machine, settings: RunningSettings): Machine = {
    debugLog(s"Running Exit Operation")
    programme
  }
}

case class JumpOperation(address: Value) extends Operation {
  val instructionLength = Value(2)

  def run()(implicit programme: Machine, settings: RunningSettings): Machine = {
    debugLog(s"Running Jump Operation value: moving to (Address: $address)")
    programme.copy(pointer = address)
  }
}

case class JumpIfTrueOperation(testValue: Value, address: Value) extends Operation {
  val instructionLength = Value(3)

  def run()(implicit programme: Machine, settings: RunningSettings): Machine = {
    debugLog(s"Running Jump if true Operation value: moving to (Address: $address for testValue: $testValue)")
    if(interpret(testValue).value > 0) {
      programme.copy(pointer = address)
    } else {
      programme.copy(pointer = programme.pointer + instructionLength)
    }
  }
}

case class JumpIfFalseOperation(testValue: Value, address: Value) extends Operation {
  val instructionLength = Value(3)

  def run()(implicit programme: Machine, settings: RunningSettings): Machine = {
    debugLog(s"Running Jump if false Operation value: moving to (Address: $address for testValue: $testValue)")
    if(interpret(testValue).value == 0) {
      programme.copy(pointer = address)
    } else {
      programme.copy(pointer = programme.pointer + instructionLength)
    }
  }
}


case class SetOperation(address: Value, value: Value) extends Operation {
  val instructionLength = Value(3)

  def run()(implicit programme: Machine, settings: RunningSettings): Machine = {
    debugLog(s"Running Set Operation setting address: $address to value: $value")
    programme.copy(pointer = programme.pointer + instructionLength,
      memory = programme.memory.set(address, interpret(value))
    )
  }
}

case class EqualOperation(address: Value, valueA: Value, valueB: Value) extends Operation {
  val instructionLength = Value(4)

  def run()(implicit programme: Machine, settings: RunningSettings): Machine = {
    debugLog(s"Running Equal Operation setting address: $address by testing equality of a: $valueA and b: $valueB")
    programme.copy(pointer = programme.pointer + instructionLength,
      memory = programme.memory.set(address, if(interpret(valueA) == interpret(valueB)) Value(1) else Value(0))
    )
  }
}

case class GreaterThanOperation(address: Value, valueA: Value, valueB: Value) extends Operation {
  val instructionLength = Value(4)

  def run()(implicit programme: Machine, settings: RunningSettings): Machine = {
    debugLog(s"Running Greater than Operation setting address: $address by testing value of a: $valueA and b: $valueB")
    programme.copy(pointer = programme.pointer + instructionLength,
    memory = programme.memory.set(address, if(interpret(valueA).value > interpret(valueB).value) Value(1) else Value(0))
    )
  }
}

case class BitwiseANDOperation(address: Value, valueA: Value, valueB: Value) extends Operation {
  val instructionLength = Value(4)

  def run()(implicit programme: Machine, settings: RunningSettings): Machine = {
    debugLog(s"Running Bitwise AND Operation setting address: $address with value of a: $valueA and b: $valueB")
    programme.copy(pointer = programme.pointer + instructionLength,
      memory = programme.memory.set(address, Value(interpret(valueA).value & interpret(valueB).value))
    )
  }
}

case class BitwiseOROperation(address: Value, valueA: Value, valueB: Value) extends Operation {
  val instructionLength = Value(4)

  def run()(implicit programme: Machine, settings: RunningSettings): Machine = {
    debugLog(s"Running Bitwise OR Operation setting address: $address with value of a: $valueA and b: $valueB")
    programme.copy(pointer = programme.pointer + instructionLength,
      memory = programme.memory.set(address, Value(interpret(valueA).value | interpret(valueB).value))
    )
  }
}

case class BitwiseNOTOperation(address: Value, value: Value) extends Operation {
  val instructionLength = Value(3)

  def run()(implicit programme: Machine, settings: RunningSettings): Machine = {
    debugLog(s"Running Bitwise NOT Operation setting address: $address with value of a: $value")
    programme.copy(pointer = programme.pointer + instructionLength,
      memory = programme.memory.set(address, Value(~(interpret(value).value) & 0x7fff))
    )
  }
}

case class CallOperation(address: Value) extends Operation {
  val instructionLength = Value(2)

  def run()(implicit programme: Machine, settings: RunningSettings): Machine = {
    debugLog(s"Running Call Operation putting next instruction ${programme.pointer + instructionLength} in stack and moving to address: $address")
    programme.copy(pointer = interpret(address),
      stack = (programme.pointer + instructionLength) +: programme.stack
    )
  }
}


case class PushOperation(value: Value) extends Operation {
  val instructionLength = Value(2)

  def run()(implicit programme: Machine, settings: RunningSettings): Machine = {
    debugLog(s"Running Push Operation value: $value on to the stack")
    programme.copy(pointer = programme.pointer + instructionLength,
      stack = interpret(value) +: programme.stack
    )
  }
}

case class PopOperation(address: Value) extends Operation {
  val instructionLength = Value(2)

  def run()(implicit programme: Machine, settings: RunningSettings): Machine = {
    debugLog(s"Running Pop Operation taking value off the stack and storing it at address: $address")
    val value = programme.stack.head
    programme.copy(pointer = programme.pointer + instructionLength,
      memory = programme.memory.set(address, value),
      stack = programme.stack.tail
    )
  }
}

case class AddOperation(address: Value, valueA: Value, valueB: Value) extends Operation {
  val instructionLength = Value(4)

  def run()(implicit programme: Machine, settings: RunningSettings): Machine = {
    debugLog(s"Running Add Operation with values: (Address: $address, a: $valueA, b: $valueB)")
    programme.copy(pointer = programme.pointer + instructionLength,
                    memory = programme.memory.set(address, interpret(valueA) + interpret(valueB)))
  }
}

case class MultiplyOperation(address: Value, valueA: Value, valueB: Value) extends Operation {
  val instructionLength = Value(4)

  def run()(implicit programme: Machine, settings: RunningSettings): Machine = {
    debugLog(s"Running Multiply Operation with values: (Address: $address, a: $valueA, b: $valueB)")
    programme.copy(pointer = programme.pointer + instructionLength,
      memory = programme.memory.set(address, interpret(valueA) * interpret(valueB)))
  }
}

case class ModOperation(address: Value, valueA: Value, valueB: Value) extends Operation {
  val instructionLength = Value(4)

  def run()(implicit programme: Machine, settings: RunningSettings): Machine = {
    debugLog(s"Running Mod Operation with values: (Address: $address, a: $valueA, b: $valueB)")
    programme.copy(pointer = programme.pointer + instructionLength,
      memory = programme.memory.set(address, interpret(valueA) % interpret(valueB)))
  }
}

case class ReadMemoryOperation(addressA: Value, addressB: Value) extends Operation {
  val instructionLength = Value(3)

  def run()(implicit programme: Machine, settings: RunningSettings): Machine = {
    debugLog(s"registers: ${programme.memory.registers}")
    debugLog(s"Running Read Memory Operation with values: (Address: $addressA, addressValue: $addressB)")
    programme.copy(pointer = programme.pointer + instructionLength,
      memory = programme.memory.set(addressA, programme.memory.getFromMainMemory(addressB)))
  }
}

case class WriteMemoryOperation(addressA: Value, value: Value) extends Operation {
  val instructionLength = Value(3)

  def run()(implicit programme: Machine, settings: RunningSettings): Machine = {
    debugLog(s"registers: ${programme.memory.registers}")
    debugLog(s"Running Write Memory Operation with values: (Address: $addressA, value: $value)")
    programme.copy(pointer = programme.pointer + instructionLength,
      memory = programme.memory.set(programme.memory.writeOnlyToMainMemory(addressA), interpret(value)))
  }
}

case object RetOperation extends Operation {
  val instructionLength = Value(1)

  def run()(implicit programme: Machine, settings: RunningSettings): Machine = {
    debugLog(s"Running Ret Operation")
    programme.copy(
      pointer = programme.memory.writeOnlyToMainMemory(programme.stack.head),
      stack = programme.stack.tail
    )
  }
}

case class OutputOperation(outputValue: Value) extends Operation {
  val instructionLength = Value(2)

  def run()(implicit programme: Machine, settings: RunningSettings): Machine = {
    debugLog(s"Running Output Operation: output - $outputValue")
    print(interpret(outputValue).value.toChar)
    programme.copy(pointer = programme.pointer + instructionLength)
  }
}

case object NoOperation extends Operation {
  val instructionLength = Value(1)

  def run()(implicit programme: Machine, settings: RunningSettings): Machine = {
    debugLog(s"Running No Operation")
    programme.copy(pointer = programme.pointer + instructionLength)
  }
}