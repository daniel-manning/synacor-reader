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

case class AddOperation(address: Value, valueA: Value, valueB: Value) extends Operation {
  val instructionLength = Value(4)

  def run()(implicit programme: Machine, settings: RunningSettings): Machine = {
    debugLog(s"Running Add Operation with values: (Address: $address, a: $valueA, b: $valueB)")
    programme.copy(pointer = programme.pointer + instructionLength,
                    memory = programme.memory.set(address, interpret(valueA) + interpret(valueB)))
  }
}

case class OuputOperation(outputValue: Value) extends Operation {
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