package models

sealed trait Operation {
  val instructionLength: Int
  def run(programme: Machine): Machine
}

case object ExitOperation extends Operation {
  val instructionLength = 1
  def run(programme: Machine): Machine =
    programme
}

case class OuputOperation(outputValue: Value) extends Operation {
  val instructionLength = 2

  def run(programme: Machine): Machine = {
    System.out.print(outputValue.value.toChar)
    programme.copy(pointer = programme.pointer + Value(2))
  }
}

case object NoOperation extends Operation {
  val instructionLength = 1

  def run(programme: Machine): Machine = {
    programme.copy(pointer = programme.pointer + Value(1))
  }
}