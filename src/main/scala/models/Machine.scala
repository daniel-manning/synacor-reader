package models

case class RunningSettings(label: String, debugOutput: Boolean)

case class Memory(programme: Map[Value, Value], registers: IndexedSeq[Value]) extends Logable {
  def get(address: Value)(implicit settings: RunningSettings): Value = {
    //debugLog(s"Trying to get value of position: $address")
    if(address.value < 32768) {
      programme.getOrElse(address, Value(0))
    } else {
      registers(address.value - 32768)
    }
  }

  def set(address: Value, value: Value): Memory = {
    if(address.value < 32768) {
      this.copy(programme = programme.updated(address, value))
    } else {
      this.copy(registers = registers.updated(address.value - 32768, value))
    }
  }
}

//machine should have 8 registers
//unbounded stack
case class Machine (pointer: Value, memory: Memory, stack: Seq[Value]) extends Logable {
  def nextOperation()(implicit settings: RunningSettings): Operation = {
    val nextOpCode = memory.get(pointer)
    //debugLog(s"now running opCode: $nextOpCode")

    nextOpCode match {
      case Value(0) => ExitOperation
      case Value(1) => SetOperation(memory.get(pointer + Value(1)), memory.get(pointer + Value(2)))
      case Value(2) => PushOperation(memory.get(pointer + Value(1)))
      case Value(3) => PopOperation(memory.get(pointer + Value(1)))
      case Value(4) => EqualOperation(memory.get(pointer + Value(1)), memory.get(pointer + Value(2)), memory.get(pointer + Value(3)))
      case Value(5) => GreaterThanOperation(memory.get(pointer + Value(1)), memory.get(pointer + Value(2)), memory.get(pointer + Value(3)))
      case Value(6) => JumpOperation(memory.get(pointer + Value(1)))
      case Value(7) => JumpIfTrueOperation(memory.get(pointer + Value(1)), memory.get(pointer + Value(2)))
      case Value(8) => JumpIfFalseOperation(memory.get(pointer + Value(1)), memory.get(pointer + Value(2)))
      case Value(9) => AddOperation(memory.get(pointer + Value(1)), memory.get(pointer + Value(2)), memory.get(pointer + Value(3)))
      case Value(10) => MultiplyOperation(memory.get(pointer + Value(1)), memory.get(pointer + Value(2)), memory.get(pointer + Value(3)))
      case Value(12) => BitwiseANDOperation(memory.get(pointer + Value(1)), memory.get(pointer + Value(2)), memory.get(pointer + Value(3)))
      case Value(13) => BitwiseOROperation(memory.get(pointer + Value(1)), memory.get(pointer + Value(2)), memory.get(pointer + Value(3)))
      case Value(14) => BitwiseNOTOperation(memory.get(pointer + Value(1)), memory.get(pointer + Value(2)))
      case Value(17) => CallOperation(memory.get(pointer + Value(1)))
      case Value(19) => OuputOperation(memory.get(pointer + Value(1)))
      case Value(21) => NoOperation
    }
  }

  def runProgramme()(implicit settings: RunningSettings): Machine = {
    LazyList.unfold(this) {
      p =>
        val op = p.nextOperation()
        val nextProgramme = op.run()(p, settings)

        op match {
          case ExitOperation => None
          case _ => Some((nextProgramme, nextProgramme))
        }
    }.last
  }

  def construct(programmeString: String): Machine = {
    this.copy(
      memory = Memory(programme = programmeString.split(",").toList.zipWithIndex.map(l => (Value(l._2), Value(l._1.toInt))).toMap,
                      registers = Vector(0,0,0,0,0,0,0,0).map(Value))
    )
  }
}

object Machine {

  def loadProgramme(programme: List[Value]): Machine =
    Machine(pointer = Value(0), Memory(programme.zipWithIndex.map(l => (Value(l._2), l._1)).toMap, Vector(0,0,0,0,0,0,0,0).map(Value)), Seq.empty)



}
