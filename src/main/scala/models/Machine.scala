package models

import scala.collection.mutable

//machine should have 8 registers
//unbounded stack
case class Machine (pointer: Value, programme: Map[Value, Value], registers: Seq[Value], stack: mutable.Stack[Value]){
  def nextOperation(): Operation = {
    programme(pointer) match {
      case Value(0) => ExitOperation
      case Value(19) => OuputOperation(programme(pointer + Value(1)))
      case Value(21) => NoOperation
    }
  }

  def runProgramme(): Machine = {
    LazyList.unfold(this) {
      p =>
        val op = p.nextOperation()
        val nextProgramme = op.run(p)

        op match {
          case ExitOperation => None
          case _ => Some((nextProgramme, nextProgramme))
        }
    }.last
  }

  def construct(programmeString: String): Machine = {
    this.copy(
      programme = programmeString.split(",").toList.zipWithIndex.map(l => (Value(l._2), Value(l._1.toInt))).toMap
    )
  }
}
