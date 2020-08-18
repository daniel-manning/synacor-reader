package models

import java.io.{BufferedReader, BufferedWriter, FileReader, FileWriter}
import scala.jdk.StreamConverters._

object MemoryWriter {
  val pointerFile = "dump/pointer"
  val memoryFile = "dump/memory"
  val registerFile = "dump/register"
  val stackFile = "dump/stack"

  def writeToFile(machine: Machine): Unit = {
    //write out pointer
    val outPointer: BufferedWriter = new BufferedWriter(new FileWriter(pointerFile));
    outPointer.write(machine.pointer.value.toString)
    outPointer.flush()
    outPointer.close()

    //write out memory
    val outMemory: BufferedWriter = new BufferedWriter(new FileWriter(memoryFile));
    val memory = machine.memory.programme.toList.foreach {
      value =>
        outMemory.write(s"(${value._1.value},${value._2.value})")
        outMemory.newLine()
        outMemory.flush()
    }
    outMemory.close()

    //write out register
    val outRegister: BufferedWriter = new BufferedWriter(new FileWriter(registerFile));
    val registers = machine.memory.registers.toList.map(_.value).mkString(",")
    outRegister.write(registers)
    outRegister.flush()
    outRegister.close()

    //write out stack
    val outStack: BufferedWriter = new BufferedWriter(new FileWriter(stackFile));
    val stack = machine.stack.toList.foreach {
      value =>
        outStack.write(value.value.toString)
        outStack.newLine()
        outStack.flush()
    }
    outStack.close()
  }

  def loadFromFile: Machine = {
    //read pointer
    val pointerReader: BufferedReader = new BufferedReader(new FileReader(pointerFile))
    val pointer: Value = Value(pointerReader.readLine().toInt)

    //read memory
    val tupleRegex = "\\((\\d+),(\\d+)\\)".r
    val programmeReader: BufferedReader = new BufferedReader(new FileReader(memoryFile))
    val programme: Map[Value, Value] = programmeReader.lines().toScala(LazyList)
      .map {
        s =>
          val matches = tupleRegex.findAllIn(s)
          //println(s"matches: ${matches}")
          (Value(matches.group(1).toInt), Value(matches.group(2).toInt))
      }.toMap

    //read register
    val registerReader: BufferedReader = new BufferedReader(new FileReader(registerFile))
    val register: IndexedSeq[Value] = registerReader.readLine().split(",").toIndexedSeq.map(s => Value(s.toInt))

    val memory: Memory = Memory(programme, register)

    //read stack
    val stackReader: BufferedReader = new BufferedReader(new FileReader(stackFile))
    val stack: Seq[Value] = stackReader.lines().toScala(LazyList).map(s => Value(s.toInt)).toList

    Machine(pointer, memory, stack)
  }
}
