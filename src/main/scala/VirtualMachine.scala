import java.nio.file.{Files, Paths}
import java.nio.{ByteBuffer, ByteOrder}

import models.{Machine, RunningSettings, Value}

object VirtualMachine extends App {

  implicit val settings: RunningSettings = RunningSettings("Synacor", debugOutput = true)

  val byteArray = Files.readAllBytes(Paths.get("../challenge.bin"))

  val intProgrammeList = byteArray.sliding(2,2).map { ba =>
    val bb = ByteBuffer.wrap(ba ++ Array.apply[Byte](0x0, 0x0))
    bb.order(ByteOrder.LITTLE_ENDIAN)
    bb.getInt
  }.toList


  val machine = Machine.loadProgramme(intProgrammeList.map(Value))

  machine.runProgramme()
}
