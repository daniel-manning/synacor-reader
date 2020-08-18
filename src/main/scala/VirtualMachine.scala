import java.io.{BufferedWriter, File, FileWriter, Writer}
import java.nio.file.{Files, Paths}
import java.nio.{ByteBuffer, ByteOrder}
import java.util.Scanner

import models.{Machine, RunningSettings, Value}

object VirtualMachine extends App {

  //character by character input
  implicit val scanner: Scanner = new Scanner(System.in)
  scanner.useDelimiter("")

  //flush debug logs off the system out
  val out: BufferedWriter = new BufferedWriter(new FileWriter("output-debug.log"));

  implicit val settings: RunningSettings = RunningSettings("Synacor", debugOutput = true, debugWriter = out)

  val byteArray = Files.readAllBytes(Paths.get("../challenge.bin"))

  val intProgrammeList = byteArray.sliding(2,2).map { ba =>
    val bb = ByteBuffer.wrap(ba ++ Array.apply[Byte](0x0, 0x0))
    bb.order(ByteOrder.LITTLE_ENDIAN)
    bb.getInt
  }.toList

  //output op code lists
/*  val file = new File("output.debug")
  val bw = new BufferedWriter(new FileWriter(file))
  bw.write(intProgrammeList.mkString(","))
  bw.close()*/


  val machine = Machine.loadProgramme(intProgrammeList.map(Value))

  machine.runProgramme()
}
