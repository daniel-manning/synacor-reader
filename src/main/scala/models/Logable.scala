package models

trait Logable {

  def debugLog(message: String)(implicit settings: RunningSettings): Unit =
    if(settings.debugOutput) {
      settings.debugWriter.write(s"${settings.label} - $message")
      settings.debugWriter.newLine()
      settings.debugWriter.flush()
    }

}
