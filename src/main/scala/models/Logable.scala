package models

trait Logable {

  def debugLog(message: String)(implicit settings: RunningSettings): Unit =
    if(settings.debugOutput) println(s"${settings.label} - $message")

}
