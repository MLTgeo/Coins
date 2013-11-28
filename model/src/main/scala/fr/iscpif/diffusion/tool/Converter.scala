package fr.iscpif.diffusion.tool

object Converter {
  implicit def stringToBoolean(st: String) = st match {
    case "0" => false
    case "1" => true
  }
}
