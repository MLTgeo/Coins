package fr.iscpif.coin.tool

/**
 * Created with IntelliJ IDEA.
 * User: marion
 * Date: 15/02/13
 * Time: 11:07
 * To change this template use File | Settings | File Templates.
 */
object Converter {
  implicit def stringToBoolean(st: String) = st match {
    case "0" => false
    case "1" => true
  }
}
