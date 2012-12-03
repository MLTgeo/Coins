/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.coin.tool

import scopt.immutable.OptionParser
import java.io.File

object Parse_calib {

  case class Config(
    towns: Option[String] = None,
    resultDir: Option[String] = None,
    mobilRate1: Option[String] = None,
    mobilRate2: Option[String] = None)

  val parser = new OptionParser[Config]("towns", "0.x") {
    def options = Seq(
      opt("t", "towns", "File for towns description") {
        (v: String, c: Config) ⇒ c.copy(towns = Some(v))
      },
      opt("r", "results", "Dir for outputs") {
        (v: String, c: Config) ⇒ c.copy(resultDir = Some(v))
      },
      opt("m", "mobilRate1", "Mobile rate interval for city a in format min, max, step") {
        (v: String, c: Config) ⇒ c.copy(mobilRate1 = Some(v))
      },
      opt("p", "mobilRate2", "Mobile rate interval for city b in format min, max, step") {
        (v: String, c: Config) ⇒ c.copy(mobilRate2 = Some(v))
      }
    )
  }

  def parseInterval(s: String) = s.split(",")

  def apply(args: Array[String]) =
    parser.parse(args, Config()) map {
      c =>
        new {
          val towns = new File(c.towns.getOrElse(error("Towns not defined")))
          val results = new File(c.resultDir.getOrElse(error("Results not defined")))
          val mobilRate1 = {
            val i = parseInterval(c.mobilRate1.getOrElse(error("Mobile rate interval not defined")))
            i(0).toDouble to i(1).toDouble by i(2).toDouble
          }
          val mobilRate2 = {
            val i = parseInterval(c.mobilRate2.getOrElse(error("Mobile rate interval not defined")))
            i(0).toDouble to i(1).toDouble by i(2).toDouble
          }

        }
    } match {
      case None => error("Unable to parse command line " + args.mkString(" "))
      case Some(c) => c
    }

}
