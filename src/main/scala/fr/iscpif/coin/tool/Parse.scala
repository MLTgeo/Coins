/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.coin.tool

import scopt.immutable.OptionParser
import java.io.File

object Parse {
  
  case class Config(
    towns: Option[String] = None,
    resultDir: Option[String] = None,
    mobilRates: Option[String] = None,
    populations: Option[String] = None)
    
  val parser = new OptionParser[Config]("towns", "0.x") {
    def options = Seq(
      opt("t", "towns", "File for towns description") {
        (v: String, c: Config) ⇒ c.copy(towns = Some(v))
      },
      opt("r", "results", "Dir for outputs") {
        (v: String, c: Config) ⇒ c.copy(resultDir = Some(v))
      },
      opt("m", "mobilrates", "Mobil rate interval in format min, max, step") {
        (v: String, c: Config) ⇒ c.copy(mobilRates = Some(v))
      },
      opt("p", "populations", "Population interval in format min, max, step") {
        (v: String, c: Config) ⇒ c.copy(populations = Some(v))
      }
    )
  }
  
  def parseInterval(s: String) = s.split(",")
  
  def apply(args: Array[String]) =
    parser.parse(args, Config()) map {
      c => new {
        val towns = new File(c.towns.getOrElse(error("Towns not defined")))
        val results = new File(c.resultDir.getOrElse(error("Resuts not defined")))
        val mobilRates = {
          val i = parseInterval(c.mobilRates.getOrElse(error("Mobil rate interval not defined")))
          i(0).toDouble to i(1).toDouble by i(2).toDouble
        }
        val populations = {
          val i = parseInterval(c.populations.getOrElse(error("Mobil rate interval not defined")))
          i(0).toInt to i(1).toInt by i(2).toInt
        }
        
      }
    } match {
      case None => error("Unable to parse command line " + args.mkString(" "))
      case Some(c) => c
    }
  
}
