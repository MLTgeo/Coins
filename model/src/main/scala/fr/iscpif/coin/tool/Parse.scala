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
    distanceDecay: Option[String] = None,
    populationWeight: Option[String] = None,
    mobilRate: Option[String] = None)

  val parser = new OptionParser[Config]("towns", "0.x") {
    def options = Seq(
      opt("t", "towns", "File for towns description") {
        (v: String, c: Config) ⇒ c.copy(towns = Some(v))
      },
      opt("r", "results", "Dir for outputs") {
        (v: String, c: Config) ⇒ c.copy(resultDir = Some(v))
      },
      opt("d", "distanceDecay", "Distance decay parameter interval in format min, max, step") {
        (v: String, c: Config) ⇒ c.copy(distanceDecay = Some(v))
      },
      opt("p", "populationWeight", "Population weight parameter interval in format min, max, step") {
        (v: String, c: Config) ⇒ c.copy(populationWeight = Some(v))
      },
      opt("m", "mobilRate", "mobility rate interval in format min, max, step") {
        (v: String, c: Config) ⇒ c.copy(mobilRate = Some(v))
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
          val distanceDecay = {
            val i = parseInterval(c.distanceDecay.getOrElse(error("Distance decay interval not defined")))
            i(0).toDouble to i(1).toDouble by i(2).toDouble
          }
          val populationWeight = {
            val i = parseInterval(c.populationWeight.getOrElse(error("Population weight interval not defined")))
            i(0).toDouble to i(1).toDouble by i(2).toDouble
          }
          val mobilRate = {
            val i = parseInterval(c.mobilRate.getOrElse(error("Mobility rate interval not defined")))
            i(0).toDouble to i(1).toDouble by i(2).toDouble
          }

        }
    } match {
      case None => error("Unable to parse command line " + args.mkString(" "))
      case Some(c) => c
    }

}
