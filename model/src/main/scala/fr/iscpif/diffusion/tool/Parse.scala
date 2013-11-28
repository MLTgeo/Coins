/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.diffusion.tool

import scopt.immutable.OptionParser
import java.io.File
import sys.error

object Parse {

  case class Config(
    towns: Option[String] = None,
    resultDir: Option[String] = None,
    distanceDecay: Option[String] = None,
    populationWeight: Option[String] = None,
    mobilRate: Option[String] = None,
    samples: Option[Set[Int]] = None)

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
      },
      opt("s", "samples", "Sample times") {
        (v: String, c: Config) => c.copy(samples = Some(v.split(",").map(_.toInt).toSet))
      }
    )
  }

  def parseInterval(s: String) = {
    val i = s.split(",")
    i(0).toDouble to i(1).toDouble by i(2).toDouble
  }

  def apply(args: Array[String]) =
    parser.parse(args, Config()) map {
      c =>
        new {
          val towns = new File(c.towns.getOrElse(error("Towns not defined")))
          val results = new File(c.resultDir.getOrElse(error("Results not defined")))
          val distanceDecay = parseInterval(c.distanceDecay.getOrElse(error("Distance decay interval not defined")))
          val populationWeight = parseInterval(c.populationWeight.getOrElse(error("Population weight interval not defined")))
          val mobilRate = parseInterval(c.mobilRate.getOrElse(error("Mobility rate interval not defined")))
          val samples = c.samples
        }
    } match {
      case None => error("Unable to parse command line " + args.mkString(" "))
      case Some(c) => c
    }

}
