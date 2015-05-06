/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.diffusion.too

import scopt._
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

  val parser = new OptionParser[Config]("towns") {
    opt[String]('t', "towns") action { (v, c) ⇒ c.copy(towns = Some(v)) } text ("File for towns description")
    opt[String]('r', "results") action { (v, c) ⇒ c.copy(resultDir = Some(v)) } text ("Dir for outputs")
    opt[String]('d', "distanceDecay") action { (v, c) ⇒ c.copy(distanceDecay = Some(v)) } text ("Distance decay parameter interval in format min, max, step")
    opt[String]('p', "populationWeight") action { (v, c) ⇒ c.copy(populationWeight = Some(v)) } text ("Population weight parameter interval in format min, max, step")
    opt[String]('m', "mobilRate") action { (v, c) ⇒ c.copy(mobilRate = Some(v)) } text ("mobility rate interval in format min, max, step")
    opt[String]('s', "samples") action { (v, c) => c.copy(samples = Some(v.split(",").map(_.toInt).toSet)) } text ("Sample times")
  }

  def parseInterval(s: String) = {
    val i = s.split(",")
    i(0).toDouble to i(1).toDouble by i(2).toDouble
  }

  def apply(args: Array[String]) =
    parser.parse(args, Config()) map {
      c =>
        new {
          lazy val towns = new File(c.towns.getOrElse(error("Towns not defined")))
          lazy val results = new File(c.resultDir.getOrElse(error("Results not defined")))
          lazy val distanceDecay = parseInterval(c.distanceDecay.getOrElse(error("Distance decay interval not defined")))
          lazy val populationWeight = parseInterval(c.populationWeight.getOrElse(error("Population weight interval not defined")))
          lazy val mobilRate = parseInterval(c.mobilRate.getOrElse(error("Mobility rate interval not defined")))
          lazy val samples = c.samples
        }
    } match {
      case None    => error("Unable to parse command line " + args.mkString(" "))
      case Some(c) => c
    }

}
