/*
 * Copyright (C) 2012 Romain Reuillon, Marion Le Texier
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package fr.iscpif.diffusion

import java.io._
import scala.io.Source
import fr.iscpif.diffusion.tool.Parse
import org.apache.commons.math3.random.{ RandomAdaptor, Well44497b }
import fr.iscpif.diffusion.tool.Converter._
import scalax.io.Resource
import Model._

object Simulation extends App {

  val param = Parse(args)

  param.results.mkdirs

  val townMatrix =
    Source.fromFile(param.towns).getLines.drop(1).filterNot(_.matches(" *")).map {
      l => l.split("\t").toArray
    }
  val cities = townMatrix.map {
    line =>
      val id = line(0).toInt
      val country = line(1).toInt
      val population = line(2).toInt
      val x = line(3).toDouble
      val y = line(4).toDouble
      val touristic = line(5)
      City(id, country, population, x, y, touristic)
  }.toIndexedSeq

  for {
    distanceDecay <- param.distanceDecay.par
    populationWeight <- param.populationWeight.par
    mobilRate <- param.mobilRate.par
    replication <- 0 until 100 par
  } compute(distanceDecay, populationWeight, mobilRate, replication)

  def compute(
    distanceDecay: Double,
    populationWeight: Double,
    mobilRate: Double,
    replication: Int) = {

    val rng = new RandomAdaptor(new Well44497b(replication))
    val (_distanceDecay, _populationWeight, _mobilRate) = (distanceDecay, populationWeight, mobilRate)

    val model = new Model with MoneyExchange {
      def distanceDecay: Double = _distanceDecay
      def populationWeight: Double = _populationWeight
      def mobilRate(city: City): Double = _mobilRate
      def touristRate: Double = 0.67
      def exchangeRate: Double = 0.5
      override def isHolidays(s: Int) = (s % 12) < 1
    }

    val file = new File(param.results, "result" + distanceDecay + "_" + populationWeight + "_" + mobilRate + "_" + replication + ".txt")
    file.delete

    val out = Resource.fromFile(file)
    def saveState(s: Int, agents: Seq[Agent]) = {
      val citiesCoins = agentsToCityWallets(agents, cities).flatten
      val write = param.samples.map(_.contains(s)).getOrElse(true)
      if (write) out.append(s"$distanceDecay,$populationWeight,$mobilRate,$replication,$s,${citiesCoins.mkString(",")}\n")
    }

    for {
      (s, i) <- model.states(cities)(rng).take(100).zipWithIndex
    } saveState(i, s)
  }

}

