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
import fr.iscpif.diffusion.too.Parse

import scala.io.Source
import tool._
import org.apache.commons.math3.random._
import fr.iscpif.diffusion.tool.Converter._
import scalax.io.Resource
import Model._

object Simulation extends App {

  val param = Parse(args)

  param.results.mkdirs

  val cities = Model.readCities(param.towns)
  def steps = 100

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
      def exchangeRate: Double = 0.5
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
      (s, i) <- model.states(cities)(rng).take(steps).zipWithIndex
    } saveState(i, s)
  }

}

