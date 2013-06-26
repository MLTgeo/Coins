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

package fr.iscpif.coin.holidays

import java.io._
import scala.io.Source
import fr.iscpif.coin.tool.Parse
import org.apache.commons.math3.random.{ RandomAdaptor, Well44497b }
import fr.iscpif.coin.tool.Converter._
import scalax.io.Resource

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
      new City(id, country, population, x, y, touristic)
  }.toIndexedSeq

  for {
    distanceDecay <- param.distanceDecay.par
    populationWeight <- param.populationWeight.par
    mobilRate <- param.mobilRate.par
    repli <- 0 until 100 par
  } compute(distanceDecay, populationWeight, mobilRate, repli)

  def compute(
    _distanceDecay: Double,
    _populationWeight: Double,
    _mobilRate: Double,
    repli: Int) = {

    val rng = new RandomAdaptor(new Well44497b(repli))

    val file = new File(param.results, "result" + _distanceDecay + "_" + _populationWeight + "_" + _mobilRate + "_" + repli + ".txt")
    file.delete

    val out = Resource.fromFile(file)

    val model = new Model {
      def distanceDecay: Double = _distanceDecay
      def populationWeight: Double = _populationWeight
      def mobilRate(city: City): Double = _mobilRate
      def steps = 10
      override def touristRate: Double = 0.67
      override def isHolidays(s: Int) = (s % 12) < 1

      def endOfStep(s: Int, agents: Iterable[Agent]) = {
        val citiesCoins = agents.groupBy(_.city.id).toList.map {
          case (c, a) =>
            val coins = a.map(_.wallet.coins).transpose.map(_.sum / a.size)
            (c, coins)
        }.sortBy { case (c, _) => c }.flatMap { case (_, coins) => coins }

        val write = param.samples.map(_.contains(s)).getOrElse(true)
        if (write) out.append(s"$distanceDecay,$populationWeight,${_mobilRate},$repli,$s,${citiesCoins.mkString(",")}\n")
      }
    }

    model.run(cities)(rng)
  }

}

