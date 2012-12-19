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

package fr.iscpif.coin.circular

import java.io._
import java.util.Random
import scala.io.Source
import fr.iscpif.coin.tool.Parse
import org.apache.commons.math3.random.{ RandomAdaptor, Well44497b }

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
      new City(id, country, population, x, y)
  }.toIndexedSeq

  for (distanceDecay <- param.distanceDecay par; populationWeight <- param.populationWeight par; mobilRate <- param.mobilRate par; repli <- 0 until 10 par)
    compute(distanceDecay, populationWeight, mobilRate, repli)

  def compute(
    _distanceDecay: Double,
    _populationWeight: Double,
    _mobilRate: Double,
    repli: Int) = {
    val rng = new RandomAdaptor(new Well44497b(repli))

    val file = new File(param.results, "result" + _distanceDecay + "_" + _populationWeight + "_" + _mobilRate + "_" + repli + ".txt")
    val out = new BufferedWriter((new FileWriter(file)))

    val model = new Model {
      def distanceDecay: Double = _distanceDecay
      def populationWeight: Double = _populationWeight
      def mobilRate(city: City): Double = _mobilRate
      def steps = 100
      def endOfStep(s: Int, agents: Iterable[Agent]) = {
        val citiesCoins = agents.groupBy(_.city.id).toList.map {
          case (c, a) =>
            val coins = a.map(_.wallet.coins).transpose.map(_.sum / a.size)
            (c, coins)
        }.sortBy { case (c, _) => c }.flatMap { case (_, coins) => coins }
        println(s"$distanceDecay,$populationWeight,${_mobilRate},$repli,$s,${citiesCoins.mkString(",")}\n")
      }
    }

    try model.run(cities)(rng)
    finally out.close
  }

}

