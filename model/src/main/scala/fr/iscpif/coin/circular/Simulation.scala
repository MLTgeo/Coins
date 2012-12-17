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

import java.io.File
import java.util.Random
import scala.annotation.tailrec
import scala.io.Source
import fr.iscpif.coin.tool.Parse
import fr.iscpif.coin.city.City
import fr.iscpif.coin.circular.City

object Simulation extends App {
  //  val city1 = new City(0, 0, 100, math.cos(0), math.sin(0))
  //  val city2 = new City(1, 1, 100, math.cos(2.0 / 3 * math.Pi), math.sin(2.0 / 3 * math.Pi))
  //  val city3 = new City(2, 2, 100, math.cos(4.0 / 3 * math.Pi), math.sin(4.0 / 3 * math.Pi))

  val city1 = new City(0, 0, 1000, 1, 1)
  val city2 = new City(1, 1, 100, 1, 2)
  val city3 = new City(2, 2, 1000, 1, 3)

  /* // val param = Parse(args)

 param.results.mkdirs

  val townMatrix =
    Source.fromFile(param.towns).getLines.drop(1).filterNot(_.matches(" *")).map {
      l => l.split("\t").toArray
    }
  val cities = townMatrix.map {
    line =>
      val idCity = line(0).toInt
      //val x = line(1).toDouble
      //val y = line(2).toDouble
      val idCountry = line(3).toInt
      val rank = line(4).toInt
      new City(idCity, idCountry, rank)
  }.toIndexedSeq*/

  val cities = List(city1, city2, city3)

  val model =
    new Model {
      def distanceDecay: Double = 2
      def populationWeight: Double = 1
      def mobilRate(city: City): Double = 0.5
      def steps = 100
      def endOfStep(s: Int, agents: Iterable[Agent]) =
        agents.groupBy(_.city.id).map {
          case (c, a) =>
            val coins = a.map(_.wallet.coins).transpose.map(_.sum / a.size)
            println(s"$s,${c},${coins.mkString(",")}")
        }
    }

  /*for (distanceDecay <- param.distanceDecay par; populationWeight <- param.populationWeight par; mobilRate <- param.mobilRate par; repli <- 0 until 100 par)
    compute(repli, distanceDecay, populationWeight, mobilRate)

  def compute(repli: Int,
              distanceDecay: Double,
              populationWeigth: Double,
              mobilRate: Double) = model.generateEchanges(repli,
    Vector(distanceDecay, 2),
    Vector(populationWeigth, 1),
    Vector(mobilRate, 0.5),
    cities,
    new File(param.results, "result" + distanceDecay + "_" + populationWeigth + "_" + mobilRate + "_" + repli + ".txt"), rng.nextLong)*/

  val rng = new Random(0)
  model.run(cities)(rng)

}
