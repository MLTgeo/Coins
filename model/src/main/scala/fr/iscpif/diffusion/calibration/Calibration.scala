/*
 * Copyright (C) 26/06/13 Romain Reuillon
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package fr.iscpif.diffusion.calibration

import scala.io.Source
import org.apache.commons.math3.random.{ Well44497b, RandomAdaptor }
import fr.iscpif.diffusion.tool.Converter._
import fr.iscpif.mgo._
import scala.util.Random
import fr.iscpif.diffusion._

object Calibration extends App {
  //val param = Parse(args)

  val townMatrix =
    Source.fromFile("Inputs.txt").getLines.drop(1).filterNot(_.matches(" *")).map {
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

  val nuts = Source.fromFile("empirics/NUTSID.txt").getLines.drop(1).map {
    l =>
      val array = l.split("\t").toSeq
      array(0) -> array(1).trim.toInt
  }.toMap

  val empirics =
    Source.fromFile("empirics/Empirics.csv").getLines.drop(1).map {
      l =>
        val array = l.split("\t").toArray
        val id = nuts(array(0))
        val t = array(1).toInt
        val ep = array.drop(2).map(_.toDouble).toSeq
        (id, t) -> ep
    }.toMap

  def compute(
    distanceDecay: Double,
    populationWeight: Double,
    mobilRate: Double,
    touristRate: Double,
    exchangeRate: Double,
    seed: Long) = {

    implicit val rng = new RandomAdaptor(new Well44497b(seed))

    val (_distanceDecay, _populationWeight, _mobilRate, _touristRate, _exchangeRate) = (distanceDecay, populationWeight, mobilRate, touristRate, exchangeRate)

    val model = new Model with MoneyExchange {
      def distanceDecay: Double = _distanceDecay
      def populationWeight: Double = _populationWeight
      def mobilRate(city: City): Double = _mobilRate
      def exchangeRate = _exchangeRate
      def touristRate: Double = _touristRate
      override def isHolidays(s: Int) = (s % 12) < 1
    }

    model.states(cities)(rng)
  }

  val seeder = new Random(42)

  val problem = new GAProblem with NSGAII {
    lazy val seeds = Iterator.continually(seeder.nextLong()).take(5).toSeq

    def mu = 200
    def lambda = 200
    def genomeSize = 5
    def steps = 100

    def min = Seq(0.0, 0.0, 0.0, 0.0, 0.0)
    def max = Seq(2.0, 1.0, 1.0, 1.0, 1.0)

    def apply(x: Seq[Double], rng: Random) = {
      val fit =
        seeds.map {
          seed =>
            compute(x(0), x(1), x(2), x(3), x(4), seed).take(121).zipWithIndex.map {
              case (state, step) => evaluate(state, step)
            }.sum
        }.sum
      Vector(fit)
    }

  }

  def evaluate(agents: Seq[Agent], step: Int): Double =
    (Model.agentsToCityWallets(agents, cities) zip cities).flatMap {
      case (wallet, city) =>
        empirics.get(city.id -> step).map {
          targetEP => (wallet zip targetEP).map { case (w, t) => math.pow(w - t, 2) }.sum
        }
    }.sum

  /*compute(0.5, 0.5, 0.5, 0.5, 0.5, 40).take(121).zipWithIndex.map {
    case (state, step) => println(step + " " + evaluate(state, step))
  }.toList  */

  implicit val rng = new Random(42)
  val res = problem.evolve.untilConverged(s => println(s.generation)).individuals
  res.foreach { i => println("genome = " + problem.scale(i.genome) + " fitness = " + i.fitness) }

}