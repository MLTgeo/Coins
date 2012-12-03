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

import java.util.Random

trait Model {

  def distanceDecay: Double
  def populationWeight: Double
  def mobilRate(city: City): Double
  def steps: Int
  def exchangeRate: Double = 0.5

  def endOfStep(step: Int, agents: Iterable[Agent])

  def distance(c1: City, c2: City) = math.hypot(c2.x - c1.x, c2.y - c1.y)

  def interactionProbability(source: City, system: Iterable[City]): Iterable[(City, Double)] = {
    val otherCities = system.filterNot(_.id == source.id)
    val absoluteAttractivity =
      otherCities.map {
        c =>
          (c, math.pow(distance(source, c), -distanceDecay) * math.pow(c.population, populationWeight))
      }

    val opportunity = absoluteAttractivity.map {
      case (_, attractivity) => attractivity
    }.sum

    absoluteAttractivity.map {
      case (c, attractivity) => (c, attractivity / opportunity)
    }
  }

  def nbCommuters(city: City, system: Iterable[City]) = {
    val nbMobile = math.round(city.population * mobilRate(city)).toInt
    interactionProbability(city, system).map {
      case (destination, proportion) =>
        (destination, math.round(proportion * nbMobile).toInt)
    }
  }

  def buildWallet(city: City, nbCountry: Int) =
    new Wallet(Array.tabulate(nbCountry)(i => if (i == city.country) 1.0 else 0.0))

  def populationMoves(mobiles: Int, attractivities: Map[City, Double]) =
    attractivities.map {
      case (destination, attractivity) => (destination, math.round(mobiles * attractivity).toInt)
    }

  def exchange(firstAgent: Agent, secondAgent: Agent, countries: Iterable[Int]) =
    for (country <- countries) {
      val firstMoneyBag = firstAgent.wallet(country)
      val secondMoneyBag = secondAgent.wallet(country)
      val global = (firstMoneyBag + secondMoneyBag) / 2
      firstAgent.wallet(country) = global
      secondAgent.wallet(country) = global
    }

  def exchangeInCities(agentsByPlace: Iterable[(City, IndexedSeq[Agent])], countries: Iterable[Int])(implicit rng: Random) =
    for ((city, agents) <- agentsByPlace) {
      val nbExchange = math.round(agents.size * exchangeRate).toInt
      for (i <- 0 until nbExchange) {
        val firstAgent = agents(rng.nextInt(agents.size))
        val secondAgent = agents(rng.nextInt(agents.size))
        exchange(firstAgent, secondAgent, countries)
      }
    }

  def run(system: Iterable[City])(implicit rng: Random) = {
    val countries = system.groupBy(_.country).keys

    val agents =
      system.flatMap {
        source =>
          val mobileAgents =
            nbCommuters(source, system).flatMap {
              case (destination, nbCommuters) => (0 until nbCommuters).map(i => new Agent(source, destination, buildWallet(source, countries.size)))
            }
          val staticAgents = (0 until source.population - mobileAgents.size).map(i => new Agent(source, source, buildWallet(source, countries.size)))
          mobileAgents.toList ::: staticAgents.toList
      }.toIndexedSeq

    val agentsBySource = agents.groupBy(_.city)
    val agentsByDestination = agents.groupBy(_.destination)

    def step = {
      exchangeInCities(agentsByDestination, countries)
      exchangeInCities(agentsBySource, countries)
    }

    (0 until steps).foreach { s => step; endOfStep(s, agents) }
  }

}