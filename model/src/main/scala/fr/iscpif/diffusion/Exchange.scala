/*
 * Copyright (C) 28/11/13 Romain Reuillon
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

package fr.iscpif.diffusion

import scala.util.Random

/**
 * Exchange function for a model.
 */
trait Exchange { this: Model =>
  /**
   * Evolve the state of the model by applying the exchanges of between agents.
   * @param agentsByPlace List of agents localized in the same city at a given instant.
   * @param countries All the countries.
   * @param rng A random number generator.
   */
  def exchangeInCities(agentsByPlace: Seq[(City, Seq[Agent])], countries: Seq[Int])(implicit rng: Random): Unit =
    for ((city, agents) <- agentsByPlace) {
      val nbExchange = math.round(agents.size * exchangeRate).toInt
      for (i <- 0 until nbExchange) {
        val firstAgent = agents(rng.nextInt(agents.size))
        val secondAgent = agents(rng.nextInt(agents.size))
        exchange(firstAgent, secondAgent, countries)
      }
    }

  /**
   * Modify the agent to apply an exchange.
   *
   * @param firstAgent One agent.
   * @param secondAgent Another agent.
   * @param countries All the countries.
   */
  def exchange(firstAgent: Agent, secondAgent: Agent, countries: Seq[Int]): Unit
}
