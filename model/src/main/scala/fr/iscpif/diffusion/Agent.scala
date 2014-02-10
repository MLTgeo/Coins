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

/**
 * Individual moving and exchanging coins.
 *
 * @param city City of residence.
 * @param destination City of work. Might be equal to source in case of a static agent.
 * @param wallet Wallet containing the diffusion proportions for each country.
 */
case class Agent(
    city: City,
    destination: City,
    wallet: Wallet) {
  def copy = Agent(city, destination, wallet.copy)
}
