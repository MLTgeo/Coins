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

object Wallet {

  /**
   * Build the wallet of an agent.
   * @param city The city of residence of the agent
   * @param nbCountries The total number of countries.
   * @return The wallet of the agent.
   */
  def apply(city: City, nbCountries: Int): Wallet =
    Wallet(Array.tabulate(nbCountries)(i => if (i == city.country) 1.0 else 0.0))

}

/**
 * Object describing the proportion of a series of coins according to their origins
 * @param coins an array of diffusion proportions indexed on the origin id
 */
case class Wallet(coins: Array[Double]) {

  def apply(origin: Int): Double = coins(origin)
  def update(origin: Int, value: Double) = coins(origin) = value

  /**
   * Add another wallet coins in this one
   * @param wallet the wallet to add
   */
  def +=(wallet: Wallet) =
    wallet.coins.zipWithIndex.foreach { case (c, i) => coins(i) += c }

  /**
   * @return the sum of the proportions of coins in the wallet (is different of 1 for cities)
   */
  def total = coins.sum

  /**
   * @return an independent copy of the content of this wallet
   */
  def copy = Wallet(coins.clone())

}