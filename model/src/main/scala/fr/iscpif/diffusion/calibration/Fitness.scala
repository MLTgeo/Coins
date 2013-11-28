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

import fr.iscpif.diffusion.{ Model, City }

object Fitness {

  def foreignCoins(wallet: Seq[Double], city: City) = wallet.zipWithIndex.filter { case (_, index) => index != city.country }.unzip._1

  def intensity(cities: Seq[City], cityWallets: Seq[Seq[Double]]) =
    for {
      (city, wallet) <- cities zip cityWallets
    } yield {
      val foreignValue = foreignCoins(wallet, city).sum
      val totalValue = wallet.sum
      foreignValue / totalValue
    }

  def diversity(cities: Seq[City], cityWallets: Seq[Seq[Double]]) =
    for {
      (city, wallet) <- cities zip cityWallets
    } yield {
      val fc = foreignCoins(wallet, city)
      val totalFC = fc.sum

      -fc.map(_ / totalFC).map {
        v => if (v == 0.0) 0.0 else v * math.log10(v)
      }.sum / math.log10(fc.size)
    }

  def spatialExtent(cities: Seq[City], cityWallets: Seq[Seq[Double]], distancesToForeign: Seq[Seq[Double]]) =
    for {
      ((city, wallet), diJ) <- cities zip cityWallets zip distancesToForeign
    } yield {
      val fc = foreignCoins(wallet, city)
      assert(fc.size == diJ.size)
      val coinDist = (fc zip diJ).map { case (f, d) => f * d }.sum / fc.sum
      val distMin = diJ.min
      val distMax = diJ.max
      (coinDist - distMin) / (distMax - distMin)
    }

  def distanceToForeignCountry(cities: Seq[City]) = {
    val countries = cities.map { _.country }.distinct.sorted
    val citiesByCountry = cities.groupBy(_.country)
    for {
      city <- cities
    } yield countries.filter(_ != city.country).map {
      country =>
        val otherCities = citiesByCountry(country)
        otherCities.map(oc => Model.distance(city, oc)).sum / otherCities.size
    }

  }
}
