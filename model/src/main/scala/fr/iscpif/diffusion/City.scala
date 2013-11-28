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
 * A source is a place gathering agents. It might be their place of residence or a destination.
 *
 * @param id Identity of the source from 0 to n. Numbered according to the line in the input file.
 * @param country Belonging country for this source.
 * @param population Total number of agent residing in the source.
 * @param x Longitude of the source.
 * @param y Latitude of the source.
 * @param touristic True if the source is a holiday destination.
 */
case class City(
  id: Int,
  country: Int,
  population: Int,
  x: Double,
  y: Double,
  touristic: Boolean = false)
