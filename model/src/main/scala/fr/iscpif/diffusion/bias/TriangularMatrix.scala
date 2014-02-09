/*
 * Copyright (C) 04/02/14 Romain Reuillon
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

package fr.iscpif.diffusion.bias

object TriangularMatrix {

  def apply(c: Seq[Seq[Double]]) =
    new TriangularMatrix {
      val content = c.map(_.toIndexedSeq).toIndexedSeq
    }

}

trait TriangularMatrix {

  def content: Seq[Seq[Double]]

  def apply(x: Int)(y: Int) =
    if (x == y) 0.0
    else if (x < y) content(x)(y - x - 1) else content(y)(x - y - 1)

  def full: Seq[Seq[Double]] = {
    val side = content.head.size
    (0 until side) map (
      x => (0 until side) map (y => apply(x)(y))
    )
  }

}
