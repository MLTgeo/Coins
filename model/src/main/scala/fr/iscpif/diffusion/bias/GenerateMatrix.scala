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

import fr.iscpif.diffusion.{ City, Model }
import java.io.File
import fr.iscpif.mgo._
import scala.util.Random
import fr.iscpif.mgo.tools.Lazy
import scalax.io.Resource

object GenerateMatrix extends App {

  val empiricalCities = Model.readCities(new File(args(0)))
  // val distanceMatrix = Array.fill(cities.size, cities.size)

  def distanceMatrix(cities: List[City]) = {
    def recursion(cities: List[City]): List[List[Double]] =
      cities match {
        case Nil => Nil
        case h :: otherCities =>
          otherCities.map(to => Model.distance(h, to)) :: recursion(otherCities)
      }
    TriangularMatrix(recursion(cities))
  }

  val empiricalMatrix = distanceMatrix(empiricalCities.toList)
  val empiricalSum = empiricalMatrix.full.flatten.sum

  def closestNeighbours(distance: TriangularMatrix) =
    for {
      (line, i) <- distance.full.zipWithIndex
    } yield {
      (line zipWithIndex).filter { case (_, j) => i != j }.minBy { case (d, _) => d }._2
    }

  def newCities(positions: Seq[Double]) =
    for {
      (c, newPos) <- empiricalCities zip positions.grouped(2).toList
    } yield c.copy(x = newPos(0), y = newPos(1))

  def fitness(distanceMatrix: TriangularMatrix) = {
    val syntheticSum = distanceMatrix.full.flatten.sum
    math.abs(empiricalSum - syntheticSum)
  }

  def perturbation(m1: TriangularMatrix, m2: TriangularMatrix) = {
    val nM1 = closestNeighbours(m1)
    val nM2 = closestNeighbours(m2)
    (nM1 zip nM2).count { case (n1, n2) => n1 != n2 }
  }

  def perturbations(matrices: Seq[TriangularMatrix]): Seq[Lazy[Double]] = {
    for {
      m1 <- matrices
    } yield Lazy(matrices.par.map { m2 => perturbation(m1, m2) }.sum.toDouble)
  }

  val minX = empiricalCities.map { _.x }.min
  val maxX = empiricalCities.map { _.x }.max
  val minY = empiricalCities.map { _.y }.min
  val maxY = empiricalCities.map { _.y }.max

  trait GenerateMatrix extends GAProblem {
    type P = TriangularMatrix

    lazy val min =
      (0 until empiricalCities.size * 2).map {
        i => if (i % 2 == 0) minX else minY
      }

    lazy val max =
      (0 until empiricalCities.size * 2).map {
        i => if (i % 2 == 0) maxX else maxY
      }

    def express(g: G): P = {
      val syntheticCities = newCities(values.get(g))
      distanceMatrix(syntheticCities.toList)
    }

    override def apply(x: P, rng: Random) = MGFitness(fitness(x))

  }

  implicit val rng = new Random(42)

  val p =
    new GenerateMatrix with NoveltyModifier with OptimumDiversityArchive with NonDominatedElitism with MG with GAGenome with CrowdingDiversity with GeneticBreeding with BinaryTournamentSelection with TournamentOnRankAndDiversity with GaussianMutation with SBXBoundedCrossover with StrictDominance with CounterTermination {
      //new GenerateMatrix with Evolution with MG with GAGenome with CounterTermination with NonDominatedElitism with GaussianMutation with SBXBoundedCrossover with ParetoRanking with StrictDominance with NoArchive with GeneticBreeding with BinaryTournamentSelection with TournamentOnRankAndDiversity with IndividualDiversityModifier with CrowdingDiversity {
      //new GenerateMatrix with Evolution with MG with GAGenome with CounterTermination with NonDominatedElitism with GaussianMutation with SBXBoundedCrossover with ParetoRanking with StrictDominance with NoArchive with GeneticBreeding with BinaryTournamentSelection with TournamentOnRankAndDiversity with RankDiversityModifier with CrowdingDiversity {

      def isGood(individual: Individual[G, P, F]): Boolean = individual.fitness.values.head < 10000

      def genomeSize = empiricalCities.size * 2
      def archiveSize = 100
      def mu = 100
      def steps = 100000
      def lambda = 100
      def sigma = 0.01
      def individualDistance(g: Seq[Individual[G, P, F]]) = perturbations(g.map(_.phenotype))
    }

  val res =
    p.evolve.untilConverged {
      s =>
        //println(s.individuals.flatMap(_.fitness.values).mkString(","))
        println(s.generation + " " + s.individuals.flatMap(_.fitness.values).sum)
        save(s.individuals.map(i => newCities(p.values.get(p.scale(i.genome)))), s.generation)
    }.individuals

  def save(configuations: Seq[Seq[City]], generation: Int) = {
    for {
      (cities, i) <- configuations.zipWithIndex
    } {
      val output = Resource.fromFile(s"/tmp/matrices/matrix$generation/$i.csv")
      for {
        c <- cities
      } output.append(s"${c.x},${c.y}\n")
    }
  }

}
