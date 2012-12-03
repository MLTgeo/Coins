/*
* To change this template, choose Tools | Templates
* and open the template in the editor.
*/

package fr.iscpif.coin.circular

import java.io.File
import java.util.Random
import scala.annotation.tailrec
import scala.io.Source
import fr.iscpif.coin.tool.Parse

object Simulation extends App {

  //  val city1 = new City(0, 0, 100, math.cos(0), math.sin(0))
  //  val city2 = new City(1, 1, 100, math.cos(2.0 / 3 * math.Pi), math.sin(2.0 / 3 * math.Pi))
  //  val city3 = new City(2, 2, 100, math.cos(4.0 / 3 * math.Pi), math.sin(4.0 / 3 * math.Pi))

  val city1 = new City(0, 0, 1000, 1, 1)
  val city2 = new City(1, 1, 100, 1, 2)
  val city3 = new City(2, 2, 1000, 1, 3)

  val cities = List(city1, city2, city3)

  val model =
    new Model {
      def distanceDecay: Double = 10
      def populationWeight: Double = 1
      def mobilRate(city: City): Double = 0.5
      def steps = 10
      def endOfStep(s: Int, agents: Iterable[Agent]) =
        agents.groupBy(_.city.id).map {
          case (c, a) =>
            val coins = a.map(_.wallet.coins).transpose.map(_.sum / a.size)
            println(s"$s,${c},${coins.mkString(",")}")
        }
    }

  val rng = new Random(0)
  model.run(cities)(rng)

}

