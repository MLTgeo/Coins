/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.coin.discrete

import java.io.BufferedWriter
import java.io.BufferedWriter
import java.io.File
import java.io.FileWriter
import java.io.Writer
import java.util.Random
import math._

import fr.iscpif.coin.city._

import org.apache.commons.math3.random.RandomAdaptor
import org.apache.commons.math3.random.Well44497a
import scala.runtime.ScalaRunTime._

trait Coins2Cities {

  def exchangeLaw(value: Int, rng: Random): Double

  val POP1 = 1000
  val POP2 = 1000

  val mobilRate1 = 0.1
  val mobilRate2 = 0.1

  val coins = List(10, 5)
  val exchangeRate = 0.5

  val steps = 200

  def population(city: City) = (if (city.id == 1) POP1 else POP2).toInt
  def defmobilRate(city: City) = if (city.id == 1) mobilRate1 else mobilRate2

  def generateEchanges(expe: Int, mobilRate: Double, POP: Int, cities: Array[City], file: String, seed: Long) = {
    val rng = new RandomAdaptor(new Well44497a(seed))
    val fw = new BufferedWriter(new FileWriter(new File(file)))

    try {

      def buildWallet(city: City, nbCities: Int, coins: List[Int]) = {
        val array = Array.fill(coins.size, nbCities)(0)
        for ((coin, index) <- coins.zipWithIndex) array(index)(city.id) = coin
        new DiscreteWallet(array)
      }

      def buildEmptyWallet(nbCoins: Int, nbCities: Int) = new DiscreteWallet(Array.fill(nbCoins, nbCities)(0))

      val mobilAgents =
        cities.flatMap {
          _city =>
            val nb = round(population(_city) * mobilRate).toInt
            (0 until nb).map {
              i =>
                new DiscreteMobilAgent {
                  val city = _city
                  val wallet = buildWallet(_city, cities.size, coins)
                }
            }

        }

      val staticAgents =
        cities.flatMap {
          _city =>
            val nb = population(_city) //round(population(city.rank) * (1-mobilRate)).toInt
            (0 until nb).map {
              i =>
                new DiscreteStaticAgent {
                  val city = _city
                  val wallet = buildWallet(_city, cities.size, coins)
                }
            }
        }

      val allAgents = mobilAgents ++ staticAgents

      def updateAgents(
        mobilAgents: Iterable[DiscreteMobilAgent],
        staticAgents: Iterable[DiscreteStaticAgent]) = {
        val mobilMoves =
          mobilAgents.map {
            agent => (agent, cities.filter(_ != agent.city).head)
          }

        val staticMoves =
          staticAgents.map {
            agent => (agent, agent.city)
          }

        val moves =
          (mobilMoves ++ staticMoves).groupBy {
            case (_, destination) => destination
          }

        moves.map {
          case (city, agentsCity) =>
            val agentsInCityArray = agentsCity.map { case (agent, _) => agent }.toArray
            val nbExchange = round(agentsCity.size * exchangeRate).toInt
            for (i <- 0 until nbExchange) {
              val sourceAgent = agentsInCityArray(rng.nextInt(agentsCity.size))
              val targetAgent = agentsInCityArray(rng.nextInt(agentsCity.size))
              exchange(sourceAgent, targetAgent)
            }
        }

      }

      def exchange(sourceAgent: DiscreteAgent, targetAgent: DiscreteAgent) = {
        //Given money by type of value
        val give =
          sourceAgent.wallet.byValue.map {
            case (value, quantity) =>
              (value, round(exchangeLaw(value, rng) * quantity).toInt)
          }

        val give2 =
          targetAgent.wallet.byValue.map {
            case (value, quantity) =>
              (value, round(exchangeLaw(value, rng) * quantity).toInt)
          }

        for ((value, quantity) <- give) withdrawCoins(sourceAgent, targetAgent, value, quantity)
        for ((value, quantity) <- give2) withdrawCoins2(targetAgent, sourceAgent, value, quantity)
      }

      def withdrawCoins(sourceAgent: DiscreteAgent, targetAgent: DiscreteAgent, value: Int, quantity: Int) = {
        def filterEmpty(coins: Array[Int]) =
          coins.zipWithIndex.filter { case (nb, _) => nb != 0 }.map { case (_, index) => index }

        val coins = sourceAgent.wallet(value)
        for (i <- 0 until quantity) {
          val notEmpty = filterEmpty(coins)
          val index = notEmpty(rng.nextInt(notEmpty.size))
          coins(index) -= 1
          targetAgent.wallet(value)(index) += 1
        }
      }

      def withdrawCoins2(targetAgent: DiscreteAgent, sourceAgent: DiscreteAgent, value: Int, quantity: Int) = {
        def filterEmpty(coins: Array[Int]) =
          coins.zipWithIndex.filter { case (nb, _) => nb != 0 }.map { case (_, index) => index }

        val coins = targetAgent.wallet(value)
        for (i <- 0 until quantity) {
          val notEmpty = filterEmpty(coins)
          val index = notEmpty(rng.nextInt(notEmpty.size))
          coins(index) -= 1
          sourceAgent.wallet(value)(index) += 1
        }
      }

      def aggregateWallets(agents: Iterable[DiscreteAgent], nbCoins: Int, nbCities: Int) = {
        val cityWallets = Array.fill(nbCities)(buildEmptyWallet(nbCoins, nbCities))
        for (agent <- agents) cityWallets(agent.city.id) += agent.wallet
        cityWallets
      }

      def proportion(wallet: DiscreteWallet) =
        wallet.coins.map {
          coinsByCity =>
            val total = coinsByCity.sum.toDouble
            coinsByCity.map(v => v / total)
        }

      def outputArray(day: Int, proportion: Array[Array[Array[Double]]]) =
        (expe :: day :: mobilRate :: POP :: proportion.toList.flatten.flatten).toArray

      def writeLine(day: Int) = fw.synchronized {
        fw.append(
          outputArray(day, aggregateWallets(allAgents, coins.size, cities.size).map { proportion }).mkString(",")
        )
        fw.append('\n')
      }

      writeLine(0)
      for (d <- 1 to steps) {
        updateAgents(mobilAgents, staticAgents)
        writeLine(d)
      }

    } finally fw.close

  }

}