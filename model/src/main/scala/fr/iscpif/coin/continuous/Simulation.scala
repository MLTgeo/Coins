/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.coin.continuous

import fr.iscpif.coin.city.City
import java.io.File
import java.util.Random
import scala.annotation.tailrec
import scala.io.Source

object Simulation extends App {
  
  val towns = args(0) 
  val resultsDir = args(1) 
  new File(resultsDir).mkdirs
  
  val townMatrix = 
    Source.fromFile(towns).getLines.drop(1).filterNot(_.matches(" *")).map {
      l => l.split("\t").toArray
    }.toArray
  
  val cities = townMatrix.map {
    line =>
    val idCity = line(0).toInt
    //val x = line(1).toDouble
    //val y = line(2).toDouble
    val idCountry = line(3).toInt
    val rank = line(4).toInt
    new City(idCity, idCountry, rank) 
  }
    
  
  def gaussian(sigma: Double, average: Double, rng: Random) = clampedGaussian(rng) * sigma + average
 
  @tailrec def clampedGaussian(rng: Random): Double = {
    val drawn = rng.nextGaussian
    if(drawn < -1 || drawn > 1) clampedGaussian(rng)
    else drawn
  }
  
  val model = 
    new ModelContinuous {
      def exchangeLaw(value: Int, rng: Random) = value match {
        case 0 => gaussian(1, 1, rng)
        case 1 => gaussian(1, 1, rng)
      }
      override val steps = 200
    }
    
  
  val rng = new Random(0)
    
  for(mobilRate2 <- 0.01 to 0.2 by 0.02 par; pop <- 100 to 2000 by 200 par; repli <- 0 until 100 par)
    model.generateEchanges(repli, mobilRate2, pop, cities, resultsDir + "result" + mobilRate2 + "_" + pop + "_"+ repli + ".txt", rng.nextLong)

  
}
  
