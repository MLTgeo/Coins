/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.coin.continuous2

class ContinuousWallet(val coins: Array[Double]) {

  def apply(cityId: Int): Double = coins(cityId)
  def update(cityId: Int, nb: Double) = coins(cityId) = nb

  def +=(wallet: ContinuousWallet) = 
    wallet.coins.zipWithIndex.foreach { case(c, i) => coins(i) += c }
  
}