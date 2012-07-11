/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.coin.discrete

class DiscreteWallet(val coins: Array[Array[Int]]) {

  def apply(value: Int, cityId: Int): Int = coins(value)(cityId)
  def update(value: Int, cityId: Int, nb: Int) = coins(value)(cityId) = nb

  def apply(value: Int): Array[Int] = coins(value)
      
  def byValue = coins.zipWithIndex.map { case (coins, value) => (value, coins.sum) }
    
  def +=(wallet: DiscreteWallet) = 
    for((coinsForCities, i) <- wallet.coins.zipWithIndex ; 
        (value, j) <- coinsForCities.zipWithIndex) this(i, j) += value
}
