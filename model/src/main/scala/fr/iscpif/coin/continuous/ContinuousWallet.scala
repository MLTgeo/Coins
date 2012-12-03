/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.coin.continuous

class ContinuousWallet(val coins: Array[Array[Double]]) {

  def apply(value: Int, cityId: Int): Double = coins(value)(cityId)
  def update(value: Int, cityId: Int, nb: Double) = coins(value)(cityId) = nb

  def apply(value: Int): Array[Double] = coins(value)

  def byValue = coins.zipWithIndex.map { case (coins, value) => (value, coins.sum) }

  def +=(wallet: ContinuousWallet) =
    for (
      (coinsForCities, i) <- wallet.coins.zipWithIndex;
      (value, j) <- coinsForCities.zipWithIndex
    ) this(i, j) += value
}