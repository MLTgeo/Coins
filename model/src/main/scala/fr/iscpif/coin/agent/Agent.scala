/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.coin.agent

import fr.iscpif.coin.city.City

trait Agent {
  type W

  def city: City
  def wallet: W
}