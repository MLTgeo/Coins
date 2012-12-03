/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.coin.discrete

import fr.iscpif.coin.agent.Agent

trait DiscreteAgent extends Agent {
  type W = DiscreteWallet
}
