/**    
  * Copyright (C) 2006, Laboratorio di Valutazione delle Prestazioni - Politecnico di Milano

  * This program is free software; you can redistribute it and/or modify
  * it under the terms of the GNU General Public License as published by
  * the Free Software Foundation; either version 2 of the License, or
  * (at your option) any later version.

  * This program is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.

  * You should have received a copy of the GNU General Public License
  * along with this program; if not, write to the Free Software
  * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
  */

/**
 * 2013. Modified for QN-Java project
 * 
 * [2013-07-13] change from routing to fork, i.e., rather than just pick one output Node, each connected output Node may receive a copied Job/Entity
 * 
 */

package jmt.gui.common.routingStrategies;

import java.util.HashMap;
import java.util.Map;

/**
 * Created by IntelliJ IDEA.
 * User: orsotronIII
 * Date: 7-lug-2005
 * Time: 15.25.12
 * To change this template use Options | File Templates.
 */
public class ProbabilityRouting extends RoutingStrategy {

	public ProbabilityRouting() {
	  
	  //QN-Java
	  description = "Define the probability for each connected station. "
      + "Each value [0, 1] determines the probability to send a copy through the path.";
	      
//		description = "It is possible to define the routing probability for each connected station. "
//				+ "If the sum of the probabilities is different from 1, all the values will be " + "scaled to sum 1.";
	}

	private HashMap<Object, Double> probabilities = new HashMap<Object, Double>();

	@Override
	public String getName() {
		return "Probabilities";
	}

	@Override
	public Map<Object, Double> getValues() {
		return probabilities;
	}

	@Override
	public ProbabilityRouting clone() {
		ProbabilityRouting pr = new ProbabilityRouting();
		pr.probabilities = new HashMap<Object, Double>(probabilities);
		return pr;
	}

	@Override
	public String getClassPath() {
		return "jmt.engine.NetStrategies.RoutingStrategies.EmpiricalStrategy";
	}

	/**
	 * Returns true if the routing strategy is dependent from the state of
	 * the model
	 * @return  true if the routing strategy is dependent from the state of
	 * the model
	 *
	 * Author: Francesco D'Aquino
	 */
	@Override
	public boolean isModelStateDependent() {
		return false;
	}
}
