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

package jmt.gui.common.routingStrategies;

import java.util.Map;

/**
 * Created by IntelliJ IDEA.
 * User: orsotronIII
 * Date: 13-lug-2005
 * Time: 10.16.38
 * To change this template use Options | File Templates.
 * Modified by Bertoli Marco
 */
public class ShortestQueueLengthRouting extends RoutingStrategy {

	public ShortestQueueLengthRouting() {
		description = "Jobs are routed to the station with the smallest " + "queue length (number of jobs, current value).";
	}

	@Override
	public String getName() {
		return "Join the Shortest Queue (JSQ)";
	}

	@Override
	public Map<Object, Double> getValues() {
		return null;
	}

	@Override
	public ShortestQueueLengthRouting clone() {
		return new ShortestQueueLengthRouting();
	}

	@Override
	public String getClassPath() {
		return "jmt.engine.NetStrategies.RoutingStrategies.ShortestQueueLengthRoutingStrategy";
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
		return true;
	}

}
