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

package jmt.engine.NetStrategies;

import jmt.common.AutoCheck;
import jmt.engine.QueueNet.JobClass;
import jmt.engine.QueueNet.NetNode;
import jmt.engine.QueueNet.NodeList;

/**
 * Use this class to implement a specific routing strategy. A routing
 * strategy is a rule which selects an output node from a node list.
 * @author Francesco Radaelli
 * @author Bertoli Marco 13-11-2005 (Added job class)
 */
public abstract class RoutingStrategy implements AutoCheck {

	/**
	 * This method should be overridden to implement a specific strategy.
	 * @param Nodes List of nodes.
	 * @param jobClass class ofcurrent job to be routed
	 * @return Selected node .
	 */
	public abstract NetNode getOutNode(NodeList Nodes, JobClass jobClass);

	public boolean check() {
		return true;
	}

  
}