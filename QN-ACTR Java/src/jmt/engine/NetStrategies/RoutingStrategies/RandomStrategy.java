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

package jmt.engine.NetStrategies.RoutingStrategies;

import jmt.engine.NetStrategies.RoutingStrategy;
import jmt.engine.QueueNet.JobClass;
import jmt.engine.QueueNet.NetNode;
import jmt.engine.QueueNet.NodeList;
import jmt.engine.random.engine.RandomEngine;

/**
 * This class implements a random strategy: the output node
 * is chosen with a random number
 * @author  Stefano Omini, Bertoli Marco
 * @version 13/11/2005
 * Reimplemented by Bertoli Marco to correct behaviour with closed classes and sinks
 */
public class RandomStrategy extends RoutingStrategy {
	private RandomEngine randomEngine;
	private int CLOSED_CLASS = JobClass.CLOSED_CLASS;

	public RandomStrategy() {
		randomEngine = RandomEngine.makeDefault();
	}

	/**
	 * Gets the output node, into which the job must be routed, using a random
	 * strategy.
	 * @param Nodes the list of output nodes
	 * @param jobClass class ofcurrent job to be routed
	 * @return The selected node.
	 */
	@Override
	public NetNode getOutNode(NodeList Nodes, JobClass jobClass) {
		int outNodes = Nodes.size();
		// Find output node
		NetNode output;
		if (outNodes > 1) {
			output = Nodes.get((int) Math.floor(randomEngine.raw() * outNodes));
		} else {
			output = Nodes.getFirst();
		}

		// Check to avoid discarding closed classes into a sink
		if (jobClass.getType() == CLOSED_CLASS && output.isSink()) {
			NetNode[] validOutputs = new NetNode[outNodes];
			outNodes = 0; // Here outNodes is used as a counter to valid elements into valisOutputs array
			for (int i = 0; i < Nodes.size(); i++) {
				if (!Nodes.get(i).isSink()) {
					validOutputs[outNodes++] = Nodes.get(i);
				}
			}

			if (outNodes == 0) {
				output = null;
			} else if (outNodes == 1) {
				output = validOutputs[0];
			} else {
				output = validOutputs[(int) Math.floor(randomEngine.raw() * outNodes)];
			}
		}
		return output;
	}
}