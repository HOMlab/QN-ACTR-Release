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

import jmt.common.exception.NetException;
import jmt.engine.NetStrategies.RoutingStrategy;
import jmt.engine.QueueNet.JobClass;
import jmt.engine.QueueNet.NetNode;
import jmt.engine.QueueNet.NodeList;
import jmt.engine.QueueNet.NodeSection;
import jmt.engine.random.engine.RandomEngine;

/**
 * This strategy sends jobs to the resource with the shortest queue.
 * Reimplemented by Bertoli Marco 11-nov-2005 (original was totally wrong)
 * @author Bertoli Marco
 */
public class ShortestQueueLengthRoutingStrategy extends RoutingStrategy {
	private int CLOSED_CLASS = JobClass.CLOSED_CLASS;
	private int infinity = Integer.MAX_VALUE;
	private byte inputSection = NodeSection.INPUT;
	private byte serviceSection = NodeSection.SERVICE;
	private int property = NodeSection.PROPERTY_ID_RESIDENT_JOBS;
	private RandomEngine random = RandomEngine.makeDefault();

	/** This strategy selects the resource with the shortest queue
	 * among the output nodes.
	 * @param Nodes List of nodes.
	 * @param jobClass class ofcurrent job to be routed
	 * @return Selected node.
	 */
	@Override
	public NetNode getOutNode(NodeList Nodes, JobClass jobClass) {
		if (Nodes.size() == 0) {
			return null;
		}
		// next nodes candidates... Holds an array of them as on equality of queue lenght they
		// will be chosen randomly
		NetNode[] next = new NetNode[Nodes.size()];
		next[0] = Nodes.get(0);
		int nextLength = 1;

		int shortestQueue;
		try {
			// Sets shortest queue to first node. Note that queue length is job queuing + job served
			// Checks if output is a sink... If class is open, sinks are preferred as have 0 queue length.
			// if class il closed, avoid it
			if (next[0].isSink()) {
				if (jobClass.getType() == CLOSED_CLASS) {
					shortestQueue = infinity;
				} else {
					shortestQueue = 0;
				}
			} else {
				shortestQueue = next[0].getSection(inputSection).getIntSectionProperty(property)
						+ next[0].getSection(serviceSection).getIntSectionProperty(property);
			}

			int tmp;
			for (int i = 1; i < Nodes.size(); i++) {
				// Stores new node in last free space
				next[nextLength] = Nodes.get(i);
				// Checks if output is a sink... If class is open, sinks are preferred as have 0 queue length.
				// if class il closed, avoid it
				if (next[nextLength].isSink()) {
					if (jobClass.getType() == CLOSED_CLASS) {
						tmp = infinity;
					} else {
						tmp = 0;
					}
				} else {
					tmp = next[nextLength].getSection(inputSection).getIntSectionProperty(property)
							+ next[nextLength].getSection(serviceSection).getIntSectionProperty(property);
				}
				if (tmp < shortestQueue) {
					// New minimum value found, put it in position 0 and reset nextLength
					shortestQueue = tmp;
					next[0] = next[nextLength];
					nextLength = 1;
				} else if (tmp == shortestQueue) {
					// This is minimum too, so increase nextLength
					nextLength++;
				}
			}
		} catch (NetException e) {
			System.out.println("Shortest Queue Routing Error: Cannot read queue length from output node");
			e.printStackTrace();
			return null;
		}
		if (shortestQueue == infinity) {
			return null;
		}
		if (nextLength > 1) {
			return next[(int) Math.floor(random.raw() * nextLength)];
		} else {
			return next[0];
		}
	}

}
