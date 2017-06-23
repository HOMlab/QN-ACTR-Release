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
 * This strategy sends jobs to the resource with the least utilization.
 * Reimplemented by Bertoli Marco 12-nov-2005 (original was wrong)
 * @author Bertoli Marco
 */
public class LeastUtilizationRoutingStrategy extends RoutingStrategy {
	private byte serviceSection = NodeSection.SERVICE;
	private int property = NodeSection.PROPERTY_ID_UTILIZATION;
	private double infinity = Double.POSITIVE_INFINITY;
	private int CLOSED_CLASS = JobClass.CLOSED_CLASS;
	private RandomEngine random = RandomEngine.makeDefault();
	private double epsilon = 1e-14; // Used to make equality checks (100 times machine precision)

	/** This strategy selects the resource with the least utilization
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
		// next nodes candidates... Holds an array of them as on equality of utilization they
		// will be chosen randomly
		NetNode[] next = new NetNode[Nodes.size()];
		next[0] = Nodes.get(0);
		int nextLength = 1;
		double leastUtilization;

		try {
			// Sets least utilization to first node
			// Checks if output is a sink... If class is open, sinks are preferred as have 0 utilization.
			// if class il closed, avoid it
			if (next[0].isSink()) {
				if (jobClass.getType() == CLOSED_CLASS) {
					leastUtilization = infinity;
				} else {
					leastUtilization = 0;
				}
			} else {
				leastUtilization = next[0].getSection(serviceSection).getDoubleSectionProperty(property);
			}
			double tmp;
			for (int i = 1; i < Nodes.size(); i++) {
				// Stores new node in last free space
				next[nextLength] = Nodes.get(i);
				// Checks if output is a sink... If class is open, sinks are preferred as have 0 utilization.
				// if class il closed, avoid it
				if (next[nextLength].isSink()) {
					if (jobClass.getType() == CLOSED_CLASS) {
						tmp = infinity;
					} else {
						tmp = 0;
					}
				} else {
					tmp = next[nextLength].getSection(serviceSection).getDoubleSectionProperty(property);
				}

				if (Math.abs(tmp - leastUtilization) < epsilon) {
					// This is minimum too, so increase nextLength
					nextLength++;
				} else if (tmp < leastUtilization) {
					// New minimum value found, put it in position 0 and reset nextLength
					leastUtilization = tmp;
					next[0] = next[nextLength];
					nextLength = 1;
				}
			}
		} catch (NetException e) {
			System.out.println("Least Utilization Routing Error: Cannot read utilization from output node");
			e.printStackTrace();
			return null;
		}
		if (Double.isInfinite(leastUtilization)) {
			return null;
		} else if (nextLength > 1) {
			return next[(int) Math.floor(random.raw() * nextLength)];
		} else {
			return next[0];
		}
	}

}
