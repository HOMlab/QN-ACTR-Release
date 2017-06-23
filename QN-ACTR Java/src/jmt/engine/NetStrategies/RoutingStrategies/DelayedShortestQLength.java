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
 * <p>Title:</p>
 * <p>Description:</p>
 * 
 * @author Bertoli Marco
 *         Date: 16-nov-2005
 *         Time: 14.27.27
 * TODO provvisoria per testing
 */
public class DelayedShortestQLength extends RoutingStrategy {
	private byte inputSection = NodeSection.INPUT;
	private byte serviceSection = NodeSection.SERVICE;
	private int property = NodeSection.PROPERTY_ID_RESIDENT_JOBS;
	private RandomEngine random = RandomEngine.makeDefault();
	private int num = 0;
	private int[] qlen, newQlen;

	/**
	 * This method should be overridden to implement a specific strategy.
	 *
	 * @param Nodes    List of nodes.
	 * @param jobClass class ofcurrent job to be routed
	 * @return Selected node .
	 */
	@Override
	public NetNode getOutNode(NodeList Nodes, JobClass jobClass) {
		if (qlen == null) {
			qlen = new int[Nodes.size()];
			newQlen = new int[Nodes.size()];
		}
		int[] next = new int[qlen.length];
		try {
			// Simulate delay (need to be reimplemented if used)
			num++;
			if (num == 1) {
				for (int i = 0; i < Nodes.size(); i++) {
					newQlen[i] = Nodes.get(i).getSection(inputSection).getIntSectionProperty(property)
							+ Nodes.get(i).getSection(serviceSection).getIntSectionProperty(property);
				}
			}
			if (num > 8) {
				num = 0;
			} else if (num > 4) { // approx 1 sec passed TODO occorre implementare un giusto test con il calendario
				qlen = newQlen;
				newQlen = new int[Nodes.size()];
			}

			// Finds shortest queue in stored array
			int cur = 1;
			next[0] = 0;
			for (int i = 1; i < qlen.length; i++) {
				if (qlen[i] < next[0]) {
					next[0] = qlen[i];
					cur = 1;
				} else if (qlen[i] == next[0]) {
					next[cur++] = i;
				}
			}
			if (cur == 1) {
				return Nodes.get(next[0]);
			} else {
				return Nodes.get(next[(int) Math.floor(random.raw() * cur)]);
			}
		} catch (NetException e) {
			System.out.println("Error: ");
			e.printStackTrace();
		}
		return null;
	}

}
